use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env::var;
use std::fmt;
use std::io;
use std::rc::{Rc, Weak};

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::source::Source;
use crate::sym::Sym;
use crate::SymTable;
use crate::token::*;

pub struct Lexer {
    token: Token,
    source: Source,
    enter: TokenEnter,
    sym_table: Rc<RefCell<SymTable>>
}


#[inline(always)]
fn range(c: char, low: char, high: char) -> bool { return low <= c && c <= high; }
fn sym(c : char) -> bool { return  range(c, 'a', 'z') || range(c, 'A', 'Z') || c == '_'; }
fn space(c : char) -> bool { return c == ' ' || c == '\t' || c == '\n'; }
fn dec_nonzero(c : char) -> bool { return range(c, '1', '9'); }
fn bin(c : char) -> bool { return range(c, '0', '1'); }
fn oct(c : char) -> bool { return range(c, '0', '7'); }
fn dec(c : char) -> bool { return range(c, '0', '9'); }
fn hex(c : char) -> bool { return dec(c) || range(c, 'a', 'f') || range(c, 'A', 'F'); }
fn eE(c : char) -> bool { return c == 'e' || c == 'E'; }
fn sgn(c : char) -> bool{ return c == '+' || c == '-'; }

impl Lexer {
    fn next(&mut self) -> Option<char> {
        self.source.next()
    }

    fn ident(&mut self, str : &mut String) -> bool{
        if self.accept_str_pred(str, sym) {
            while self.accept_str_pred(str, sym) || self.accept_str_pred(str, dec) {}
            return true;
        }
        return false;
    }

    fn eof(&self) -> bool{
        return self.source.peek() == None;
    }

    fn finish(&mut self, kind: TokenKind) {
        self.token.kind = kind;
        self.token.symbol = None;
    }

    fn finish_ident(&mut self, str: String) {
        for kind in TokenKind::iter(){
            if kind.keyword() == str{
                return self.finish(kind);
            }
        }

        self.finish_str(TokenKind::Ident, str)
    }

    fn finish_str(&mut self, kind: TokenKind, str: String) {
        let mut sym_table = RefCell::borrow_mut(&self.sym_table);
        self.token.kind = kind;
        self.token.symbol = Some(sym_table.from(str));
    }

    fn accept_char(&mut self, expect : char) -> bool{
        self.source.accept( |actual| actual == expect)
    }

    fn accept_pred(&mut self, pred: fn(char) -> bool) -> bool{
        self.source.accept(pred)
    }

    fn accept_str(&mut self, str: &mut String) -> bool{
        self.source.accept_str(str)
    }

    fn accept_str_char(&mut self, str: &mut String, expect : char) -> bool{
        self.source.accept_str_pred(str, |actual| actual == expect)
    }

    fn accept_str_pred(&mut self, str: &mut String, pred: fn(char) -> bool) -> bool{
        self.source.accept_str_pred(str, pred)
    }

    pub fn next_token(&mut self) -> Token {
        self.next_token_impl();
        self.token.loc = self.source.loc();
        self.token.enter = self.enter;
        self.enter = TokenEnter::Token;
        self.token.clone()
    }

    fn match_sign(&mut self) -> Option<TokenKind>{
        if let Some(ch) = self.source.peek() {
            let result = Some(match ch {
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Star,
                '^' => TokenKind::Hat,
                '=' => TokenKind::Assign,
                '!' => TokenKind::Not,
                ',' => TokenKind::Comma,
                '.' => TokenKind::Dot,
                ':' => TokenKind::Colon,
                ';' => TokenKind::Semicolon,
                '|' => TokenKind::Or,
                '&' => TokenKind::And,
                '<' => TokenKind::LAngle,
                '>' => TokenKind::RAngle,
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,
                _ => return None
            });
            self.next();
            result
        }else{
            None
        }
    }

    fn next_token_impl(&mut self) {
        loop {
            self.source.reset_loc();

            if self.eof() {
                return self.finish(TokenKind::Eof);
            }

            if self.accept_pred(space) {
                while self.accept_pred(space) {}

                if self.source.last.line != self.source.current.line {
                    self.enter = TokenEnter::NL;
                }else if self.enter == TokenEnter::Token{
                    self.enter = TokenEnter::Space;
                }

                continue;
            }

            if self.accept_char('/') {
                if self.accept_char('*') { // arbitrary comment
                    let mut depth = 1;
                    loop {
                        if self.eof() {
                            return self.finish(TokenKind::Error);
                        }

                        if self.accept_char('/') {
                            if self.accept_char('*') {
                                depth += 1;
                            }
                        } else if self.accept_char('*') {
                            if self.accept_char('/') {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }

                            continue;
                        }

                        self.next();
                    }
                    continue;
                }
                if self.accept_char('/') {
                    loop {
                        if self.eof() {
                            return self.finish(TokenKind::Error);
                        }

                        if self.accept_char('\n') {
                            break;
                        } else {
                            self.next();
                        }
                    }
                    continue;
                }
                return self.finish(TokenKind::Slash);
            }

            if let Some(token) = self.match_sign(){
                return self.finish(token);
            }

            let mut str = String::new();

            // identifiers/keywords
            if self.ident(&mut str) {
                if str == "true" || str == "false" {
                    return self.finish_str(TokenKind::LitBool, str);
                }
                return self.finish_ident(str);
            }

            // char literal
            if self.accept_char('\'') {
                if self.accept_char('\'') {
                    return self.finish(TokenKind::Error);
                }

                self.accept_str_char(&mut str, '\\');
                self.accept_str(&mut str);

                if self.eof() || !self.accept_char('\'') {
                    return self.finish(TokenKind::Error);
                }

                return self.finish_str(TokenKind::LitChar, str);
            }

            // string literal
            if self.accept_char('"') {
                while !self.accept_char('"') {
                    self.accept_str_char(&mut str, '\\');
                    self.accept_str(&mut str);

                    if self.eof() {
                        return self.finish(TokenKind::Error);
                    }
                }
                return self.finish_str(TokenKind::LitStr, str);
            }

            if self.accept_str_pred(&mut str, dec) {
                while self.accept_str_pred(&mut str, dec) {
                    if self.eof() {
                        return self.finish(TokenKind::Error);
                    }
                }

                return if self.accept_str_char(&mut str, '.') {
                    while self.accept_str_pred(&mut str, dec) {
                        if self.eof() {
                            return self.finish(TokenKind::Error);
                        }
                    }

                    self.finish_str(TokenKind::LitReal, str)
                } else {
                    self.finish_str(TokenKind::LitInt, str)
                }
            }

            return self.finish(TokenKind::Error);
        }
    }

    pub fn new(source: Source, sym_table: Rc<RefCell<SymTable>>) -> Lexer {
        Lexer {
            token : Token{
                kind: TokenKind::Error,
                symbol: None,
                loc: Loc {
                    source: 0,
                    begin: Pos::zero(),
                    end: Pos::zero(),
                },
                enter: TokenEnter::NL
            },
            enter: TokenEnter::NL,
            sym_table,
            source,
        }
    }
}