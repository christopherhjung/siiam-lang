use std::env::var;
use std::io;
use std::fmt;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::source::Source;

pub struct Lexer {
    token: Token,
    source: Source,
    enter: TokenEnter
}

#[derive(Debug, Copy, Clone)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
}

impl Pos {
    pub fn zero() -> Pos{
        Pos{ line: 0, col: 0 }
    }
}

#[derive(Debug)]
pub struct Loc {
    pub file: String,
    pub begin: Pos,
    pub end: Pos
}

impl Clone for Loc {
    fn clone(&self) -> Self {
        Loc{
            file: self.file.clone(),
            begin: self.begin,
            end: self.end,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub variant: TokenVariant,
    pub symbol: Option<Symbol>,
    pub loc: Loc,
    pub enter: TokenEnter
}
#[derive(PartialEq, Copy, Clone, Debug, EnumIter)]
pub enum TokenVariant {
    Or,
    And,
    Not,
    Plus,
    Minus,
    Star,
    Hat,
    Slash,
    Assign,

    LAngle, RAngle,
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,
    Comma, Semicolon, Colon, Arrow,
    Dot,
    While, For, If, Else,
    Return,
    Let, Fn, Struct,

    LitString, LitChar, LitInteger, LitReal, LitBool,
    TypeString, TypeUnit, TypeByte, TypeChar, TypeInt, TypeLong, TypeFloat, TypeDouble, TypeBool,

    NL,
    Error,
    Eof,
    Num,
    Identifier
}

impl TokenVariant{
    fn keyword(&self) -> &str{
        match self {
            TokenVariant::While =>  "while",
            TokenVariant::For =>  "for",
            TokenVariant::If =>  "if",
            TokenVariant::Else =>  "else",
            TokenVariant::Let =>  "let",
            TokenVariant::Fn =>  "fn",
            TokenVariant::Return =>  "return",
            TokenVariant::TypeString =>  "String",
            TokenVariant::TypeChar =>  "char",
            TokenVariant::TypeBool =>  "bool",
            TokenVariant::TypeByte =>  "i8",
            TokenVariant::TypeInt =>  "i32",
            TokenVariant::TypeLong =>  "i64",
            TokenVariant::TypeFloat =>  "f32",
            TokenVariant::TypeDouble =>  "f64",
            TokenVariant::TypeUnit =>  "Unit",
            TokenVariant::Struct =>  "struct",
            _ =>  "",
        }
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum TokenEnter {
    Token, Space, NL
}

#[derive(Debug)]
pub struct Symbol {
    pub value: String,
}

impl Symbol{
    fn new( str : String ) -> Symbol{
        Symbol{
            value: str.clone()
        }
    }
}

impl Clone for Symbol {
    fn clone(&self) -> Self {
        Symbol::new(self.value.clone())
    }
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

    fn identifier(&mut self, str : &mut String) -> bool{
        if self.accept_str_pred(str, sym) {
            while self.accept_str_pred(str, sym) || self.accept_str_pred(str, dec) {}
            return true;
        }
        return false;
    }

    fn is_eof(&self) -> bool{
        return self.source.peek() == None;
    }

    fn finish(&mut self, variant: TokenVariant) {
        self.token.variant = variant;
        self.token.symbol = None;
    }

    fn finish_str(&mut self, variant: TokenVariant, str: String) {
        self.token.variant = variant;
        if variant == TokenVariant::Identifier {
            for elem in TokenVariant::iter(){
                if elem.keyword() == str{
                    self.token.variant = elem;
                    self.token.symbol = None;
                    return;
                }
            }
        }
        self.token.symbol = Some(Symbol::new(str));
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

    fn match_sign(&mut self) -> Option<TokenVariant>{
        if let Some(ch) = self.source.peek() {
            let result = Some(match ch {
                '+' => TokenVariant::Plus,
                '-' => TokenVariant::Minus,
                '*' => TokenVariant::Star,
                '^' => TokenVariant::Hat,
                '=' => TokenVariant::Assign,
                '!' => TokenVariant::Not,
                ',' => TokenVariant::Comma,
                '.' => TokenVariant::Dot,
                ':' => TokenVariant::Colon,
                ';' => TokenVariant::Semicolon,
                '|' => TokenVariant::Or,
                '&' => TokenVariant::And,
                '<' => TokenVariant::LAngle,
                '>' => TokenVariant::RAngle,
                '(' => TokenVariant::LParen,
                ')' => TokenVariant::RParen,
                '[' => TokenVariant::LBracket,
                ']' => TokenVariant::RBracket,
                '{' => TokenVariant::LBrace,
                '}' => TokenVariant::RBrace,
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

            if self.is_eof() {
                return self.finish(TokenVariant::Eof);
            }

            if self.accept_pred(space) {
                loop {
                    if !self.accept_pred(space) {
                        break;
                    }
                }
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
                        if self.is_eof() {
                            return self.finish(TokenVariant::Error);
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
                        if self.is_eof() {
                            return self.finish(TokenVariant::Error);
                        };

                        if self.accept_char('\n') {
                            break;
                        } else {
                            self.next();
                        }
                    }
                    continue;
                }
                return self.finish(TokenVariant::Slash);
            }

            if let Some(token) = self.match_sign(){
                return self.finish(token);
            }

            let mut str = String::new();

            // identifiers/keywords
            if self.identifier(&mut str) {
                if str == "true" || str == "false" {
                    return self.finish_str(TokenVariant::LitBool, str);
                }
                return self.finish_str(TokenVariant::Identifier, str);
            }

            // char literal
            if self.accept_char('\'') {
                if self.accept_char('\'') {
                    return self.finish(TokenVariant::Error);
                }

                self.accept_str_char(&mut str, '\\');
                self.accept_str(&mut str);

                if self.is_eof() || !self.accept_char('\'') {
                    return self.finish(TokenVariant::Error);
                }

                return self.finish_str(TokenVariant::LitChar, str);
            }

            // string literal
            if self.accept_char('"') {
                while !self.accept_char('"') {
                    self.accept_str_char(&mut str, '\\');
                    self.accept_str(&mut str);

                    if self.is_eof() {
                        return self.finish(TokenVariant::Error);
                    }
                }
                return self.finish_str(TokenVariant::LitString, str);
            }

            if self.accept_str_pred(&mut str, dec) {
                while self.accept_str_pred(&mut str, dec) {
                    if self.is_eof() {
                        return self.finish(TokenVariant::Error);
                    }
                }

                return if self.accept_str_char(&mut str, '.') {
                    while self.accept_str_pred(&mut str, dec) {
                        if self.is_eof() {
                            return self.finish(TokenVariant::Error);
                        }
                    }

                    self.finish_str(TokenVariant::LitReal, str)
                } else {
                    self.finish_str(TokenVariant::LitInteger, str)
                }
            }

            return self.finish(TokenVariant::Error);
        }
    }

    pub fn new(source: Source) -> Lexer {
        Lexer {
            token : Token{
                variant: TokenVariant::Error,
                symbol: None,
                loc: Loc {
                    file: String::new(),
                    begin: Pos::zero(),
                    end: Pos::zero(),
                },
                enter: TokenEnter::NL
            },
            enter: TokenEnter::NL,
            source,
        }
    }
}