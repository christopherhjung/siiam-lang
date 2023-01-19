use std::io;
use std::fmt;
use crate::source::Source;

pub struct Lexer {
    token: Token,
    source: Source,
    last_nl : bool,
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

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum TokenVariant {
    OrOr,
    AndAnd,
    Inc,
    Dec,
    Not,
    SHL, SHR,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    OR, XOR, AND,
    Add,
    Sub, MUL,
    Div, REM,
    AddAsgn,
    SubAsgn,
    MulAsgn,
    DivAsgn,
    Asgn,

    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,
    Comma, Semicolon, Colon, Arrow,
    Dot,
    While, For, If, Else,
    Return,
    ID,
    Let, Fn, Struct,

    LitString, LitChar, LitInteger, LitReal, LitBool,
    TypeString, TypeUnit, TypeByte, TypeChar, TypeInt, TypeLong, TypeFloat, TypeDouble, TypeBool,

    NL,
    Error,
    Eof,
    Num,
    Identifier
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum TokenEnter {
    Token, Space, NL
}

#[derive(Debug)]
pub struct Symbol {
    name: String,
}

impl Symbol{
    fn new( str : String ) -> Symbol{
        Symbol{
            name : str.clone()
        }
    }
}

impl Clone for Symbol {
    fn clone(&self) -> Self {
        Symbol::new(self.name.clone())
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
        self.last_nl = variant == TokenVariant::NL;
        self.token.symbol = None;
    }

    fn finish_str(&mut self, variant: TokenVariant, str: String) {
        self.token.variant = variant;
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

    fn next_token_impl(&mut self) {
        loop {
            self.source.reset_loc();

            if self.is_eof()
            {
                return self.finish(TokenVariant::Eof);
            }

            if self.accept_pred(space) {
                loop {
                    if !self.accept_pred(space) {
                        break;
                    }
                }
                if self.source.last.line != self.source.current.line && !self.last_nl {
                    self.enter = TokenEnter::NL;
                    //return self.finish(TokenVariant::NL);
                }else if self.enter == TokenEnter::Token{
                    self.enter = TokenEnter::Space;
                }

                continue;
            }

            // +, ++, +=
            if self.accept_char('+') {
                if self.accept_char('+') {
                    return self.finish(TokenVariant::Inc);
                }
                if self.accept_char('=') {
                    return self.finish(TokenVariant::AddAsgn);
                }
                return self.finish(TokenVariant::Add);
            }

            // -, --, -=, ->
            if self.accept_char('-') {
                if self.accept_char('-') {
                    return self.finish(TokenVariant::Dec);
                }
                if self.accept_char('>'){
                    return self.finish(TokenVariant::Arrow);
                }
                if self.accept_char('=') {
                    return self.finish(TokenVariant::SubAsgn);
                }
                return self.finish(TokenVariant::Sub);
            }

            // *, *=
            if self.accept_char('*') {
                if self.accept_char('=') {
                    return self.finish(TokenVariant::MulAsgn);
                }
                return self.finish(TokenVariant::MUL);
            }

            if self.accept_char('<') {
                if self.accept_char('=') {
                    return self.finish(TokenVariant::Le);
                }
                return self.finish(TokenVariant::Lt);
            }

            if self.accept_char('>') {
                if self.accept_char('=') {
                    return self.finish(TokenVariant::Ge);
                }
                return self.finish(TokenVariant::Gt);
            }

            // =, ==
            if self.accept_char('=') {
                if self.accept_char('=') {
                    return self.finish(TokenVariant::Eq);
                }
                return self.finish(TokenVariant::Asgn);
            }

            if self.accept_char('!') {
                if self.accept_char('=') {
                    return self.finish(TokenVariant::Ne);
                }
                return self.finish(TokenVariant::Not);
            }

            if self.accept_char('/') {
                if self.accept_char('=') {
                    return self.finish(TokenVariant::DivAsgn);
                }
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
                return self.finish(TokenVariant::Div);
            }

            if self.accept_char(',') {
                return self.finish(TokenVariant::Comma);
            }
            if self.accept_char(':') {
                return self.finish(TokenVariant::Colon);
            }
            if self.accept_char(';') {
                return self.finish(TokenVariant::Semicolon);
            }
            if self.accept_char('(') {
                return self.finish(TokenVariant::LParen);
            }
            if self.accept_char(')') {
                return self.finish(TokenVariant::RParen);
            }
            if self.accept_char('[') {
                return self.finish(TokenVariant::LBracket);
            }
            if self.accept_char(']') {
                return self.finish(TokenVariant::RBracket);
            }
            if self.accept_char('{') {
                return self.finish(TokenVariant::LBrace);
            }
            if self.accept_char('}') {
                return self.finish(TokenVariant::RBrace);
            }
            if self.accept_char('.') {
                return self.finish(TokenVariant::Dot);
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
            last_nl : false,
            enter: TokenEnter::NL,
            source,
        }
    }
}