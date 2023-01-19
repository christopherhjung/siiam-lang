use std::io;
use std::fmt;
use crate::source::Source;

pub struct Lexer {
    token: Token,
    source: Source,
    last_nl : bool,
}

#[derive(Debug, Copy, Clone)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
}

impl Pos {
    fn zero() -> Pos{
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
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum TokenVariant {
    OrOr,
    AndAnd,
    INC, DEC, NOT,
    SHL, SHR,
    EQ, NE, LT, LE, GT, GE,
    OR, XOR, AND,
    ADD, SUB, MUL, DIV, REM,
    AddAsgn,
    SubAsgn,
    MulAsgn,
    DivAsgn,
    ASGN,

    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,
    Comma, Semicolon, Colon, Arrow, DOT,
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

#[derive(Debug)]
pub struct Symbol {
    name: String,
}

impl Symbol{
    fn new( str : &String ) -> Symbol{
        Symbol{
            name : str.clone()
        }
    }
}

impl Clone for Symbol {
    fn clone(&self) -> Self {
        Symbol::new(&self.name)
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
        if self.acceptStrPred(str, sym) {
            while self.acceptStrPred(str, sym) || self.acceptStrPred(str, dec) {}
            return true;
        }
        return false;
    }

    fn isEof(&mut self) -> bool{
        return self.source.peek() == None;
    }

    fn finish(&mut self, variant: TokenVariant) {
        self.token.variant = variant;
        self.last_nl = variant == TokenVariant::NL;
        self.token.symbol = None;
    }

    fn finishStr(&mut self, variant: TokenVariant, str: &String) {
        self.token.variant = variant;
        self.token.symbol = Some(Symbol::new(str));
    }

    fn acceptChar(&mut self, expect : char) -> bool{
        self.source.accept( |actual| actual == expect)
    }

    fn acceptPred(&mut self, pred: fn(char) -> bool) -> bool{
        self.source.accept(pred)
    }

    fn acceptStrChar(&mut self, str: &mut String, expect : char) -> bool{
        self.source.acceptStr(str, |actual| actual == expect)
    }

    fn acceptStrPred(&mut self, str: &mut String, pred: fn(char) -> bool) -> bool{
        self.source.acceptStr(str, pred)
    }

    pub fn nextToken(&mut self) -> Token {
        self.nextTokenImpl();
        self.token.loc = self.source.loc();
        self.token.clone()
    }

    fn nextTokenImpl(&mut self) {
        loop {
            self.source.resetLoc();

            if self.isEof()
            {
                return self.finish(TokenVariant::Eof);
            }

            if self.acceptPred(space) {
                loop {
                    if !self.acceptPred(space) {
                        break;
                    }
                }
                if self.source.last.line != self.source.peek.line && !self.last_nl {
                    return self.finish(TokenVariant::NL);
                }
                continue;
            }

            // +, ++, +=
            if self.acceptChar('+') {
                if self.acceptChar('+') {
                    return self.finish(TokenVariant::INC);
                }
                if self.acceptChar('=') {
                    return self.finish(TokenVariant::AddAsgn);
                }
                return self.finish(TokenVariant::ADD);
            }

            // -, --, -=, ->
            if self.acceptChar('-') {
                if self.acceptChar('-') {
                    return self.finish(TokenVariant::DEC);
                }
                if self.acceptChar('>'){
                    return self.finish(TokenVariant::Arrow);
                }
                if self.acceptChar('=') {
                    return self.finish(TokenVariant::SubAsgn);
                }
                return self.finish(TokenVariant::SUB);
            }

            // *, *=
            if self.acceptChar('*') {
                if self.acceptChar('=') {
                    return self.finish(TokenVariant::MulAsgn);
                }
                return self.finish(TokenVariant::MUL);
            }

            if self.acceptChar('<') {
                if self.acceptChar('=') {
                    return self.finish(TokenVariant::LE);
                }
                return self.finish(TokenVariant::LT);
            }

            if self.acceptChar('>') {
                if self.acceptChar('=') {
                    return self.finish(TokenVariant::GE);
                }
                return self.finish(TokenVariant::GT);
            }

            // =, ==
            if self.acceptChar('=') {
                if self.acceptChar('=') {
                    return self.finish(TokenVariant::EQ);
                }
                return self.finish(TokenVariant::ASGN);
            }

            if self.acceptChar('!') {
                if self.acceptChar('=') {
                    return self.finish(TokenVariant::NE);
                }
                return self.finish(TokenVariant::Error);
            }

            if self.acceptChar('/') {
                if self.acceptChar('=') {
                    return self.finish(TokenVariant::DivAsgn);
                }
                if self.acceptChar('*') { // arbitrary comment
                    let mut depth = 1;
                    loop {
                        if self.isEof() {
                            return self.finish(TokenVariant::Error);
                        }

                        if self.acceptChar('/') & &self.acceptChar('*') {
                            depth += 1;
                        } else if self.acceptChar('*') {
                            if self.acceptChar('/') {
                                depth-=1;
                                if (depth == 0) {
                                    break;
                                }
                            }
                        } else {
                            self.next();
                        }
                    }
                    continue;
                }
                if self.acceptChar('/') {
                    loop {
                        if self.isEof() {
                            return self.finish(TokenVariant::Error);
                        };

                        if self.acceptChar('\n') {
                            break;
                        } else {
                            self.next();
                        }
                    }
                    continue;
                }
                return self.finish(TokenVariant::DIV);
            }

            if self.acceptChar(',') {
                return self.finish(TokenVariant::Comma);
            }
            if self.acceptChar(':') {
                return self.finish(TokenVariant::Colon);
            }
            if self.acceptChar(';') {
                return self.finish(TokenVariant::Semicolon);
            }
            if self.acceptChar('(') {
                return self.finish(TokenVariant::LParen);
            }
            if self.acceptChar(')') {
                return self.finish(TokenVariant::RParen);
            }
            if self.acceptChar('[') {
                return self.finish(TokenVariant::LBracket);
            }
            if self.acceptChar(']') {
                return self.finish(TokenVariant::RBracket);
            }
            if self.acceptChar('{') {
                return self.finish(TokenVariant::LBrace);
            }
            if self.acceptChar('}') {
                return self.finish(TokenVariant::RBrace);
            }
            if self.acceptChar('.') {
                return self.finish(TokenVariant::DOT);
            }

            let mut str = String::new();

            // identifiers/keywords
            if self.identifier(&mut str) {
                if str == "true" || str == "false" {
                    return self.finishStr(TokenVariant::LitBool, &mut str);
                }
                return self.finishStr(TokenVariant::Identifier, &mut str);
            }

            // char literal
            if self.acceptChar('\'') {
                while !self.acceptChar('\'') {
                    self.acceptStrChar(&mut str, '\\');
                    str.push(self.next().unwrap());

                    if self.isEof() {
                        return self.finish(TokenVariant::Error);
                    };
                }
                return self.finishStr(TokenVariant::LitChar, &mut str);
            }

            // string literal
            if self.acceptChar('"') {
                while !self.acceptChar('"') {
                    self.acceptStrChar(&mut str, '\\');
                    str.push(self.next().unwrap());

                    if self.isEof() {
                        return self.finish(TokenVariant::Error);
                    }
                }
                return self.finishStr(TokenVariant::LitString, &mut str);
            }

            if self.acceptStrPred(&mut str, dec) {
                while self.acceptStrPred(&mut str, dec) {
                    if self.isEof() {
                        return self.finish(TokenVariant::Error);
                    }
                }

                return if self.acceptStrChar(&mut str, '.') {
                    while self.acceptStrPred(&mut str, dec) {
                        if self.isEof() {
                            return self.finish(TokenVariant::Error);
                        }
                    }

                    self.finishStr(TokenVariant::LitReal, &mut str)
                } else {
                    self.finishStr(TokenVariant::LitInteger, &mut str)
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
                }
            },
            last_nl : false,
            source,
        }
    }
}