use strum_macros::EnumIter;

use crate::Sym;

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

#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub source: u32,
    pub begin: Pos,
    pub end: Pos
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub symbol: Option<Sym>,
    pub loc: Loc,
    pub enter: TokenEnter
}

#[derive(PartialEq, Copy, Clone, Debug, EnumIter)]
pub enum TokenKind {
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
    While, For, If, Else, Continue,
    Return,
    Let, Fn, Struct,

    LitStr, LitChar, LitInt, LitReal, LitBool,
    TypeStr, TypeUnit, TypeByte, TypeChar, TypeI32, TypeI64, TypeF32, TypeF64, TypeBool,

    NL,
    Error,
    Eof,
    Num,
    Ident
}

impl TokenKind {
    pub fn keyword(&self) -> &str{
        match self {
            TokenKind::While =>  "while",
            TokenKind::For =>  "for",
            TokenKind::Continue =>  "continue",
            TokenKind::If =>  "if",
            TokenKind::Else =>  "else",
            TokenKind::Let =>  "let",
            TokenKind::Fn =>  "fn",
            TokenKind::Return =>  "return",
            TokenKind::TypeStr =>  "String",
            TokenKind::TypeChar =>  "char",
            TokenKind::TypeBool =>  "bool",
            TokenKind::TypeByte =>  "i8",
            TokenKind::TypeI32 =>  "i32",
            TokenKind::TypeI64 =>  "i64",
            TokenKind::TypeF32 =>  "f32",
            TokenKind::TypeF64 =>  "f64",
            TokenKind::TypeUnit =>  "Unit",
            TokenKind::Struct =>  "struct",
            _ =>  "",
        }
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum TokenEnter {
    Token, Space, NL
}