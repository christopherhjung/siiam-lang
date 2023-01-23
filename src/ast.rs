use std::cell::RefCell;
use std::cmp::max;
use std::rc::{Rc, Weak};
use crate::sym::{Sym};
use crate::{TokenKind};
use crate::ast::Operator::Dec;

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Box<Decl>>,
}


#[derive(Debug)]
pub struct StructDecl {
    pub fields: Vec<Box<Decl>>,
}

#[derive(Debug)]
pub struct FieldDecl {
    pub ast_type: Box<Ty>,
    pub index: usize,
}

#[derive(Debug)]
pub struct FnDecl {
    pub params: Vec<Box<Decl>>,
    pub return_type: Option<Box<Ty>>,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct LetDecl {
    pub ast_type: Option<Box<Ty>>,
}

#[derive(Debug)]
pub struct Decl{
    pub ident: Box<Ident>,
    pub shadows: Option<*const Decl>,
    pub depth: usize,
    pub kind : DeclKind
}

impl Decl{
    pub fn new(ident : Box<Ident>, kind : DeclKind ) -> Decl{
         Decl{
             ident,
             shadows: None,
             depth: 0,
             kind
         }
    }
}

#[derive(Debug)]
pub enum DeclKind{
    LetDecl(LetDecl),
    FnDecl(FnDecl),
    FieldDecl(FieldDecl),
    StructDecl(StructDecl)
}

#[derive(Debug)]
pub struct Ident {
    pub sym: Sym,
}

#[derive(Debug)]
pub enum Ty {
    Prim(PrimTy),
    Struct(StructTy),
    Fn(FnTy),
}

#[derive(Debug)]
pub enum PrimTy{
    Str,
    Unit,
    Byte,
    Char,
    Int,
    Long,
    Float,
    Double,
    Bool
}

#[derive(Debug)]
pub struct StructTy {
    pub ident_use: Box<IdentUse>,
}

#[derive(Debug)]
pub struct FnTy {
    pub param_types: Vec<Box<Ty>>,
    pub return_type: Box<Ty>,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprStmt),
    Let(LetStmt)
}
#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Block(Block),
    Ident(IdentExpr),
    FnCall(FnCallExpr),
    Field(FieldExpr),
    If(IfExpr),
    While(WhileExpr),
    Prefix(PrefixExpr),
    Infix(InfixExpr),
    Postfix(PostfixExpr)
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Box<Stmt>>,
}

#[derive(Debug)]
pub struct PrefixExpr {
    pub expr: Box<Expr>,
    pub op: Operator,
}

#[derive(Debug)]
pub struct PostfixExpr {
    pub expr: Box<Expr>,
    pub op: Operator,
}

#[derive(Debug)]
pub struct InfixExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: Operator,
}

#[derive(Debug)]
pub struct IdentUse {
    pub ident: Box<Ident>,
    pub decl: Option<*const Decl>,
}

impl IdentUse {
    pub fn new(ident: Box<Ident>) -> IdentUse {
        IdentUse { ident, decl: None }
    }
}

#[derive(Debug)]
pub struct IdentExpr {
    pub ident_use: Box<IdentUse>,
}

#[derive(Debug)]
pub enum Literal {
    Str(String),
    Char(char),
    Int(i64),
    Real(f64),
    Bool(bool),
}

#[derive(Debug)]
pub struct LetStmt {
    pub local_decl: Box<Decl>,
    pub init : Option<Box<Expr>>
}

#[derive(Debug)]
pub struct FnCallExpr {
    pub callee : Box<Expr>,
    pub args : Vec<Box<Expr>>
}

#[derive(Debug)]
pub struct FieldExpr{
    pub target_ : Box<Expr>,
    pub identifier_ : Box<IdentUse>,
    pub index_ : usize,
}

#[derive(Debug)]
pub struct IfExpr {
    pub condition : Box<Expr>,
    pub if_branch : Box<Expr>,
    pub else_branch : Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct WhileExpr {
    condition : Box<Expr>,
    body : Box<Expr>,
    else_branch : Option<Box<Expr>>,
}

#[derive(Debug)]
pub enum Operator {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,

    OrOr,
    AndAnd,
    Eq,
    Ne,
    Or,
    Xor,
    And,
    Shl,
    Shr,
    Add,
    Sub,

    Inc,
    Dec,
    Not,
    Lt,
    Le,
    Gt,
    Ge,
    Mul,
    Div,
    Rem,
    Arrow,
    Dot,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
pub enum Prec {
    Bottom,
    Assign,
    OrOr,
    AndAnd,
    Rel,
    Or,
    Xor,
    And,
    Shift,
    Add,
    Mul,
    As,
    Unary,
    Top,
}

impl Prec {
    pub fn next(&self) -> Prec {
        match self {
            Prec::Bottom => Prec::Assign,
            Prec::Assign => Prec::OrOr,
            Prec::OrOr => Prec::AndAnd,
            Prec::AndAnd => Prec::Or,
            Prec::Or => Prec::Xor,
            Prec::Xor => Prec::And,
            Prec::And => Prec::Shift,
            Prec::Shift => Prec::Add,
            Prec::Add => Prec::Mul,
            Prec::Mul => Prec::As,
            Prec::As => Prec::Unary,
            Prec::Unary => Prec::Top,
            _ => Prec::Top,
        }
    }
}

impl Operator {
    pub fn is_postfix(&self) -> bool {
        return match self {
            Operator::Inc |
            Operator::Dec |
            Operator::LeftBracket |
            Operator::LeftParen |
            Operator::Dot => true,
            _ => false
        };
    }

    pub fn is_prefix(&self) -> bool {
        return match self {
            Operator::Add |
            Operator::Sub |
            Operator::Inc |
            Operator::Dec |
            Operator::Not => true,
            _ => false
        };
    }

    pub fn is_infix(&self) -> bool {
        return match self {
            Operator::OrOr |
            Operator::AndAnd |
            Operator::Assign |
            Operator::Eq |
            Operator::Ne |
            Operator::Lt |
            Operator::Le |
            Operator::Gt |
            Operator::Ge |
            Operator::Add |
            Operator::Sub |
            Operator::Mul |
            Operator::Div => true,
            _ => false
        };
    }

    pub fn prec(&self) -> Prec {
        match self {
            Operator::OrOr => Prec::OrOr,
            Operator::AndAnd => Prec::AndAnd,
            Operator::Eq | Operator::Ne => Prec::Rel,
            Operator::Or => Prec::Or,
            Operator::Xor => Prec::Xor,
            Operator::And => Prec::Add,
            Operator::Shl | Operator::Shr => Prec::Shift,
            Operator::Add | Operator::Sub => Prec::Add,
            Operator::Mul | Operator::Div | Operator::Rem => Prec::Mul,
            _ => Prec::Bottom
        }
    }
}