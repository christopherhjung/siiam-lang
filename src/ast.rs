use std::cell::RefCell;
use std::cmp::max;

use strum_macros::EnumIter;

use crate::ast::Op::Dec;
use crate::check::*;
use crate::sym::Sym;

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Box<Decl>>,
}

#[derive(Debug)]
pub struct StructDecl {
    pub members: Vec<Box<Decl>>,
}

#[derive(Debug)]
pub struct MemberDecl {
    pub ast_ty: Box<ASTTy>,
    pub index: usize,
}

#[derive(Debug)]
pub struct FnDecl {
    pub params: Vec<Box<Decl>>,
    pub ret_ty: Option<Box<ASTTy>>,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct LocalDecl {
    pub ast_ty: Option<Box<ASTTy>>,
}

#[derive(Debug)]
pub struct Decl{
    pub ident: Box<Ident>,
    pub shadows: Option<*const Decl>,
    pub depth: usize,
    pub ty: Option<TyRef>,
    pub kind : DeclKind
}

impl Decl{
    pub fn new(ident : Box<Ident>, kind : DeclKind ) -> Decl{
         Decl{
             ident,
             shadows: None,
             depth: 0,
             ty: None,
             kind
         }
    }
}

#[derive(Debug)]
pub enum DeclKind{
    LocalDecl(LocalDecl),
    FnDecl(FnDecl),
    MemberDecl(MemberDecl),
    StructDecl(StructDecl)
}

#[derive(Debug)]
pub struct Ident {
    pub sym: Sym,
}

#[derive(Debug)]
pub enum ASTTy {
    Prim(PrimTy),
    Struct(ASTStructTy),
    Fn(ASTFnTy),
    Err
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, EnumIter)]
pub enum PrimTy{
    Str,
    Unit,
    Byte,
    Char,
    I32,
    I64,
    F32,
    F64,
    Bool
}

#[derive(Debug)]
pub struct ASTStructTy {
    pub ident_use: Box<IdentUse>,
}

#[derive(Debug)]
pub struct ASTFnTy {
    pub param_types: Vec<Box<ASTTy>>,
    pub return_type: Box<ASTTy>,
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
pub enum ExprKind {
    Literal(Literal),
    Block(Block),
    Ident(IdentExpr),
    FnCall(FnCallExpr),
    Field(FieldExpr),
    If(IfExpr),
    While(WhileExpr),
    Prefix(PrefixExpr),
    Infix(InfixExpr),
    Postfix(PostfixExpr),
    Ret(RetExpr)
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<TyRef>,
}

impl Expr{
    pub fn new( kind: ExprKind ) -> Expr{
        Expr{
            kind,
            ty: None
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Box<Stmt>>,
}

#[derive(Debug)]
pub struct PrefixExpr {
    pub expr: Box<Expr>,
    pub op: Op,
}

#[derive(Debug)]
pub struct PostfixExpr {
    pub expr: Box<Expr>,
    pub op: Op,
}

#[derive(Debug)]
pub struct RetExpr {
    pub expr: Option<Box<Expr>>,
    pub decl: Option<*const Decl>,
}

impl RetExpr {
    pub fn new(expr: Option<Box<Expr>>) -> RetExpr {
        RetExpr { expr, decl: None }
    }
}

#[derive(Debug)]
pub struct InfixExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: Op,
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
    pub target: Box<Expr>,
    pub identifier: Box<IdentUse>,
    pub index: usize,
}

#[derive(Debug)]
pub struct IfExpr {
    pub condition : Box<Expr>,
    pub true_branch: Box<Expr>,
    pub false_branch: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct WhileExpr {
    pub condition : Box<Expr>,
    pub body : Box<Expr>,
    pub else_branch : Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,

    Or,
    And,
    Eq,
    Ne,
    BitOr,
    BitXor,
    BitAnd,
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
    LeftBracket,
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
pub enum Prec {
    Bottom,
    Assign,
    Or,
    And,
    Rel,
    BitOr,
    BitXor,
    BitAnd,
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
            Prec::Assign => Prec::Or,
            Prec::Or => Prec::And,
            Prec::And => Prec::BitOr,
            Prec::BitOr => Prec::BitXor,
            Prec::BitXor => Prec::BitAnd,
            Prec::BitAnd => Prec::Shift,
            Prec::Shift => Prec::Add,
            Prec::Add => Prec::Mul,
            Prec::Mul => Prec::As,
            Prec::As => Prec::Unary,
            Prec::Unary => Prec::Top,
            _ => Prec::Top,
        }
    }
}

impl Op {
    pub fn is_postfix(&self) -> bool {
        return match self {
            Op::Inc |
            Op::Dec |
            Op::LeftBracket |
            Op::LeftParen |
            Op::Dot => true,
            _ => false
        };
    }

    pub fn is_prefix(&self) -> bool {
        return match self {
            Op::Add |
            Op::Sub |
            Op::Inc |
            Op::Dec |
            Op::Not => true,
            _ => false
        };
    }

    pub fn is_infix(&self) -> bool {
        return match self {
            Op::Or |
            Op::And |
            Op::Assign |
            Op::Eq |
            Op::Ne |
            Op::Lt |
            Op::Le |
            Op::Gt |
            Op::Ge |
            Op::Add |
            Op::Sub |
            Op::Mul |
            Op::Div => true,
            _ => false
        };
    }

    pub fn prec(&self) -> Prec {
        match self {
            Op::Or => Prec::Or,
            Op::And => Prec::And,
            Op::Eq | Op::Ne => Prec::Rel,
            Op::BitOr => Prec::BitOr,
            Op::BitXor => Prec::BitXor,
            Op::BitAnd => Prec::Add,
            Op::Shl | Op::Shr => Prec::Shift,
            Op::Add | Op::Sub => Prec::Add,
            Op::Mul | Op::Div | Op::Rem => Prec::Mul,
            _ => Prec::Bottom
        }
    }
}

impl Op{
    pub fn sign(&self) -> String {
        String::from(match self {
            Op::Or => "|",
            Op::And => "&",
            Op::Eq => "==",
            Op::Ne => "!=",
            Op::Lt => "<",
            Op::Le => "<=",
            Op::Gt => ">",
            Op::Ge => ">=",
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Inc => "++",
            Op::Dec => "--",
            Op::Assign => "=",
            Op::AddAssign => "+=",
            Op::SubAssign => "-=",
            Op::MulAssign => "*=",
            Op::DivAssign => "/=",
            _ => "?"
        })
    }
}