use std::rc::Weak;
use crate::lexer::Symbol;
use crate::TokenVariant;

pub struct Module {
    pub items: Vec<Box<dyn Item>>,
}

pub trait Item {}

pub struct StructDecl {
    pub identifier: Box<Ident>,
    pub fields: Vec<Box<FieldDecl>>,
}

impl Item for StructDecl {}

pub struct FieldDecl {
    pub identifier: Box<Ident>,
    pub shadows: Option<Box<Decl>>,
    pub depth: u32,
    pub ast_type: Box<dyn ASTType>,
    pub index: usize,
}

pub struct FnDecl {
    pub identifier: Box<Ident>,
    pub params: Vec<Box<Decl>>,
    pub return_type: Option<Box<dyn ASTType>>,
    pub body: Box<Block>,
}

impl Item for FnDecl{}

pub struct Ident {
    pub sym: Symbol,
}

pub trait ASTNode {
    fn dump() -> String {
        String::from("test")
    }
}

impl ASTNode for Ident {}

pub trait ASTType {}

pub struct PrimASTType {
    pub variant: TokenVariant,
}

impl ASTType for PrimASTType {}

pub struct SuperASTType {
    pub ident_use: Box<IdentUse>,
}

impl ASTType for SuperASTType {}

pub struct FnASTType {
    pub param_types: Vec<Box<dyn ASTType>>,
    pub return_type: Box<dyn ASTType>,
}

impl ASTType for FnASTType {}

pub struct Decl {
    pub identifier: Box<Ident>,
    pub shadows: Option<Weak<Decl>>,
    pub depth: u32,
    pub ast_type: Option<Box<dyn ASTType>>,
}

pub trait Stmt {}

pub trait Expr {}

pub struct Block {
    pub stmts: Vec<Box<dyn Stmt>>,
}

impl Expr for Block {}

pub struct ExprStmt {
    pub expr: Box<dyn Expr>,
}

impl Stmt for ExprStmt {}

pub struct PrefixExpr {
    pub expr: Box<dyn Expr>,
    pub op: Operator,
}

impl Expr for PrefixExpr {}

pub struct PostfixExpr {
    pub expr: Box<dyn Expr>,
    pub op: Operator,
}

impl Expr for PostfixExpr {}

pub struct InfixExpr {
    pub lhs: Box<dyn Expr>,
    pub rhs: Box<dyn Expr>,
    pub op: Operator,
}

impl Expr for InfixExpr {}

pub struct IdentUse {
    pub ident: Box<Ident>,
    pub decl: Option<Box<Decl>>,
}

impl IdentUse {
    pub fn new(ident: Box<Ident>) -> IdentUse {
        IdentUse { ident, decl: None }
    }
}

impl ASTNode for IdentUse {}

pub struct IdentExpr {
    pub ident_use: Box<IdentUse>,
}

impl Expr for IdentExpr {}

pub enum Literal {
    String(String),
    Char(char),
    Int(i64),
    Real(f64),
    Bool(bool),
}

impl Expr for Literal {}

pub struct LetStmt {
    pub local_decl: Box<Decl>,
    pub init : Option<Box<dyn Expr>>
}

impl Stmt for LetStmt{}

pub struct FnCallExpr {
    pub callee : Box<dyn Expr>,
    pub args : Vec<Box<dyn Expr>>
}

impl Expr for FnCallExpr{}

pub struct FieldExpr{
    pub target_ : Box<dyn Expr>,
    pub identifier_ : Box<IdentUse>,
    pub index_ : usize,
}

impl Expr for FieldExpr{}

pub struct IfExpr {
    pub condition : Box<dyn Expr>,
    pub if_branch : Box<dyn Expr>,
    pub else_branch : Option<Box<dyn Expr>>,
}

impl Expr for IfExpr{}

pub struct WhileExpr {
    condition : Box<dyn Expr>,
    body : Box<dyn Expr>,
    else_branch : Option<Box<dyn Expr>>,
}

impl Expr for WhileExpr{}

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


#[derive(PartialEq, PartialOrd, Debug)]
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