use std::process::id;
use std::rc::Rc;

use crate::{Lexer, TokenVariant};
use crate::lexer::{Symbol, Token, TokenEnter};

const LOOKAHEAD_SIZE: usize = 3;

pub struct Parser {
    lexer: Lexer,
    lookahead: [Token; LOOKAHEAD_SIZE],
    lookahead_idx: usize,
    new_lines: usize,
}

pub struct Module {
    items: Vec<Box<dyn Item>>,
}

pub trait Item {}

pub struct StructDecl {
    identifier: Box<Ident>,
    fields: Vec<Box<FieldDecl>>,
}

impl Item for StructDecl {}

struct FieldDecl {
    identifier: Box<Ident>,
    shadows: Option<Box<Decl>>,
    depth: u32,
    ast_type: Box<dyn ASTType>,
    index: usize,
}

pub struct FnDecl {
    identifier: Box<Ident>,
    params: Vec<Box<Decl>>,
    return_type: Option<Box<dyn ASTType>>,
    body: Box<Block>,
}

impl Item for FnDecl{}

struct Ident {
    sym: Symbol,
}

trait ASTNode {
    fn dump() -> String {
        String::from("test")
    }
}

impl ASTNode for Ident {}

trait ASTType {}

struct PrimASTType {
    variant: TokenVariant,
}

impl ASTType for PrimASTType {}

struct FnASTType {
    param_types: Vec<Box<dyn ASTType>>,
    return_type: Box<dyn ASTType>,
}

impl ASTType for FnASTType {}

pub struct Decl {
    identifier: Box<Ident>,
    shadows: Option<Box<Decl>>,
    depth: u32,
    ast_type: Option<Box<dyn ASTType>>,
}

trait Stmt {}

trait Expr {}

pub struct Block {
    stmts: Vec<Box<dyn Stmt>>,
}

struct ExprStmt {
    expr: Box<dyn Expr>,
}

impl Stmt for ExprStmt {}

struct PrefixExpr {
    expr: Box<dyn Expr>,
    op: Operator,
}

impl Expr for PrefixExpr {}

struct PostfixExpr {
    expr: Box<dyn Expr>,
    op: Operator,
}

impl Expr for PostfixExpr {}

struct InfixExpr {
    lhs: Box<dyn Expr>,
    rhs: Box<dyn Expr>,
    op: Operator,
}

impl Expr for InfixExpr {}

struct IdentUse {
    ident: Box<Ident>,
    decl: Option<Box<Decl>>,
}

impl IdentUse {
    fn new(ident: Box<Ident>) -> IdentUse {
        IdentUse { ident, decl: None }
    }
}

impl ASTNode for IdentUse {}

struct IdentExpr {
    identUse: Box<IdentUse>,
}

impl Expr for IdentExpr {}

enum Literal {
    String(String),
    Char(char),
    Int(i64),
    Real(f64),
    Bool(bool),
}

impl Expr for Literal {}

struct LetStmt {
    local_decl: Box<Decl>,
    init : Option<Box<dyn Expr>>
}

impl Stmt for LetStmt{}

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
}

impl Prec {
    fn next(&self) -> Prec {
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
            _ => Prec::Unary,
        }
    }
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

impl Operator {
    fn is_postfix(&self) -> bool {
        return match self {
            Operator::Inc |
            Operator::Dec |
            Operator::LeftBracket |
            Operator::LeftParen |
            Operator::Dot => true,
            _ => false
        };
    }

    fn is_prefix(&self) -> bool {
        return match self {
            Operator::Add |
            Operator::Sub |
            Operator::Inc |
            Operator::Dec |
            Operator::Not => true,
            _ => false
        };
    }

    fn is_infix(&self) -> bool {
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

    fn prec(&self) -> Prec {
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

impl Parser {
    fn shift(&mut self) {
        let mut new_token = self.lexer.next_token();
        self.lookahead[self.lookahead_idx] = new_token;
        self.lookahead_idx = (self.lookahead_idx + 1) % LOOKAHEAD_SIZE;
    }

    fn lex(&mut self) -> Token {
        let result: Token = self.lookahead().clone();
        self.shift();
        return result;
    }

    fn variant(&mut self) -> TokenVariant {
        self.lookahead().variant
    }

    fn enter(&mut self) -> TokenEnter {
        self.lookahead().enter
    }

    fn lookahead(&mut self) -> &mut Token {
        self.lookahead_at(0)
    }

    fn lookahead_at(&mut self, i: usize) -> &mut Token {
        assert!(i < LOOKAHEAD_SIZE, "lookahead overflow!");
        return &mut self.lookahead[(i + self.lookahead_idx) % LOOKAHEAD_SIZE];
    }
    fn check(&mut self, variant: TokenVariant) -> bool {
        variant == self.variant()
    }

    fn accept(&mut self, variant: TokenVariant) -> bool {
        if variant == self.variant() {
            self.shift();
            return true;
        }

        return false;
    }

    fn follow(&mut self, variant: TokenVariant) -> bool {
        let enter = self.lookahead().enter;

        if enter == TokenEnter::Token {
            return self.accept(variant);
        }

        return false;
    }

    fn expect(&mut self, variant: TokenVariant) {
        println!("actual:{:?} expect:{:?}", self.variant(), variant);
        assert!(self.accept(variant), "Variants do not match!");
        println!("actual:{:?} expect:{:?} !!", self.variant(), variant);
    }

    fn expect_enter(&mut self, enter: TokenEnter) {
        assert_eq!(self.lookahead().enter, enter, "Variants do not match!");
    }

    pub fn parse_module(&mut self) -> Box<Module> {
        let mut items = Vec::new();
        while !self.accept(TokenVariant::Eof) && !self.accept(TokenVariant::RBrace) {
            items.push(self.parse_item());
        }
        Box::new(Module { items })
    }


    fn parse_item(&mut self) -> Box<dyn Item> {
        match self.variant() {
            TokenVariant::Fn => return self.parse_fn(),
            TokenVariant::Struct => return self.parse_struct(),
            _ => unreachable!()
        }
    }

    fn parse_identifier(&mut self) -> Box<Ident> {
        println!("actual:{:?} expect:Identifier", self.variant());
        assert!(self.check(TokenVariant::Identifier));
        let sym = self.lex().symbol.unwrap();
        let ident = Ident { sym };
        return Box::new(ident);
    }

    fn parse_struct(&mut self) -> Box<StructDecl> {
        self.expect(TokenVariant::Struct);
        let identifier = self.parse_identifier();
        self.expect(TokenVariant::LBrace);
        let fields = self.parse_field_list();
        return Box::new(StructDecl { identifier, fields });
    }


    fn parse_fn(&mut self) -> Box<FnDecl> {
        self.expect(TokenVariant::Fn);
        let identifier = self.parse_identifier();
        self.expect(TokenVariant::LParen);
        let params = self.parse_param_list();
        let mut return_type : Option<Box<dyn ASTType>> = None;
        if self.accept(TokenVariant::Colon) {
            return_type = Some(self.parse_type());
        }

        let body = self.parse_block();

        Box::new(FnDecl{
            identifier,
            params,
            return_type,
            body
        })
    }

    fn parse_block(&mut self) -> Box<Block>{
        self.expect(TokenVariant::LBrace);
        let stmts = self.parse_statement_list(TokenVariant::LBrace);
        println!("{:?}", stmts.len());
        Box::new(Block{ stmts })
    }

    fn parse_field(&mut self, i: usize) -> Box<FieldDecl> {
        let param_id = self.parse_identifier();
        self.accept(TokenVariant::Colon);

        return Box::new(FieldDecl {
            identifier: param_id,
            shadows: None,
            depth: 0,
            ast_type: self.parse_type(),
            index: i,
        });
    }

    fn parse_type(&mut self) -> Box<dyn ASTType> {
        match self.variant() {
            TokenVariant::TypeBool |
            TokenVariant::TypeByte |
            TokenVariant::TypeChar |
            TokenVariant::TypeString |
            TokenVariant::TypeInt |
            TokenVariant::TypeLong |
            TokenVariant::TypeFloat |
            TokenVariant::TypeDouble |
            TokenVariant::TypeUnit => {
                let token = self.lex();
                return Box::new(PrimASTType { variant: token.variant });
            }
            /*TokenVariant::ID => {
                return Box::new(SuperASTType(IdentUse(parse_identifier())));
            }*/
            TokenVariant::Fn => {
                self.lex();
                self.accept(TokenVariant::LParen);
                let param_types = self.parse_type_list();
                self.accept(TokenVariant::Arrow);
                let return_type = self.parse_type();
                return Box::new(FnASTType { param_types, return_type });
            }
            _ => {
                println!("unreachable:{:?}", self.variant());
                unreachable!()
            }
        }
    }

    fn parse_list<T>(&mut self, mut f: impl FnMut() -> Box<T>, separator: TokenVariant, delimiter: TokenVariant) -> Vec<Box<T>> {
        let mut exprs = Vec::new();
        while !self.accept(delimiter) {
            if !exprs.is_empty() {
                self.expect(separator);
            }
            exprs.push(f());
        }
        return exprs;
    }
    /*
        fn parse_expr_list(&mut self, seperator: TokenVariant, delimiter: TokenVariant) -> Vec<Box<dyn Expr>> {
            let mut exprs = Vec::new();
            while !self.accept(delimiter) {
                if !exprs.is_empty() {
                    self.expect(separator);
                }
                exprs.push(parse_expr());
            }
            return exprs;
        }*/

    fn parse_operator(&mut self) -> Option<Operator> {
        Some(if self.accept(TokenVariant::Plus) {
            //+, ++, +=
            if self.follow(TokenVariant::Plus) {
                Operator::Inc
            } else if self.follow(TokenVariant::Assign) {
                Operator::Assign
            } else {
                Operator::Add
            }
        } else if self.accept(TokenVariant::Minus) {
            // -, --, -=, ->
            if self.follow(TokenVariant::Minus) {
                Operator::Dec
            } else if self.follow(TokenVariant::RAngle) {
                Operator::Arrow
            } else if self.follow(TokenVariant::Assign) {
                Operator::SubAssign
            } else {
                Operator::Sub
            }
        } else if self.accept(TokenVariant::Star) {
            // *, *=
            if self.follow(TokenVariant::Assign) {
                Operator::MulAssign
            } else {
                Operator::Mul
            }
        } else if self.accept(TokenVariant::Slash) {
            // /, /=
            if self.follow(TokenVariant::Assign) {
                Operator::DivAssign
            } else {
                Operator::Div
            }
        } else if self.accept(TokenVariant::LAngle) {
            // <, <=, <<
            if self.follow(TokenVariant::Assign) {
                Operator::Le
            } else if self.follow(TokenVariant::LAngle) {
                Operator::Shl
            } else {
                Operator::Lt
            }
        } else if self.accept(TokenVariant::RAngle) {
            // >, >=, >>
            if self.follow(TokenVariant::Assign) {
                Operator::Ge
            } else {
                Operator::Gt
            }
        } else if self.accept(TokenVariant::Assign) {
            // =, ==
            if self.follow(TokenVariant::Assign) {
                Operator::Eq
            } else {
                Operator::Assign
            }
        } else if self.accept(TokenVariant::Not) {
            if self.follow(TokenVariant::Assign) {
                Operator::Ne
            } else {
                Operator::Not
            }
        } else if self.accept(TokenVariant::Or) {
            if self.follow(TokenVariant::Or) {
                Operator::OrOr
            } else {
                Operator::Or
            }
        } else if self.accept(TokenVariant::And) {
            if self.follow(TokenVariant::Or) {
                Operator::AndAnd
            } else {
                Operator::And
            }
        } else if self.accept(TokenVariant::LBracket) {
            Operator::LeftBracket
        } else if self.accept(TokenVariant::LParen) {
            Operator::LeftParen
        } else {
            return None;
        })
    }

    fn parse_prefix_expr(&mut self, op: Operator) -> Box<dyn Expr> {
        let expr = self.parse_expr_prec(op.prec());
        Box::new(PrefixExpr { expr, op })
    }

    fn parse_primary_expr(&mut self) -> Box<dyn Expr> {
        match self.variant() {
            TokenVariant::LitInteger |
            TokenVariant::LitReal |
            TokenVariant::LitString |
            TokenVariant::LitChar |
            TokenVariant::LitBool => self.parse_literal(),
            TokenVariant::Identifier => Box::new(IdentExpr { identUse: Box::new(IdentUse::new(self.parse_identifier())) }),
            TokenVariant::LParen => {
                self.lex();
                let expr = self.parse_expr();
                self.expect(TokenVariant::RParen);
                expr
            }
            //TokenVariant::If => parse_if(),
            _ => unreachable!()
        }
    }

    fn parse_infix_expr(&mut self, lhs: Box<dyn Expr>, op: Operator) -> Box<dyn Expr> {
        let rhs = self.parse_expr_prec(op.prec().next());
        Box::new(InfixExpr { lhs, rhs, op })
    }

    fn parse_postfix_expr(&mut self, lhs: Box<dyn Expr>, op : Operator) -> Box<dyn Expr> {
        match op {
            Operator::Inc |
            Operator::Dec => Box::new(PostfixExpr { expr: lhs, op }),
            /*Token::L_Paren => {
                std::vector < Expr * > exprs = parse_expr_list(Token::Comma, Token::R_Paren);
                return new FnCallExpr(lhs, std::move (exprs));
            }
            Token::DOT:
                return new FieldExpr(lhs, new IdentUse(parse_identifier()));*/
            _ => unreachable!()
        }
    }


    fn parse_expr_prec(&mut self, prec: Prec) -> Box<dyn Expr> {
        let mut lhs = if let Some(op) = self.parse_operator() {
            self.parse_prefix_expr(op)
        } else {
            self.parse_primary_expr()
        };

        loop {
            if self.enter() == TokenEnter::NL {
                break;
            }

            let op = self.parse_operator().unwrap();
            if op.is_infix() {
                if prec > op.prec() {
                    break;
                }

                lhs = self.parse_infix_expr(lhs, op);
            } else if op.is_postfix() {
                if prec > Prec::Unary {
                    break;
                }

                lhs = self.parse_postfix_expr(lhs, op);
            } else {
                break;
            }
        }

        return lhs;
    }


    fn parse_literal(&mut self) -> Box<Literal> {
        let token = self.lex();
        let str = token.symbol.unwrap().value;
        Box::new(match token.variant
        {
            TokenVariant::LitReal => Literal::Real(str.parse::<f64>().unwrap()),
            TokenVariant::LitInteger => Literal::Int(str.parse::<i64>().unwrap()),
            TokenVariant::LitString => Literal::String(str),
            TokenVariant::LitChar => Literal::Char(str.chars().next().unwrap()),
            TokenVariant::LitBool => Literal::Bool(str == "true"),
            _ => unreachable!()
        })
    }

    fn parse_expr(&mut self) -> Box<dyn Expr> {
        let result = self.parse_expr_prec(Prec::Bottom);
        self.accept(TokenVariant::Semicolon);
        result
    }

    fn parse_stmts(&mut self) -> Box<dyn Stmt>{
        match self.variant() {
            TokenVariant::Let => self.parse_decl(),
            _=> Box::new(ExprStmt{ expr: self.parse_expr() })
        }
    }

    fn parse_decl(&mut self) -> Box<LetStmt>{
        self.accept(TokenVariant::Let);
        let identifier = self.parse_identifier();

        let mut ast_type = None;
        if self.accept(TokenVariant::Colon) {
            ast_type = Some(self.parse_type());
        }

        let mut init = None;
        if self.accept(TokenVariant::Assign) {
            init = Some(self.parse_expr());
        }

        Box::new(LetStmt{ local_decl: Box::new(Decl{
            identifier,
            shadows: None,
            depth: 0,
            ast_type
        }), init })
    }

    fn parse_statement_list(&mut self, delimiter: TokenVariant) -> Vec<Box<dyn Stmt>> {
        let mut stmts = Vec::new();
        while self.variant() != TokenVariant::RBrace {
            if !stmts.is_empty() {
                self.expect(TokenVariant::Comma);
                assert_eq!(self.enter(), TokenEnter::NL, "Statement does not start with a new line");
            }
            stmts.push(self.parse_stmts());
        }
        return stmts;
    }

    fn parse_param(&mut self) -> Box<Decl>{
        let param_id = self.parse_identifier();
        self.accept(TokenVariant::Colon);

        Box::new(Decl{
            identifier: param_id,
            shadows: None,
            depth: 0,
            ast_type: Some(self.parse_type())
        })
    }

    fn parse_param_list(&mut self) -> Vec<Box<Decl>> {
        let mut params = Vec::new();
        while !self.accept(TokenVariant::RParen) {
            if !params.is_empty() {
                self.expect(TokenVariant::Comma);
            }
            params.push(self.parse_param());
        }
        return params;
    }
    /*
        fn parse_field_list(&mut self) -> Vec<Box<FieldDecl>>{
            let mut i = 0;

            let fun = || {
                let result = self.parse_field(i);
                i+=1;
                result
            };

            return self.parse_list(fun, TokenVariant::NL, TokenVariant::RBrace);
        }*/

    fn parse_field_list(&mut self) -> Vec<Box<FieldDecl>> {
        let mut exprs = Vec::new();
        while !self.accept(TokenVariant::RBrace) {
            if !exprs.is_empty() {
                self.expect_enter(TokenEnter::NL);
            }
            exprs.push(self.parse_field(exprs.len()));
        }
        return exprs;
    }

    fn parse_type_list(&mut self) -> Vec<Box<dyn ASTType>> {
        return vec![];
    }


    /*
        std::vector<Stmt*> parse_statement_list(Token::Variant delimiter){
        return parse_list<Stmt*>([&]{return parse_stmts();}, Token::NL, delimiter);
        }

        std::vector<ParamDecl*> parse_param_list(){
        return parse_list<ParamDecl*>([&]{return parse_param();}, Token::Comma, Token::R_Paren);
        }

        std::vector<FieldDecl*> parse_field_list(){
        size_t i;
        return parse_list<FieldDecl*>([&]{return parse_field(i++);}, Token::NL, Token::R_Brace);
        }

        std::vector<ASTType*> parse_type_list(){
        return parse_list<ASTType*>([&]{return parse_type();}, Token::Comma, Token::R_Paren);
        }*/

    /*
        fn parse_fn(&mut self) -> FnDecl{
            expect(Token::Fn);
            let identifier = parse_identifier();
            expect(Token::L_Paren);
            let params = parse_param_list( );
            ASTType* returnType = nullptr;
            if accept(Token::Colon) {
                returnType = parse_type();
            }
            let block = parse_block();


            Box(FnDecl(identifier, params, returnType, block))
        }

        fn parse_list() -> Vec<Box<Item>>{
            return vec![];
        }*/


    /*
        std::vector<T> parse_list(std::function<T(void)> f, Token::Variant separator, Token::Variant delimiter ) : Vec<T>{
            std::vector<T> exprs;
            while(!accept(delimiter)){
                if(!exprs.empty()){
                    expect(separator);
                }
                exprs.push_back(f());
            }
            return exprs;
        }*/
    /*
        std::vector<Expr*> parse_expr_list(Token::Variant seperator, Token::Variant delimiter){
            return parse_list<Expr*>([&]{return parse_expr();}, seperator, delimiter);
        }

        std::vector<Stmt*> parse_statement_list(Token::Variant delimiter){
            return parse_list<Stmt*>([&]{return parse_stmts();}, Token::NL, delimiter);
        }

        std::vector<ParamDecl*> parse_param_list(){
            return parse_list<ParamDecl*>([&]{return parse_param();}, Token::Comma, Token::R_Paren);
        }

        std::vector<FieldDecl*> parse_field_list(){
        size_t i;
            return parse_list<FieldDecl*>([&]{return parse_field(i++);}, Token::NL, Token::R_Brace);
        }

        std::vector<ASTType*> parse_type_list(){
            return parse_list<ASTType*>([&]{return parse_type();}, Token::Comma, Token::R_Paren);
        }*/

    pub fn new(mut lexer: Lexer) -> Parser {
        let lookahead = [lexer.next_token(), lexer.next_token(), lexer.next_token()];

        Parser {
            lexer,
            lookahead,
            lookahead_idx: 0,
            new_lines: 0,
        }
    }
}