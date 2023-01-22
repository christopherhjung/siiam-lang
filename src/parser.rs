use std::process::id;
use std::rc::Rc;
use sha2::digest::generic_array::typenum::Exp;

use crate::{Lexer, SymTable, TokenVariant};
use crate::sym::{Sym, SymRef};
use crate::lexer::{Token, TokenEnter};
use crate::ast::*;

const LOOKAHEAD_SIZE: usize = 3;

pub struct Parser {
    lexer: Lexer,
    lookahead: [Token; LOOKAHEAD_SIZE],
    lookahead_idx: usize,
    new_lines: usize,
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

    fn enter(&mut self, enter: TokenEnter) -> bool {
        self.lookahead().enter == enter
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
        if self.enter(TokenEnter::Token) {
            return self.accept(variant);
        }

        return false;
    }

    fn expect(&mut self, variant: TokenVariant) {
        assert!(self.accept(variant), "Variants do not match!");
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
        return match self.variant() {
            TokenVariant::Fn => self.parse_fn(),
            TokenVariant::Struct => self.parse_struct(),
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
        let ident = self.parse_identifier();
        self.expect(TokenVariant::LParen);
        let params = self.parse_param_list();
        let mut return_type : Option<Box<dyn ASTType>> = None;
        if self.accept(TokenVariant::Colon) {
            return_type = Some(self.parse_type());
        }

        let body = self.parse_block();

        Box::new(FnDecl{
            share: DeclShare::new(ident),
            params,
            return_type,
            body
        })
    }

    fn parse_block(&mut self) -> Box<Expr>{
        self.expect(TokenVariant::LBrace);
        let stmts = self.parse_statement_list();
        println!("{:?}", stmts.len());
        Box::new(Expr::Block(Block{ stmts }))
    }

    fn parse_field(&mut self, i: usize) -> Box<FieldDecl> {
        let ident = self.parse_identifier();
        self.accept(TokenVariant::Colon);

        return Box::new(FieldDecl {
            share: DeclShare::new(ident),
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
                return Box::new(PrimASTType {
                    variant: token.variant
                });
            }
            TokenVariant::Identifier => {
                return Box::new(SuperASTType{
                    ident_use: Box::new(IdentUse {
                        ident: self.parse_identifier(),
                        decl: None
                    })
                });
            }
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
            } else if self.follow(TokenVariant::RAngle) {
                Operator::Shr
            } else {
                Operator::Gt
            }
        } else if self.accept(TokenVariant::Assign) {
            //=, ==
            if self.follow(TokenVariant::Assign) {
                Operator::Eq
            } else {
                Operator::Assign
            }
        } else if self.accept(TokenVariant::Not) {
            // !=, !
            if self.follow(TokenVariant::Assign) {
                Operator::Ne
            } else {
                Operator::Not
            }
        } else if self.accept(TokenVariant::Or) {
            //||, |
            if self.follow(TokenVariant::Or) {
                Operator::OrOr
            } else {
                Operator::Or
            }
        } else if self.accept(TokenVariant::And) {
            //&&, &
            if self.follow(TokenVariant::And) {
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

    fn parse_prefix_expr(&mut self, op: Operator) -> Box<Expr> {
        let expr = self.parse_expr_prec(op.prec());
        Box::new(Expr::PrefixExpr(PrefixExpr { expr, op }))
    }

    fn parse_primary_expr(&mut self) -> Box<Expr> {
        match self.variant() {
            TokenVariant::LitInt |
            TokenVariant::LitReal    |
            TokenVariant::LitString  |
            TokenVariant::LitChar    |
            TokenVariant::LitBool => Box::new(Expr::Literal(self.parse_literal())),
            TokenVariant::Identifier => Box::new(Expr::Ident(IdentExpr { ident_use: Box::new(IdentUse::new(self.parse_identifier())) })),
            TokenVariant::LParen => {
                self.lex();
                let expr = self.parse_expr();
                self.expect(TokenVariant::RParen);
                expr
            }
            TokenVariant::If => self.parse_if(),
            _ => unreachable!()
        }
    }

    fn parse_infix_expr(&mut self, lhs: Box<Expr>, op: Operator) -> Box<Expr> {
        let rhs = self.parse_expr_prec(op.prec().next());
        Box::new(Expr::InfixExpr(InfixExpr { lhs, rhs, op }))
    }

    fn parse_postfix_expr(&mut self, lhs: Box<Expr>, op : Operator) -> Box<Expr> {
        match op {
            Operator::Inc |
            Operator::Dec => Box::new(Expr::PostfixExpr(PostfixExpr { expr: lhs, op })),
            Operator::LeftParen => Box::new(Expr::FnCallExpr(FnCallExpr{ callee: lhs, args: self.parse_expr_list(TokenVariant::Comma, TokenVariant::RParen) })),
            Operator::Dot => Box::new(Expr::FieldExpr(FieldExpr{
                target_: lhs,
                identifier_: Box::new(IdentUse::new(self.parse_identifier()) ),
                index_: 0
            })),
            _ => unreachable!()
        }
    }


    fn parse_expr_prec(&mut self, prec: Prec) -> Box<Expr> {
        let mut lhs = match self.parse_operator() {
            Some(op) => self.parse_prefix_expr(op),
            None => self.parse_primary_expr()
        };

        loop {
            if self.enter(TokenEnter::NL) {
                break;
            }

            if let Some(op) = self.parse_operator(){
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
                    unreachable!();
                }
            }else{
                break;
            }
        }

        return lhs;
    }


    fn parse_literal(&mut self) -> Literal {
        let token = self.lex();
        let str = &token.symbol.unwrap().value;

        match token.variant
        {
            TokenVariant::LitReal => Literal::Real(str.parse::<f64>().unwrap()),
            TokenVariant::LitInt => Literal::Int(str.parse::<i64>().unwrap()),
            TokenVariant::LitString => Literal::String(str.clone()),
            TokenVariant::LitChar => Literal::Char(str.chars().next().unwrap()),
            TokenVariant::LitBool => Literal::Bool(str == "true"),
            _ => unreachable!()
        }
    }

    fn parse_expr(&mut self) -> Box<Expr> {
        let result = self.parse_expr_prec(Prec::Bottom);
        self.accept(TokenVariant::Semicolon);
        result
    }

    fn parse_stmts(&mut self) -> Box<Stmt>{
        match self.variant() {
            TokenVariant::Let => self.parse_decl(),
            _=> Box::new(Stmt::Expr(ExprStmt{ expr: self.parse_expr() }))
        }
    }


    fn parse_if(&mut self) -> Box<Expr>{
        self.expect(TokenVariant::If);
        let condition = self.parse_expr();
        self.check(TokenVariant::LBrace);
        let if_branch = self.parse_block();

        let mut else_branch : Option<Box<Expr>> = None;
        if self.accept(TokenVariant::Else) {
            else_branch = Some(self.parse_block());
        }

        Box::new(Expr::IfExpr(IfExpr{condition, if_branch, else_branch}))
    }

    fn parse_decl(&mut self) -> Box<Stmt>{
        self.accept(TokenVariant::Let);
        let ident = self.parse_identifier();

        let mut ast_type = None;
        if self.accept(TokenVariant::Colon) {
            ast_type = Some(self.parse_type());
        }

        let mut init = None;
        if self.accept(TokenVariant::Assign) {
            init = Some(self.parse_expr());
        }

        Box::new(Stmt::Let(LetStmt{
            local_decl: Box::new(LetDecl {
                share : DeclShare::new(ident),
                ast_type
            }),
            init
        }))
    }

    fn parse_expr_list(&mut self, separator: TokenVariant, delimiter: TokenVariant) -> Vec<Box<Expr>> {
        let mut exprs = Vec::new();
        while !self.accept(delimiter) {
            if !exprs.is_empty() {
                self.expect(separator);
            }
            exprs.push(self.parse_expr());
        }
        return exprs;
    }

    fn parse_statement_list(&mut self) -> Vec<Box<Stmt>> {
        let mut stmts = Vec::new();
        while !self.accept(TokenVariant::RBrace) {
            if !stmts.is_empty() {
                assert!(self.enter(TokenEnter::NL), "Statement does not start with a new line");
            }
            stmts.push(self.parse_stmts());
        }
        return stmts;
    }

    fn parse_param(&mut self) -> Box<LetDecl>{
        let ident = self.parse_identifier();
        self.accept(TokenVariant::Colon);

        Box::new(LetDecl {
            share: DeclShare::new(ident),
            ast_type: Some(self.parse_type())
        })
    }

    fn parse_param_list(&mut self) -> Vec<Box<LetDecl>> {
        let mut params = Vec::new();
        while !self.accept(TokenVariant::RParen) {
            if !params.is_empty() {
                self.expect(TokenVariant::Comma);
            }
            params.push(self.parse_param());
        }
        return params;
    }

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