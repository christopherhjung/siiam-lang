use std::cell::RefCell;
use std::process::id;
use std::rc::Rc;

use sha2::digest::generic_array::typenum::Exp;

use crate::{Lexer, SymTable};
use crate::ast::*;
use crate::sym::Sym;
use crate::token::*;

const LOOKAHEAD_SIZE: usize = 3;

pub struct Parser {
    lexer: Lexer,
    ahead: [Token; LOOKAHEAD_SIZE],
    ahead_offset: usize,
    new_lines: usize,
    sym_table: Rc<RefCell<SymTable>>
}

impl Parser {
    fn shift(&mut self) {
        let mut new_token = self.lexer.next_token();
        self.ahead[self.ahead_offset] = new_token;
        self.ahead_offset = (self.ahead_offset + 1) % LOOKAHEAD_SIZE;
    }

    fn lex(&mut self) -> Token {
        let result: Token = self.ahead().clone();
        self.shift();
        return result;
    }

    fn kind(&mut self) -> TokenKind {
        self.ahead().kind
    }

    fn enter(&mut self, enter: TokenEnter) -> bool {
        self.ahead().enter == enter
    }

    fn ahead(&mut self) -> &mut Token {
        self.ahead_at(0)
    }

    fn ahead_at(&mut self, i: usize) -> &mut Token {
        assert!(i < LOOKAHEAD_SIZE, "lookahead overflow!");
        return &mut self.ahead[(i + self.ahead_offset) % LOOKAHEAD_SIZE];
    }
    fn check_kind(&mut self, kind: TokenKind) -> bool {
        kind == self.kind()
    }

    fn accept(&mut self, kind: TokenKind) -> bool {
        if kind == self.kind() {
            self.shift();
            return true;
        }

        return false;
    }

    fn follow(&mut self, kind: TokenKind) -> bool {
        if self.enter(TokenEnter::Token) {
            return self.accept(kind);
        }

        return false;
    }

    fn expect(&mut self, kind: TokenKind) {
        assert!(self.accept(kind), "Kinds do not match!");
    }

    fn expect_enter(&mut self, enter: TokenEnter) {
        assert_eq!(self.ahead().enter, enter, "Kinds do not match!");
    }

    pub fn parse_module(&mut self) -> Box<Module> {
        let mut items = Vec::new();
        while !self.accept(TokenKind::Eof) && !self.accept(TokenKind::RBrace) {
            items.push(self.parse_item());
        }
        Box::new(Module { items })
    }


    fn parse_item(&mut self) -> Box<Decl> {
        return match self.kind() {
            TokenKind::Fn => self.parse_fn(),
            TokenKind::Struct => self.parse_struct(),
            _ => unreachable!()
        }
    }

    fn parse_ident(&mut self) -> Box<Ident> {
        assert!(self.check_kind(TokenKind::Ident));
        let sym = self.lex().symbol.unwrap();
        return Box::new(Ident { sym });
    }

    fn parse_struct(&mut self) -> Box<Decl> {
        self.expect(TokenKind::Struct);
        let ident = self.parse_ident();
        self.expect(TokenKind::LBrace);
        let fields = self.parse_field_list();
        Box::new(Decl::new(ident, DeclKind::StructDecl(StructDecl{ members: fields })))
    }

    fn parse_fn(&mut self) -> Box<Decl> {
        self.expect(TokenKind::Fn);
        let ident = self.parse_ident();
        self.expect(TokenKind::LParen);
        let params = self.parse_param_list();
        let mut return_type : Option<Box<ASTTy>> = None;
        if self.accept(TokenKind::Colon) {
            return_type = Some(self.parse_type());
        }

        let body = self.parse_block();

        Box::new(Decl::new(ident, DeclKind::FnDecl(FnDecl{
            params,
            return_type,
            body
        })))
    }

    fn parse_block(&mut self) -> Box<Expr>{
        self.expect(TokenKind::LBrace);
        let stmts = self.parse_statement_list();
        Box::new(Expr::new(ExprKind::Block(Block{ stmts })))
    }

    fn parse_field(&mut self, i: usize) -> Box<Decl> {
        let ident = self.parse_ident();
        self.accept(TokenKind::Colon);

        Box::new(Decl::new(ident, DeclKind::MemberDecl(MemberDecl {
            ast_type: self.parse_type(),
            index: i,
        })))
    }

    fn prim_type(&mut self) -> PrimTy{
        let ty = match self.kind() {
            TokenKind::TypeBool => PrimTy::Bool,
            TokenKind::TypeByte => PrimTy::Byte,
            TokenKind::TypeChar => PrimTy::Char,
            TokenKind::TypeStr => PrimTy::Str,
            TokenKind::TypeInt => PrimTy::Int,
            TokenKind::TypeLong => PrimTy::Long,
            TokenKind::TypeFloat => PrimTy::Float,
            TokenKind::TypeDouble => PrimTy::Double,
            TokenKind::TypeUnit => PrimTy::Unit,
            _ => unreachable!()
        };

        self.shift();
        ty
    }

    fn parse_type(&mut self) -> Box<ASTTy> {
        Box::new(match self.kind() {
            TokenKind::TypeBool |
            TokenKind::TypeByte |
            TokenKind::TypeChar |
            TokenKind::TypeStr |
            TokenKind::TypeInt |
            TokenKind::TypeLong |
            TokenKind::TypeFloat |
            TokenKind::TypeDouble |
            TokenKind::TypeUnit => {
                ASTTy::Prim(self.prim_type())
            }
            TokenKind::Ident => {
                let ident = self.parse_ident();
                ASTTy::Struct(StructTy {
                    ident_use: Box::new(IdentUse::new(ident))
                })
            }
            TokenKind::Fn => {
                self.lex();
                self.accept(TokenKind::LParen);
                let param_types = self.parse_type_list();
                self.accept(TokenKind::Arrow);
                let return_type = self.parse_type();
                ASTTy::Fn(FnTy { param_types, return_type })
            }
            _ => ASTTy::Err
        })
    }

    fn parse_list<T>(&mut self, mut f: impl FnMut() -> Box<T>, separator: TokenKind, delimiter: TokenKind) -> Vec<Box<T>> {
        let mut exprs = Vec::new();
        while !self.accept(delimiter) {
            if !exprs.is_empty() {
                self.expect(separator);
            }
            exprs.push(f());
        }
        return exprs;
    }

    fn parse_operator(&mut self) -> Option<Op> {
        Some(if self.accept(TokenKind::Plus) {
            //+, ++, +=
            if self.follow(TokenKind::Plus) {
                Op::Inc
            } else if self.follow(TokenKind::Assign) {
                Op::Assign
            } else {
                Op::Add
            }
        } else if self.accept(TokenKind::Minus) {
            // -, --, -=, ->
            if self.follow(TokenKind::Minus) {
                Op::Dec
            } else if self.follow(TokenKind::RAngle) {
                Op::Arrow
            } else if self.follow(TokenKind::Assign) {
                Op::SubAssign
            } else {
                Op::Sub
            }
        } else if self.accept(TokenKind::Star) {
            // *, *=
            if self.follow(TokenKind::Assign) {
                Op::MulAssign
            } else {
                Op::Mul
            }
        } else if self.accept(TokenKind::Slash) {
            // /, /=
            if self.follow(TokenKind::Assign) {
                Op::DivAssign
            } else {
                Op::Div
            }
        } else if self.accept(TokenKind::LAngle) {
            // <, <=, <<
            if self.follow(TokenKind::Assign) {
                Op::Le
            } else if self.follow(TokenKind::LAngle) {
                Op::Shl
            } else {
                Op::Lt
            }
        } else if self.accept(TokenKind::RAngle) {
            // >, >=, >>
            if self.follow(TokenKind::Assign) {
                Op::Ge
            } else if self.follow(TokenKind::RAngle) {
                Op::Shr
            } else {
                Op::Gt
            }
        } else if self.accept(TokenKind::Assign) {
            //=, ==
            if self.follow(TokenKind::Assign) {
                Op::Eq
            } else {
                Op::Assign
            }
        } else if self.accept(TokenKind::Not) {
            // !=, !
            if self.follow(TokenKind::Assign) {
                Op::Ne
            } else {
                Op::Not
            }
        } else if self.accept(TokenKind::Or) {
            //||, |
            if self.follow(TokenKind::Or) {
                Op::Or
            } else {
                Op::BitOr
            }
        } else if self.accept(TokenKind::And) {
            //&&, &
            if self.follow(TokenKind::And) {
                Op::And
            } else {
                Op::BitAnd
            }
        } else if self.accept(TokenKind::LBracket) {
            Op::LeftBracket
        } else if self.accept(TokenKind::LParen) {
            Op::LeftParen
        } else {
            return None;
        })
    }

    fn parse_prefix_expr(&mut self, op: Op) -> Box<Expr> {
        let expr = self.parse_expr_prec(op.prec());
        Box::new(Expr::new(ExprKind::Prefix(PrefixExpr { expr, op })))
    }

    fn parse_primary_expr(&mut self) -> Box<Expr> {
        match self.kind() {
            TokenKind::LitInt  |
            TokenKind::LitReal |
            TokenKind::LitStr  |
            TokenKind::LitChar |
            TokenKind::LitBool => Box::new(Expr::new(ExprKind::Literal(self.parse_literal()))),
            TokenKind::Ident => Box::new(Expr::new(ExprKind::Ident(IdentExpr { ident_use: Box::new(IdentUse::new(self.parse_ident())) }))),
            TokenKind::LParen => {
                self.lex();
                let expr = self.parse_expr();
                self.expect(TokenKind::RParen);
                expr
            }
            TokenKind::If => self.parse_if(),
            _ => unreachable!()
        }
    }

    fn parse_infix_expr(&mut self, lhs: Box<Expr>, op: Op) -> Box<Expr> {
        let rhs = self.parse_expr_prec(op.prec().next());
        Box::new(Expr::new(ExprKind::Infix(InfixExpr { lhs, rhs, op })))
    }

    fn parse_postfix_expr(&mut self, lhs: Box<Expr>, op : Op) -> Box<Expr> {
        match op {
            Op::Inc |
            Op::Dec => Box::new(Expr::new(ExprKind::Postfix(PostfixExpr { expr: lhs, op }))),
            Op::LeftParen => Box::new(Expr::new(ExprKind::FnCall(FnCallExpr{ callee: lhs, args: self.parse_expr_list(TokenKind::Comma, TokenKind::RParen) }))),
            Op::Dot => Box::new(Expr::new(ExprKind::Field(FieldExpr{
                target: lhs,
                identifier: Box::new(IdentUse::new(self.parse_ident()) ),
                index: 0
            }))),
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
        let sym = &token.symbol.unwrap();
        let str = self.sym_table.borrow().get(*sym);

        match token.kind
        {
            TokenKind::LitReal => Literal::Real(str.parse::<f64>().unwrap()),
            TokenKind::LitInt => Literal::Int(str.parse::<i64>().unwrap()),
            TokenKind::LitStr => Literal::Str(str),
            TokenKind::LitChar => Literal::Char(str.chars().next().unwrap()),
            TokenKind::LitBool => Literal::Bool(str == "true"),
            _ => unreachable!()
        }
    }

    fn parse_expr(&mut self) -> Box<Expr> {
        let result = self.parse_expr_prec(Prec::Bottom);
        self.accept(TokenKind::Semicolon);
        result
    }

    fn parse_stmts(&mut self) -> Box<Stmt>{
        match self.kind() {
            TokenKind::Let => self.parse_decl(),
            _ => Box::new(Stmt::Expr(ExprStmt{ expr: self.parse_expr() }))
        }
    }


    fn parse_if(&mut self) -> Box<Expr>{
        self.expect(TokenKind::If);
        let condition = self.parse_expr();
        self.check_kind(TokenKind::LBrace);
        let if_branch = self.parse_block();

        let mut else_branch : Option<Box<Expr>> = None;
        if self.accept(TokenKind::Else) {
            else_branch = Some(self.parse_block());
        }

        Box::new(Expr::new(ExprKind::If(IfExpr{condition, if_branch, else_branch})))
    }

    fn parse_decl(&mut self) -> Box<Stmt>{
        self.accept(TokenKind::Let);
        let ident = self.parse_ident();

        let mut ast_type = None;
        if self.accept(TokenKind::Colon) {
            ast_type = Some(self.parse_type());
        }

        let mut init = None;
        if self.accept(TokenKind::Assign) {
            init = Some(self.parse_expr());
        }

        Box::new(Stmt::Let(LetStmt{
            local_decl: Box::new(Decl::new(ident, DeclKind::LetDecl(LetDecl {
                ast_type
            }))),
            init
        }))
    }

    fn parse_expr_list(&mut self, separator: TokenKind, delimiter: TokenKind) -> Vec<Box<Expr>> {
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
        while !self.accept(TokenKind::RBrace) {
            if !stmts.is_empty() {
                assert!(self.enter(TokenEnter::NL), "Statement does not start with a new line");
            }
            stmts.push(self.parse_stmts());
        }
        return stmts;
    }

    fn parse_param(&mut self) -> Box<Decl>{
        let ident = self.parse_ident();
        self.accept(TokenKind::Colon);

        Box::new(Decl::new(ident, DeclKind::LetDecl(LetDecl {
            ast_type: Some(self.parse_type())
        })))
    }

    fn parse_param_list(&mut self) -> Vec<Box<Decl>> {
        let mut params = Vec::new();
        while !self.accept(TokenKind::RParen) {
            if !params.is_empty() {
                self.expect(TokenKind::Comma);
            }
            params.push(self.parse_param());
        }
        return params;
    }

    fn parse_field_list(&mut self) -> Vec<Box<Decl>> {
        let mut exprs = Vec::new();
        while !self.accept(TokenKind::RBrace) {
            if !exprs.is_empty() {
                self.expect_enter(TokenEnter::NL);
            }
            exprs.push(self.parse_field(exprs.len()));
        }
        return exprs;
    }

    fn parse_type_list(&mut self) -> Vec<Box<ASTTy>> {
        let mut types = Vec::new();
        while !self.accept(TokenKind::RParen) {
            if !types.is_empty() {
                self.expect(TokenKind::Comma);
            }
            types.push(self.parse_type());
        }
        return types;
    }

    pub fn new(mut lexer: Lexer, sym_table: Rc<RefCell<SymTable>>) -> Parser {
        let lookahead = [lexer.next_token(), lexer.next_token(), lexer.next_token()];

        Parser {
            lexer,
            ahead: lookahead,
            ahead_offset: 0,
            new_lines: 0,
            sym_table
        }
    }
}