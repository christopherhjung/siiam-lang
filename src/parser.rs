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
    params: Vec<Box<Decl>>,
    return_type: Box<dyn ASTType>,
    body: Box<Block>,
}

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
    shadows: Box<Decl>,
    depth: u32,
    ast_type: Box<dyn ASTType>,
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

    fn lookahead(&mut self) -> &mut Token {
        self.lookahead_at(0)
    }

    fn lookahead_at(&mut self, i: usize) -> &mut Token {
        assert!(i < LOOKAHEAD_SIZE, "lookahead overflow!");
        return &mut self.lookahead[(i + self.lookahead_idx) % LOOKAHEAD_SIZE];
    }
    fn check(&mut self, variant: TokenVariant) -> bool {
        variant == self.lookahead().variant
    }

    fn accept(&mut self, variant: TokenVariant) -> bool {
        if variant == self.lookahead().variant {
            self.shift();
            return true;
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
        match self.lookahead().variant {
            //TokenVariant::Fn => return self.parse_fn(),
            TokenVariant::Struct => return self.parse_struct(),
            _ => unreachable!()
        }
    }

    fn parse_identifier(&mut self) -> Box<Ident> {
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
        match self.lookahead().variant {
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
            TokenVariant::ID => {
                return Box::new(SuperASTType(IdentUse(parse_identifier())));
            }
            TokenVariant::Fn => {
                self.lex();
                self.accept(TokenVariant::LParen);
                let param_types = self.parse_type_list();
                self.accept(TokenVariant::Arrow);
                let return_type = self.parse_type();
                return Box::new(FnASTType { param_types, return_type });
            }
            _ => unreachable!()
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

    fn parse_expr_list(&mut self, seperator: TokenVariant, delimiter: TokenVariant) -> Vec<Box<dyn Expr>> {
        let mut exprs = Vec::new();
        while !self.accept(delimiter) {
            if !exprs.is_empty() {
                self.expect(separator);
            }
            exprs.push(parse_expr());
        }
        return exprs;
    }

    fn parse_operator(){

    }

    fn parse_expr_prec(&mut self, prec: Prec) -> Box<dyn Expr> {
        let lhs = if lookahead().is_prefix() {
            parse_prefix_expr()
        } else {
            parse_primary_expr()
        };

        loop {
            if checkNL() {
                break;
            }

            if lookahead().is_infix() {
                if prec > lookahead().prec() {
                    break;
                }

                lhs = parse_infix_expr(lhs);
            } else if lookahead().is_postfix() {
                if prec > Prec::Unary {
                    break;
                }

                lhs = parse_postfix_expr(lhs);
            } else {
                break;
            }
        }

        return lhs;
    }

    fn parse_expr(&mut self) -> Box<dyn Expr> {
        let result = parse_expr(Prec::Bottom);
        self.accept(Token::Semicolon);
        result
    }

    fn parse_statement_list(&mut self, delimiter: TokenVariant) -> Vec<Box<dyn Stmt>> {
        return vec![];
    }

    fn parse_param_list(&mut self) -> Vec<Box<Decl>> {
        return vec![];
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