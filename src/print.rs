use std::cell::RefCell;
use std::rc::Rc;
use crate::{Decl, Module, Sym, SymTable, Visitor};
use crate::ast::{DeclKind, Expr, ExprKind, Literal, PrimTy, Stmt};
use crate::ast::ExprKind::Block;
use crate::check::{FnTy, Ty, TyRef};

pub struct ProgramPrinter{
    pub result : String,
    depth : usize,
    need_indent: bool,
    sym_table: Rc<RefCell<SymTable>>
}

impl ProgramPrinter{
    pub fn new(sym_table: Rc<RefCell<SymTable>>) -> ProgramPrinter {
        ProgramPrinter{
            result : String::new(),
            depth : 0,
            need_indent: true,
            sym_table
        }
    }

    fn print<S: Into<String>>(&mut self, str: S){
        if self.need_indent {
            self.need_indent = false;
            for _ in 0 .. self.depth{
                self.result.push_str("    ");
            }
        }
        self.result.push_str(&*str.into());
    }

    fn nl(&mut self){
        self.need_indent = true;
        self.result.push('\n');
    }

    fn shift(&mut self){
        self.depth+=1;
    }

    fn pop(&mut self){
        self.depth-=1;
    }

    fn keyword(&mut self, str: &str){
        self.print(str);
    }

    fn space(&mut self){
        self.print(" ");
    }

    fn print_symbol(&mut self, sym: Sym){
        let name = SymTable::get(&RefCell::borrow(&self.sym_table), sym);
        self.print(name);
    }

    fn print_ty(&mut self, ty: &TyRef){
        let ty_ref = RefCell::borrow(ty);
        match &*ty_ref {
            Ty::Prim(prim_ty) => {
                match prim_ty {
                    PrimTy::Bool => self.print("bool"),
                    PrimTy::Int => self.print("i32"),
                    PrimTy::Long => self.print("i64"),
                    PrimTy::Float => self.print("f32"),
                    PrimTy::Double => self.print("f64"),
                    PrimTy::Unit => self.print("()"),
                    PrimTy::Char => self.print("char"),
                    PrimTy::Str => self.print("str"),
                    _ => self.print("?")
                }
            },
            Ty::Struct(struct_ty) => {
                self.print_symbol(struct_ty.name)
            },
            _ => self.print("?")
        }
    }

    fn print_option_ty(&mut self, option_ty: &Option<TyRef>){
        if let Some(ty) = &option_ty{
            self.print_ty(ty)
        }else{
            self.print("?");
        }
    }
}

impl Visitor for ProgramPrinter {
    fn enter_module(&mut self, module: &mut Module) {
        self.keyword("mod");
        self.space();
        self.print("{");
        self.nl();
        self.shift();
    }

    fn exit_module(&mut self, module: &mut Module) {
        self.pop();
        self.print("}");
        self.nl();
    }

    fn exit_decl(&mut self, decl: &mut Decl) {
        match &decl.kind {
            DeclKind::StructDecl(kind) => {
                self.pop();
                self.print("}");
                self.nl();
            },
            _ => {}
        }
    }

    fn visit_decl(&mut self, decl: &mut Decl) {
        match &mut decl.kind {
            DeclKind::StructDecl(kind) => {
                self.keyword("struct");
                self.space();
                self.print_symbol(decl.ident.sym);
                self.print("{");
                self.nl();
                self.shift();

                for mut member in &mut kind.members{
                    self.visit_decl(&mut member);
                }

                self.pop();
                self.print("}");
                self.nl();
            },
            DeclKind::MemberDecl(kind) => {
                self.print_symbol(decl.ident.sym);
                self.print(" : ");
                self.print_option_ty(&decl.ty);
                self.nl();
            },
            DeclKind::FnDecl(kind) => {
                self.keyword("fn");
                self.space();
                self.print_symbol(decl.ident.sym);
                self.print("(");

                for mut decl in &mut kind.params{
                    self.visit_decl(&mut decl);
                }
                self.print(")");

                if let Some(fn_ty) = &decl.ty{
                    self.print(" : ");
                    let ty = RefCell::borrow(fn_ty);
                    if let Ty::Fn(FnTy{ret_ty: Some(ret_ty), .. }) = &*ty{
                        self.print_ty(ret_ty);
                    }else{
                        self.print("?");
                    }
                }

                self.visit_expr(&mut kind.body);
            },
            _ => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                self.visit_expr(&mut expr_stmt.expr);
                self.nl();
            },
            Stmt::Let(let_stmt) =>{
                self.keyword("let");
                self.space();
                self.print_symbol(let_stmt.local_decl.ident.sym);
                self.print(" : ");
                self.print_option_ty(&let_stmt.local_decl.ty);
                if let Some( init) = &mut let_stmt.init{
                    self.print(" = ");
                    self.visit_expr( init);
                }
                self.nl();
            }
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Block(block) => {
                self.print("{");
                self.nl();
                self.shift();

                for stmt in &mut block.stmts{
                    self.visit_stmt(stmt);
                }

                self.pop();
                self.print("}");
                self.nl();
            },
            ExprKind::Literal(lit) => {
                match lit {
                    Literal::Str(str) => self.print(str.to_string()),
                    Literal::Bool(bool) => self.print(bool.to_string()),
                    Literal::Char(char) => self.print(char.to_string()),
                    Literal::Int(char) => self.print(char.to_string()),
                    Literal::Real(real) => self.print(real.to_string()),
                    _ => {}
                }
            },
            ExprKind::Ident(ident_expr) => {
                self.print_symbol(ident_expr.ident_use.ident.sym);
            },
            ExprKind::Infix(infix) => {
                self.visit_expr(&mut infix.lhs);
                self.space();
                self.print( infix.op.sign());
                self.space();
                self.visit_expr(&mut infix.rhs);
            },
            ExprKind::Prefix(prefix_expr) => {
                self.print( prefix_expr.op.sign());
                self.visit_expr(&mut prefix_expr.expr);
            },
            ExprKind::Postfix(postfix_expr) => {
                self.visit_expr(&mut postfix_expr.expr);
                self.print( postfix_expr.op.sign());
            }
            _ => {}
        }
    }
}