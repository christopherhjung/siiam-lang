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

    fn print_ty(&mut self, ty_ref: &TyRef){
        match &**ty_ref {
            Ty::Prim(prim_ty) => {
                match prim_ty {
                    PrimTy::Bool => self.print("bool"),
                    PrimTy::I32 => self.print("i32"),
                    PrimTy::I64 => self.print("i64"),
                    PrimTy::F32 => self.print("f32"),
                    PrimTy::F64 => self.print("f64"),
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
            }
            DeclKind::LocalDecl(_) => {
                self.print_symbol(decl.ident.sym);
                self.print(" : ");
                self.print_option_ty(&decl.ty);
            }
            DeclKind::MemberDecl(_) => {
                self.print_symbol(decl.ident.sym);
                self.print(" : ");
                self.print_option_ty(&decl.ty);
                self.nl();
            },
            DeclKind::FnDecl(fn_decl) => {
                self.keyword("fn");
                self.space();
                self.print_symbol(decl.ident.sym);
                self.print("(");
                let mut sep = "";
                for mut decl in &mut fn_decl.params{
                    self.print(sep);
                    self.visit_decl(&mut decl);
                    sep = ", ";
                }
                self.print(")");

                if let Some(fn_ty) = &decl.ty{
                    self.print(" : ");
                    let ty = &**fn_ty;
                    if let Ty::Fn(FnTy{ret_ty: Some(ret_ty), .. }) = &*ty{
                        self.print_ty(ret_ty);
                    }else{
                        self.print("?");
                    }
                }

                self.visit_expr(&mut fn_decl.body);
                self.nl();
            }
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
                self.visit_decl(&mut let_stmt.local_decl);
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
            ExprKind::If(if_expr) => {
                self.keyword("if");
                self.space();
                self.visit_expr(&mut if_expr.condition);
                self.visit_expr(&mut if_expr.true_branch);
                if let Some( false_branch) = &mut if_expr.false_branch{
                    self.keyword("else");
                    if let ExprKind::If(_) = false_branch.kind{
                        self.space();
                    }
                    self.visit_expr(false_branch);
                }
            }
            ExprKind::While(while_expr) => {
                self.keyword("while");
                self.space();
                self.visit_expr(&mut while_expr.condition);
                self.visit_expr(&mut while_expr.body);
                if let Some( else_branch) = &mut while_expr.else_branch{
                    self.keyword("else");
                    if let ExprKind::If(_) = else_branch.kind{
                        self.space();
                    }
                    self.visit_expr(else_branch);
                }
            }
            ExprKind::Block(block) => {
                self.print("{");
                self.nl();
                self.shift();

                for stmt in &mut block.stmts{
                    self.visit_stmt(stmt);
                }

                self.pop();
                self.print("}");
            }
            ExprKind::Literal(lit) => {
                match lit {
                    Literal::Str(str) => self.print(str.to_string()),
                    Literal::Bool(bool) => self.print(bool.to_string()),
                    Literal::Char(char) => self.print(char.to_string()),
                    Literal::Int(char) => self.print(char.to_string()),
                    Literal::Real(real) => self.print(real.to_string()),
                    _ => {}
                }
            }
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
            },
            ExprKind::Ret(ret_expr) => {
                self.print("return");
                if let Some(val) = &mut ret_expr.expr{
                    self.space();
                    self.visit_expr(val);
                }
            },
            ExprKind::FnCall(call_expr) => {
                self.visit_expr(&mut *call_expr.callee);
                self.print("(");
                let mut sep = "";
                for arg in &mut call_expr.args{
                    self.print(sep);
                    self.visit_expr(&mut *arg);
                    sep = ", ";
                }
                self.print(")");
            }
            _ => {}
        }
    }
}