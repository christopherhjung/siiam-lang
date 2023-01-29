use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::ast::*;
use crate::Module;
use crate::sym::Sym;
use crate::visitor::Visitor;

pub struct NameBinder {
    levels : Vec<usize>,
    decls : Vec<*const Decl>,
    symbol2decl : HashMap<usize, *const Decl>
}

impl NameBinder {
    pub fn push_scope(&mut self){
        self.levels.push(self.decls.len())
    }

    pub fn pop_scope(&mut self){
        let last_level = *self.levels.last().unwrap();

        for _ in last_level .. self.decls.len(){
            let last_decl_ref = self.decls.pop().unwrap();
            let mut last_decl = unsafe { &*last_decl_ref };

            if let Some(shadows) = last_decl.shadows{
                self.symbol2decl.insert(
                    last_decl.ident.sym.id,
                    shadows
                );
            }
        }

        self.levels.pop();
    }

    fn lookup(&mut self, sym : Sym) -> Option<*const Decl>{
        if let Some(decl) = self.symbol2decl.get(&sym.id){
            Some(*decl)
        }else{
            None
        }
    }

    pub fn find_local(&mut self, sym : Sym) -> Option<*const Decl>{
        if let Some(other) = self.lookup(sym) {
            let mut decl = unsafe { &*other };
            if decl.depth == self.levels.len(){
                return Some(other)
            }
        }

        return None
    }

    pub fn insert(&mut self, decl : &mut Decl){
        let decl_ptr = decl as *const Decl;
        let sym = decl.ident.sym;

        decl.depth = self.levels.len();
        decl.shadows = self.lookup(sym);

        if let Some( other ) = self.find_local(sym){
            unreachable!();
        }

        self.symbol2decl.insert(sym.id, decl_ptr);
        self.decls.push(decl_ptr);
    }

    pub fn resolve(&mut self, module: &mut Module){
        self.visit_module(module);
    }

    pub fn new() -> NameBinder {
        NameBinder {
            levels: Vec::new(),
            decls: Vec::new(),
            symbol2decl: HashMap::new()
        }
    }
}

impl Visitor for NameBinder {
    fn enter_decl(&mut self, decl: &mut Decl) {
        match &mut decl.kind {
            DeclKind::LocalDecl(_) => self.insert(decl),
            DeclKind::FnDecl(_) => self.insert(decl),
            DeclKind::StructDecl(_) => self.insert(decl),
            DeclKind::MemberDecl(member) => {
                let mut ast_type : &mut ASTTy = &mut member.ast_ty;
                if let ASTTy::Struct(struct_ty) = &mut ast_type{
                    let decl = self.lookup(struct_ty.ident_use.ident.sym);
                    struct_ty.ident_use.decl = decl;
                }
            },
            _ => return
        }
    }

    fn enter_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Block(_) => self.push_scope(),
            ExprKind::Ident(ident_expr) => {
                let ident_use = &mut ident_expr.ident_use;
                let decl = self.lookup(ident_use.ident.sym);
                ident_use.decl = decl
            },
            _ => {}
        }
    }

    fn exit_expr(&mut self, expr: &mut Expr) {
        if let ExprKind::Block(_) = expr.kind{
            self.pop_scope();
        }
    }
}
