use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use crate::ast::*;
use crate::visitor::Visitor;
use crate::Module;
use crate::sym::{Sym};

pub struct NameResolution {
    levels : Vec<usize>,
    decls : Vec<*const Decl>,
    symbol2decl : HashMap<usize, *const Decl>
}

impl NameResolution {
    pub fn push_scope(&mut self){
        self.levels.push(self.decls.len())
    }

    pub fn pop_scope(&mut self){
        let last_level = *self.levels.last().unwrap();

        for i in last_level .. self.decls.len(){
            let last_decl_ref = self.decls.pop().unwrap();
            let mut last_decl = unsafe { &*last_decl_ref };

            self.symbol2decl.insert(
                last_decl.ident.sym.id,
                last_decl.shadows.as_ref().unwrap().clone()
            );
        }

        self.levels.pop();
    }

    fn lookup(&mut self, sym : Sym) -> Option<*const Decl>{
        if let Some(rc) = self.symbol2decl.get(&sym.id){
            Some(rc.clone())
        }else{
            None
        }
    }

    pub fn find_local(&mut self, sym : Sym) -> Option<*const Decl>{
        if let Some(other) = self.lookup(sym) {
            let mut decl = unsafe { &*other };
            if decl.depth == self.levels.len(){
                return Some(other.clone())
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

    pub fn new() -> NameResolution {
        NameResolution {
            levels: Vec::new(),
            decls: Vec::new(),
            symbol2decl: HashMap::new()
        }
    }
}

impl Visitor for NameResolution{
    fn enter_decl(&mut self, decl: &mut Decl) {
        match &mut decl.kind {
            DeclKind::LetDecl(kind) => self.insert(decl),
            DeclKind::FnDecl(kind) => self.insert(decl),
            _ => return
        }
    }

    fn visit_ident_expr(&mut self, ident_expr: &mut IdentExpr) {

        let ident_use = &mut ident_expr.ident_use;
        let decl = self.lookup(ident_use.ident.sym);
        ident_use.decl = decl;
        //println!("{:#?}", decl);
        //println!("{:#?}", ident_expr);
    }
}
