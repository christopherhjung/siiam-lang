use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use crate::ast::*;
use crate::Module;
use crate::sym::{Sym, SymRef};

pub struct NameResolution {
    levels : Vec<usize>,
    decls : Vec<Rc<RefCell<Decl>>>,
    symbol2decl : HashMap<*const Sym, Rc<RefCell<Decl>>>
}

fn key(sym: &SymRef) -> *const Sym{
    &**sym as *const Sym
}

impl NameResolution {
    pub fn push_scope(&mut self){
        self.levels.push(self.decls.len())
    }

    pub fn pop_scope(&mut self){
        let last_level = *self.levels.last().unwrap();

        for i in last_level .. self.decls.len(){
            let last_decl_ref = self.decls.pop().unwrap();
            let mut last_decl = RefCell::borrow_mut(&last_decl_ref);

            self.symbol2decl.insert(
                key(&last_decl.share().ident.sym),
                last_decl.share().shadows.as_ref().unwrap().clone()
            );
        }

        self.levels.pop();
    }

    fn lookup(&mut self, sym : &SymRef) -> Option<Rc<RefCell<Decl>>>{
        if let Some(rc) = self.symbol2decl.get(&key(sym)){
            Some(rc.clone())
        }else{
            None
        }
    }

    pub fn find_local(&mut self, sym : &SymRef) -> Option<Rc<RefCell<Decl>>>{
        if let Some(other) = self.lookup(sym) {
            if RefCell::borrow_mut(&other).share().depth == self.levels.len(){
                return Some(other.clone())
            }
        }

        return None
    }

    pub fn insert(&mut self, decl : Rc<RefCell<Decl>>){
        let mut cell = RefCell::borrow_mut(&decl);
        let share = cell.share();
        let sym = share.ident.sym.clone();

        share.depth = self.levels.len();
        share.shadows = self.lookup(&sym);

        if let Some( other ) = self.find_local(&sym){
            unreachable!();
        }

        self.symbol2decl.insert(key(&sym), decl.clone());
        self.decls.push(decl.clone());
    }

    pub fn new() -> NameResolution {
        NameResolution {
            levels: Vec::new(),
            decls: Vec::new(),
            symbol2decl: HashMap::new()
        }
    }
}

pub trait NameResolutionImpl {
    fn resolve(&mut self, res: &mut NameResolution){

    }
}

impl NameResolutionImpl for Module{
    fn resolve(&mut self, res: &mut NameResolution){
        for item in &mut self.items{
            item.resolve(res);
        }
    }
}

impl NameResolutionImpl for Ident{
    fn resolve(&mut self, res: &mut NameResolution){
    }
}

impl NameResolutionImpl for IdentUse{
    fn resolve(&mut self, res: &mut NameResolution){
        self.decl = res.lookup(&self.ident.sym);
    }
}

impl NameResolutionImpl for FnDecl{
    fn resolve(&mut self, res: &mut NameResolution){
        //res.insert(self);
        res.push_scope();
    }
}

impl NameResolutionImpl for StructDecl{
    fn resolve(&mut self, res: &mut NameResolution){
    }
}

impl NameResolutionImpl for LetDecl{
    fn resolve(&mut self, res: &mut NameResolution){
    }
}

impl NameResolutionImpl for Decl{
    fn resolve(&mut self, res: &mut NameResolution){
        match self {
            Decl::LetDecl(decl) => decl.resolve(res),
            Decl::FnDecl(decl) => decl.resolve(res),
            Decl::FieldDecl(decl) => decl.resolve(res)
        }
    }
}


/*
impl <T: ASTNode> Reference for T{
    fn reference(&mut self){
    }
}*/