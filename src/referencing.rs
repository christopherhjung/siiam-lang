use std::collections::HashMap;
use std::rc::Weak;
use crate::ast::Decl;
use crate::lexer::Symbol;

struct Referencing{
    levels : Vec<usize>,
    decls : Vec<Weak<Decl>>,
    symbol2decl : HashMap<Symbol, Weak<Decl>>
}

impl Referencing{
    fn push_scope(&mut self){
        self.levels.push(self.decls.len())
    }

    fn pop_scope(&mut self){
        let last_level = *self.levels.last().unwrap();

        for i in last_level .. self.decls.len(){
            let last_decl_weak = self.decls.pop().unwrap();
            let last_decl = unsafe { &*last_decl_weak.as_ptr() };
            //self.symbol2decl[last_decl.identifier.sym] = Weak::clone(*last_decl.shadows);
        }

        self.levels.pop();
    }
}