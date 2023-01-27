use std::collections::HashMap;
use std::rc::{Rc, Weak};

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Sym {
    pub id: usize
}

pub struct SymTable {
    id2str: Vec<String>,
    str2sym: HashMap<String, Sym>
}

impl SymTable {
    pub fn from( &mut self, str : String ) -> Sym {
        if let Some(sym) = self.str2sym.get(&str){
            *sym
        }else{
            let sym = Sym{id: self.str2sym.len()};
            self.str2sym.insert(str.clone(), sym);
            self.id2str.push(str);
            sym
        }
    }

    pub fn get(&self, sym: Sym ) -> String {
        self.id2str[sym.id].clone()
    }

    pub fn new() -> SymTable{
        SymTable{
            id2str: Vec::new(),
            str2sym: HashMap::new()
        }
    }
}