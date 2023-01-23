use std::collections::HashMap;
use std::rc::{Rc, Weak};

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Sym {
    pub id: usize
}

pub struct SymTable {
    str : Vec<String>,
    map : HashMap<String, Sym>
}

impl SymTable {
    pub fn from( &mut self, str : String ) -> Sym {
        if let Some(sym) = self.map.get(&str){
            sym.clone()
        }else{
            let sym = Sym{id: self.map.len()};
            self.map.insert(str.clone(), sym);
            self.str.push(str);
            sym.clone()
        }
    }

    pub fn get(&self, sym: Sym ) -> String {
        self.str[sym.id].clone()
    }

    pub fn new() -> SymTable{
        SymTable{
            str : Vec::new(),
            map : HashMap::new()
        }
    }
}