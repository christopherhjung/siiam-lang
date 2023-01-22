use std::collections::HashMap;
use std::rc::{Rc, Weak};

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Sym {
    pub value: String
}

impl Sym{
    pub(crate) fn key(&self) -> *const Sym{
        self as *const Sym
    }
}

pub type SymRef = Rc<Sym>;

pub struct SymTable {
    map : HashMap<String, Rc<Sym>>
}

impl SymTable {
    pub fn from( &mut self, str : String ) -> SymRef {
        let rc = Rc::new(Sym{value: str.clone()});
        self.map.insert(str, rc.clone());
        rc
    }

    pub fn new() -> SymTable{
        SymTable{
            map : HashMap::new()
        }
    }
}