use std::alloc::alloc;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::ops::{Deref, Index};
use std::ptr;
use std::ptr::{null, null_mut};
use std::rc::Rc;
use sha2::{Digest, Sha256};
use sha2::digest::Update;
use crate::sign::Signature;
use crate::utils::Array;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use crate::def::{Def, DefProxy, DefPtr, Variant};

pub struct World{
    sign_size: usize,
    sea : HashMap<Signature, Box<Def>>,
    pending : HashMap<DefPtr, Box<Def>>,
    axioms : HashMap<Axiom, DefPtr>
}

#[derive(EnumIter, Hash, Eq, PartialEq)]
pub enum Axiom{
    Bot, Tuple, Pack, Extract, App, Pi, Lam, Var,
    TypeI32
}

impl World{
    fn as_mut(&self) -> *mut World{
        self as *const _ as *mut World
    }

    pub fn axiom( &self, ax : Axiom ) -> DefProxy {
        DefProxy {
            world: self.as_mut(),
            ptr: *self.axioms.get(&ax).unwrap()
        }
    }

    fn new_def_proxy(&mut self, def: DefPtr) -> DefProxy {
        DefProxy::new(self.as_mut(), def)
    }

    fn new_def(&mut self, ops : Vec<DefProxy>) -> DefProxy {
        self.new_data_def(ops, Array::empty())
    }

    fn new_data_def(&mut self, ops : Vec<DefProxy>, data: Array<u8>) -> DefProxy {
        let mut def = Def::new_boxed(ops, data);

        let def_ptr : DefPtr = &*def;
        if def.variant == Variant::Pending{
            self.pending.insert(def_ptr, def);
        }else{
            let sign = Signature::from(&*def);
            def.sign = Some(sign);
            self.sea.insert(sign, def);
        }

        self.new_def_proxy(def_ptr)
    }

    pub fn finalize(&mut self, def_ref : &mut DefProxy){
        if let Some(mut def) = self.pending.remove(&mut def_ref.ptr){
            let sign = Signature::from(&*def);
            def.sign = Some(sign);
            match self.sea.entry(sign) {
                Occupied(entry) => {def_ref.ptr = &**entry.get();},
                Vacant(entry) => {entry.insert(def);}
            }
        }else{
            panic!()
        }
    }

    fn default(&mut self) -> DefProxy {
        self.bot()
    }

    pub fn lit_int( &mut self, val : i32 ) -> DefProxy {
        self.default()
    }

    pub fn ty_int( &mut self, width : i32 ) -> DefProxy {
        self.default()
    }

    pub fn bot( &mut self ) -> DefProxy {
        self.axiom(Axiom::Bot)
    }

    pub fn tuple(&mut self, mut elems: Vec<DefProxy> ) -> DefProxy {
        elems.insert(0, self.axiom(Axiom::Tuple));
        self.new_def(elems)
    }

    pub fn pack(&mut self, shape: DefProxy, body: DefProxy) -> DefProxy {
        self.new_def(vec![self.axiom(Axiom::Pack), shape, body])
    }

    pub fn extract(&mut self, tup: DefProxy, index: DefProxy) -> DefProxy {
        self.new_def(vec![self.axiom(Axiom::Extract), tup, index])
    }

    pub fn app(&mut self, callee: DefProxy, arg: DefProxy) -> DefProxy {
        self.new_def(vec![self.axiom(Axiom::App), callee, arg])
    }

    pub fn pi(&mut self, domain: DefProxy, co_domain : DefProxy) -> DefProxy {
        self.new_def(vec![self.axiom(Axiom::Pi), domain, co_domain])
    }

    pub fn lam(&mut self, ty : DefProxy) -> DefProxy {
        self.new_def(vec![self.axiom(Axiom::Lam), ty, DefProxy::null(self.as_mut())])
    }

    pub fn var(&mut self, lam: DefProxy) -> DefProxy {
        self.new_def(vec![self.axiom(Axiom::Var), lam])
    }

    pub fn set_body(&mut self, lam: DefProxy, body: DefProxy){
        lam.set_op(1, body);
        //close loop
    }

    pub fn new() -> Box<World>{
        let sea = HashMap::new();
        let axioms = HashMap::new();

        let mut world = Box::new(World{
            sign_size: 32,
            pending: HashMap::new(),
            sea,
            axioms
        });

        let origin = Def::new_boxed(Vec::new(), Array::empty());

        let origin_ref = world.new_def(Vec::new());
        let mut link = origin_ref.ptr;

        for axiom in Axiom::iter(){
            let mut link_arr = Vec::new();
            link_arr.push( DefProxy::no_world(link));
            let axiom_ref = world.new_def(link_arr);
            link = axiom_ref.ptr;
            world.axioms.insert(axiom, axiom_ref.ptr);
        }

        world
    }
}

