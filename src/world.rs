use std::alloc::alloc;
use std::cell::{RefCell, RefMut, UnsafeCell};
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::ops::{Deref, DerefMut, Index};
use std::ptr;
use std::ptr::{null, null_mut};
use std::rc::Rc;
use sha2::{Digest, Sha256};
use sha2::digest::Update;
use crate::sign::{AcyclicSigner, CyclicSigner, Signature};
use crate::array::Array;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use crate::def::{Def, DefProxy, DefPtr, Mode};
use crate::utils::UnsafeMut;

pub struct World{
    sign_size: usize,
    sea : HashMap<Signature, Box<Def>>,
    pending : HashMap<DefPtr, Box<Def>>,
    axioms : HashMap<Axiom, DefPtr>,
    axioms_rev : HashMap<DefPtr, Axiom>
}

#[derive(EnumIter, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum Axiom{
    Bot, Tuple, Pack, Extract, App, Pi, Lam, Var,
    TypeI32
}

pub struct DepCheck {
    visited: HashSet<DefPtr>
}

impl DepCheck {
    pub fn valid(proxy: DefProxy) -> bool{
        let mut check = DepCheck {
            visited: HashSet::new()
        };

        check.valid_impl(proxy.ptr)
    }

    fn valid_impl( &mut self, current: DefPtr) -> bool{
        if current == null(){
            return false;
        }

        if !self.visited.insert(current){
            return true;
        }

        for dep_ptr in DefProxy::from(current).ops() {
            if !self.valid_impl(*dep_ptr){
                return false
            }
        }

        return true
    }
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
        let mut proxy = self.new_def_proxy(def_ptr);
        if def.mode == Mode::Pending{
            self.pending.insert(def_ptr, def);
        }else{
            let signed = proxy.sign();
            self.sea.insert(proxy.sign().unwrap(), def);
        }

        proxy
    }

    pub fn finalize(&mut self, proxy: &mut DefProxy){
        if proxy.sign.is_some() || !DepCheck::valid(*proxy){
            return
        }

        if proxy.mode == Mode::Constructed{
            AcyclicSigner::sign(proxy);
        }else{
            CyclicSigner::sign(*proxy);
        }
    }

    pub fn sign(&mut self, proxy: DefProxy, sign: &Signature) -> DefProxy{
        if let Some(mut def) = self.pending.remove(&proxy.ptr){
            def.sign = Some(*sign);
            def.mode = Mode::Constructed;
            match self.sea.entry(*sign) {
                Occupied(entry) =>  self.new_def_proxy(&**entry.get()),
                Vacant(entry) => self.new_def_proxy(&**entry.insert(def)),
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
        lam.set_op(2, body);
    }

    pub fn new_boxed() -> Box<World>{
        let mut world = Box::new(World{
            sign_size: 32,
            pending: HashMap::new(),
            sea : HashMap::new(),
            axioms : HashMap::new(),
            axioms_rev: HashMap::new()
        });

        let origin = world.new_def(Vec::new());
        let mut link = origin.ptr;

        for axiom in Axiom::iter(){
            let mut link_arr = Vec::new();
            link_arr.push( DefProxy::from(link));
            let axiom_ref = world.new_def(link_arr);
            link = axiom_ref.ptr;
            world.axioms.insert(axiom, axiom_ref.ptr);
            world.axioms_rev.insert(axiom_ref.ptr, axiom);
        }

        world
    }
}

