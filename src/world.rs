use std::alloc::alloc;
use std::cell::{RefCell, RefMut};
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::ops::{Deref, Index};
use std::ptr;
use std::ptr::{null, null_mut};
use std::rc::Rc;
use sha2::{Digest, Sha256};
use sha2::digest::Update;
use crate::sign::Signature;
use crate::array::Array;

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

        for def_ptr in DefProxy::from(current).ops() {
            if self.visited.insert(*def_ptr){
                if !self.valid_impl(*def_ptr){
                    return false
                }
            }
        }

        return true
    }
}


pub struct SignNode{
    index : usize,
    low_link : usize,
    sign : Signature
}

pub struct Signer{
    index: usize,
    world: *mut World,
    nodes: HashMap<DefPtr, Rc<RefCell<SignNode>>>
}

impl Signer {
    pub fn sign(proxy: DefProxy){
        let mut signer = Signer {
            index: 0,
            world: proxy.world,
            nodes: HashMap::new()
        };

        signer.discover(proxy.ptr)
    }

    fn node(&mut self, ptr: DefPtr) -> Rc<RefCell<SignNode>>{
        let node = match self.nodes.entry(ptr) {
            Occupied(entry) => entry.get().clone(),
            Vacant(entry) => {
                let last_idx = self.index;
                self.index = last_idx + 1;

                let def = unsafe{&*ptr};
                let sign = if let Some(sign) = def.sign{
                    sign
                }else{
                    Signature::zero()
                };

                entry.insert(
                    Rc::new(
                        RefCell::new(
                            SignNode{
                                index: last_idx,
                                low_link: last_idx,
                                sign
                            })
                    )
                ).clone()
            }
        };

        node
    }

    fn discover(&mut self, curr: DefPtr){
        if self.nodes.contains_key(&curr){
            return
        }

        let curr_rc_node = self.node(curr);

        for dep_ptr in DefProxy::from(curr).ops() {
            self.discover(*dep_ptr);

            let dep_rc_node = self.node(*dep_ptr);
            let mut curr_node = RefCell::borrow_mut(&curr_rc_node);
            let dep_node = RefCell::borrow(&dep_rc_node);
            if dep_node.index < curr_node.index{
                curr_node.low_link = min(curr_node.low_link, dep_node.index);
            }
        }

        let mut curr_node = RefCell::borrow(&curr_rc_node);
        if curr_node.index == curr_node.low_link{
            println!("cycle begin");
            let index = curr_node.index;
            self.test(curr, index, true);
            println!("cycle end");
        }
    }

    fn test(&mut self, curr: DefPtr, index: usize, init: bool){
        let curr_rc_node = self.node(curr);
        let mut curr_node = RefCell::borrow(&curr_rc_node);

        if curr_node.low_link != index {
            return
        }

        println!("{:?} {:?}", curr_node.sign, curr);

        if curr_node.low_link == curr_node.low_link && !init {
            return
        }

        for dep_ptr in DefProxy::from(curr).ops() {
            self.test(*dep_ptr, index, false)
        }
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
        if def.variant == Variant::Pending{
            self.pending.insert(def_ptr, def);
        }else{
            let sign = Signature::from(&*def);
            def.sign = Some(sign);
            self.sea.insert(sign, def);
        }

        self.new_def_proxy(def_ptr)
    }

    pub fn finalize(&mut self, proxy: &mut DefProxy){
        if proxy.sign.is_some() || !DepCheck::valid(*proxy){
            return
        }

        Signer::sign(*proxy);
        //calc signature!!
        //let sign = Signature::from(&*def);
        //def.sign = Some(sign);
    }

    pub fn sign(&mut self, proxy: &mut DefProxy){
        if let Some(mut def) = self.pending.remove(&mut proxy.ptr){
            match self.sea.entry(def.sign.unwrap()) {
                Occupied(entry) => { proxy.ptr = &**entry.get();},
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
        lam.set_op(2, body);
    }

    pub fn new_boxed() -> Box<World>{
        let sea = HashMap::new();
        let axioms = HashMap::new();

        let mut world = Box::new(World{
            sign_size: 32,
            pending: HashMap::new(),
            sea,
            axioms
        });

        let origin = world.new_def(Vec::new());
        let mut link = origin.ptr;

        for axiom in Axiom::iter(){
            let mut link_arr = Vec::new();
            link_arr.push( DefProxy::from(link));
            let axiom_ref = world.new_def(link_arr);
            link = axiom_ref.ptr;
            world.axioms.insert(axiom, axiom_ref.ptr);
        }

        world
    }
}

