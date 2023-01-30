use std::alloc::alloc;
use std::collections::HashMap;
use std::ops::Index;
use std::ptr;
use std::ptr::{null, null_mut};
use std::rc::Rc;
use sha2::{Digest, Sha256};
use sha2::digest::Update;
use crate::hash::Signature;
use crate::utils::Array;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Clone, Copy)]
pub struct DefRef{
    world: *mut World,
    ptr : *const Def
}

struct Def{
    ops: Array<*const Def>,
    data: Array<u8>,
    sign: Option<Signature>,
}

impl IntoIterator for DefRef{
    type Item = DefRef;
    type IntoIter = DefRefIterator;

    fn into_iter(self) -> Self::IntoIter {
        DefRefIterator{
            def: self,
            idx: 0
        }
    }
}

impl IntoIterator for Def{
    type Item = &'static Def;
    type IntoIter = DefIterator;

    fn into_iter(self) -> Self::IntoIter {
        DefIterator{
            def: &self as *const Def,
            idx: 0
        }
    }
}

struct DefIterator{
    def : *const Def,
    idx: usize
}

impl Iterator for DefIterator{
    type Item = &'static Def;

    fn next(&mut self) -> Option<Self::Item> {
        let def = unsafe{&*self.def};
        if self.idx < def.ops.len(){
            None
        }else{
            let def_ptr = def.ops.get(self.idx);
            let res = Some(unsafe{&**def_ptr});
            self.idx+=1;
            res
        }
    }
}


pub struct DefRefIterator{
    def : DefRef,
    idx: usize
}

impl Iterator for DefRefIterator{
    type Item = DefRef;

    fn next(&mut self) -> Option<Self::Item> {
        let def = unsafe{&*self.def.ptr};
        if self.idx < def.ops.len(){
            None
        }else{
            let def_ptr = def.ops.get(self.idx);
            let res = Some(DefRef{ world: self.def.world, ptr: unsafe{&**def_ptr} });
            self.idx+=1;
            res
        }
    }
}

impl DefRef {
    fn zero(world: *mut World) -> DefRef{
        Self::new(world, null())
    }

    fn new( world: *mut World, ptr : *const Def ) -> DefRef{
        DefRef{ world, ptr }
    }

    fn as_ptr(&self) -> *const Def{
        self.ptr
    }

    pub fn op_len(&self) -> usize{
        let def = unsafe{&*self.ptr};
        def.ops.len()
    }

    pub fn data_len(&self) -> usize{
        let def = unsafe{&*self.ptr};
        def.data.len()
    }

    pub fn has(&self, idx: usize) -> bool {
        let def = unsafe{&*self.ptr};
        let op_ptr = *def.ops.index(idx);
        op_ptr != null()
    }

    pub fn op( &self, idx : usize ) -> DefRef{
        let def = unsafe{&*self.ptr};
        let op_ptr = *def.ops.index(idx);
        DefRef::new(self.world, op_ptr)
    }

    pub fn data_arr(&self) -> &[u8]{
        let def = unsafe{&*self.ptr};
        unsafe { std::slice::from_raw_parts(def.data.get_ptr(0), def.data.len()) }
    }

    pub fn set_op( &self, idx : usize, op: DefRef){
        let def = unsafe{&*self.ptr};
        def.ops.set(idx, op.as_ptr())
    }

    pub fn data<T>( &self, idx : usize ) -> &T{
        let def = unsafe{&*self.ptr};
        let data_ptr = unsafe{def.data.get_ptr(idx)};
        unsafe {&*(data_ptr as *const T)}
    }

    pub fn sign(&self) -> Signature{
        let def = unsafe{&mut *(self.ptr as *mut Def)};
        if def.sign.is_none(){
            def.sign = Some(Signature::zero());
            let sign = Signature::from(*self);
            def.sign = Some(sign);
            sign
        }else{
            def.sign.unwrap()
        }
    }
}

struct SignBuilder{}

impl SignBuilder{
    fn test(def_ref: DefRef){
        for def in def_ref{

        }
    }
}

pub struct World{
    sign_size: usize,
    sea : HashMap<Signature, Box<Def>>,
    axioms : HashMap<Axiom, DefRef>
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

    pub fn axiom( &self, ax : Axiom ) -> DefRef{
        *self.axioms.get(&ax).unwrap()
    }

    fn new_def( &mut self, ops : Vec<DefRef> ) -> DefRef{
        self.new_data_def(ops, Array::new(0))
    }

    fn new_data_def( &mut self, ops : Vec<DefRef>, data: Array<u8> ) -> DefRef{
        let op_arr = Array::new(ops.len());
        let mut i = 0;
        for def_ref in &ops{
            op_arr.set(i, def_ref.ptr);
            i += 1;
        }

        let def = Box::new( Def{
            ops: op_arr,
            data,
            sign: None
        });

        let def_ref = DefRef{
            world: self.as_mut(),
            ptr: &*def
        };

        self.sea.insert(def_ref.sign(), def);
        def_ref
    }

    fn default(&mut self) -> DefRef{
        self.bot()
    }

    pub fn lit_int( &mut self, val : i32 ) -> DefRef{
        self.default()
    }

    pub fn ty_int( &mut self, width : i32 ) -> DefRef{
        self.default()
    }

    pub fn bot( &mut self ) -> DefRef{
        self.axiom(Axiom::Bot)
    }

    pub fn tuple( &mut self, mut elems: Vec<DefRef> ) -> DefRef{
        elems.insert(0, self.axiom(Axiom::Tuple));
        self.new_def(elems)
    }

    pub fn pack( &mut self, shape: DefRef, body: DefRef ) -> DefRef{
        self.new_def(vec![self.axiom(Axiom::Pack), shape, body])
    }

    pub fn extract( &mut self, tup: DefRef, index: DefRef ) -> DefRef{
        self.new_def(vec![self.axiom(Axiom::Extract), tup, index])
    }

    pub fn app( &mut self, callee: DefRef, arg: DefRef ) -> DefRef{
        self.new_def(vec![self.axiom(Axiom::App), callee, arg])
    }

    pub fn pi( &mut self, domain: DefRef, co_domain : DefRef ) -> DefRef{
        self.new_def(vec![self.axiom(Axiom::Pi), domain, co_domain])
    }

    pub fn lam( &mut self, ty : DefRef ) -> DefRef{
        self.new_def(vec![self.axiom(Axiom::Lam), ty, DefRef::zero(self.as_mut())])
    }

    pub fn var( &mut self, lam: DefRef ) -> DefRef{
        self.new_def(vec![self.axiom(Axiom::Var), lam])
    }

    pub fn set_body( &mut self, lam: DefRef, body: DefRef ){
        lam.set_op(1, body);
        //close loop
    }

    pub fn new() -> Box<World>{
        let sea = HashMap::new();
        let axioms = HashMap::new();

        let mut world = Box::new(World{
            sign_size: 32,
            sea,
            axioms
        });

        let origin = Box::from(Def{
            ops: Array::new(0),
            data: Array::new(0),
            sign: None
        });

        let origin_ref = DefRef::new(World::as_mut(&world), &*origin);
        world.sea.insert(origin_ref.sign(), origin);

        let mut link = origin_ref;

        for axiom in Axiom::iter(){
            let link_arr = Array::new(1);
            link_arr.set(0, link.as_ptr());

            let axiom_def = Box::from(Def{
                ops: link_arr,
                data: Array::new(0),
                sign: None
            });

            let def_ref = DefRef::new(World::as_mut(&world), &*axiom_def);
            link = def_ref;
            world.axioms.insert(axiom, def_ref);
            world.sea.insert(def_ref.sign(), axiom_def);
        }

        world
    }
}



impl From<DefRef> for Signature{
    fn from(def_ref: DefRef) -> Self {
        let mut hash = Sha256::new();

        for i in 0 .. def_ref.op_len(){
            let sign = if def_ref.has(i){
                def_ref.op(i).sign()
            }else {
                Signature::zero()
            };

            hash = Update::chain( hash, sign)
        }

        hash = Update::chain(hash, def_ref.data_arr());

        let arr =  hash.finalize();

        Signature{ data: <[u8; 32]>::from(arr) }
    }
}

