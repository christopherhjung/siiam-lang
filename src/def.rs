use std::alloc::alloc;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::ops::{Deref, Index};
use std::ptr;
use std::ptr::{null, null_mut};
use std::rc::Rc;
use sha2::{Digest, Sha256};
use sha2::digest::Update;
use crate::hash::Signature;
use crate::utils::Array;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use crate::World;

pub type DefPtr = *const Def;

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Variant{
    Constructed, Pending
}


pub struct Def{
    pub ops: Array<DefPtr>,
    pub data: Array<u8>,
    pub sign: Option<Signature>,
    pub variant: Variant
}

impl Def{
    pub fn new_boxed(ops : Vec<DefProxy>, data: Array<u8> ) -> Box<Def>{
        let op_arr = Array::new(ops.len());
        let mut i = 0;
        let mut variant = Variant::Constructed;
        for def_ref in &ops{
            op_arr.set(i, def_ref.ptr);
            if def_ref.ptr == null() || def_ref.variant() == Variant::Pending{
                variant = Variant::Pending;
            }
            i += 1;
        }

        Box::new( Def{
            ops: op_arr,
            data,
            sign: None,
            variant
        })
    }
}

impl IntoIterator for DefProxy {
    type Item = DefProxy;
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
            def: &self as DefPtr,
            idx: 0
        }
    }
}

pub struct DefIterator{
    def : DefPtr,
    idx: usize
}


#[derive(Clone, Copy)]
pub struct DefProxy {
    pub world: *mut World,
    pub ptr : DefPtr
}

impl DefProxy {
    pub fn null(world: *mut World) -> DefProxy {
        Self::new(world, null())
    }

    pub fn no_world(ptr : DefPtr) -> DefProxy {
        Self::new(null_mut(), ptr)
    }

    pub fn new( world: *mut World, ptr : DefPtr ) -> DefProxy {
        DefProxy { world, ptr }
    }

    fn variant(&self) -> Variant{
        let def = unsafe{&*self.ptr};
        def.variant
    }

    fn as_ptr(&self) -> DefPtr{
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

    pub fn op( &self, idx : usize ) -> DefProxy {
        let def = unsafe{&*self.ptr};
        let op_ptr = *def.ops.index(idx);
        DefProxy::new(self.world, op_ptr)
    }

    pub fn data_arr(&self) -> &[u8]{
        let def = unsafe{&*self.ptr};
        unsafe { std::slice::from_raw_parts(def.data.get_ptr(0), def.data.len()) }
    }

    pub fn set_op( &self, idx : usize, op: DefProxy){
        let def = unsafe{&*self.ptr};
        def.ops.set(idx, op.as_ptr())
    }

    pub fn data<T>( &self, idx : usize ) -> &T{
        let def = unsafe{&*self.ptr};
        let data_ptr = unsafe{def.data.get_ptr(idx)};
        unsafe {&*(data_ptr as *const T)}
    }

    pub fn sign(&mut self) -> Option<Signature>{
        /*
        if def.sign.is_none(){
            def.sign = Some(Signature::zero());
            let sign = Signature::from(*self);
            def.sign = Some(sign);
            sign
        }else{
            def.sign.unwrap()
        }*/
        let def = unsafe{&mut *(self.ptr as *mut Def)};
        let mut w = unsafe{&mut *self.world};
        w.finalize(self);
        def.sign
    }
}

impl Deref for DefProxy{
    type Target = Def;

    fn deref(&self) -> &Self::Target {
        unsafe {&*self.ptr}
    }
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
    def : DefProxy,
    idx: usize
}

impl Iterator for DefRefIterator{
    type Item = DefProxy;

    fn next(&mut self) -> Option<Self::Item> {
        let def = unsafe{&*self.def.ptr};
        if self.idx < def.ops.len(){
            None
        }else{
            let def_ptr = def.ops.get(self.idx);
            let res = Some(DefProxy { world: self.def.world, ptr: *def_ptr });
            self.idx+=1;
            res
        }
    }
}

struct SignBuilder{}

impl SignBuilder{
    fn test(def_ref: DefProxy){
        for def in def_ref{

        }
    }
}