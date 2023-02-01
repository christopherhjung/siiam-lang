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
use crate::array::Array;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use crate::world::World;
use crate::WorldImpl;

pub type DefLink = *const DefModel;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Mode {
    Constructed, Pending
}

pub enum DefKind {
    Constructed(Signature), Pending
}

pub struct DefModel {
    pub ops: Array<DefLink>,
    pub data: Array<u8>,
    //pub sign: Option<Signature>,
    //pub mode: Mode,
    pub kind: DefKind
}

pub struct Def {
    pub world: Rc<WorldImpl>,
    pub link: DefLink
}

impl DefModel {
    pub fn new_boxed(ops : Vec<DefLink>, data: Array<u8> ) -> Box<DefModel>{
        let op_arr = Array::new(ops.len());
        let mut i = 0;
        let mut variant = Mode::Pending;
        for def_ref in &ops{
            op_arr.set(i, *def_ref);
            if *def_ref == null() || def_ref == Mode::Pending{
                variant = Mode::Pending;
            }
            i += 1;
        }

        Box::new( DefModel {
            ops: op_arr,
            data,
            kind: DefKind::Pending
        })
    }
}

impl Def {
    pub fn world(&self) -> World{
        World{
            impl_: self.world.clone()
        }
    }

    pub fn null(world: &Rc<WorldImpl>) -> Def {
        Self::new(world: world.clone(), null())
    }

    pub fn from(ptr : DefLink) -> Def {
        Self::new(Rc::new_uninit(), ptr)
    }

    pub fn new(world: &Rc<WorldImpl>, ptr : DefLink) -> Def {
        Def { world: world.clone(), link: ptr }
    }

    fn variant(&self) -> Mode {
        let def = unsafe{&*self.link };
        def.mode
    }

    fn link(&self) -> DefLink {
        self.link
    }

    pub fn op_len(&self) -> usize{
        let def = unsafe{&*self.link };
        def.ops.len()
    }

    pub fn data_len(&self) -> usize{
        let def = unsafe{&*self.link };
        def.data.len()
    }

    pub fn has(&self, idx: usize) -> bool {
        let def = unsafe{&*self.link };
        let op_ptr = *def.ops.index(idx);
        op_ptr != null()
    }

    pub fn ops(&self) -> &Array<DefLink>{
        &self.ops
    }

    pub fn op( &self, idx : usize ) -> Def {
        let def = unsafe{&*self.link };
        let op_ptr = *def.ops.index(idx);
        Def::new(self.world, op_ptr)
    }

    pub fn data_arr(&self) -> &[u8]{
        let def = unsafe{&*self.link };
        unsafe { std::slice::from_raw_parts(def.data.get_ptr(0), def.data.len()) }
    }

    pub fn set_op( &self, idx : usize, op: &Def){
        let def = unsafe{&*self.link };
        def.ops.set(idx, op.link())
    }

    pub fn data<T>( &self, idx : usize ) -> &T{
        let def = unsafe{&*self.link };
        let data_ptr = unsafe{def.data.get_ptr(idx)};
        unsafe {&*(data_ptr as *const T)}
    }

    pub fn sign(&mut self) -> Option<Signature>{
        let mut w = unsafe{&mut *self.world};
        w.finalize(self);
        let def = unsafe{&*self.link };
        def.sign
    }
}

impl Deref for Def {
    type Target = DefModel;

    fn deref(&self) -> &Self::Target {
        unsafe {&*self.link }
    }
}
