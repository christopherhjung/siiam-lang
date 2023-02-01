use std::alloc::alloc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::mem::MaybeUninit;
use std::ops::{Deref, Index};
use std::ptr;
use std::ptr::{null, null_mut};
use std::rc::Rc;
use sha2::{Digest, Sha256};
use sha2::digest::Update;
use crate::sign::{AcyclicSigner, Signature};
use crate::array::Array;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use crate::token::TokenKind;
use crate::utils::{MutBox, UnsafeMut};
use crate::world::World;
use crate::WorldImpl;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct DefLink{
    ptr: *const DefModel
}

impl DefLink{
    pub fn null() -> DefLink{
        DefLink{
            ptr: null()
        }
    }

    pub fn is_null(&self) -> bool{
        self.ptr == null()
    }
}

impl From<&Box<DefModel>> for DefLink{
    fn from(def : &Box<DefModel>) -> Self {
        DefLink{
            ptr: &**def as *const _
        }
    }
}

impl From<&mut Box<DefModel>> for DefLink{
    fn from(def : &mut Box<DefModel>) -> Self {
        DefLink{
            ptr: &**def as *const _
        }
    }
}

impl Deref for DefLink {
    type Target = DefModel;

    fn deref(&self) -> &Self::Target {
        unsafe {&*self.ptr}
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Mode {
    Constructed, Pending
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum DefKind {
    Constructed(Signature), Forwarding(DefLink), Pending
}

pub struct DefModel {
    pub ops: Array<DefLink>,
    pub data: Array<u8>,
    pub kind: DefKind
}

pub struct Def {
    pub world: Rc<MutBox<WorldImpl>>,
    pub link: DefLink
}

impl DefModel {
    pub fn new_boxed(ops : Array<DefLink>, data: Array<u8> ) -> Box<DefModel>{
        let mut pending = false;
        for def_ref in &ops{
            if def_ref.is_null() || def_ref.kind == DefKind::Pending{
                pending = true;
            }
        }

        let mut res = Box::new( DefModel {
            ops,
            data,
            kind: DefKind::Pending
        });

        res.kind  = if pending{
            DefKind::Pending
        }else{
            DefKind::Constructed(AcyclicSigner::sign(&*res))
        };

        res
    }
}

impl Def {
    pub fn world(&self) -> World{
        World{
            impl_: self.world.clone()
        }
    }

    pub fn new(world: &Rc<MutBox<WorldImpl>>, link: DefLink) -> Def {
        Def { world: world.clone(), link }
    }

    pub fn op( &self, idx : usize ) -> Def {
        Def::new(&self.world,*self.ops.get(idx))
    }

    pub fn ops_offset<const COUNT: usize>(&self, offset : usize) -> [Def; COUNT]{
        assert_eq!(COUNT + offset, self.ops.len());
        let mut array: MaybeUninit<[Def; COUNT]> = MaybeUninit::uninit();
        let ptr = array.as_mut_ptr() as *mut Def;

        unsafe {
            for idx in offset .. self.ops.len(){
                ptr::write(
                    ptr.add(idx - offset),
                    Def::new(&self.world, *self.ops.get(idx))
                );
            }

            array.assume_init()
        }
    }

    pub fn ops<const COUNT: usize>(&self) -> [Def; COUNT]{
        self.ops_offset(0)
    }

    pub fn args<const COUNT: usize>(&self) -> [Def; COUNT]{
        self.ops_offset(1)
    }

    fn link(&self) -> DefLink {
        self.link
    }

    pub fn op_len(&self) -> usize{
        self.ops.len()
    }

    pub fn data_len(&self) -> usize{
        self.data.len()
    }

    pub fn has(&self, idx: usize) -> bool {
        !self.ops.get(idx).is_null()
    }

    pub fn data_arr(&self) -> &[u8]{
        let def = unsafe{&*self.link };
        unsafe { std::slice::from_raw_parts(def.data.get_ptr(0), def.data.len()) }
    }

    pub fn set_op( &self, idx : usize, op: &Def){
        self.ops.set(idx, op.link())
    }
/*
    pub fn data<T>( &self, idx : usize ) -> &T{
        let def = unsafe{&*self.link };
        let data_ptr = unsafe{def.data.get_ptr(idx)};
        unsafe {&*(data_ptr as *const T)}
    }*/

    pub fn construct(&self) -> Def{
        let new_def = self.world.get().construct(self.link);
        Def::new(&self.world,new_def)
    }

    pub fn sign(&self) -> Option<Signature>{
        if let DefKind::Constructed(sign) = &self.kind{
            Some(*sign)
        }else{
            None
        }
    }
}

impl Deref for Def {
    type Target = DefModel;

    fn deref(&self) -> &Self::Target {
        unsafe {&*self.link }
    }
}
