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
pub enum DefState {
    Constructed(Signature), Pending
}

#[derive(Clone)]
pub enum DefKind {
    Data(Array<u8>), Node(Array<DefLink>)
}

pub struct DefModel {
    pub ax: DefLink,
    pub kind: DefKind,
    pub state: DefState
}

pub struct Def {
    pub world: Rc<MutBox<WorldImpl>>,
    pub link: DefLink
}

impl DefModel{
    pub fn is_root(&self) -> bool{
        if let DefState::Constructed(sign) = &self.state{
            sign.is_zero()
        }else {
            false
        }
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

    /*
    pub fn op( &self, idx : usize ) -> Def {
        Def::new(&self.world,*self.ops.get(idx))
    }*/

    pub fn ops_offset<const COUNT: usize>(&self, offset : usize) -> [Def; COUNT]{
        if let DefKind::Node(ops) = &self.kind{
            assert_eq!(COUNT + offset, ops.len());
            let mut array: MaybeUninit<[Def; COUNT]> = MaybeUninit::uninit();
            let ptr = array.as_mut_ptr() as *mut Def;

            unsafe {
                for idx in offset .. ops.len(){
                    ptr::write(
                        ptr.add(idx - offset),
                        Def::new(&self.world, *ops.get(idx))
                    );
                }

                array.assume_init()
            }
        }else{
            panic!()
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
/*
    pub fn op_len(&self) -> usize{
        self.ops.len()
    }*/
/*
    pub fn data_len(&self) -> usize{
        self.data.len()
    }*/
/*
    pub fn has(&self, idx: usize) -> bool {
        !self.ops.get(idx).is_null()
    }*/
/*
    pub fn data_arr(&self) -> &[u8]{
        let def = unsafe{&*self.link };
        unsafe { std::slice::from_raw_parts(def.data.get_ptr(0), def.data.len()) }
    }*/

    pub fn set_op( &self, idx : usize, op: &Def){
        if let DefKind::Node(ops) = &self.kind{
            ops.set(idx, op.link())
        }
    }
/*
    pub fn data<T>( &self, idx : usize ) -> &T{
        let def = unsafe{&*self.link };
        let data_ptr = unsafe{def.data.get_ptr(idx)};
        unsafe {&*(data_ptr as *const T)}
    }*/

    /*
    pub fn construct(&self) -> Def{
        let new_def = self.world.get().construct(self.link);
        Def::new(&self.world,new_def)
    }*/

    pub fn sign(&self) -> Option<Signature>{
        if let DefState::Constructed(sign) = &self.state {
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
