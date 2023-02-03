use std::alloc::alloc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::hash::{Hash, Hasher};
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
use crate::data::Data;
use crate::token::TokenKind;
use crate::utils::{MutBox, UnsafeMut};
use crate::world::World;
use crate::WorldImpl;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct DefLink{
    ptr: *const DefModel
}

#[derive(Copy, Clone, Eq, Debug)]
pub struct DefKey {
    link: DefLink
}

impl DefKey {
    pub fn new(link : DefLink) -> DefKey {
        DefKey {
            link
        }
    }
}

impl PartialEq for DefKey {
    fn eq(&self, other: &Self) -> bool {
        if let DefState::Pending(mode) = self.link.state{
            if mode == PendingMode::Nominal{
                self.link == other.link
            }else{
                self.link.deref() == other.link.deref()
            }
        }else{
            panic!()
        }
    }
}

impl Hash for DefKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.link.deref().hash(state);
    }
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

    pub fn new(ptr: *const DefModel) -> DefLink{
        DefLink{
            ptr
        }
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
pub enum PendingMode{
    Structural,
    Nominal
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum DefState {
    Constructed(Signature),
    Pending(PendingMode)
}

pub enum DefKind {
    Data(Data),
    Node(Array<DefLink>)
}

pub struct DefModel {
    pub ax: DefLink,
    pub kind: DefKind,
    pub state: DefState
}

impl PartialEq for DefModel{
    fn eq(&self, other: &Self) -> bool {
        if self.ax != other.ax {
            false
        }else{
            match (&self.kind, &other.kind) {
                (DefKind::Node(lhs_ops), DefKind::Node(rhs_ops)) => {
                    lhs_ops == rhs_ops
                }
                (DefKind::Data(lhs_data), DefKind::Data(rhs_data)) => {
                    lhs_data == rhs_data
                }
                _ => false
            }
        }
    }
}

impl Hash for DefModel{
    fn hash<H: Hasher>(&self, state: &mut H) {

        match self.state {
            DefState::Pending(mode) => {
                state.write_u8(mode as u8);
            }
            _ => panic!()
        }

        ptr::hash(self.ax.ptr, state);
        match &self.kind {
            DefKind::Node(ops) => {
                for op in ops{
                    ptr::hash(op.ptr, state);
                }
            }
            DefKind::Data(data) => {
                state.write(data.slice())
            }
        }
    }
}

pub struct Def {
    pub world: Rc<MutBox<WorldImpl>>,
    pub link: DefLink
}

impl Clone for Def{
    fn clone(&self) -> Self {
        self.new_raw(self.link)
    }
}

impl PartialEq for Def{
    fn eq(&self, other: &Self) -> bool {
        self.link == other.link
    }
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
            world: self.world.clone()
        }
    }

    pub fn new(world: &Rc<MutBox<WorldImpl>>, link: DefLink) -> Def {
        Def { world: world.clone(), link }
    }

    fn new_raw(&self, link: DefLink) -> Def {
        Self::new(&self.world, link)
    }

    pub fn op(&self, idx: usize) -> Def{
        if let DefKind::Node(ops) = &self.kind{
            assert!(idx < ops.len());
            self.new_raw(*ops.get(idx))
        }else{
            panic!()
        }
    }

    pub fn ops_offset<const COUNT: usize>(&self, offset : usize) -> [Def; COUNT]{
        if let DefKind::Node(ops) = &self.kind{
            assert_eq!(COUNT + offset, ops.len());
            let mut array: MaybeUninit<[Def; COUNT]> = MaybeUninit::uninit();
            let ptr = array.as_mut_ptr() as *mut Def;

            unsafe {
                for idx in offset .. ops.len(){
                    ptr.add(idx - offset)
                        .write(self.new_raw(*ops.get(idx)))
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

    fn link(&self) -> DefLink {
        self.link
    }

    pub fn set_op( &self, idx : usize, op: &Def){
        if self.state != DefState::Pending(PendingMode::Nominal){
            panic!("Setting of non nominal Defs is not supported!");
        }

        if let DefKind::Node(ops) = &self.kind{
            ops.set(idx, op.link())
        }
    }

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
