use std::alloc::alloc;
use std::borrow::BorrowMut;
use std::cell::{Cell, RefCell, RefMut, UnsafeCell};
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut, Index};
use std::ptr;
use std::ptr::{null, null_mut};
use std::rc::{Rc, Weak};
use sha2::{Digest, Sha256};
use sha2::digest::Update;
use crate::sign::{AcyclicSigner, CyclicSigner, Signature};
use crate::array::Array;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use crate::def::{DefModel, Def, DefLink, Mode, DefKind};
use crate::def::Mode::Constructed;
use crate::utils::{MutBox, UnsafeMut};


pub struct World{
    pub impl_: Rc<MutBox<WorldImpl>>
}

impl World{
}

impl Clone for World{
    fn clone(&self) -> Self {
        World{
            impl_: self.impl_.clone()
        }
    }
}

pub struct WorldImpl {
    sign_size: usize,
    sea : HashMap<Signature, Box<DefModel>>,
    pending : HashMap<DefLink, Box<DefModel>>,
    axioms : HashMap<Axiom, DefLink>,
    axioms_rev : HashMap<DefLink, Axiom>,
}

#[derive(EnumIter, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum Axiom{
    Bot, Tuple, Pack, Extract, App, Pi, Lam, Var,
    TypeI32
}

impl WorldImpl {
    fn as_mut(&self) -> *mut WorldImpl {
        self as *const _ as *mut WorldImpl
    }

    pub fn axiom( &self, ax : Axiom ) -> DefLink {
        *self.axioms.get(&ax).unwrap()
    }

    fn new_def(&mut self, ops : Vec<DefLink>) -> DefLink {
        self.new_data_def(ops, Array::empty())
    }

    fn new_data_def(&mut self, ops : Vec<DefLink>, data: Array<u8>) -> DefLink {
        let def = DefModel::new_boxed(Array::from(ops), data);
        self.insert_def(def)
    }

    pub fn insert_def(&mut self, def : Box<DefModel>) -> DefLink{
        let def_ptr = DefLink::from(&def);
        if def.kind == DefKind::Pending{
            self.pending.insert(def_ptr, def);
        }else if let DefKind::Constructed(sign) = def.kind{
            self.sea.insert(sign, def);
        }

        def_ptr
    }

    pub fn construct(&mut self, def: DefLink) -> DefLink{
        if def.kind == DefKind::Pending && DepCheck::valid(def){
            CyclicSigner::sign( self, def)
        }else{
            def
        }
    }

    pub fn sign(&mut self, link: DefLink, sign: &Signature) -> DefLink {
        if let Some(mut def) = self.pending.remove(&link){
            match self.sea.entry(*sign) {
                Occupied(entry) =>
                    DefLink::from(entry.get()),
                Vacant(entry) => {
                    def.kind = DefKind::Constructed(*sign);
                    DefLink::from(entry.insert(def))
                },
            }
        }else{
            panic!()
        }
    }

    fn default(&mut self) -> DefLink {
        self.bot()
    }

    pub fn lit_int( &mut self, val : i32 ) -> DefLink {
        self.default()
    }

    pub fn ty_int( &mut self, width : i32 ) -> DefLink {
        self.default()
    }

    pub fn bot( &mut self ) -> DefLink {
        self.axiom(Axiom::Bot)
    }

    pub fn tuple(&mut self, mut elems: Vec<DefLink> ) -> DefLink {
        elems.insert(0, self.axiom(Axiom::Tuple));
        self.new_def(elems)
    }

    pub fn pack(&mut self, shape: DefLink, body: DefLink) -> DefLink {
        self.new_def(vec![self.axiom(Axiom::Pack), shape, body])
    }

    pub fn extract(&mut self, tup: DefLink, index: DefLink) -> DefLink {
        self.new_def(vec![self.axiom(Axiom::Extract), tup, index])
    }

    pub fn app(&mut self, callee: DefLink, arg: DefLink) -> DefLink {
        self.new_def(vec![self.axiom(Axiom::App), callee, arg])
    }

    pub fn pi(&mut self, domain: DefLink, co_domain : DefLink) -> DefLink {
        self.new_def(vec![self.axiom(Axiom::Pi), domain, co_domain])
    }

    pub fn lam(&mut self, ty : DefLink) -> DefLink {
        self.new_def(vec![self.axiom(Axiom::Lam), ty, DefLink::null()])
    }

    pub fn var(&mut self, lam: DefLink) -> DefLink {
        self.new_def(vec![self.axiom(Axiom::Var), lam])
    }

    fn set_op(def_link: DefLink, val: DefLink, idx: usize){
        let def = &*def_link;
        def.ops.set(idx, val)
    }

    pub fn set_body(&mut self, lam: DefLink, body: DefLink){
        Self::set_op(lam,body, 2);
    }

    pub fn new_boxed() -> Box<WorldImpl>{
        let mut world = Box::new(WorldImpl {
            sign_size:  32,
            pending:    HashMap::new(),
            sea:        HashMap::new(),
            axioms:     HashMap::new(),
            axioms_rev: HashMap::new()
        });

        let mut link = world.new_def(Vec::new());

        for axiom in Axiom::iter(){
            let mut link_arr = Vec::new();
            link_arr.push( link);
            link = world.new_def(link_arr);
            world.axioms.insert(axiom, link);
            world.axioms_rev.insert(link, axiom);
        }

        world
    }
}

impl World {
    fn as_mut(&self) -> *mut WorldImpl {
        self as *const _ as *mut WorldImpl
    }

    pub fn new_def(&self, link : DefLink) -> Def{
        Def{
            world: self.impl_.clone(),
            link
        }
    }

    pub fn axiom( &mut self, ax : Axiom ) -> Def {
        let link = self.impl_.axiom(ax);
        self.new_def(link)
    }

    pub fn lit_int( &mut self, val : i32 ) -> Def {
        let link = self.impl_.get().lit_int(val);
        self.new_def(link)
    }

    pub fn ty_int( &mut self, width : i32 ) -> Def {
        let link = self.impl_.get().ty_int(width);
        self.new_def(link)
    }

    pub fn bot( &mut self ) -> Def {
        self.axiom(Axiom::Bot)
    }
/*
    pub fn tuple(&mut self, mut elems: Vec<Def> ) -> Def {
        elems.insert(0, self.axiom(Axiom::Tuple));
        self.new_def(elems)
    }*/

    pub fn pack(&mut self, shape: &Def, body: &Def) -> Def {
        let link = self.impl_.get().pack(shape.link, body.link);
        self.new_def(link)
    }

    pub fn extract(&mut self, tup: &Def, index: &Def) -> Def {
        let link = self.impl_.get().extract(tup.link, index.link);
        self.new_def(link)
    }

    pub fn app(&mut self, callee: &Def, arg: &Def) -> Def {
        let link = self.impl_.get().app(callee.link, arg.link);
        self.new_def(link)
    }

    pub fn pi(&mut self, domain: &Def, co_domain : &Def) -> Def {
        let link = self.impl_.get().pi(domain.link, co_domain.link);
        self.new_def(link)
    }

    pub fn lam(&mut self, ty : &Def) -> Def {
        let link = self.impl_.get().lam(ty.link);
        self.new_def(link)
    }

    pub fn var(&mut self, lam: &Def) -> Def {
        let link = self.impl_.get().var(lam.link);
        self.new_def(link)
    }

    pub fn set_body(&mut self, lam: &Def, body: &Def){
        lam.set_op(2, body);
    }

    pub fn construct<const COUNT: usize>(&self, defs: [&Def; COUNT]) -> [Def; COUNT]{
        let mut array: MaybeUninit<[Def; COUNT]> = MaybeUninit::uninit();
        let mut ptr = array.as_mut_ptr() as *mut Def;

        unsafe {
            for def in defs{
                ptr::write(ptr, self.new_def(def.link));
                ptr = ptr.add(1);
            }

            array.assume_init()
        }
    }

    pub fn new() -> World{
        let world_impl = WorldImpl::new_boxed();

        World{
            impl_: Rc::new(MutBox::from(world_impl))
        }
    }
}



pub struct DepCheck {
    visited: HashSet<DefLink>
}

impl DepCheck {
    pub fn valid(link: DefLink) -> bool{
        let mut check = DepCheck {
            visited: HashSet::new()
        };

        check.valid_impl(link)
    }

    fn valid_impl(&mut self, current: DefLink) -> bool{
        if current.is_null(){
            return false;
        }

        if !self.visited.insert(current){
            return true;
        }

        for dep_ptr in &current.ops {
            if !self.valid_impl(*dep_ptr){
                return false
            }
        }

        return true
    }
}