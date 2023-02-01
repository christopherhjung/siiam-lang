use std::alloc::alloc;
use std::cell::{Cell, RefCell, RefMut, UnsafeCell};
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
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
use crate::utils::UnsafeMut;


pub struct World{
    pub impl_: Rc<WorldImpl>
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
        let mut def = DefModel::new_boxed(ops, data);

        let def_ptr : DefLink = &*def;
        if def.kind == Mode::Pending{
            self.pending.insert(def_ptr, def);
        }else if let DefKind::Constructed(sign) = def.kind{
            self.sea.insert(sign, def);
        }

        def_ptr
    }

    pub fn finalize(&mut self, link: DefLink){
        if proxy.sign.is_some() || !DepCheck::valid(link){
            return
        }

        if proxy.mode == Mode::Constructed{
            AcyclicSigner::sign(proxy);
        }else{
            CyclicSigner::sign(*proxy);
        }
    }

    pub fn sign(&mut self, proxy: Def, sign: &Signature) -> Def {
        if let Some(mut def) = self.pending.remove(&proxy.link){
            def.kind = DefKind::Constructed(*sign);
            match self.sea.entry(*sign) {
                Occupied(entry) =>  self.new_def_proxy(&**entry.get()),
                Vacant(entry) => self.new_def_proxy(&**entry.insert(def)),
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
        self.new_def(vec![self.axiom(Axiom::Lam), ty, null()])
    }

    pub fn var(&mut self, lam: DefLink) -> DefLink {
        self.new_def(vec![self.axiom(Axiom::Var), lam])
    }

    pub fn set_body(&mut self, lam: DefLink, body: DefLink){
        lam.set_op(2, body);
    }

    pub fn new_boxed() -> Box<WorldImpl>{
        let mut world = Box::new(WorldImpl {
            sign_size: 32,
            pending: HashMap::new(),
            sea : HashMap::new(),
            axioms : HashMap::new(),
            axioms_rev: HashMap::new()
        });

        let mut link = world.new_def(Vec::new());

        for axiom in Axiom::iter(){
            let mut link_arr = Vec::new();
            link_arr.push( link);
            let axiom_ref = world.new_def(link_arr);
            link = axiom_ref.ptr;
            world.axioms.insert(axiom, axiom_ref.ptr);
            world.axioms_rev.insert(axiom_ref.ptr, axiom);
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
            link: link
        }
    }

    pub fn axiom( &self, ax : Axiom ) -> Def {
        self.new_def(self.impl_.axiom(ax))
    }

    pub fn lit_int( &mut self, val : i32 ) -> Def {
        self.new_def(self.impl_.lit_int(val))
    }

    pub fn ty_int( &mut self, width : i32 ) -> Def {
        self.new_def(self.impl_.ty_int(width))
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
        self.new_def(self.impl_.pack(shape.link, body.link))
    }

    pub fn extract(&mut self, tup: &Def, index: &Def) -> Def {
        self.new_def(self.impl_.extract(tup.link, index.link))
    }

    pub fn app(&mut self, callee: &Def, arg: &Def) -> Def {
        self.new_def(self.impl_.app(callee.link, arg.link))
    }

    pub fn pi(&mut self, domain: &Def, co_domain : &Def) -> Def {
        self.new_def(self.impl_.pi(domain.link, co_domain.link))
    }

    pub fn lam(&mut self, ty : &Def) -> Def {
        self.new_def(self.impl_.lam(ty.link))
    }

    pub fn var(&mut self, lam: &Def) -> Def {
        self.new_def(self.impl_.var(lam.link))
    }

    pub fn set_body(&mut self, lam: &Def, body: &Def){
        lam.set_op(2, body);
    }

    pub fn new_boxed() -> Box<WorldImpl>{
        let mut world = Box::new(WorldImpl {
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
            link_arr.push( Def::from(link));
            let axiom_ref = world.new_def(link_arr);
            link = axiom_ref.ptr;
            world.axioms.insert(axiom, axiom_ref.ptr);
            world.axioms_rev.insert(axiom_ref.ptr, axiom);
        }

        world
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
        if current == null(){
            return false;
        }

        if !self.visited.insert(current){
            return true;
        }

        for dep_ptr in Def::from(current).ops() {
            if !self.valid_impl(*dep_ptr){
                return false
            }
        }

        return true
    }
}