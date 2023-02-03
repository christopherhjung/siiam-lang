use std::alloc::alloc;
use std::borrow::BorrowMut;
use std::cell::{Cell, RefCell, RefMut, UnsafeCell};
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::hash::Hash;
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut, Index};
use std::ptr;
use std::ptr::{null, null_mut};
use std::rc::{Rc, Weak};
use sha2::{Digest, Sha256};
use sha2::digest::generic_array::arr;
use sha2::digest::Update;
use crate::sign::{AcyclicSigner, CyclicSigner, Signature};
use crate::array::Array;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use crate::{arr_for_each, array};
use crate::data::Data;
use crate::def::{DefModel, Def, DefLink, Mode, DefState, DefKind, DefKey};
use crate::def::Mode::Constructed;
use crate::utils::{MutBox, UnsafeMut};


pub struct World{
    pub impl_: Rc<MutBox<WorldImpl>>
}

impl Clone for World{
    fn clone(&self) -> Self {
        World{
            impl_: self.impl_.clone()
        }
    }
}

pub struct WorldImpl {
    sea : HashMap<Signature, Box<DefModel>>,
    axioms : HashMap<Axiom, DefLink>,
    axioms_rev : HashMap<DefLink, Axiom>,
}

#[derive(EnumIter, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum Axiom{
    Bot, Data, Tuple, Pack, Extract, App, Pi, Lam, Var, Add, Mul,
    Literal,
    TyInt
}

impl WorldImpl {
    pub fn axiom( &self, ax : Axiom ) -> DefLink {
        *self.axioms.get(&ax).unwrap()
    }

    pub fn insert_def(&mut self, def : Box<DefModel>) -> DefLink{
        if let DefState::Constructed(sign) = def.state {
            if let Some(last) = self.sea.get(&sign){
                DefLink::from(last)
            }else{
                let link = DefLink::from(&def);
                self.sea.insert(sign, def);
                link
            }
        }else{
            panic!();
        }
    }

    pub fn new_boxed() -> Box<WorldImpl>{
        let mut world = Box::new(WorldImpl {
            sea:        HashMap::new(),
            axioms:     HashMap::new(),
            axioms_rev: HashMap::new()
        });

        let root = world.insert_def(Box::new(DefModel{
            ax:    DefLink::null(),
            kind:  DefKind::Node(Array::empty()),
            state: DefState::Constructed(Signature::zero())
        }));
        let mut link = root;

        for axiom in Axiom::iter(){
            let mut axiom_def = Box::from(DefModel{
                ax: root,
                kind: DefKind::Node(array![link]),
                state: DefState::Pending
            });
            axiom_def.state = DefState::Constructed(AcyclicSigner::sign(&*axiom_def));
            link = world.insert_def(axiom_def);
            world.axioms.insert(axiom, link);
            world.axioms_rev.insert(link, axiom);
        }

        world
    }
}

impl World {
    pub fn new() -> World{
        let world_impl = WorldImpl::new_boxed();

        World{
            impl_: Rc::new(MutBox::from(world_impl))
        }
    }

    pub fn axiom(&self, ax: Axiom) -> Def{
        Def::new(&self.impl_, self.impl_.get().axiom(ax))
    }

    pub fn builder(&self) -> Builder{
        Builder{
            world: self.impl_.clone(),
            pending: Default::default()
        }
    }
}

pub struct Builder{
    world: Rc<MutBox<WorldImpl>>,
    pending : HashMap<DefKey, Box<DefModel>>,
}

struct DefMap {}
impl DefMap {
    pub fn get_or_insert(map: &mut HashMap<DefKey, Box<DefModel>>, model: Box<DefModel>) -> DefLink{
        let link = DefLink::from(&model);
        match map.entry(DefKey::new(link)) {
            Occupied(entry) => {
                DefLink::from(entry.get())
            }
            Vacant(entry) => {
                DefLink::from(entry.insert(model))
            }
        }
    }
}

impl Builder{
    pub fn construct_def(&mut self, def: &Def) -> Def{
        let [def] = self.construct_defs(&[def]);
        def
    }

    pub fn construct_defs<const COUNT: usize>(&mut self, defs: &[&Def; COUNT]) -> [Def; COUNT]{
        let mut signer = CyclicSigner::new( self.world.get());

        for def in *defs{
            if !DepCheck::valid(def.link) {
                panic!()
            }

            signer.discover(def.link);
        }

        arr_for_each!(COUNT, |i: usize| {
            let old = defs[i].link;
            let new = signer.old2new(old);
            Def::new(&self.world, new)
        })
    }

    fn insert_def(&mut self, model: Box<DefModel>) -> DefLink{
        DefMap::get_or_insert(&mut self.pending, model)
    }

    fn create_node_def(&mut self, ax: DefLink, ops: Array<DefLink>) -> Def {
        let link = self.create_node_def_link(ax, ops);
        Def::new(&self.world, link)
    }

    fn create_node_def_link(&mut self, ax: DefLink, ops: Array<DefLink>) -> DefLink {
        let def = Box::from(DefModel{
            ax,
            kind: DefKind::Node(ops),
            state: DefState::Pending
        });

        self.insert_def(def)
    }

    fn create_data_def(&mut self, data: Data) -> Def {
        let link = self.create_data_def_link(data);
        Def::new(&self.world, link)
    }

    fn create_data_def_link(&mut self, data: Data) -> DefLink {
        let ax = self.world.axiom(Axiom::Data);
        let def = Box::from(DefModel{
            ax,
            kind: DefKind::Data(data),
            state: DefState::Pending
        });

        self.insert_def(def)
    }




    fn axiom_raw(&self, ax: Axiom) -> DefLink {
        self.world.axiom(ax)
    }

    pub fn axiom(&mut self, ax: Axiom) -> Def {
        Def::new(&self.world, self.axiom_raw(ax))
    }

    pub fn bot(&mut self) -> Def {
        self.axiom(Axiom::Bot)
    }

    pub fn lit(&mut self, value: i32, ty: &Def) -> Def {
        let literal_ax = self.axiom_raw(Axiom::Literal);
        let data = Data::from::<i32>(value);
        let value_def = self.create_data_def(data);
        let literal = self.create_node_def(literal_ax, array![ty.link, value_def.link]);
        literal
    }

    pub fn ty_int(&mut self, width: i32) -> Def {
        let ty_int_ax = self.axiom_raw(Axiom::TyInt);
        let data = Data::from::<i32>(width);
        let value_def = self.create_data_def(data);
        let literal = self.create_node_def(ty_int_ax, array![value_def.link]);
        literal
    }

    pub fn pack(&mut self, shape: &Def, body: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Pack);
        self.create_node_def(ax, array![shape.link, body.link])
    }

    pub fn extract(&mut self, tup: &Def, index: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Extract);
        self.create_node_def(ax, array![tup.link, index.link])
    }

    pub fn app(&mut self, callee: &Def, arg: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::App);
        self.create_node_def(ax, array![callee.link, arg.link])
    }

    pub fn pi(&mut self, domain: &Def, co_domain : &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Pi);
        self.create_node_def(ax, array![domain.link, co_domain.link])
    }

    pub fn lam(&mut self, ty : &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Lam);
        self.create_node_def(ax, array![ty.link, DefLink::null()])
    }

    pub fn var(&mut self, lam: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Var);
        self.create_node_def(ax, array![lam.link])
    }

    pub fn add(&mut self, lhs: &Def, rhs: &Def) -> Def {
        if lhs.link.ax == self.axiom_raw(Axiom::Add){
            let [lhs, rhs] = lhs.ops();
            println!("heheh {:?}", lhs.link)
        }else if rhs.ax == self.axiom_raw(Axiom::Add){
            println!("lool")
        }

        if lhs == rhs{
            let ty_i32 = self.ty_int(32);
            let lit_2 = self.lit(2, &ty_i32);
            return self.mul(lhs, &lit_2)
        }

        let ax = self.axiom_raw(Axiom::Add);
        self.create_node_def(ax, array![lhs.link, rhs.link])
    }

    pub fn mul(&mut self, lhs: &Def, rhs: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Mul);
        self.create_node_def(ax, array![lhs.link, rhs.link])
    }

    pub fn set_body(&mut self, lam: &Def, body: &Def){
        assert_eq!(lam.link.ax, self.axiom_raw(Axiom::Lam));
        lam.set_op(1, body);
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

        if let DefKind::Node(ops) = &current.kind{
            for dep_ptr in ops {
                if !self.valid_impl(*dep_ptr){
                    return false
                }
            }
        }

        return true
    }
}