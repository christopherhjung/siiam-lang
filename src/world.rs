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
use crate::def::{DefModel, Def, DefLink, DefState, DefKind, DefKey, PendingMode};
use crate::utils::{MutBox, UnsafeMut};


pub struct World{
     pub world: Rc<MutBox<WorldImpl>>
}

impl Clone for World{
    fn clone(&self) -> Self {
        World{
            world: self.world.clone()
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
    Bot, Data, Tuple, Sigma, Pack, Extract, App, Pi, Lam, Var, Add, Mul,
    Literal,
    TyIdx, TyInt, TyReal, TyUnit,
    Slot, Alloc, Store, Load, Free, Ptr, Mem
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
                state: DefState::Pending(PendingMode::Structural)
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
            world: Rc::new(MutBox::from(world_impl))
        }
    }

    pub fn axiom(&self, ax: Axiom) -> Def{
        Def::new(&self.world, self.world.get().axiom(ax))
    }

    pub fn builder(&self) -> Builder{
        Builder{
            world: self.world.clone(),
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

    fn placeholder(&self) -> Def{
        Def::new(&self.world, DefLink::null())
    }

    fn insert_def(&mut self, model: Box<DefModel>) -> DefLink{
        DefMap::get_or_insert(&mut self.pending, model)
    }

    fn node_def(&mut self, ax: DefLink, ops: Array<DefLink>) -> Def {
        let link = self.node_def_link(ax, ops);
        Def::new(&self.world, link)
    }

    fn node_def_link(&mut self, ax: DefLink, ops: Array<DefLink>) -> DefLink {
        let mut state = PendingMode::Structural;
        for op in &ops{
            if op.is_null(){
                state = PendingMode::Nominal;
                break;
            }
        }

        let def = Box::from(DefModel{
            ax,
            kind: DefKind::Node(ops),
            state: DefState::Pending(state)
        });

        self.insert_def(def)
    }

    fn data_def(&mut self, data: Data) -> Def {
        let link = self.data_def_link(data);
        Def::new(&self.world, link)
    }

    fn data_def_link(&mut self, data: Data) -> DefLink {
        let ax = self.world.axiom(Axiom::Data);
        let def = Box::from(DefModel{
            ax,
            kind: DefKind::Data(data),
            state: DefState::Pending(PendingMode::Structural)
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

    pub fn ty_unit(&mut self) -> Def {
        self.axiom(Axiom::TyUnit)
    }

    pub fn lit(&mut self, value: u32, ty: &Def) -> Def {
        let literal_ax = self.axiom_raw(Axiom::Literal);
        let data = Data::from::<u32>(value);
        let value_def = self.data_def(data);
        let literal = self.node_def(literal_ax, array![ty.link, value_def.link]);
        literal
    }

    pub fn lit_idx(&mut self, arity: u32, idx: u32 ) -> Def{
        let arity = self.ty_idx(arity);
        self.lit(idx, &arity)
    }

    pub fn ty_int(&mut self, width: u32) -> Def {
        let ty_int_ax = self.axiom_raw(Axiom::TyInt);
        let data = Data::from::<u32>(width);
        let value_def = self.data_def(data);
        let literal = self.node_def(ty_int_ax, array![value_def.link]);
        literal
    }

    pub fn lit_int(&mut self, width: u32, idx: u32 ) -> Def{
        let arity = self.ty_int(width);
        self.lit(idx, &arity)
    }

    pub fn ty_idx(&mut self, arity: u32) -> Def {
        let ty_int_ax = self.axiom_raw(Axiom::TyIdx);
        let data = Data::from::<u32>(arity);
        let value_def = self.data_def(data);
        let literal = self.node_def(ty_int_ax, array![value_def.link]);
        literal
    }

    pub fn ty_real(&mut self, width: i32) -> Def {
        let ty_int_ax = self.axiom_raw(Axiom::TyReal);
        let data = Data::from::<i32>(width);
        let value_def = self.data_def(data);
        let literal = self.node_def(ty_int_ax, array![value_def.link]);
        literal
    }

    pub fn tuple<const COUNT: usize>(&mut self, elems: [&Def; COUNT]) -> Def {
        let elem_links = Array::new(COUNT);
        for idx in 0 .. COUNT{
            elem_links.set(idx, elems[idx].link)
        }
        let raw = self.tuple_arr_raw(elem_links);
        Def::new(&self.world, raw)
    }

    pub fn tuple_arr(&mut self, elems: Array<Def>) -> Def {
        let elem_links = Array::new(elems.len());
        for idx in 0 .. elems.len(){
            elem_links.set(idx, elems.get(idx).link)
        }
        let raw = self.tuple_arr_raw(elem_links);
        Def::new(&self.world, raw)
    }

    fn tuple_arr_raw(&mut self, elem: Array<DefLink>) -> DefLink {
        let ax = self.axiom_raw(Axiom::Tuple);
        self.node_def_link(ax,elem)
    }

    pub fn sigma<const COUNT: usize>(&mut self, elems: [&Def; COUNT]) -> Def {
        let elem_links = Array::new(COUNT);
        for idx in 0 .. COUNT{
            elem_links.set(idx, elems[idx].link)
        }
        let raw = self.sigma_arr_raw(elem_links);
        Def::new(&self.world, raw)
    }

    pub fn sigma_arr(&mut self, elems: Array<DefLink>) -> Def {
        let raw = self.sigma_arr_raw(elems);
        Def::new(&self.world, raw)
    }

    pub fn sigma_arr_raw(&mut self, elem: Array<DefLink>) -> DefLink {
        let ax = self.axiom_raw(Axiom::Sigma);
        self.node_def_link(ax,elem)
    }

    pub fn pack(&mut self, shape: &Def, body: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Pack);
        self.node_def(ax, array![shape.link, body.link])
    }

    pub fn extract(&mut self, tup: &Def, index: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Extract);
        self.node_def(ax, array![tup.link, index.link])
    }

    pub fn app(&mut self, callee: &Def, arg: &Def) -> Def {
        self.app_raw( callee.link, arg.link)
    }

    fn app_raw(&mut self, callee: DefLink, arg: DefLink) -> Def {
        let ax = self.axiom_raw(Axiom::App);
        self.node_def(ax, array![callee, arg])
    }

    fn app_raw_arr(&mut self, callee: DefLink, arg: Array<DefLink>) -> Def {
        let ax = self.axiom_raw(Axiom::App);
        let arg = self.tuple_arr_raw(arg);
        self.node_def(ax, array![callee, arg])
    }

    pub fn pi(&mut self, domain: &Def, co_domain : &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Pi);
        self.node_def(ax, array![domain.link, co_domain.link])
    }

    pub fn lam(&mut self, ty : &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Lam);
        self.node_def(ax, array![ty.link, DefLink::null()])
    }

    pub fn var(&mut self, lam: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Var);
        self.node_def(ax, array![lam.link])
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
        let arg = self.tuple_arr_raw(array![lhs.link, rhs.link]);
        self.app_raw(ax, arg)
    }

    pub fn mul(&mut self, lhs: &Def, rhs: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Mul);
        self.node_def(ax, array![lhs.link, rhs.link])
    }

    pub fn slot(&mut self, ty: &Def, mem: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Slot);
        let curry = self.node_def_link(ax, array![ty.link]);
        self.app_raw_arr(curry, array![mem.link])
    }

    pub fn alloc(&mut self, ty: &Def, mem: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Alloc);
        let curry = self.node_def_link(ax, array![ty.link]);
        self.app_raw_arr(curry, array![mem.link])
    }

    pub fn load(&mut self, mem: &Def, ptr: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Load);
        let ty = self.bot();
        let curry = self.node_def_link(ax, array![ty.link]);
        self.app_raw_arr(curry, array![mem.link, ptr.link])
    }

    pub fn store(&mut self, mem: &Def, ptr: &Def, val: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Store);
        let ty = self.bot();
        let curry = self.node_def_link(ax, array![ty.link]);
        self.app_raw_arr(curry, array![mem.link, ptr.link, val.link])
    }

    pub fn free(&mut self, mem: &Def, ptr: &Def) -> Def {
        let ax = self.axiom_raw(Axiom::Load);
        let ty = self.bot();
        let curry = self.node_def_link(ax, array![ty.link]);
        self.app_raw_arr(curry, array![mem.link, ptr.link])
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