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
use sha2::digest::generic_array::arr;
use sha2::digest::Update;
use crate::sign::{AcyclicSigner, CyclicSigner, Signature};
use crate::array::Array;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use crate::def::{DefModel, Def, DefLink, Mode, DefState, DefKind};
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
    sign_size: usize,
    sea : HashMap<Signature, Box<DefModel>>,
    axioms : HashMap<Axiom, DefLink>,
    axioms_rev : HashMap<DefLink, Axiom>,
}

#[derive(EnumIter, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum Axiom{
    Bot, Tuple, Pack, Extract, App, Pi, Lam, Var,
    TyI32
}

impl WorldImpl {
    fn as_mut(&self) -> *mut WorldImpl {
        self as *const _ as *mut WorldImpl
    }

    pub fn axiom( &self, ax : Axiom ) -> DefLink {
        *self.axioms.get(&ax).unwrap()
    }
/*
    fn create_node_def(&mut self, ax: DefLink, ops : Vec<DefLink>) -> DefLink {
        let def = Box::from(DefModel{
            ax,
            kind: DefKind::Node(Array::from(ops)),
            state: DefState::Pending
        });

        self.insert_def(def)
    }

    fn create_data_def(&mut self, ax: DefLink, data : Array<u8>) -> DefLink {
        let def = Box::from(DefModel{
            ax,
            kind: DefKind::Data(data),
            state: DefState::None
        });

        self.insert_def(def)
    }*/

    pub fn insert_def(&mut self, def : Box<DefModel>) -> DefLink{
        let def_ptr = DefLink::from(&def);
        if def.state == DefState::Pending{
            panic!()
        }else if let DefState::Constructed(sign) = def.state {
            self.sea.insert(sign, def);
        }

        def_ptr
    }

    pub fn new_boxed() -> Box<WorldImpl>{
        let mut world = Box::new(WorldImpl {
            sign_size:  32,
            sea:        HashMap::new(),
            axioms:     HashMap::new(),
            axioms_rev: HashMap::new()
        });

        let root = world.insert_def(Box::new(DefModel{
            ax: DefLink::null(),
            kind: DefKind::Node(Array::empty()),
            state: DefState::Constructed(Signature::zero())
        }));
        let mut link = root;

        for axiom in Axiom::iter(){
            let mut axiom_def = Box::from(DefModel{
                ax: root,
                kind: DefKind::Node(Array::from(vec![link])),
                state: DefState::Pending
            });
            axiom_def.state = DefState::Constructed(AcyclicSigner::sign(&*axiom_def));

            link = world.insert_def(axiom_def);
            println!("{:?}", link.state);
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

    pub fn new() -> World{
        let world_impl = WorldImpl::new_boxed();

        World{
            impl_: Rc::new(MutBox::from(world_impl))
        }
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
    pending : HashMap<DefLink, Box<DefModel>>,
}

macro_rules! arr_test(
    ($size: expr, $factory: expr) => ({
        unsafe fn get_item_ptr<T>(slice: *mut [T], index: usize) -> *mut T {
            (slice as *mut T).offset(index as isize)
        }

        let mut arr = ::std::mem::MaybeUninit::<[_; $size]>::uninit();
        unsafe {
            for i in 0..$size {
                ::std::ptr::write(get_item_ptr(arr.as_mut_ptr(), i), $factory(i));
            }
            arr.assume_init()
        }
    })
);

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

        arr_test!(COUNT, |i: usize| {
            let old = defs[i].link;
            let new = signer.old2new(old);
            Def::new(&self.world, new)
        })
    }
}

impl DefFactory for Builder{
    fn create_def(&mut self, ax: DefLink, ops: Vec<DefLink>) -> Def {
        let def = Box::from(DefModel{
            ax,
            kind: DefKind::Node(Array::from(ops)),
            state: DefState::Pending
        });
/*
        let link = if def.state == DefState::Pending{

            self.pending.insert(link, def);
            link
        }else{
            self.world.get().insert_def(def)
        };*/

        let link = DefLink::from(&def);
        self.pending.insert(link, def);
        Def::new(&self.world, link)
    }

    fn axiom(&mut self, ax: Axiom) -> DefLink {
        self.world.axiom(ax)
    }
}


pub trait DefFactory {
    fn create_def(&mut self, ax: DefLink, ops: Vec<DefLink>) -> Def;
    fn axiom(&mut self, ax : Axiom) -> DefLink;

    fn bot(&mut self) -> Def {
        let ax = self.axiom(Axiom::Bot);
        self.create_def(ax, vec![])
    }

    fn pack(&mut self, shape: &Def, body: &Def) -> Def {
        let ax = self.axiom(Axiom::Pack);
        self.create_def(ax, vec![shape.link, body.link])
    }

    fn extract(&mut self, tup: &Def, index: &Def) -> Def {
        let ax = self.axiom(Axiom::Extract);
        self.create_def(ax, vec![tup.link, index.link])
    }

    fn app(&mut self, callee: &Def, arg: &Def) -> Def {
        let ax = self.axiom(Axiom::App);
        self.create_def(ax, vec![callee.link, arg.link])
    }

    fn pi(&mut self, domain: &Def, co_domain : &Def) -> Def {
        let ax = self.axiom(Axiom::Pi);
        self.create_def(ax, vec![domain.link, co_domain.link])
    }

    fn lam(&mut self, ty : &Def) -> Def {
        let ax = self.axiom(Axiom::Lam);
        self.create_def(ax, vec![ty.link, DefLink::null()])
    }

    fn var(&mut self, lam: &Def) -> Def {
        let ax = self.axiom(Axiom::Var);
        self.create_def(ax, vec![lam.link])
    }

    fn set_body(&mut self, lam: &Def, body: &Def){
        assert_eq!(lam.link.ax, self.axiom(Axiom::Lam));
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