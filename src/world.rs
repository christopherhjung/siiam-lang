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
use crate::builder::Builder;
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
    Bot, Data,
    Tuple, Sigma, Pack, Extract,
    App, Pi, Lam, Var, Ret,
    Literal,
    TyIdx, TyInt, TyReal,
    Nothing,
    Add, Sub, Mul, Div,
    Gt, Ne,
    Slot, Alloc, Store, Load, Free, Ptr, Mem
}

impl WorldImpl {
    pub fn axiom( &self, ax : Axiom ) -> DefLink {
        *self.axioms.get(&ax).unwrap()
    }

    pub fn def2axiom( &self, link : DefLink ) -> Option<Axiom> {
        self.axioms_rev.get(&link).cloned()
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