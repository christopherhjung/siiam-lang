use std::alloc::alloc;
use std::collections::HashMap;
use std::ops::Index;
use std::ptr;
use std::ptr::{null, null_mut};
use std::rc::Rc;
use crate::hash::Signature;
use crate::utils::HeapArray;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Clone, Copy)]
pub struct DefRef{
    world: *mut World,
    ptr : *const Def
}

struct Def{
    ops: HeapArray<*const Def>,
    data: HeapArray<u8>,
    sign: Signature,
}

impl DefRef {
    fn new( world: *mut World, ptr : *const Def ) -> DefRef{
        DefRef{ world, ptr }
    }

    pub fn op( &self, index : usize ) -> DefRef{
        let def = unsafe{&*self.ptr};
        let op_ptr = *def.ops.index(index);
        DefRef::new(self.world, op_ptr)
    }

    pub fn data<T>( &self, idx : usize ) -> &T{
        let def = unsafe{&*self.ptr};
        let data_ptr = unsafe{def.data.get_ptr(idx)};
        unsafe {&*(data_ptr as *const T)}
    }

    pub fn sign(&self) -> Signature{
        let def = unsafe{&*self.ptr};
        def.sign
    }
}

pub struct World{
    hash_size : usize,
    sea : HashMap<Signature, Box<Def>>,
    axioms : HashMap<Axiom, DefRef>
}

#[derive(EnumIter, Hash, Eq, PartialEq)]
enum Axiom{
    Bot, Tuple
}

impl World{
    fn axiom( &self, ax : Axiom ) -> DefRef{
        *self.axioms.get(&ax).unwrap()
    }

    fn default(&mut self) -> DefRef{
        self.bot()
    }

    pub fn lit_int( &mut self, val : i32 ) -> DefRef{
        self.default()
    }

    pub fn ty_int( &mut self, width : i32 ) -> DefRef{
        self.default()
    }

    pub fn bot( &mut self ) -> DefRef{
        self.axiom(Axiom::Bot)
    }

    pub fn tuple( &mut self, elems: &[DefRef] ) -> DefRef{
        self.default()
    }

    pub fn pack( &mut self, shape: DefRef, body: DefRef ) -> DefRef{
        self.default()
    }

    pub fn extract( &mut self, tup: DefRef, index: DefRef ) -> DefRef{
        self.default()
    }

    pub fn app( &mut self, callee: DefRef, arg: DefRef ) -> DefRef{
        self.default()
    }

    pub fn pi( &mut self, domain: DefRef, co_domain : DefRef ) -> DefRef{
        self.default()
    }

    pub fn lam( &mut self, ty : DefRef ) -> DefRef{
        self.default()
    }

    pub fn var( &mut self, lam: DefRef ) -> DefRef{
        self.default()
    }

    pub fn set_body( &mut self, lam: DefRef, body: DefRef ){

    }

    pub fn new() -> Box<World>{
        let sea = HashMap::new();
        let axioms = HashMap::new();

        let mut world = Box::new(World{
            hash_size: 32,
            sea,
            axioms
        });

        let mut_world_ptr = unsafe{&*world as *const _ as *mut World};

        let init_sign = Signature::zero();

        for axiom in Axiom::iter(){
            let axiom_def = Box::from(Def{
                ops: HeapArray::new(0),
                data: HeapArray::new(0),
                sign: Signature::random()
            });

            println!("{:?}", axiom_def.sign.toHex());

            world.axioms.insert(axiom, DefRef::new(mut_world_ptr, &*axiom_def));
            world.sea.insert(axiom_def.sign, axiom_def);
        }

        world
    }
}

unsafe fn test(){
    let mut world = World::new();

    let zero = world.lit_int(0);
    let one = world.lit_int(1);

    let int_ty = world.ty_int(32);
    let bot = world.bot();
    let pi = world.pi(int_ty, bot);

    let cn = world.lam(pi);
    let var = world.var(cn);
    let input = world.extract(var, zero);
    let ret_pi = world.extract(var, one);

    let app = world.app(ret_pi, input);
    world.set_body(cn, app);
}


