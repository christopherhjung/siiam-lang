use std::collections::HashMap;
use std::ptr::{null, null_mut};
use std::rc::Rc;

#[derive(Clone, Copy)]
struct DefRef{
    world: *mut World,
    ptr : *const Def
}

struct Def{
    ops : Vec<DefRef>,
    data : *const u8
}

struct World{
    sea : HashMap<String, Box<Def>>,
    bot : Box<Def>
}

impl World{
    fn default(&mut self) -> DefRef{
        DefRef{
            world: self,
            ptr: &*self.bot
        }
    }

    fn lit_int( &mut self, val : i32 ) -> DefRef{
        self.default()
    }

    fn ty_int( &mut self, width : i32 ) -> DefRef{
        self.default()
    }

    fn bot( &mut self ) -> DefRef{
        self.default()
    }

    fn tuple( &mut self, elems: &[DefRef] ) -> DefRef{
        self.default()
    }

    fn pi( &mut self, domain: DefRef, co_domain : DefRef ) -> DefRef{
        self.default()
    }

    fn lam( &mut self, ty : DefRef ) -> DefRef{
        self.default()
    }

    fn var( &mut self, lam: DefRef ) -> DefRef{
        self.default()
    }

    fn extract( &mut self, tup: DefRef, index: DefRef ) -> DefRef{
        self.default()
    }

    fn pack( &mut self, shape: DefRef, body: DefRef ) -> DefRef{
        self.default()
    }
}

unsafe fn test(){
    let mut world : World = World{ sea: Default::default(), bot: Box::from(Def { ops: vec![], data: null() }) };

    let zero = world.lit_int(0);
    let one = world.lit_int(1);

    let int_ty = world.ty_int(32);
    let bot = world.bot();
    let pi = world.pi(int_ty, bot);

    let cn = world.lam(pi);
    let var = world.var(cn);
    let input = world.extract(var, zero);
    let ret_pi = world.extract(var, one);

    //cn.body = world.app(ret_pi, input);
}


