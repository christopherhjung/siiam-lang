use std::cmp::min;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter::Map;
use std::mem::MaybeUninit;
use hex::ToHex;
use rand::{distributions::Alphanumeric, Rng};
use sha2::{Digest, Sha256, Sha384, Sha512};
use std::ptr::{eq, null};
use std::rc::Rc;
use std::time::{Duration, Instant};
use sha2::digest::Update;
use crate::def::{DefModel, Def, DefLink, Mode, DefState, DefKind};
use crate::utils::UnsafeMut;
use crate::world::World;
use crate::{Array, WorldImpl};

#[derive(Copy, Clone, Hash)]
pub struct Signature {
    pub data : [u8; 32]
}

impl Signature {
    pub fn toHex( &self ) -> String{
        return hex::encode(self.data);
    }
    pub fn is_zero( &self ) -> bool{
        self.data == [0; 32]
    }

    pub fn random(  ) -> Signature {
        let s: String = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(7)
            .map(char::from)
            .collect();

        let output = Sha256::digest(s.as_bytes());
        let mut hash = Signature { data: [0; 32] };
        hash.data.copy_from_slice(output.as_ref());
        hash
    }

    pub fn zero() -> Signature {
        Signature { data: [0; 32] }
    }


}

impl Debug for Signature{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&*self.toHex())
    }
}

impl AsRef<[u8]> for Signature{
    fn as_ref(&self) -> &[u8] {
        &self.data as &[u8]
    }
}

impl PartialEq for Signature {
    fn eq(&self, other: &Self) -> bool{
        self.data == other.data
    }
}

impl Eq for Signature {}



pub struct AcyclicSigner{

}

impl AcyclicSigner{
    pub fn sign(def: &DefModel) -> Signature {
        let mut hash = Sha256::new();


        if let DefState::Constructed(sign) = &def.ax.state{
            hash = Digest::chain( hash, sign);
        }else{
            panic!()
        }

        match &def.kind {
            DefKind::Node(ops) => {
                for op_ptr in ops{
                    let sign = if let DefState::Constructed(sign) = op_ptr.state {
                        sign
                    }else{
                        Signature::zero()
                    };

                    hash = Digest::chain( hash, sign.data)
                }
            }
            DefKind::Data(data) => {
                let data_arr = unsafe{std::slice::from_raw_parts(data.get_ptr(0), data.len())};
                hash = Update::chain(hash, data_arr);
            }
        }

        let arr =  hash.finalize();

        Signature{ data: <[u8; 32]>::from(arr) }
    }
}





pub struct SignNode{
    index : usize,
    low_link : usize,
    closed: bool,
    link: DefLink,
    unique: UnsafeMut<SignNode>,
    signs : [Signature; 2]
}

impl SignNode{
    pub fn unique(&self) -> &Self{
        if self.unique.is_null(){
            self
        }else{
            self.unique.unique()
        }
    }
}

pub struct CyclicSigner<'a> {
    index: usize,
    world: &'a mut WorldImpl,
    nodes: HashMap<DefLink, Box<SignNode>>,
    old2new: HashMap<DefLink, DefLink>,
}

impl<'a> CyclicSigner<'a> {
    pub fn new(world: &'a mut WorldImpl) -> CyclicSigner<'a>{
        CyclicSigner {
            index: 0,
            world,
            nodes: HashMap::new(),
            old2new: HashMap::new(),
        }
    }

    pub fn old2new(&mut self, old: DefLink) -> DefLink{
        if let Some(new) = self.old2new.get(&old){
            *new
        }else{
            old
        }
    }

    fn insert(&mut self, link: DefLink){
        if !self.nodes.contains_key(&link){
            let last_idx = self.index;
            self.index = last_idx + 1;
            self.nodes.insert(link,
                  Box::new(
                      SignNode{
                          index: last_idx,
                          low_link: last_idx,
                          closed : false,
                          link,
                          unique: UnsafeMut::null(),
                          signs : [Signature::zero(); 2]
                      }
                  )
            );
        }
    }

    fn node(&mut self, link: DefLink) -> UnsafeMut<SignNode>{
        self.insert(link);
        UnsafeMut::from(self.nodes.get(&link).unwrap())
    }

    pub fn discover(&mut self, curr: DefLink) -> bool{
        if self.nodes.contains_key(&curr){
            return true;
        }

        if let DefState::Constructed(_) = curr.state {
            return false;
        }

        let mut curr_node = self.node(curr);

        if let DefKind::Node(ops) = &curr.kind{
            for dep_ptr in ops {
                if self.discover(*dep_ptr){
                    let dep_node = self.node(*dep_ptr);
                    if !dep_node.closed{
                        curr_node.low_link = min(curr_node.low_link, dep_node.low_link);
                    }
                }
            }
        }

        if curr_node.index == curr_node.low_link{
            self.sign(curr);
        }

        return true;
    }

    fn sign_node(&mut self, def : DefLink, slot: usize){
        let mut node = self.node(def);
        let mut hash = Sha256::new();


        match &def.kind {
            DefKind::Node(ops) => {
                for op in ops{
                    let sign = if let DefState::Constructed(sign) = op.state {
                        sign
                    }else{
                        let dep_node = self.node(*op);
                        dep_node.unique().signs[slot]
                    };

                    hash = Digest::chain( hash, sign)
                }
            }
            DefKind::Data(data) => {
                let data_arr = unsafe{std::slice::from_raw_parts(data.get_ptr(0), data.len())};
                hash = Update::chain(hash, data_arr);
            }
        }

        node.signs[1 - slot].data = <[u8; 32]>::from(hash.finalize());
    }

    fn blend(&mut self, vec: &Vec<DefLink>){
        for epoch in 0 .. vec.len(){
            for def in vec {
                self.sign_node(*def, epoch % 2)
            }
        }
    }

    fn sign(&mut self, curr: DefLink){
        let mut old_defs = Vec::new();
        self.collect(curr, &mut old_defs);
        self.blend(&old_defs);
        self.filter(&old_defs);
    }

    fn unique_defs(&mut self, old_defs: &Vec<DefLink>) -> Vec<DefLink>{
        let mut unique_map = HashMap::new();
        let mut unique_defs = Vec::new();
        let len = old_defs.len();

        for old in old_defs {
            let mut node = self.node(*old);
            let sign = &node.signs[len % 2];

            if unique_map.contains_key(sign){
                node.unique = self.node(*unique_map.get(sign).unwrap());
            }else{
                unique_map.insert(*sign, *old);
                unique_defs.push(*old);
            }
        }

        unique_defs
    }

    fn filter(&mut self, old_defs: &Vec<DefLink>){
        if old_defs.len() == 1{
            //no cycle
        }

        let mut unique_defs = self.unique_defs(old_defs);

        if unique_defs.len() != old_defs.len(){
            for old in &unique_defs {
                let mut node = self.node(*old);
                node.signs = [Signature::zero(); 2];
            }

            self.blend(&unique_defs);
        }

        self.create_new_defs(&old_defs, &unique_defs);
    }

    fn create_new_defs(&mut self, old_defs: &Vec<DefLink>, unique_defs: &Vec<DefLink>){
        let mut map = HashMap::new();

        for def in unique_defs {
            if let DefKind::Node(ops) = &def.kind{
                let node = self.node(*def);
                let sign = &node.signs[unique_defs.len() % 2];
                map.insert(*def, Box::new(DefModel {
                    ax: def.ax,
                    kind: DefKind::Node(Array::new(ops.len())),
                    state: DefState::Constructed(*sign)
                }));
            }
        }

        for old in unique_defs {
            if let DefKind::Node(ops) = &old.kind{
                let new = map.get(old).unwrap();
                for idx in 0 .. ops.len(){
                    let op = ops.get(idx);
                    let new_link = if let Some(new_op) = map.get(op){
                        DefLink::from(new_op)
                    }else{
                        *op
                    };

                    if let DefKind::Node(new_ops) = &new.kind{
                        new_ops.set(idx, new_link);
                    }
                }
            }else{
                panic!()
            }
        }
        self.add_mapping(&old_defs, &mut map);
    }

    fn add_mapping(&mut self, defs: &Vec<DefLink>, map : &mut HashMap<DefLink, Box<DefModel>>){
        for old in defs {
            let node = self.node(*old);

            let new = if node.unique.is_null(){
                let model = map.remove(old).unwrap();
                self.world.insert_def(model)
            }else{
                *self.old2new.get(&node.unique.link).unwrap()
            };

            self.old2new.insert(*old, new);
        }
    }

    fn collect(&mut self, curr: DefLink, list: &mut Vec<DefLink>){
        if let Some(curr_node) = self.nodes.get(&curr){
            if curr_node.index == curr_node.low_link && !list.is_empty(){
                return
            }

            UnsafeMut::from(curr_node).closed = true;
            list.push(curr);

            if let DefKind::Node(ops) = &curr.kind{
                for dep_ptr in ops {
                    self.collect(*dep_ptr, list)
                }
            }else{
                panic!();
            }
        }
    }
}

