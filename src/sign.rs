use std::cmp::min;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::io::BufRead;
use std::iter::Map;
use std::mem::MaybeUninit;
use hex::ToHex;
use rand::{distributions::Alphanumeric, Rng};
use sha2::{Digest, Sha256, Sha384, Sha512};
use std::ptr::{eq, null};
use std::rc::Rc;
use std::time::{Duration, Instant};
use sha2::digest::{FixedOutput, Update};
use crate::def::{DefModel, Def, DefLink, DefState, DefKind};
use crate::utils::UnsafeMut;
use crate::world::World;
use crate::{Array, WorldImpl};

#[derive(Copy, Clone, Hash)]
pub struct Signature {
    pub data : [u8; 32]
}

impl Signature {
    pub fn to_hex(&self ) -> String{
        return hex::encode(self.data);
    }
    pub fn is_zero( &self ) -> bool{
        self.data == [0; 32]
    }

    pub fn zero() -> Signature {
        Signature { data: [0; 32] }
    }
}

impl Debug for Signature{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&*self.to_hex())
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
        let mut hash = Sha512::new();

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
                hash = Update::chain(hash, data.slice());
            }
        }

        let arr =  hash.finalize();
        Signature{ data: arr.as_slice()[0..32].try_into().expect("Wrong length") }
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
            for op in ops {
                if self.discover(*op){
                    let dep_node = self.node(*op);
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
        let mut hash = Sha512::new();

        if let DefState::Constructed(sign) = &def.ax.state{
            Update::update( &mut hash, sign);
        }else{
            panic!()
        }

        match &def.kind {
            DefKind::Node(ops) => {
                for op in ops{
                    let sign = if let DefState::Constructed(sign) = op.state {
                        sign
                    }else{
                        let dep_node = self.node(*op);

                        if let Some(new) = self.old2new.get(&dep_node.link){
                            if let DefState::Constructed(sign) = new.state {
                                sign
                            }else{
                                panic!()
                            }
                        }else{
                            dep_node.unique().signs[slot]
                        }
                    };

                    Update::update( &mut hash, sign)
                }
            }
            DefKind::Data(data) => {
                Update::update(&mut hash, data.slice());
            }
        }

        node.signs[1 - slot].data = hash
            .finalize_fixed()
            .as_slice()[0..32]
            .try_into()
            .expect("Wrong length")
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
        self.disambiguate(&old_defs);
    }

    fn unique_defs(&mut self, old_defs: &Vec<DefLink>) -> Vec<DefLink>{
        let mut unique_map = HashMap::new();
        let len = old_defs.len();

        for old in old_defs {
            let mut node = self.node(*old);
            let sign = &node.signs[len % 2];

            if unique_map.contains_key(sign){
                node.unique = self.node(*unique_map.get(sign).unwrap());
            }else{
                unique_map.insert(*sign, *old);
            }
        }

        unique_map.values().cloned().collect()
    }

    fn disambiguate(&mut self, old_defs: &Vec<DefLink>){
        if old_defs.len() == 1{
            self.create_new_defs(&old_defs, &old_defs);
        }else{
            let mut unique_defs = self.unique_defs(old_defs);

            if unique_defs.len() != old_defs.len(){
                for old in &unique_defs {
                    let mut node = self.node(*old);
                    node.signs = [Signature::zero(); 2];
                }

                self.blend(&unique_defs);
            }

            self.create_new_defs(&old_defs, &unique_defs);
        };
    }

    fn create_new_defs(&mut self, old_defs: &Vec<DefLink>, unique_defs: &Vec<DefLink>){
        let mut map = HashMap::new();

        for def in unique_defs {
            let op_len = if let DefKind::Node(ops) = &def.kind{
                ops.len()
            }else{
                0
            };

            let node = self.node(*def);
            let sign = &node.signs[unique_defs.len() % 2];
            map.insert(*def, Box::new(DefModel {
                ax: def.ax,
                kind: DefKind::Node(Array::new(op_len)),
                state: DefState::Constructed(*sign)
            }));
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
        if let Some(curr_node) = self.nodes.get_mut(&curr){
            if curr_node.index == curr_node.low_link && !list.is_empty() || curr_node.closed{
                return
            }

            list.push(curr);
            curr_node.closed = true;
            if let DefKind::Node(ops) = &curr.kind{
                for dep_ptr in ops {
                    self.collect(*dep_ptr, list)
                }
            }
        }
    }
}

