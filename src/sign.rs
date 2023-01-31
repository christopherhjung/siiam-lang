use std::cmp::min;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use hex::ToHex;
use rand::{distributions::Alphanumeric, Rng};
use sha2::{Digest, Sha256, Sha384, Sha512};
use std::ptr::{eq, null};
use std::time::{Duration, Instant};
use sha2::digest::Update;
use crate::def::{Def, DefProxy, DefPtr, Mode};
use crate::utils::UnsafeMut;
use crate::World;

#[derive(Copy, Clone, Hash)]
pub struct Signature {
    pub data : [u8; 32]
}

impl Signature {
    pub fn toHex( &self ) -> String{
        return hex::encode(self.data);
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

    pub fn zero(  ) -> Signature {
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
    pub fn sign(def: &Def) -> Signature {
        let mut hash = Sha256::new();

        for op_ptr in &def.ops{
            let sign = if *op_ptr == null(){
                Signature::zero()
            }else {
                let op = unsafe{&**op_ptr};
                if let Some(sign) = op.sign{
                    sign
                }else{
                    Signature::zero()
                }
            };

            hash = Digest::chain( hash, sign)
        }

        let data_arr = unsafe{std::slice::from_raw_parts(def.data.get_ptr(0), def.data.len())};
        hash = Update::chain(hash, data_arr);

        let arr =  hash.finalize();

        Signature{ data: <[u8; 32]>::from(arr) }
    }
}





pub struct SignNode{
    index : usize,
    low_link : usize,
    sign : Signature,
    closed: bool,
    signs : [Signature; 2]
}

pub struct CyclicSigner {
    index: usize,
    world: *mut World,
    nodes: HashMap<DefPtr, Box<SignNode>>
}

impl CyclicSigner {
    pub fn sign(proxy: DefProxy) -> DefProxy{
        let mut signer = CyclicSigner {
            index: 0,
            world: proxy.world,
            nodes: HashMap::new()
        };

        signer.discover(proxy.ptr);
        proxy
    }

    fn node(&mut self, ptr: DefPtr) -> UnsafeMut<SignNode>{
        let node = match self.nodes.entry(ptr) {
            Occupied(entry) => UnsafeMut::from(entry.get()),
            Vacant(entry) => {
                let last_idx = self.index;
                self.index = last_idx + 1;

                let def = unsafe{&*ptr};

                let sign = if let Some(sign) = def.sign{
                    sign
                }else{
                    Signature::zero()
                };

                let b = entry.insert(
                    Box::new(
                        SignNode{
                            index: last_idx,
                            low_link: last_idx,
                            sign,
                            closed : false,
                            signs : [Signature::zero(); 2]
                        })
                );

                UnsafeMut::from(b)
            }
        };

        node
    }

    fn discover(&mut self, curr: DefPtr) -> bool{
        if self.nodes.contains_key(&curr){
            return true;
        }

        let curr_def = unsafe{&*curr};
        if curr_def.mode == Mode::Constructed{
            return false;
        }

        let mut curr_node = self.node(curr);

        for dep_ptr in DefProxy::from(curr).ops() {
            if self.discover(*dep_ptr){
                let dep_node = self.node(*dep_ptr);
                if !dep_node.closed{
                    curr_node.low_link = min(curr_node.low_link, dep_node.low_link);
                }
            }
        }

        if curr_node.index == curr_node.low_link{
            self.sign_impl(curr);
        }

        return true;
    }

    fn sign_node(&mut self, def_ptr : DefPtr, slot: usize){
        let def = unsafe{&*def_ptr};
        let mut node = self.node(def_ptr);

        let mut hash = Sha256::new();

        for op_ptr in &def.ops{
            let op = unsafe{&**op_ptr};
            let sign = if let Some(sign) = op.sign{
                sign
            }else{
                let dep_node = self.node(*op_ptr);
                dep_node.signs[slot]
            };

            hash = Digest::chain( hash, sign)
        }

        let data_arr = unsafe{std::slice::from_raw_parts(def.data.get_ptr(0), def.data.len())};
        hash = Update::chain(hash, data_arr);

        node.signs[1 - slot].data = <[u8; 32]>::from(hash.finalize());
    }

    fn sign_impl(&mut self, curr: DefPtr){
        let mut list = Vec::new();
        self.collect(curr, &mut list);

        let len = list.len();

        for epoch in 0 .. len{
            for def_ptr in &list{
                self.sign_node(*def_ptr, epoch % 2)
            }
        }

        println!("----------------------- {}", len);
        for def_ptr in &list{
            let w = unsafe{&mut *self.world};
            let node = self.node(*def_ptr);

            //w.get_or_insert(def_ptr, node.signs[len % 2]);

            println!("{:?}", node.signs[len % 2]);
        }
        println!("-----------------------");
    }

    fn collect(&mut self, curr: DefPtr, list: &mut Vec<DefPtr>){
        if let Some(curr_node) = self.nodes.get(&curr){
            if curr_node.index == curr_node.low_link && !list.is_empty(){
                return
            }

            UnsafeMut::from(curr_node).closed = true;
            list.push(curr);
            for dep_ptr in DefProxy::from(curr).ops() {
                self.collect(*dep_ptr, list)
            }
        }
    }
}

