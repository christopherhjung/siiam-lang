use hex::ToHex;
use rand::{distributions::Alphanumeric, Rng};
use sha2::{Digest, Sha256, Sha384, Sha512};
use std::ptr::eq;
use std::time::{Duration, Instant};

pub struct Hash{
    pub data : [u8; 32]
}

impl Hash{
    pub fn toHex( &self ) -> String{
        return hex::encode(self.data);
    }

    pub fn random(  ) -> Hash{
        let s: String = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(7)
            .map(char::from)
            .collect();

        let output = Sha256::digest(s.as_bytes());
        let mut hash = Hash { data: [0; 32] };
        hash.data.copy_from_slice(output.as_ref());
        return hash;
    }

    pub fn zero(  ) -> Hash{
        return Hash { data: [0; 32] };
    }
}

impl PartialEq for Hash{
    fn eq(&self, other: &Self) -> bool{
        return self.data == other.data
    }
}

impl Eq for Hash {}