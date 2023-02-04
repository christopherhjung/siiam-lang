use std::collections::HashSet;
use crate::def::{DefKind, DefLink};

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