use std::alloc::{alloc, dealloc, Layout};
use std::ptr::{null, null_mut};

pub struct Array<T> {
    ptr: *mut T,
    len: usize,
}

impl<T> Array<T> {
    pub fn empty() -> Self{
        Self::new(0)
    }

    pub fn new(len: usize) -> Self {
        let ptr = if len == 0{
            null_mut()
        }else{
            unsafe {
                let layout = Layout::from_size_align_unchecked(len, std::mem::size_of::<T>());
                alloc(layout) as *mut T
            }
        };
        Self { ptr, len }
    }

    pub fn get_ptr(&self, idx: usize) -> *mut T{
        unsafe {self.ptr.add(idx)}
    }

    pub fn get(&self, idx: usize) -> &T {
        assert!(idx < self.len);
        unsafe { &*(self.ptr.add(idx)) }
    }
    pub fn get_mut(&self, idx: usize) -> &mut T {
        assert!(idx < self.len);
        unsafe { &mut *(self.ptr.add(idx)) }
    }
    pub fn set(&self, idx: usize, val : T){
        *self.get_mut(idx) = val;
    }
    pub fn len(&self) -> usize {
        self.len
    }
}


impl<T> Drop for Array<T> {
    fn drop(&mut self) {
        if self.len == 0 {
            return
        }
        unsafe {
            dealloc(
                self.ptr as *mut u8,
                Layout::from_size_align_unchecked(self.len, std::mem::size_of::<T>()),
            )
        };
    }
}

impl<T> std::ops::Index<usize> for Array<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        self.get(index)
    }
}
impl<T> std::ops::IndexMut<usize> for Array<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index)
    }
}