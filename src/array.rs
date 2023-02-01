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
                let layout = Layout::from_size_align_unchecked(len * std::mem::size_of::<T>(), std::mem::size_of::<usize>());
                alloc(layout) as *mut T
            }
        };
        Self { ptr, len }
    }

    pub fn get_ptr(&self, idx: usize) -> *mut T{
        unsafe {self.ptr.add(idx)}
    }

    pub fn get(&self, idx: usize) -> &T {
        unsafe { self.slot(idx) }
    }

    pub fn set(&self, idx: usize, val : T){
        unsafe { *self.slot(idx) = val }
    }

    unsafe fn slot(&self, idx: usize) -> &mut T {
        assert!(idx < self.len);
        &mut *self.ptr.add(idx)
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

impl<'a, T> IntoIterator for &'a Array<T>{
    type Item = &'a T;
    type IntoIter = ArrayIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        ArrayIterator{
            idx: 0,
            arr: &self
        }
    }
}

pub struct ArrayIterator<'a, T>{
    idx: usize,
    arr: &'a Array<T>
}

impl<'a, T> Iterator for ArrayIterator<'a, T>{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.arr.len{
            let last_idx = self.idx;
            self.idx = last_idx + 1;
            Some(self.arr.get(last_idx))
        }else{
            None
        }
    }
}