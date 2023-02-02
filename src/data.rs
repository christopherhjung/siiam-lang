use std::alloc::{alloc, dealloc, Layout};
use std::ptr::{null, null_mut};

pub struct Data {
    ptr: *mut u8,
    len: usize,
}

impl Data {
    pub fn empty() -> Self{
        Self::new(0)
    }

    pub fn new(len: usize) -> Self {
        if len == 0{
            Self { ptr:null_mut(), len: 0 }
        }else{
            let ptr = unsafe {
                alloc(Self::layout(len)) as *mut _ as *mut u8
            };
            Self { ptr, len }
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn put<T>(&self, val : T) {
        unsafe {*(self.ptr as *mut T) = val}
    }

    pub fn get<T>(&self) -> &T {
        unsafe {&*(self.ptr as *mut T)}
    }

    pub fn slice(&self) -> &[u8]{
        unsafe{std::slice::from_raw_parts(self.ptr, self.len)}
    }

    pub fn from<T>(val: T) -> Data {
        let data = Data::new(std::mem::size_of::<usize>());
        data.put(val);
        data
    }

    pub unsafe fn layout(len: usize) -> Layout{
        Layout::from_size_align_unchecked(len, std::mem::size_of::<usize>())
    }
}

impl Drop for Data{
    fn drop(&mut self) {
        if self.len == 0 {
            return
        }
        unsafe {
            dealloc(
                self.ptr as *mut u8,
                Self::layout(self.len)
            )
        };
    }
}