use std::ops::{Deref, DerefMut};

pub struct UnsafeMut<T>{
    ptr: *mut T
}

impl<T> From<&Box<T>> for UnsafeMut<T>{
    fn from(b: &Box<T>) -> Self {
        let b_ref = &**b;
        let b_ptr = b_ref as *const T;
        let b_mut_ptr = b_ptr as *mut T;
        UnsafeMut {
            ptr: b_mut_ptr
        }
    }
}

impl<T> From<&mut Box<T>> for UnsafeMut<T>{
    fn from(b: &mut Box<T>) -> Self {
        let b_ref = &**b;
        let b_ptr = b_ref as *const T;
        let b_mut_ptr = b_ptr as *mut T;
        UnsafeMut {
            ptr: b_mut_ptr
        }
    }
}

impl<T> Deref for UnsafeMut<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {&*self.ptr}
    }
}

impl<T> DerefMut for UnsafeMut<T>{
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {&mut *self.ptr}
    }
}