

use std::borrow::{Borrow, BorrowMut};
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::ptr::{null, null_mut};
use std::rc::Rc;

pub struct UnsafeMut<T>{
    ptr: *mut T
}

impl<T> UnsafeMut<T> {
    pub fn null() -> UnsafeMut<T>{
        UnsafeMut{
            ptr: null_mut()
        }
    }

    pub fn is_null(&self) -> bool{
        self.ptr == null_mut()
    }
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

pub struct MutBox<T>{
    inner : Box<T>
}

impl<T> MutBox<T>{
    pub fn new(val : T) -> MutBox<T>{
        MutBox{
            inner: Box::new(val)
        }
    }
}

impl<T> From<Box<T>> for MutBox<T>{
    fn from( b : Box<T> ) -> Self {
        MutBox{
            inner: b
        }
    }
}

impl<T> Deref for MutBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {&*self.inner}
    }
}

impl<T> DerefMut for MutBox<T>{
    fn deref_mut(&mut self) -> &mut Self::Target {
        let mut_ptr = &*self.inner as *const _ as *mut T;
        unsafe {&mut *mut_ptr}
    }
}

impl<T> MutBox<T> {
    pub fn get(&self) -> &mut T {
        let mut_ptr = &*self.inner as *const _ as *mut T;
        unsafe {&mut *mut_ptr}
    }
}

macro_rules! arr(
    ($size: expr, $factory: expr) => ({
        unsafe fn get_item_ptr<T>(slice: *mut [T], index: usize) -> *mut T {
            (slice as *mut T).offset(index as isize)
        }

        let mut arr = ::std::mem::MaybeUninit::<[_; $size]>::uninit();
        unsafe {
            for i in 0..$size {
                ::std::ptr::write(get_item_ptr(arr.as_mut_ptr(), i), $factory(i));
            }
            arr.assume_init()
        }
    })
);

#[macro_export]
macro_rules! array {
    () => (
        Array::empty()
    );
    ($($x:expr),+ $(,)?) => ({
        let elems = [$($x),+];
        let arr = Array::new(elems.len());
        for idx in 0 .. elems.len(){
            arr.set(idx, elems[idx]);
        }

        arr
    });
}

#[macro_export]
macro_rules! arr_for_each (
    ($size: expr, $factory: expr) => ({
        unsafe fn get_item_ptr<T>(slice: *mut [T], index: usize) -> *mut T {
            (slice as *mut T).add(index)
        }

        let mut arr = std::mem::MaybeUninit::<[_; $size]>::uninit();
        unsafe {
            for i in 0..$size {
                std::ptr::write(get_item_ptr(arr.as_mut_ptr(), i), $factory(i));
            }
            arr.assume_init()
        }
    })
);

#[macro_export]
macro_rules! zip {
    ($x: expr) => ($x);
    ($x: expr, $($y: expr), +) => (
        $x.into_iter().zip(
            zip!($($y), +))
    )
}