use std::{
    cell::UnsafeCell,
    mem::{transmute, MaybeUninit},
    ops::{Deref, DerefMut},
};

use crate::repr::value::Value;

const MAX_SIZE: usize = 4096;
#[derive(Debug)]
pub struct FixedStack {
    // This abomination is necessary because of aliasing rules
    // In particular, we cannot take a &mut because we have pointers into the stack that mutate it
    // MaybeUninit is mostly an optimization, since we already have len
    stack: [UnsafeCell<MaybeUninit<Value>>; MAX_SIZE],
    len: UnsafeCell<usize>,
}

impl FixedStack {
    pub fn new() -> Self {
        // this is just used to initialize the array, and is not actually mutated
        #[allow(clippy::declare_interior_mutable_const)]
        const UNINIT: UnsafeCell<MaybeUninit<Value>> = UnsafeCell::new(MaybeUninit::uninit());
        Self {
            stack: [UNINIT; MAX_SIZE],
            len: UnsafeCell::new(0),
        }
    }

    pub fn len(&self) -> usize {
        unsafe {
            *self.len.get()
        }
    }

    #[must_use]
    pub fn push(&self, value: Value) -> bool {
        unsafe {
            let len = self.len();
            if len >= MAX_SIZE {
                return false;
            }

            (*self.stack[len].get()).write(value);
            *self.len.get() += 1;
            true
        }
    }

    #[must_use]
    pub fn peek(&self, i: usize) -> Option<Value> {
        unsafe {
            let len = self.len();
            let i = len - i - 1;
            if i >= len {
                return None;
            }
            Some(*self.get_ptr(i))
        }
    }

    pub fn pop(&self) -> Option<Value> {
        let res = self.peek(0);
        if let Some(_) = res {
            unsafe {
                *self.len.get() -= 1;
            }
        }
        res
    }

    /// index must be an index to a value already pushed
    pub unsafe fn get_ptr(&self, index: usize) -> *mut Value {
        // MaybeUninit is repr(transparent)
        transmute(self.stack[index].get())
    }
}

impl Deref for FixedStack {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        unsafe {
            // this is sound: https://stackoverflow.com/questions/55313460/is-it-sound-to-transmute-a-maybeuninitt-n-to-maybeuninitt-n
            transmute(&self.stack[..*self.len.get()])
        }
    }
}

