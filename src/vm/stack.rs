use std::{cell::UnsafeCell, mem::transmute};

use crate::repr::value::Value;

const MAX_SIZE: usize = 4096;
#[derive(Debug)]
pub struct FixedStack {
    // This abomination is necessary because of aliasing rules
    // In particular, we cannot take a &mut because we have pointers into the stack that mutate it
    // MaybeUninit is mostly an optimization, since we already have len
    stack: [UnsafeCell<Value>; MAX_SIZE],
    len: UnsafeCell<usize>,
}

impl FixedStack {
    pub fn new() -> Self {
        // this is just used to initialize the array, and is not actually mutated
        #[allow(clippy::declare_interior_mutable_const)]
        const UNINIT: UnsafeCell<Value> = UnsafeCell::new(Value::Num(f64::MAX));
        Self {
            stack: [UNINIT; MAX_SIZE],
            len: UnsafeCell::new(0),
        }
    }

    pub fn len(&self) -> usize {
        unsafe { *self.len.get() }
    }

    #[must_use]
    pub fn push(&self, value: Value) -> bool {
        unsafe {
            let len = self.len();
            if len >= MAX_SIZE {
                return false;
            }

            *self.stack[len].get() = value;
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

    pub fn get(&self, i: usize) -> Option<Value> {
        if i >= self.len() {
            None
        } else {
            unsafe { Some(*self.get_ptr(i)) }
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

    pub unsafe fn get_ptr(&self, index: usize) -> *mut Value {
        // MaybeUninit is repr(transparent)
        self.stack[index].get()
    }

    /// SAFETY: The burden of following aliasing rules is on the callee
    ///         With great power comes great responsibility
    pub unsafe fn slice(&self) -> &[Value] {
        // UnsafeCell is repr(transparent)
        transmute(&self.stack[..self.len()])
    }
}
