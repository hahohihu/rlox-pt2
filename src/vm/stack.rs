use core::slice;
use std::cell::{UnsafeCell, Cell};

use crate::value::Value;

const MAX_SIZE: usize = 4096;
#[derive(Debug)]
pub struct FixedStack {
    // This abomination is necessary because of aliasing rules
    // In particular, we cannot take a &mut because we have pointers into the stack that mutate it
    // MaybeUninit is mostly an optimization, since we already have len
    stack: [UnsafeCell<Value>; MAX_SIZE],
    len: Cell<usize>,
}

impl FixedStack {
    pub fn new() -> Self {
        // this is just used to initialize the array, and is not actually mutated
        #[allow(clippy::declare_interior_mutable_const)]
        const UNINIT: UnsafeCell<Value> = UnsafeCell::new(Value::Num(f64::MAX));
        Self {
            stack: [UNINIT; MAX_SIZE],
            len: Cell::new(0),
        }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len.get()
    }

    #[inline(always)]
    pub unsafe fn unchecked_push(&self, value: Value) {
        let len = self.len();
        self.len.set(len + 1);
        *self.stack.get_unchecked(len).get() = value;
    }

    #[inline(always)]
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

    #[inline(always)]
    pub fn get(&self, i: usize) -> Option<Value> {
        if i >= self.len() {
            None
        } else {
            unsafe { Some(*self.get_ptr(i)) }
        }
    }

    #[inline(always)]
    pub fn pop(&self) -> Option<Value> {
        let res = self.peek(0);
        if let Some(_) = res {
            self.len.set(self.len() - 1);
        }
        res
    }

    #[inline(always)]
    pub unsafe fn get_ptr(&self, index: usize) -> *mut Value {
        // MaybeUninit is repr(transparent)
        self.stack.get_unchecked(index).get()
    }

    /// SAFETY: The burden of following aliasing rules is on the callee
    ///         With great power comes great responsibility
    pub unsafe fn slice(&self) -> &[Value] {
        let slice = &self.stack[..self.len()];
        // UnsafeCell is repr(transparent)
        slice::from_raw_parts(slice.as_ptr() as *const Value, slice.len())
    }
}
