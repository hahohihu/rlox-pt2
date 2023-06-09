use core::slice;
use std::cell::{Cell, UnsafeCell};

use crate::value::Value;

const MAX_SIZE: usize = 4096;
#[derive(Debug)]
pub struct FixedStack {
    // Interior mutability is needed because because we have pointers into the stack that mutate it
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

    pub fn len(&self) -> usize {
        self.len.get()
    }

    pub fn clear(&self) {
        self.len.set(0);
    }

    #[inline(always)]
    pub unsafe fn push(&self, value: Value) {
        let len = self.len.get();
        self.len.set(len + 1);
        *self.stack.get_unchecked(len).get() = value;
    }

    #[inline(always)]
    pub unsafe fn peek(&self, i: usize) -> Value {
        *self.get_ptr(self.len.get() - i - 1)
    }

    #[inline(always)]
    pub unsafe fn pop(&self) -> Value {
        let len = self.len.get();
        self.len.set(len - 1);
        *self.get_ptr(len - 1)
    }

    #[inline(always)]
    pub unsafe fn get_ptr(&self, index: usize) -> *mut Value {
        // MaybeUninit is repr(transparent)
        self.stack.get_unchecked(index).get()
    }

    /// SAFETY: The burden of following aliasing rules is on the callee
    ///         With great power comes great responsibility
    pub unsafe fn slice(&self) -> &[Value] {
        let slice = &self.stack[..self.len.get()];
        // UnsafeCell is repr(transparent)
        slice::from_raw_parts(slice.as_ptr() as *const Value, slice.len())
    }
}
