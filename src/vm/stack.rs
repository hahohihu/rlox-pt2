use std::{
    mem::{transmute, MaybeUninit},
    ops::{Deref, DerefMut},
};

use crate::repr::value::Value;

const MAX_SIZE: usize = 4096;
#[derive(Clone, Debug)]
pub struct FixedStack {
    stack: [MaybeUninit<Value>; MAX_SIZE],
    len: usize,
}

impl FixedStack {
    pub fn new() -> Self {
        Self {
            stack: [MaybeUninit::uninit(); MAX_SIZE],
            len: 0,
        }
    }

    #[must_use]
    pub fn push(&mut self, value: Value) -> bool {
        if self.len >= MAX_SIZE {
            return false;
        }
        self.stack[self.len].write(value);
        self.len += 1;
        true
    }

    #[must_use]
    pub fn peek(&self, i: usize) -> Option<Value> {
        let i = self.len - i - 1;
        if i >= self.len {
            return None;
        }
        unsafe { Some(self.stack[i].assume_init()) }
    }

    pub fn pop(&mut self) -> Option<Value> {
        let res = self.peek(0);
        if let Some(_) = res {
            self.len -= 1;
        }
        res
    }
}

impl Deref for FixedStack {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        unsafe {
            // this is sound: https://stackoverflow.com/questions/55313460/is-it-sound-to-transmute-a-maybeuninitt-n-to-maybeuninitt-n
            transmute(&self.stack[..self.len])
        }
    }
}

impl DerefMut for FixedStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { transmute(&mut self.stack[..self.len]) }
    }
}
