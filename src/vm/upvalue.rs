use std::fmt::Display;

use crate::value::{valid::ValidPtr, Value};

#[derive(Copy, Clone, Debug)]
pub struct Upvalue {
    pub value: ValidPtr<Value>,
    /// This should never be directly accessed after initialization, it's just storage
    closed: Value,
    pub marked: bool,
    pub next_open: Option<ValidPtr<Upvalue>>,
}

impl Upvalue {
    pub fn new(value: ValidPtr<Value>, next_open: Option<ValidPtr<Upvalue>>) -> Self {
        Self {
            value,
            closed: Value::Num(f64::MAX),
            marked: false,
            next_open,
        }
    }

    pub fn close(my: ValidPtr<Self>) {
        unsafe {
            /*
            This is deeply unsound under stack borrows, since it pops off the &mut VM that calls it
            But that VM only ever references the stack immutably, so that seems rather silly
            Tree borrows are ok with this, and hopefully whatever model Rust winds up with does too
            Regardless, this is a bit of a wont-fix. Fixing it would require making the VM extremely un-ergonomic
            And this is a learning project after all.
             */
            (*my.as_ptr()).closed = *my.value;
            (*my.as_ptr()).value = ValidPtr::from_ptr(&mut (*my.as_ptr()).closed);
        }
    }

    pub fn mark(&self) {
        self.closed.mark();
    }
}

impl PartialEq for Upvalue {
    fn eq(&self, other: &Self) -> bool {
        self.value.as_ptr() == other.value.as_ptr()
    }
}

impl Eq for Upvalue {}

impl Display for Upvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<upvalue = {} @ {:?}>", *self.value, self.value.as_ptr())
    }
}
