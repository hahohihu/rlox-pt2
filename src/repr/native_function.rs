use std::fmt::{Debug, Display};

use super::{value::Value, string::UnsafeString};

pub enum CallError {
    ArityMismatch(u8),
    TypeMismatch(u8, &'static str),
}

#[derive(Copy, Clone)]
pub struct NativeFunction {
    pub name: UnsafeString,
    pub function: fn(&[Value]) -> Result<Value, CallError>
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for NativeFunction {}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native function {}>", self.name)
    }
}

impl NativeFunction {
    pub unsafe fn free(&self) {
        self.name.free();
    }

    pub fn call(&self, args: &[Value]) -> Result<Value, CallError> {
        (self.function)(args)
    }
}
