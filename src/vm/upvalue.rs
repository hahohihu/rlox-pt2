use std::fmt::Display;

use crate::value::{valid::ValidPtr, Value};

#[derive(Copy, Clone, Debug)]
pub struct Upvalue {
    pub value: ValidPtr<Value>,
    pub closed: Value,
    pub marked: bool,
    pub next_open: Option<ValidPtr<Upvalue>>,
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
