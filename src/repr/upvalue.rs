use std::fmt::Display;

use super::{valid::ValidPtr, value::Value};

#[derive(Copy, Clone, Debug)]
pub struct ObjUpvalue {
    pub value: ValidPtr<Value>,
    pub next: Option<ValidPtr<ObjUpvalue>>,
}

impl ObjUpvalue {
    pub unsafe fn free(self) {
        ValidPtr::free(self.value);
    }
}

impl PartialEq for ObjUpvalue {
    fn eq(&self, other: &Self) -> bool {
        self.value.as_ptr() == other.value.as_ptr()
    }
}

impl Eq for ObjUpvalue {}

impl Display for ObjUpvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<upvalue = {} @ {:?}>", *self.value, self.value.as_ptr())
    }
}
