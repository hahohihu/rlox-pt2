use std::fmt::Display;

use super::{valid::ValidPtr, value::Value};

#[derive(Copy, Clone, Debug)]
pub struct ObjUpvalue {
    pub value: ValidPtr<Value>,
    pub next: Option<ValidPtr<ObjUpvalue>>,
}

impl ObjUpvalue {
    pub fn as_ref(&self) -> &Value {
        self.value.as_ref()
    }

    pub fn as_ptr(self) -> *mut Value {
        self.value.as_ptr()
    }

    pub unsafe fn free(self) {
        self.value.free();
    }
}

impl PartialEq for ObjUpvalue {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl Eq for ObjUpvalue {}

impl Display for ObjUpvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<upvalue = {} @ {:?}>",
            unsafe { *self.as_ptr() },
            self.as_ptr()
        )
    }
}
