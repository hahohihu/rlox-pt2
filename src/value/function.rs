use std::fmt::Display;

use crate::vm::upvalue::Upvalue;

use super::{string::UnsafeString, valid::ValidPtr};

#[derive(Copy, Clone, Debug, Eq)]
pub struct ObjFunction {
    pub arity: u8,
    pub upvalues: u8,
    pub addr: usize,
    pub name: UnsafeString,
}

impl Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function {} @ {}>", self.name, self.addr)
    }
}

impl PartialEq for ObjFunction {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr
    }
}

impl ObjFunction {
    pub unsafe fn free(&self) {
        self.name.free();
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ObjClosure {
    pub function: ObjFunction,
    pub upvalues: ValidPtr<[ValidPtr<Upvalue>]>,
}

impl PartialEq for ObjClosure {
    fn eq(&self, other: &Self) -> bool {
        self.function == other.function && self.upvalues.as_ptr() == other.upvalues.as_ptr()
    }
}

impl Eq for ObjClosure {}

impl ObjClosure {
    pub unsafe fn free(&self) {
        ValidPtr::free(self.upvalues);
    }

    pub fn mark(&self) {
        for upvalue in &*self.upvalues {
            unsafe {
                (*upvalue.as_ptr()).marked = true;
            }
            upvalue.mark();
        }
    }
}
