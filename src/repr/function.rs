use std::fmt::Display;

use super::string::UnsafeString;

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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ObjClosure {
    pub function: ObjFunction,
}

impl ObjClosure {
    pub unsafe fn free(&self) {}
}
