use super::valid::ValidPtr;
use crate::common::alloc;
use std::{
    borrow::Borrow,
    fmt::Display,
    hash::{Hash, Hasher},
    ops::Add,
};

#[repr(transparent)]
#[derive(Copy, Clone, Debug)]
pub struct UnsafeString {
    str: ValidPtr<str>,
}

impl Display for UnsafeString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.str.fmt(f)
    }
}

impl PartialEq for UnsafeString {
    fn eq(&self, other: &Self) -> bool {
        *self.str == *other.str
    }
}

impl Eq for UnsafeString {}

impl Hash for UnsafeString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.str.hash(state)
    }
}

impl From<String> for UnsafeString {
    fn from(value: String) -> Self {
        Self {
            str: ValidPtr::from(value.into_boxed_str()),
        }
    }
}

impl From<&str> for UnsafeString {
    fn from(value: &str) -> Self {
        alloc::trace!("Allocating string '{value}'");
        Self::from(String::from(value))
    }
}

impl Add<UnsafeString> for UnsafeString {
    type Output = UnsafeString;
    fn add(self, rhs: UnsafeString) -> Self::Output {
        let concatenated = String::from(&*self.str) + &*rhs.str;
        alloc::trace!("Allocating string '{concatenated}'");
        Self::from(concatenated)
    }
}

impl UnsafeString {
    pub unsafe fn free(self) {
        alloc::trace!("Freeing string '{self}'");
        drop(Box::from_raw(self.str.as_ptr()));
    }
}

impl Borrow<str> for UnsafeString {
    fn borrow(&self) -> &str {
        &self.str
    }
}
