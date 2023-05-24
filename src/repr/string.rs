use super::alloc;
use super::valid::ValidPtr;
use std::{fmt::Display, ops::Add};

#[derive(Copy, Clone, Debug)]
pub struct UnsafeString {
    str: ValidPtr<str>,
}

impl Display for UnsafeString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.str.as_ref().fmt(f)
    }
}

impl PartialEq for UnsafeString {
    fn eq(&self, other: &Self) -> bool {
        self.str.as_ref() == other.str.as_ref()
    }
}

impl From<String> for UnsafeString {
    fn from(value: String) -> Self {
        Self {
            str: ValidPtr::from(value.into_boxed_str()),
        }
    }
}

impl Add<UnsafeString> for UnsafeString {
    type Output = UnsafeString;
    fn add(self, rhs: UnsafeString) -> Self::Output {
        let concatenated = String::from(self.str.as_ref()) + rhs.str.as_ref();
        alloc::trace!("Allocating string '{concatenated}'");
        Self::from(concatenated)
    }
}

impl UnsafeString {
    pub unsafe fn free(self) {
        drop(Box::from_raw(self.str.as_ptr()))
    }
}
