use std::{ops::Deref, ptr::NonNull};

#[repr(transparent)]
#[derive(Debug)]
/// SAFETY: The presiding assumption here is that we basically just got this from Box and it's okay to use,
///     but it needs to be a raw pointer so we can share it and garbage collect efficiently
pub struct ValidPtr<T: ?Sized>(NonNull<T>);

impl<T: ?Sized> Clone for ValidPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: ?Sized> Copy for ValidPtr<T> {}

impl<T: ?Sized> ValidPtr<T> {
    pub fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    pub unsafe fn free(ptr: Self) {
        drop(Box::from_raw(ptr.0.as_ptr()))
    }

    /// ValidPtr frees using Box::from_raw, so this must either come from the same allocation as Box, or must not be freed through ValidPtr
    pub unsafe fn from_ptr(ptr: NonNull<T>) -> Self {
        Self(ptr)
    }
}

impl<T: ?Sized> From<Box<T>> for ValidPtr<T> {
    fn from(value: Box<T>) -> Self {
        unsafe { Self(NonNull::new_unchecked(Box::leak(value) as *mut _)) }
    }
}

impl<T: ?Sized> Deref for ValidPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // If anything goes wrong, the code at fault is either
        // 1. Code using pointers
        // 2. Code that frees these
        // 3. Potentially, code that leaks these past the lifetime of the VMq
        unsafe { self.0.as_ref() }
    }
}
