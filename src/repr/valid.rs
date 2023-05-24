use std::ptr::NonNull;

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
    pub fn as_ref(&self) -> &T {
        unsafe {
            // If there are any problems here, it must be that someone used as_ptr and did something unsafe anyways
            self.0.as_ref()
        }
    }

    pub fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }
}

impl<T: ?Sized> From<Box<T>> for ValidPtr<T> {
    fn from(value: Box<T>) -> Self {
        unsafe { Self(NonNull::new_unchecked(Box::leak(value) as *mut _)) }
    }
}
