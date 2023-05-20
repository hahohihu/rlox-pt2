use std::{fmt::Display};

#[cfg(not(feature = "verbose_allocations"))]
use crate::noop as trace;
#[cfg(feature = "verbose_allocations")]
use tracing::trace;

mod valid {
    use std::ptr::NonNull;

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
}
use valid::*;

#[repr(transparent)]
#[derive(Copy, Clone, Debug)]
pub struct Object {
    object: ValidPtr<ObjectInner>,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.object.as_ref() == other.object.as_ref()
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.object.as_ref().kind.fmt(f)
    }
}

impl Object {
    pub fn typename(&self) -> &'static str {
        self.object.as_ref().kind.typename()
    }

    fn from_inner(kind: ObjectKind) -> Object {
        let object = ValidPtr::from(Box::new(ObjectInner { kind }));
        Object { object }
    }

    pub fn make_str(value: String) -> Object {
        trace!("Allocating string '{value}'");
        let str = ObjectKind::from(value);
        Self::from_inner(str)
    }

    pub fn is_string(&self) -> bool {
        let inner = self.object.as_ref();
        matches!(inner.kind, ObjectKind::String { .. })
    }

    pub fn concatenate_strings(&self, other: &Self) -> Self {
        let (lhs, rhs) = (self.object.as_ref().kind, other.object.as_ref().kind);
        let (ObjectKind::String { str: lhs }, ObjectKind::String {str: rhs}) = (lhs, rhs) else {
            unreachable!("TODO: This is scuffed, but it's a slight defensive measure");
        };
        Object::make_str(String::from(lhs.as_ref()) + rhs.as_ref())
    }

    pub unsafe fn free(&self) {
        trace!("Freeing {self}");
        self.object.as_ref().kind.free();
        drop(Box::from_raw(self.object.as_ptr()));
    }

    pub fn compare_str(&self, s: &str) -> bool {
        let inner = self.object.as_ref();
        matches!(inner.kind, ObjectKind::String { str } if str.as_ref() == s)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct ObjectInner {
    kind: ObjectKind,
}

#[non_exhaustive]
#[derive(Copy, Clone, Debug)]
enum ObjectKind {
    // ! If mutability is ever added, many of these "as_ref" may become suspicious (as far as a safe API goes)
    String { str: ValidPtr<str> },
}

impl PartialEq for ObjectKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ObjectKind::String { str: a }, ObjectKind::String { str: b }) => {
                a.as_ref() == b.as_ref()
            }
        }
    }
}

impl Display for ObjectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String { str } => write!(f, "\"{}\"", str.as_ref()),
        }
    }
}

impl From<String> for ObjectKind {
    fn from(value: String) -> Self {
        let boxed = value.into_boxed_str();
        let str = ValidPtr::from(boxed);
        ObjectKind::String { str }
    }
}

impl ObjectKind {
    fn typename(&self) -> &'static str {
        match self {
            Self::String { .. } => "string",
        }
    }

    unsafe fn free(&self) {
        match self {
            Self::String { str } => unsafe {
                drop(Box::from_raw(str.as_ptr()));
            },
        }
    }
}
