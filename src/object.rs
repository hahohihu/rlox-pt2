use std::{fmt::Display, ptr::NonNull};

#[repr(transparent)]
#[derive(Copy, Clone, Debug)]
pub struct Object {
    object: NonNull<ObjectInner>,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.object.as_ref() == other.object.as_ref() }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { self.object.as_ref().fmt(f) }
    }
}

impl Object {
    pub fn typename(&self) -> &'static str {
        unsafe { self.object.as_ref().typename() }
    }

    pub fn make_str(value: &str) -> Object {
        let object = ObjectInner::from(value);
        let object = Box::leak(Box::new(object));
        unsafe {
            Object {
                object: NonNull::new_unchecked(object as *mut _),
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ObjectInner {
    String { str: NonNull<str> },
}

impl PartialEq for ObjectInner {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ObjectInner::String { str: a }, ObjectInner::String { str: b }) => {
                unsafe {
                    // SAFETY: These are always valid, and only take a shared reference
                    a.as_ref() == b.as_ref()
                }
            }
        }
    }
}

impl Display for ObjectInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String { str } => unsafe { write!(f, "\"{}\"", str.as_ref()) },
        }
    }
}

impl From<&str> for ObjectInner {
    fn from(value: &str) -> Self {
        let boxed = value.to_string().into_boxed_str();
        let str = unsafe { NonNull::new_unchecked(Box::leak(boxed) as *mut _) };
        ObjectInner::String { str }
    }
}

impl ObjectInner {
    pub fn typename(&self) -> &'static str {
        match self {
            Self::String { .. } => "string",
        }
    }
}
