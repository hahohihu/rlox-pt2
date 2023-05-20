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

    fn from_inner(inner: ObjectInner) -> Object {
        let object = Box::leak(Box::new(inner));
        unsafe {
            Object {
                object: NonNull::new_unchecked(object as *mut _),
            }
        }
    }

    pub fn make_str(value: String) -> Object {
        let str = ObjectInner::from(value);
        Self::from_inner(str)
    }

    pub fn is_string(&self) -> bool {
        let inner = unsafe { self.object.as_ref() };
        matches!(inner, ObjectInner::String { .. })
    }

    pub fn concatenate(&self, other: &Self) -> Self {
        let (lhs, rhs) = unsafe { (self.object.as_ref(), other.object.as_ref()) };
        let (ObjectInner::String { str: lhs }, ObjectInner::String {str: rhs}) = (lhs, rhs) else {
            unreachable!("TODO: This is scuffed, but it's a slight defensive measure");
        };
        Object::make_str(unsafe { String::from(lhs.as_ref()) + rhs.as_ref() })
    }
}

#[non_exhaustive]
#[derive(Copy, Clone, Debug)]
pub enum ObjectInner {
    // ! If mutability is ever added, many of these "as_ref" may become suspicious (as far as a safe API goes)
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

impl From<String> for ObjectInner {
    fn from(value: String) -> Self {
        let boxed = value.into_boxed_str();
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
