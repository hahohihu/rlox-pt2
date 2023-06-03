use super::alloc;
use super::function::ObjFunction;
use super::{string::UnsafeString, valid::ValidPtr};
use std::fmt::Display;

#[repr(transparent)]
#[derive(Copy, Clone, Debug)]
pub struct Object {
    inner: ValidPtr<ObjectInner>,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.inner.as_ref() == other.inner.as_ref()
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.as_ref().kind.fmt(f)
    }
}

impl From<ObjectKind> for Object {
    fn from(value: ObjectKind) -> Self {
        Self::from(ObjectInner { kind: value })
    }
}

impl From<ObjectInner> for Object {
    fn from(value: ObjectInner) -> Self {
        Object {
            inner: ValidPtr::from(Box::new(value)),
        }
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Self::from(ObjectKind::from(value))
    }
}

impl From<UnsafeString> for Object {
    fn from(str: UnsafeString) -> Self {
        Self::from(ObjectKind::String { str })
    }
}

impl From<ObjFunction> for Object {
    fn from(fun: ObjFunction) -> Self {
        Self::from(ObjectKind::Function { fun })
    }
}

impl Object {
    pub fn typename(self) -> &'static str {
        self.inner.as_ref().kind.typename()
    }

    pub fn is_string(self) -> bool {
        self.inner.as_ref().kind.is_string()
    }

    pub unsafe fn assume_string(self) -> UnsafeString {
        self.inner.as_ref().kind.assume_string()
    }

    pub unsafe fn free(self) {
        alloc::trace!("Freeing {self}");
        self.inner.as_ref().kind.free();
        self.inner.free();
    }
}

impl TryFrom<Object> for ObjFunction {
    type Error = ();

    fn try_from(value: Object) -> Result<Self, Self::Error> {
        value.inner.as_ref().kind.try_into()
    }
}


// ==================================================== Internals below here

#[derive(Copy, Clone, Debug, PartialEq)]
struct ObjectInner {
    kind: ObjectKind,
}

#[non_exhaustive]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ObjectKind {
    String { str: UnsafeString },
    Function { fun: ObjFunction },
}

impl Display for ObjectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String { str } => str.fmt(f),
            Self::Function { fun } => write!(f, "<function {}>", fun.name),
        }
    }
}

impl From<String> for ObjectKind {
    fn from(value: String) -> Self {
        ObjectKind::String {
            str: UnsafeString::from(value),
        }
    }
}

impl TryFrom<ObjectKind> for ObjFunction {
    type Error = ();

    fn try_from(value: ObjectKind) -> Result<Self, Self::Error> {
        if let ObjectKind::Function { fun } = value {
            Ok(fun)
        } else {
            Err(())
        }
    }
}


impl ObjectKind {
    fn typename(self) -> &'static str {
        match self {
            Self::String { .. } => "string",
            Self::Function { .. } => "function",
        }
    }

    fn is_string(self) -> bool {
        matches!(self, ObjectKind::String { .. })
    }

    unsafe fn assume_string(self) -> UnsafeString {
        match self {
            Self::String { str } => str,
            _ => unreachable!()
        }
    }

    unsafe fn free(self) {
        match self {
            Self::String { str } => str.free(),
            Self::Function { fun } => fun.free()
        }
    }
}
