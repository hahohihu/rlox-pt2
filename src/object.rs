use crate::valid::ValidPtr;
use std::fmt::Display;

// debugging
#[cfg(not(feature = "verbose_allocations"))]
use crate::noop as trace;
#[cfg(feature = "verbose_allocations")]
use tracing::trace;

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

impl Object {
    pub fn typename(self) -> &'static str {
        self.inner.as_ref().kind.typename()
    }

    fn from_inner(kind: ObjectKind) -> Object {
        let object = ValidPtr::from(Box::new(ObjectInner { kind }));
        Object { inner: object }
    }

    pub fn make_str(value: String) -> Object {
        trace!("Allocating string '{value}'");
        let str = ObjectKind::from(value);
        Self::from_inner(str)
    }

    pub fn is_string(self) -> bool {
        self.inner.as_ref().kind.is_string()
    }

    pub fn concatenate_strings(lhs: Self, rhs: Self) -> Self {
        Self::from_inner(ObjectKind::concatenate_strings(
            lhs.inner.as_ref().kind,
            rhs.inner.as_ref().kind,
        ))
    }

    pub unsafe fn free(self) {
        trace!("Freeing {self}");
        self.inner.as_ref().kind.free();
        drop(Box::from_raw(self.inner.as_ptr()));
    }

    pub fn compare_str(&self, s: &str) -> bool {
        self.inner.as_ref().kind.compare_str(s)
    }
}

// ==================================================== Internals below here

#[derive(Copy, Clone, Debug, PartialEq)]
struct ObjectInner {
    kind: ObjectKind,
}

#[non_exhaustive]
#[derive(Copy, Clone, Debug)]
enum ObjectKind {
    String { str: ValidPtr<str> },
}

impl PartialEq for ObjectKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ObjectKind::String { str: a }, ObjectKind::String { str: b }) => {
                // TODO: Interning after benchmarking. Felt premature at chapter 20.
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
    fn typename(self) -> &'static str {
        match self {
            Self::String { .. } => "string",
        }
    }

    fn is_string(self) -> bool {
        matches!(self, ObjectKind::String { .. })
    }

    fn concatenate_strings(lhs: Self, rhs: Self) -> ObjectKind {
        let (ObjectKind::String { str: lhs }, ObjectKind::String {str: rhs}) = (lhs, rhs) else {
            unreachable!("TODO: This is scuffed, but it's a slight defensive measure");
        };
        ObjectKind::from(String::from(lhs.as_ref()) + rhs.as_ref())
    }

    fn compare_str(self, other: &str) -> bool {
        matches!(self, Self::String { str } if str.as_ref() == other)
    }

    unsafe fn free(self) {
        match self {
            Self::String { str } => unsafe {
                drop(Box::from_raw(str.as_ptr()));
            },
        }
    }
}
