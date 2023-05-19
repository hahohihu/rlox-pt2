use std::{fmt::{Display}, ptr::NonNull};

use crate::object::{ObjectInner, Object};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Nil,
    Object(Object)
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => n.fmt(f),
            Self::Bool(b) => b.fmt(f),
            Self::Nil => write!(f, "nil"),
            Self::Object(obj) => obj.fmt(f),
        }
    }
}

impl Value {
    pub fn typename(&self) -> &'static str {
        match self {
            Self::Bool(_) => "boolean",
            Self::Num(_) => "number",
            Self::Nil => "nil",
            Self::Object(obj) => obj.typename(),
        }
    }

    pub fn falsey(&self) -> bool {
        matches!(self, Self::Bool(false) | Self::Nil)
    }
}