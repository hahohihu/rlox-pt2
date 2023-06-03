use super::{alloc, function::ObjFunction};
use std::fmt::Display;

use super::object::Object;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Nil,
    Object(Object),
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

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Num(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        alloc::trace!("Allocating string '{value}'");
        Self::Object(Object::from(String::from(value)))
    }
}

impl<T: Into<Object>> From<T> for Value {
    fn from(value: T) -> Self {
        Value::Object(value.into())
    }
}

impl TryFrom<Value> for ObjFunction {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::Object(obj) = value {
            obj.try_into()
        } else {
            Err(())
        }
    }
}
