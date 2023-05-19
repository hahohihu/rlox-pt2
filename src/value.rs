use std::fmt::Display;

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub enum Value {
    Num(f64),
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => n.fmt(f),
            Self::Bool(b) => b.fmt(f),
        }
    }
}

impl Value {
    pub fn typename(&self) -> &'static str {
        match self {
            Value::Bool(_) => "boolean",
            Value::Num(_) => "number",
        }
    }
}