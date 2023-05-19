use vm::{interpret, InterpretError};

mod chunk;
mod lex;
mod parse;
mod ui;
mod value;
mod vm;
mod object;

fn main() -> Result<(), InterpretError> {
    tracing_subscriber::fmt::init();
    interpret(r#""a" == "a""#)
}
