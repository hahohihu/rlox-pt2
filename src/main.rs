use vm::{interpret, InterpretError};

mod chunk;
mod lex;
mod object;
mod parse;
mod ui;
mod value;
mod vm;

fn main() -> Result<(), InterpretError> {
    tracing_subscriber::fmt::init();
    interpret(r#""a" + "a" == "aa""#)
}
