use vm::{interpret, InterpretError};

mod chunk;
mod lex;
mod parse;
mod ui;
mod value;
mod vm;

fn main() -> Result<(), InterpretError> {
    tracing_subscriber::fmt::init();
    interpret("!!!!nil")
}
