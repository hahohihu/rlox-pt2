use chunk::{Chunk, OpCode};
use vm::interpret;

mod lex;
mod parse;
mod ui;
mod chunk;
mod value;
mod vm;

fn main() {
    let mut chunk = Chunk::new();
    let source = "42; return;";
    unsafe {
        chunk.add_constant(42.0);
        chunk.write_byte(OpCode::Constant as u8, (0..2).into());
        chunk.write_byte(0, (0..2).into());
        chunk.write_byte(OpCode::Negate as u8, (3..3).into());
        chunk.write_byte(OpCode::Return as u8, (4..10).into());
    }
    interpret(chunk, source);
}
