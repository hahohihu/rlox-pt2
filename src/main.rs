use chunk::{Chunk, OpCode};

mod lex;
mod parse;
mod ui;
mod chunk;
mod value;

fn main() {
    let mut chunk = Chunk::new();
    unsafe {
        chunk.write_byte(OpCode::Return as u8);
        chunk.add_constant(42.0);
        chunk.write_byte(OpCode::Constant as u8);
        chunk.write_byte(0);
    }
    chunk.disassemble("test");
}
