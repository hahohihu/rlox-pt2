use chunk::{Chunk, OpCode};

mod lex;
mod parse;
mod ui;
mod chunk;

fn main() {
    let mut chunk = Chunk::new();
    unsafe {
        chunk.write_byte(OpCode::Return as u8);
    }
    chunk.disassemble("test");
}
