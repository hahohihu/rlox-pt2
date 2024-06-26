use crate::bytecode::chunk::Chunk;
use std::io::Write;

mod codegen;
pub mod parse;

pub use parse::parse;

pub fn compile(source: &str, mut stderr: impl Write) -> Option<Chunk> {
    let ast = parse::parse(source, &mut stderr)?;
    codegen::generate(source, stderr, &ast).ok()
}
