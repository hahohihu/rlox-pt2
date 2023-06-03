use crate::repr::chunk::Chunk;
use std::io::Write;

mod codegen;
mod parse;

pub use parse::parse;

pub fn compile(source: &str, mut output: impl Write) -> Option<Chunk> {
    let ast = match parse::parse(source, &mut output) {
        Ok(ast) => ast,
        Err(e) => {
            e.print(output, source);
            return None;
        }
    };
    codegen::generate(source, output, &ast).ok()
}
