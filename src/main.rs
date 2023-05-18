use vm::interpret;

mod lex;
mod parse;
mod ui;
mod chunk;
mod value;
mod vm;

fn main() {
    interpret("");
}
