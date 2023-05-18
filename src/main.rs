use vm::interpret;

mod chunk;
mod lex;
mod parse;
mod ui;
mod value;
mod vm;

fn main() {
    interpret("(42 + 2 * (2 + 2)) / 4");
}
