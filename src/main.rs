#![allow(clippy::redundant_pattern_matching)]
use std::{
    env::args,
    fs::File,
    io::{stderr, Read},
    process::ExitCode,
};

use vm::interpret;

mod chunk;
mod lex;
mod object;
mod parse;
mod ui;
mod util;
mod valid;
mod value;
mod vm;

fn read_file(filename: &str) -> std::io::Result<String> {
    let mut file = File::open(filename)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    Ok(source)
}

fn main() -> ExitCode {
    tracing_subscriber::fmt::init();
    let mut args = args();
    args.next();
    let Some(filename) = args.next() else {
        eprintln!("Usage: rlox <filename>");
        return ExitCode::FAILURE;
    };
    let source = match read_file(&filename) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Failed to read {filename} with error: {:?}", e);
            return ExitCode::FAILURE;
        }
    };
    match interpret(&source, stderr()) {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}
