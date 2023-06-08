#![no_main]

use libfuzzer_sys::fuzz_target;
use rlox::compiler::parse::ast;
use rlox::compiler::parse::parse;

fuzz_target!(|data: ast::FuzzStatements| {
    let mut stderr = vec![];
    parse(&data.to_string(), &mut stderr).unwrap();
});
