#![no_main]

use libfuzzer_sys::fuzz_target;
use rlox::compiler::parse::ast;

fuzz_target!(|data: ast::Statements| {
    let mut stderr = vec![];
    let mut stdout = vec![];
    let _ = rlox::vm::interpret(&data.to_string(), &mut stdout, &mut stderr);
});
