#![no_main]

use libfuzzer_sys::fuzz_target;
use rlox::compiler::parse::ast;
use rlox::compiler::parse::parser::{parse_res, ParseError};

fuzz_target!(|data: ast::FuzzStatements| {
    let mut stderr = vec![];
    match parse_res(&data.to_string(), &mut stderr) {
        Ok(_) => {},
        Err(ParseError::AssignmentDepth { .. }) => {},
        Err(e) => panic!("{e:?}"),
    }
});
