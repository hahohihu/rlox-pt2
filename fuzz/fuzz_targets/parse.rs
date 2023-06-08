#![no_main]

use libfuzzer_sys::fuzz_target;
use rlox::compiler::parse::parse;

fuzz_target!(|data: &str| {
    let mut stderr = vec![];
    let _ = parse(data, &mut stderr);
});
