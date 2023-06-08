#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    let mut stderr = vec![];
    let mut stdout = vec![];
    rlox::vm::interpret(data, &mut stdout, &mut stderr);
});
