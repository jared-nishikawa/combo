#![no_main]

extern crate combo;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(text) = std::str::from_utf8(data) {
        let p = combo::parser::list_parser();

        let _ = p(text);
    }
});
