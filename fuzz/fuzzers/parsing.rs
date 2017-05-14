#![no_main]
extern crate libfuzzer_sys;
extern crate serde_pickle;

#[export_name="rust_fuzzer_test_input"]
pub extern fn go(data: &[u8]) {
    serde_pickle::value_from_slice(data);
}
