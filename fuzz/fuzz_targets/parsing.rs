#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    serde_pickle::value_from_slice(data, Default::default());
});
