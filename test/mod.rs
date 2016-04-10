// Copyright (c) 2015-2016 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

extern crate rand;

mod value_tests {
    use std::fs::File;
    use std::collections::{BTreeMap, BTreeSet};
    use std::iter::FromIterator;
    use num_bigint::BigInt;
    use super::rand::{Rng, thread_rng};
    use {value_from_reader, value_to_vec, value_from_slice};
    use {Value, HashableValue};
    use error::{Error, ErrorCode};

    // combinations of (python major, pickle proto) to test
    const TEST_CASES: &'static [(u32, u32)] = &[
        (2, 0), (2, 1), (2, 2),
        (3, 0), (3, 1), (3, 2), (3, 3), (3, 4)
    ];

    fn get_test_object() -> Value {
        // Reproduces the test_object from test/data/generate.py.
        let longish = BigInt::from(10000000000u64) * BigInt::from(10000000000u64);
        let mut map = BTreeMap::new();
        map.insert(HashableValue::None, Value::None);
        map.insert(HashableValue::Bool(false), Value::Tuple(Box::new([Value::Bool(false),
                                                                      Value::Bool(true)])));
        map.insert(HashableValue::I64(10), Value::I64(100000));
        map.insert(HashableValue::Int(longish.clone()), Value::Int(longish));
        map.insert(HashableValue::F64(1.0), Value::F64(1.0));
        map.insert(HashableValue::Bytes(b"bytes".to_vec()), Value::Bytes(b"bytes".to_vec()));
        map.insert(HashableValue::String("string".into()), Value::String("string".into()));
        map.insert(HashableValue::Tuple(Box::new([HashableValue::I64(1), HashableValue::I64(2)])),
                   Value::Tuple(Box::new([Value::I64(1), Value::I64(2), Value::I64(3)])));
        let set = BTreeSet::from_iter(vec![HashableValue::I64(42), HashableValue::I64(0)]);
        map.insert(HashableValue::FrozenSet(set.clone()), Value::FrozenSet(set.clone()));
        map.insert(HashableValue::Tuple(Box::new([])),
                   Value::List(vec![
                       Value::List(vec![Value::I64(1), Value::I64(2), Value::I64(3)]),
                       Value::Set(set),
                       Value::Dict(BTreeMap::new())
                   ]));
        Value::Dict(map)
    }

    #[test]
    fn unpickle_all() {
        let comparison = get_test_object();

        for &(major, proto) in TEST_CASES {
            println!("testing: py{} proto{}", major, proto);
            let file = File::open(format!("test/data/tests_py{}_proto{}.pickle", major, proto)).unwrap();
            let unpickled = value_from_reader(file).unwrap();
            assert_eq!(unpickled, comparison);
        }
    }

    #[test]
    fn roundtrip() {
        let dict = get_test_object();
        let vec: Vec<_> = value_to_vec(&dict, true).unwrap();
        let tripped = value_from_slice(&vec).unwrap();
        assert_eq!(dict, tripped);
    }

    #[test]
    fn recursive() {
        for proto in &[0, 1, 2, 3, 4] {
            let file = File::open(format!("test/data/test_recursive_proto{}.pickle", proto)).unwrap();
            match value_from_reader(file) {
                Err(Error::Syntax(ErrorCode::Recursive)) => { }
                _ => assert!(false, "wrong/no error returned for recursive structure")
            }
        }
    }

    #[test]
    fn fuzzing() {
        // Tries to ensure that we don't panic when encountering strange streams.
        for _ in 0..1000 {
            let mut stream = [0u8; 1000];
            thread_rng().fill_bytes(&mut stream);
            if *stream.last().unwrap() == b'.' { continue; }
            // These must all fail with an error, since we skip the check if the
            // last byte is a STOP opcode.
            assert!(value_from_slice(&stream).is_err());
        }
    }
}

#[cfg(test)]
mod benches {
    extern crate test;

    use byteorder::{LittleEndian, WriteBytesExt};
    use self::test::Bencher;
    use value_from_slice;

    #[bench]
    fn unpickle_list(b: &mut Bencher) {
        let mut buffer = b"\x80\x02]q\x00(".to_vec();
        for i in 0..1000 {
            buffer.extend(b"]r");
            buffer.write_u32::<LittleEndian>(i + 1).unwrap();
            buffer.push(b'M');
            buffer.write_u16::<LittleEndian>(i as u16).unwrap();
            buffer.push(b'a');
        }
        buffer.extend(b"e.");
        b.iter(|| value_from_slice(&buffer));
    }

    #[bench]
    fn unpickle_nested_list(b: &mut Bencher) {
        let mut buffer = b"\x80\x02".to_vec();
        for i in 0..1000 {
            buffer.extend(b"]r");
            buffer.write_u32::<LittleEndian>(i).unwrap();
        }
        for _ in 0..1000 {
            buffer.push(b'a');
        }
        buffer.push(b'.');
        b.iter(|| value_from_slice(&buffer));
    }
}
