// Copyright (c) 2015-2016 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

extern crate rand;
extern crate quickcheck;
extern crate serde_json;

mod arby;

macro_rules! treemap {
    ($($k:expr => $v:expr),*) => {
        {
            let mut m = BTreeMap::new();
            $(m.insert($k, $v);)*
            m
        }
    };
}

mod struct_tests {
    use std::fmt;
    use serde::ser;
    use {to_vec, value_from_slice, Value};

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct Inner {
        a: (),
        b: usize,
        c: Vec<String>,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct Outer {
        inner: Vec<Inner>,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct Unit;

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct Newtype(i32);

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct Tuple(i32, bool);

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    #[serde(deny_unknown_fields)]
    enum Animal {
        Dog,
        AntHive(Vec<String>),
        Frog(String, Vec<isize>),
        Cat { age: usize, name: String },
    }

    fn test_encode_ok<T>(value: T, json: &'static str)
        where T: PartialEq + fmt::Debug + ser::Serialize,
    {
        let vec = to_vec(&value, true).unwrap();
        let val: Value = value_from_slice(&vec).unwrap();
        let ser_fmt = format!("{}", val);
        assert_eq!(ser_fmt, json);
    }

    #[test]
    fn encode_types() {
        test_encode_ok((), "()");
        test_encode_ok(None::<i32>, "None");
        test_encode_ok(Some(false), "False");
        test_encode_ok(4.5_f64, "4.5");
    }

    #[test]
    fn encode_struct() {
        test_encode_ok(Unit,
                       r#"()"#);
        test_encode_ok(Newtype(42),
                       r#"42"#);
        test_encode_ok(Tuple(42, false),
                       r#"(42, False)"#);
        test_encode_ok(Inner { a: (), b: 32, c: vec!["doc".into()] },
                       r#"{"a": (), "b": 32, "c": ["doc"]}"#);
    }

    #[test]
    fn encode_enum() {
        test_encode_ok(Animal::Dog,
                       r#"("Dog", )"#);
        test_encode_ok(Animal::AntHive(vec!["ant".into(), "aunt".into()]),
                       r#"("AntHive", ["ant", "aunt"])"#);
        test_encode_ok(Animal::Frog("Henry".into(), vec![1, 5]),
                       r#"("Frog", ["Henry", [1, 5]])"#);
        test_encode_ok(Animal::Cat { age: 5, name: "Molyneux".into() },
                       r#"("Cat", {"age": 5, "name": "Molyneux"})"#);
    }
}

mod value_tests {
    use std::fs::File;
    use std::collections::{BTreeMap, BTreeSet};
    use std::iter::FromIterator;
    use num_bigint::BigInt;
    use super::rand::{Rng, thread_rng};
    use super::quickcheck::{QuickCheck, StdGen};
    use super::serde_json;
    use {value_from_reader, value_to_vec, value_from_slice, to_vec, from_slice};
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
        let set = BTreeSet::from_iter(vec![HashableValue::I64(42), HashableValue::I64(0)]);
        Value::Dict(treemap!(
            HashableValue::None => Value::None,
            HashableValue::Bool(false) => Value::Tuple(vec![Value::Bool(false),
                                                            Value::Bool(true)]),
            HashableValue::I64(10) => Value::I64(100000),
            HashableValue::Int(longish.clone()) => Value::Int(longish),
            HashableValue::F64(1.0) => Value::F64(1.0),
            HashableValue::Bytes(b"bytes".to_vec()) => Value::Bytes(b"bytes".to_vec()),
            HashableValue::String("string".into()) => Value::String("string".into()),
            HashableValue::FrozenSet(set.clone()) => Value::FrozenSet(set.clone()),
            HashableValue::Tuple(vec![HashableValue::I64(1), HashableValue::I64(2)]) =>
                Value::Tuple(vec![Value::I64(1), Value::I64(2), Value::I64(3)]),
            HashableValue::Tuple(vec![]) =>
                Value::List(vec![
                    Value::List(vec![Value::I64(1), Value::I64(2), Value::I64(3)]),
                    Value::Set(set),
                    Value::Dict(BTreeMap::new())
                ])))
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

    #[test]
    fn qc_roundtrip() {
        fn roundtrip(original: Value) {
            let vec: Vec<_> = value_to_vec(&original, true).unwrap();
            let tripped = value_from_slice(&vec).unwrap();
            assert_eq!(original, tripped);
        }
        QuickCheck::new().gen(StdGen::new(thread_rng(), 10))
                         .tests(10000)
                         .quickcheck(roundtrip as fn(_));
    }

    #[test]
    fn roundtrip_json() {
        let original: serde_json::Value = serde_json::from_str(r#"[
            {"null": null,
             "false": false,
             "true": true,
             "int": -1238571,
             "float": 1.5e10,
             "list": [false, 5, "true", 3.8]
            }
        ]"#).unwrap();
        let vec: Vec<_> = to_vec(&original, true).unwrap();
        let tripped = from_slice(&vec).unwrap();
        assert_eq!(original, tripped);
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
