// Copyright (c) 2015-2016 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

mod value_tests {
    use std::fs::File;
    use std::collections::{BTreeMap, BTreeSet};
    use std::iter::FromIterator;
    use num_bigint::BigInt;
    use {value_from_reader, value_to_vec, value_from_slice};
    use {Value, HashableValue};

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
}
