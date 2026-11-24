// Copyright (c) 2015-2024 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

mod arby;

macro_rules! pyobj {
    (n=None)     => { Value::None };
    (b=True)     => { Value::Bool(true) };
    (b=False)    => { Value::Bool(false) };
    (i=$i:expr)  => { Value::I64($i) };
    (ii=$i:expr) => { Value::Int($i.clone()) };
    (f=$f:expr)  => { Value::F64($f) };
    (bb=$b:expr) => { Value::Bytes($b.to_vec()) };
    (s=$s:expr)  => { Value::String($s.into()) };
    (t=($($m:ident=$v:tt),*))  => { Value::Tuple(vec![$(pyobj!($m=$v)),*]) };
    (l=[$($m:ident=$v:tt),*])  => { Value::List(vec![$(pyobj!($m=$v)),*]) };
    (ss=($($m:ident=$v:tt),*)) => { Value::Set(BTreeSet::from_iter(vec![$(hpyobj!($m=$v)),*])) };
    (fs=($($m:ident=$v:tt),*)) => { Value::FrozenSet(BTreeSet::from_iter(vec![$(hpyobj!($m=$v)),*])) };
    (d={$($km:ident=$kv:tt => $vm:ident=$vv:tt),*}) => {
        Value::Dict(BTreeMap::from_iter(vec![$((hpyobj!($km=$kv),
                                                pyobj!($vm=$vv))),*])) };
}

macro_rules! hpyobj {
    (n=None)     => { HashableValue::None };
    (b=True)     => { HashableValue::Bool(true) };
    (b=False)    => { HashableValue::Bool(false) };
    (i=$i:expr)  => { HashableValue::I64($i) };
    (ii=$i:expr) => { HashableValue::Int($i.clone()) };
    (f=$f:expr)  => { HashableValue::F64($f) };
    (bb=$b:expr) => { HashableValue::Bytes($b.to_vec()) };
    (s=$s:expr)  => { HashableValue::String($s.into()) };
    (t=($($m:ident=$v:tt),*))  => { HashableValue::Tuple(vec![$(hpyobj!($m=$v)),*]) };
    (fs=($($m:ident=$v:tt),*)) => { HashableValue::FrozenSet(BTreeSet::from_iter(vec![$(hpyobj!($m=$v)),*])) };
}

mod struct_tests {
    use crate::{
        from_slice, from_value, to_value, to_vec, value_from_slice, value_to_vec, HashableValue, SerOptions,
        Value,
    };
    use serde::{de, ser};
    use serde_derive::{Deserialize, Serialize};
    use std::collections::BTreeMap;
    use std::fmt;
    use std::iter::FromIterator;

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

    fn test_encode_ok<T>(value: T, target: Value)
    where
        T: PartialEq + ser::Serialize,
    {
        // Test serialization via pickle.
        let vec = to_vec(&value, Default::default()).unwrap();
        let py_val: Value = value_from_slice(&vec, Default::default()).unwrap();
        assert_eq!(py_val, target);
        // Test direct serialization to Value.
        let py_val: Value = to_value(&value).unwrap();
        assert_eq!(py_val, target);
    }

    fn test_encode_ok_with_opt<T>(value: T, target: Value, options: SerOptions)
    where
        T: PartialEq + ser::Serialize,
    {
        let vec = to_vec(&value, options).unwrap();
        let py_val: Value = value_from_slice(&vec, Default::default()).unwrap();
        assert_eq!(py_val, target);
    }

    fn test_decode_ok<'de, T>(pyvalue: Value, target: T)
    where
        T: PartialEq + fmt::Debug + de::Deserialize<'de>,
    {
        // Test deserialization from pickle.
        let vec = value_to_vec(&pyvalue, Default::default()).unwrap();
        let val: T = from_slice(&vec, Default::default()).unwrap();
        assert_eq!(val, target);
        // Test direct deserialization from Value.
        let val: T = from_value(pyvalue).unwrap();
        assert_eq!(val, target);
    }

    #[test]
    fn encode_types() {
        test_encode_ok((), pyobj!(n = None));
        test_encode_ok(true, pyobj!(b = True));
        test_encode_ok(None::<i32>, pyobj!(n = None));
        test_encode_ok(Some(false), pyobj!(b = False));
        test_encode_ok(10000000000_i64, pyobj!(i = 10000000000));
        test_encode_ok(4.5_f64, pyobj!(f = 4.5));
        test_encode_ok('ä', pyobj!(s = "ä"));
        test_encode_ok("string", pyobj!(s = "string"));
        // serde doesn't encode into bytes...
        test_encode_ok(&b"\x00\x01"[..], pyobj!(l = [i = 0, i = 1]));
        test_encode_ok(vec![1, 2, 3], pyobj!(l = [i = 1, i = 2, i = 3]));
        test_encode_ok((1, 2, 3), pyobj!(t = (i = 1, i = 2, i = 3)));
        test_encode_ok(&[1, 2, 3][..], pyobj!(l = [i = 1, i = 2, i = 3]));
        // serde 1.0: fixed-size arrays are now tuples...
        test_encode_ok([1, 2, 3], pyobj!(t = (i = 1, i = 2, i = 3)));
        test_encode_ok(BTreeMap::from_iter(vec![(1, 2), (3, 4)]), pyobj!(d={i=1 => i=2, i=3 => i=4}));
    }

    #[test]
    fn encode_struct() {
        test_encode_ok(Unit, pyobj!(n = None));
        test_encode_ok(Newtype(42), pyobj!(i = 42));
        test_encode_ok(Tuple(42, false), pyobj!(t = (i = 42, b = False)));
        test_encode_ok(
            Inner { a: (), b: 32, c: vec!["doc".into()] },
            pyobj!(d={s="a" => n=None, s="b" => i=32,
                                 s="c" => l=[s="doc"]}),
        );
    }

    #[test]
    fn encode_enum() {
        test_encode_ok(Animal::Dog, pyobj!(s = "Dog"));
        test_encode_ok(
            Animal::AntHive(vec!["ant".into(), "aunt".into()]),
            pyobj!(d={s="AntHive" => l=[s="ant", s="aunt"]}),
        );
        test_encode_ok(
            Animal::Frog("Henry".into(), vec![1, 5]),
            pyobj!(d={s="Frog" => l=[s="Henry", l=[i=1, i=5]]}),
        );
        test_encode_ok(
            Animal::Cat { age: 5, name: "Molyneux".into() },
            pyobj!(d={s="Cat" => d={s="age" => i=5, s="name" => s="Molyneux"}}),
        );
    }

    #[test]
    fn encode_enum_compat() {
        test_encode_ok_with_opt(Animal::Dog, pyobj!(t = (s = "Dog")), SerOptions::new().compat_enum_repr());
        test_encode_ok_with_opt(
            Animal::AntHive(vec!["ant".into(), "aunt".into()]),
            pyobj!(t = (s = "AntHive", l = [s = "ant", s = "aunt"])),
            SerOptions::new().compat_enum_repr(),
        );
        test_encode_ok_with_opt(
            Animal::Frog("Henry".into(), vec![1, 5]),
            pyobj!(t = (s = "Frog", l = [s = "Henry", l = [i = 1, i = 5]])),
            SerOptions::new().compat_enum_repr(),
        );
        test_encode_ok_with_opt(
            Animal::Cat { age: 5, name: "Molyneux".into() },
            pyobj!(t=(s="Cat", d={s="age" => i=5, s="name" => s="Molyneux"})),
            SerOptions::new().compat_enum_repr(),
        );
    }

    #[test]
    fn decode_types() {
        test_decode_ok(pyobj!(n = None), ());
        test_decode_ok(pyobj!(b = True), true);
        test_decode_ok(pyobj!(b = True), Some(true));
        test_decode_ok::<Option<bool>>(pyobj!(n = None), None);
        test_decode_ok(pyobj!(i = 10000000000), 10000000000_i64);
        test_decode_ok(pyobj!(f = 4.5), 4.5_f64);
        test_decode_ok(pyobj!(s = "ä"), 'ä');
        test_decode_ok(pyobj!(s = "string"), String::from("string"));
        // Vec<u8> doesn't decode from serde bytes...
        test_decode_ok(pyobj!(bb = b"bytes"), String::from("bytes"));
        test_decode_ok(pyobj!(l = [i = 1, i = 2, i = 3]), vec![1, 2, 3]);
        test_decode_ok(pyobj!(t = (i = 1, i = 2, i = 3)), (1, 2, 3));
        test_decode_ok(pyobj!(l = [i = 1, i = 2, i = 3]), [1, 2, 3]);
        test_decode_ok(pyobj!(d={i=1 => i=2, i=3 => i=4}), BTreeMap::from_iter(vec![(1, 2), (3, 4)]));
    }

    #[test]
    fn decode_struct() {
        test_decode_ok(pyobj!(n = None), Unit);
        test_decode_ok(pyobj!(i = 42), Newtype(42));
        test_decode_ok(pyobj!(t = (i = 42, b = False)), Tuple(42, false));
        test_decode_ok(
            pyobj!(d={s="a" => n=None, s="b" => i=32, s="c" => l=[s="doc"]}),
            Inner { a: (), b: 32, c: vec!["doc".into()] },
        );
    }

    #[test]
    fn decode_enum() {
        // tuple representation
        test_decode_ok(pyobj!(t = (s = "Dog")), Animal::Dog);
        test_decode_ok(
            pyobj!(t = (s = "AntHive", l = [s = "ant", s = "aunt"])),
            Animal::AntHive(vec!["ant".into(), "aunt".into()]),
        );
        test_decode_ok(
            pyobj!(t = (s = "Frog", l = [s = "Henry", l = [i = 1, i = 5]])),
            Animal::Frog("Henry".into(), vec![1, 5]),
        );
        test_decode_ok(
            pyobj!(t=(s="Cat", d={s="age" => i=5, s="name" => s="Molyneux"})),
            Animal::Cat { age: 5, name: "Molyneux".into() },
        );
        test_decode_ok(
            pyobj!(l=[t=(s="Dog"), t=(s="Dog"),
                                 t=(s="Cat", d={s="age" => i=5, s="name" => s="?"})]),
            vec![Animal::Dog, Animal::Dog, Animal::Cat { age: 5, name: "?".into() }],
        );

        // string/dict representation
        test_decode_ok(pyobj!(s = "Dog"), Animal::Dog);
        test_decode_ok(
            pyobj!(d={s="AntHive" => l=[s="ant", s="aunt"]}),
            Animal::AntHive(vec!["ant".into(), "aunt".into()]),
        );
        test_decode_ok(
            pyobj!(d={s="Frog" => l=[s="Henry", l=[i=1, i=5]]}),
            Animal::Frog("Henry".into(), vec![1, 5]),
        );
        test_decode_ok(
            pyobj!(d={s="Cat" => d={s="age" => i=5, s="name" => s="Molyneux"}}),
            Animal::Cat { age: 5, name: "Molyneux".into() },
        );
        test_decode_ok(
            pyobj!(l=[s="Dog", s="Dog",
                                 d={s="Cat" => d={s="age" => i=5, s="name" => s="?"}}]),
            vec![Animal::Dog, Animal::Dog, Animal::Cat { age: 5, name: "?".into() }],
        );
    }
}

mod value_tests {
    use crate::error::{Error, ErrorCode};
    use crate::Deserializer;
    use crate::{from_slice, to_vec, value_from_reader, value_from_slice, value_to_vec};
    use crate::{DeOptions, HashableValue, SerOptions, Value};
    use num_bigint::BigInt;
    use quickcheck::{QuickCheck, StdGen};
    use rand::{thread_rng, RngCore};
    use std::collections::{BTreeMap, BTreeSet};
    use std::fs::File;
    use std::iter::FromIterator;

    // combinations of (python major, pickle proto) to test
    const TEST_CASES: &[(u32, u32)] = &[
        (2, 0),
        (2, 1),
        (2, 2),
        (3, 0),
        (3, 1),
        (3, 2),
        (3, 3),
        (3, 4),
        (3, 5),
    ];

    fn get_test_object(pyver: u32) -> Value {
        // Reproduces the test_object from test/data/generate.py.
        let longish = BigInt::from(10000000000u64) * BigInt::from(10000000000u64);
        let mut obj = pyobj!(d={
            n=None           => n=None,
            b=False          => t=(b=False, b=True),
            i=1000           => i=100000,
            ii=longish       => ii=longish,
            f=1.0            => f=1.0,
            bb=b"bytes"      => bb=b"bytes",
            s="string"       => s="string",
            fs=(i=0, i=42)   => fs=(i=0, i=42),
            t=(i=1, i=2)     => t=(i=1, i=2, i=3),
            t=()             => l=[
                l=[i=1, i=2, i=3],
                ss=(i=0, i=42),
                d={},
                bb=b"\x00\x55\xaa\xff"
            ]
        });
        // Unfortunately, __dict__ keys are strings and so are pickled
        // differently depending on major version.
        match &mut obj {
            Value::Dict(map) => {
                if pyver == 2 {
                    map.insert(hpyobj!(i = 7), pyobj!(d={bb=b"attr" => i=5}));
                } else {
                    map.insert(hpyobj!(i = 7), pyobj!(d={s="attr" => i=5}));
                    map.insert(hpyobj!(i = 8), pyobj!(t = (s = "abc", i = 10)));
                    map.insert(hpyobj!(i = 9), pyobj!(d={s="type" => s="abcd", s="quantity" => i=100}));
                    map.insert(hpyobj!(i = 42), pyobj!(t = (i = 30)));
                    map.insert(hpyobj!(i = 43), pyobj!(t = (i = 20)));
                }
            }
            _ => unreachable!(),
        }
        obj
    }

    #[test]
    fn unpickle_all() {
        for &(major, proto) in TEST_CASES {
            let file = File::open(format!("test/data/tests_py{}_proto{}.pickle", major, proto)).unwrap();
            let comparison = get_test_object(major);
            let unpickled = value_from_reader(file, DeOptions::default().keep_restore_state()).unwrap();
            assert_eq!(unpickled, comparison, "py {}, proto {}", major, proto);
        }
    }

    #[test]
    fn roundtrip() {
        let dict = get_test_object(2);
        let vec: Vec<_> = value_to_vec(&dict, Default::default()).unwrap();
        let tripped = value_from_slice(&vec, Default::default()).unwrap();
        assert_eq!(dict, tripped);
    }

    #[test]
    fn recursive() {
        for proto in &[0, 1, 2, 3, 4, 5] {
            let file = File::open(format!("test/data/test_recursive_proto{}.pickle", proto)).unwrap();
            match value_from_reader(file, Default::default()) {
                Err(Error::Syntax(ErrorCode::Recursive)) => {}
                _ => panic!("wrong/no error returned for recursive structure"),
            }

            let replace_opt = DeOptions::default().replace_recursive_structures();
            let file = File::open(format!("test/data/test_recursive_proto{}.pickle", proto)).unwrap();
            assert_eq!(
                value_from_reader(file, replace_opt).unwrap(),
                Value::List(vec![Value::Tuple(vec![Value::List(vec![Value::None])])])
            );
        }
    }

    #[test]
    fn fuzzing() {
        // Tries to ensure that we don't panic when encountering strange streams.
        for _ in 0..1000 {
            let mut stream = [0u8; 1000];
            thread_rng().fill_bytes(&mut stream);
            if *stream.last().unwrap() == b'.' {
                continue;
            }
            // These must all fail with an error, since we skip the check if the
            // last byte is a STOP opcode.
            assert!(value_from_slice(&stream, Default::default()).is_err());
        }
    }

    #[test]
    fn qc_roundtrip() {
        fn roundtrip(original: Value) {
            let vec: Vec<_> = value_to_vec(&original, Default::default()).unwrap();
            let tripped = value_from_slice(&vec, Default::default()).unwrap();
            assert_eq!(original, tripped);
        }
        QuickCheck::new()
            .gen(StdGen::new(thread_rng(), 10))
            .tests(5000)
            .quickcheck(roundtrip as fn(_));
    }

    #[test]
    fn roundtrip_json() {
        let original: serde_json::Value = serde_json::from_str(
            r#"[
            {"null": null,
             "false": false,
             "true": true,
             "int": -1238571,
             "float": 1.5e10,
             "list": [false, 5, "true", 3.8]
            }
        ]"#,
        )
        .unwrap();
        let vec: Vec<_> = to_vec(&original, Default::default()).unwrap();
        let tripped: serde_json::Value = from_slice(&vec, Default::default()).unwrap();
        assert_eq!(original, tripped);
    }

    #[test]
    fn bytestring_v2_py3_roundtrip() {
        let original = Value::Bytes(b"123\xff\xfe".to_vec());
        let vec: Vec<_> = value_to_vec(&original, SerOptions::new().proto_v2()).unwrap();
        // Python 3 default deserializer attempts to decode strings
        let mut de = Deserializer::new(vec.as_slice(), DeOptions::new().decode_strings());
        let tripped: Value = de.deserialize_value().unwrap();
        assert_eq!(original, tripped);
        de.end().unwrap();
    }

    #[test]
    fn unresolvable_global() {
        let data = std::fs::read("test/data/test_unresolvable_global.pickle").unwrap();
        assert!(value_from_slice(&data, Default::default()).is_err());
        let val = value_from_slice(&data, DeOptions::new().replace_unresolved_globals()).unwrap();
        assert_eq!(val, Value::None);
        let serde_val: serde_json::Value =
            from_slice(&data, DeOptions::new().replace_unresolved_globals()).unwrap();
        assert_eq!(serde_val, serde_json::Value::Null);
    }
}

mod depth_tests {
    use crate::error::{Error, ErrorCode};
    use crate::{from_slice, value_from_slice, DeOptions, Deserializer, Result, Value};
    use serde::de::{Deserialize as _, IgnoredAny};
    use serde_derive::Deserialize;
    use std::fmt::Debug;

    // The default limit.
    const LIMIT: usize = 128;

    #[derive(Debug, PartialEq, Deserialize)]
    enum E {
        Leaf,
        Node(Box<E>),
    }

    // E::Node nested n times around E::Leaf.
    fn nodes(n: usize) -> E {
        let mut e = E::Leaf;
        for _ in 0..n {
            e = E::Node(Box::new(e));
        }
        e
    }

    // Nesting depth of a decoded value.
    fn depth(value: &Value) -> usize {
        match value {
            Value::List(items) | Value::Tuple(items) => 1 + items.iter().map(depth).max().unwrap_or(0),
            Value::Dict(map) => 1 + map.values().map(depth).max().unwrap_or(0),
            _ => 0,
        }
    }

    fn assert_parse_error<T: Debug>(result: Result<T>, what: &str) {
        match result {
            Err(Error::Eval(ErrorCode::NestingTooDeep, _)) => {}
            other => panic!("{}: expected NestingTooDeep while parsing, got {:?}", what, other),
        }
    }

    fn assert_convert_error<T: Debug>(result: Result<T>, what: &str) {
        match result {
            Err(Error::Syntax(ErrorCode::NestingTooDeep)) => {}
            other => panic!("{}: expected NestingTooDeep while converting, got {:?}", what, other),
        }
    }

    // Containers nested n deep: `before` and `after` wrap the innermost
    // container once per further level.
    fn nest(n: usize, before: &[u8], inner: &[u8], after: &[u8]) -> Vec<u8> {
        let mut buf = b"\x80\x04".to_vec();
        buf.extend(before.repeat(n - 1));
        buf.extend(inner);
        buf.extend(after.repeat(n - 1));
        buf.push(b'.');
        buf
    }

    // A name, and the `before`, `inner` and `after` arguments of nest().
    type Shape = (&'static str, &'static [u8], &'static [u8], &'static [u8]);

    // How each opcode that builds or fills a container nests inline.
    const INLINE: &[Shape] = &[
        ("TUPLE1", b"", b")", b"\x85"),
        ("TUPLE2, 1st item", b"", b")", b"N\x86"),
        ("TUPLE2, 2nd item", b"N", b")", b"\x86"),
        ("TUPLE3, 1st item", b"", b")", b"NN\x87"),
        ("TUPLE3, 2nd item", b"N", b")", b"N\x87"),
        ("TUPLE3, 3rd item", b"NN", b")", b"\x87"),
        ("TUPLE", b"(", b")", b"t"),
        ("LIST", b"(", b"]", b"l"),
        ("APPEND", b"]", b"]", b"a"),
        ("APPENDS", b"](", b"]", b"e"),
        ("DICT", b"(K\x00", b"}", b"d"),
        ("SETITEM", b"}K\x00", b"}", b"s"),
        ("SETITEMS", b"}(K\x00", b"}", b"u"),
        ("FROZENSET", b"(", b")", b"\x91"),
        ("ADDITEMS", b"\x8f(", b")", b"\x90"),
        ("INST", b"(", b")", b"i__main__\nC\n"),
        ("OBJ", b"N(", b")", b"o"),
        ("NEWOBJ_EX", b"c__main__\nC\n", b")", b"}\x92"),
        ("NEWOBJ", b"c__main__\nC\n", b")", b"\x85\x81"),
        ("BUILD", b"N", b")", b"\x85b"),
        ("DUP", b"", b")", b"2\x85"),
    ];

    // Nesting with every container memoized, as Python 3 does it.
    const MEMOIZED: &[Shape] = &[
        ("lists", b"]\x94", b"]\x94", b"a"),
        ("tuples", b"", b")", b"\x85\x94"),
        ("dicts", b"}\x94K\x00", b"}\x94", b"s"),
        ("frozensets", b"(", b"(\x91\x94", b"\x91\x94"),
        ("sets", b"\x8f\x94(", b"\x8f\x94", b"\x90"),
    ];

    // Memoize the stack top under index i, like Python does.
    fn put(buf: &mut Vec<u8>, i: usize) {
        if i < 256 {
            buf.extend(&[b'q', i as u8]);
        } else {
            buf.push(b'r');
            buf.extend(&(i as u32).to_le_bytes());
        }
    }

    // [[[]]] with n lists, as Python pickles it.
    fn memo_lists(n: usize) -> Vec<u8> {
        let mut buf = b"\x80\x02".to_vec();
        for i in 0..n {
            buf.push(b']');
            put(&mut buf, i);
        }
        buf.extend(b"a".repeat(n - 1));
        buf.push(b'.');
        buf
    }

    // {"Node": {"Node": ... "Leaf"}} with n dicts, as Python pickles it.
    fn memo_enum_dicts(n: usize) -> Vec<u8> {
        let mut buf = b"\x80\x02}q\x00X\x04\x00\x00\x00Nodeq\x01".to_vec();
        for i in 1..n {
            buf.push(b'}');
            put(&mut buf, i + 1);
            buf.extend(b"h\x01");
        }
        buf.extend(b"X\x04\x00\x00\x00Leaf");
        put(&mut buf, n + 1);
        buf.extend(b"s".repeat(n));
        buf.push(b'.');
        buf
    }

    // ("Node", ("Node", ... "Leaf")) with n tuples, as Python pickles it.
    fn memo_enum_tuples(n: usize) -> Vec<u8> {
        let mut buf = b"\x80\x02X\x04\x00\x00\x00Nodeq\x00".to_vec();
        buf.extend(b"h\x00".repeat(n - 1));
        buf.extend(b"X\x04\x00\x00\x00Leafq\x01");
        for i in 0..n {
            buf.push(b'\x86');
            put(&mut buf, i + 2);
        }
        buf.push(b'.');
        buf
    }

    // "Node" n times, then "Leaf", each as a pickle of its own: in this form,
    // the data of a variant is read from the next pickle in the stream.
    fn enum_stream(n: usize) -> Vec<u8> {
        let mut buf = b"\x8c\x04Node.".repeat(n);
        buf.extend(b"\x8c\x04Leaf.");
        buf
    }

    // The ways to nest E: a name, and a function taking the number of levels.
    type Enums = &'static [(&'static str, fn(usize) -> Vec<u8>)];
    const ENUMS: Enums = &[
        ("dicts", memo_enum_dicts),
        ("tuples", memo_enum_tuples),
        ("stream", enum_stream),
    ];

    // A set, frozenset or list of n - 1 nested tuples, reduced like
    // value_to_vec writes sets: REDUCE gets the list of items in a tuple, one
    // level deeper than the result.
    fn reduced_set(n: usize, name: &str) -> Vec<u8> {
        let mut buf = b"\x80\x03cbuiltins\n".to_vec();
        buf.extend(name.as_bytes());
        buf.extend(b"\n]()");
        buf.extend(b"\x85".repeat(n - 2));
        buf.extend(b"e\x85R.");
        buf
    }

    // Instances of an unknown class, each reduced from a 1-tuple holding the
    // memoized previous one; with keep_restore_state, n tuples nest once
    // REDUCE resolves the memo references.
    fn memo_reduces(n: usize) -> Vec<u8> {
        let mut buf = b"\x80\x02)q\x00".to_vec();
        for i in 1..n {
            buf.extend(b"c__main__\nC\nh");
            buf.push((i - 1) as u8);
            buf.extend(b"\x85R");
            put(&mut buf, i);
        }
        buf.push(b'.');
        buf
    }

    #[test]
    fn inline_nesting() {
        let options = DeOptions::new().keep_restore_state();
        for &(name, before, inner, after) in INLINE {
            let result = from_slice::<IgnoredAny>(&nest(LIMIT, before, inner, after), options.clone());
            assert!(result.is_ok(), "{}: {:?}", name, result);
            // Parsing allows one more level (see reduce_nesting), not two.
            let result = from_slice::<IgnoredAny>(&nest(LIMIT + 1, before, inner, after), options.clone());
            assert_convert_error(result, name);
            let result = from_slice::<IgnoredAny>(&nest(LIMIT + 2, before, inner, after), options.clone());
            assert_parse_error(result, name);
        }
        let value = value_from_slice(&nest(LIMIT, b"", b")", b"\x85"), DeOptions::new()).unwrap();
        assert_eq!(depth(&value), LIMIT);
        let pickle = nest(LIMIT + 1, b"", b")", b"\x85");
        assert_convert_error(value_from_slice(&pickle, DeOptions::new()), "tuples");
        let pickle = nest(LIMIT + 2, b"", b")", b"\x85");
        let err = value_from_slice(&pickle, DeOptions::new()).unwrap_err();
        assert_eq!(err.to_string(), "eval error at offset 132: nesting too deep");
    }

    #[test]
    fn memoized_nesting() {
        let pickle = memo_lists(LIMIT);
        assert_eq!(depth(&value_from_slice(&pickle, DeOptions::new()).unwrap()), LIMIT);
        from_slice::<serde_json::Value>(&pickle, DeOptions::new()).unwrap();
        from_slice::<IgnoredAny>(&pickle, DeOptions::new()).unwrap();
        let pickle = memo_lists(LIMIT + 1);
        assert_convert_error(value_from_slice(&pickle, DeOptions::new()), "Value");
        assert_convert_error(from_slice::<serde_json::Value>(&pickle, DeOptions::new()), "json");
        assert_convert_error(from_slice::<IgnoredAny>(&pickle, DeOptions::new()), "IgnoredAny");

        for &(name, before, inner, after) in MEMOIZED {
            let result = from_slice::<IgnoredAny>(&nest(LIMIT, before, inner, after), DeOptions::new());
            assert!(result.is_ok(), "{}: {:?}", name, result);
            let pickle = nest(LIMIT + 1, before, inner, after);
            assert_convert_error(value_from_slice(&pickle, DeOptions::new()), name);
            assert_convert_error(from_slice::<IgnoredAny>(&pickle, DeOptions::new()), name);
        }
    }

    #[test]
    fn recursive_enum() {
        for &(name, make) in ENUMS {
            let result = from_slice::<E>(&make(LIMIT), DeOptions::new());
            assert_eq!(result.ok(), Some(nodes(LIMIT)), "{}", name);
            assert_convert_error(from_slice::<E>(&make(LIMIT + 1), DeOptions::new()), name);
        }
    }

    #[test]
    fn restore_state_nesting() {
        let options = DeOptions::new().keep_restore_state();
        let value = value_from_slice(&memo_reduces(LIMIT), options.clone()).unwrap();
        assert_eq!(depth(&value), LIMIT);
        assert_convert_error(value_from_slice(&memo_reduces(LIMIT + 1), options.clone()), "limit + 1");
        assert_parse_error(value_from_slice(&memo_reduces(LIMIT + 2), options), "limit + 2");
    }

    #[test]
    fn reduce_nesting() {
        // The tuple of REDUCE arguments may be one level deeper than the limit.
        for &name in &["set", "frozenset", "list"] {
            from_slice::<IgnoredAny>(&reduced_set(LIMIT, name), DeOptions::new()).unwrap();
            value_from_slice(&reduced_set(LIMIT, name), DeOptions::new()).unwrap();
            assert_parse_error(value_from_slice(&reduced_set(LIMIT + 1, name), DeOptions::new()), name);
            // The result is as deep as its list, so two more tuples are too deep.
            let mut pickle = reduced_set(LIMIT, name);
            pickle.pop();
            pickle.extend(b"\x85\x85.");
            assert_parse_error(value_from_slice(&pickle, DeOptions::new()), name);
        }
        // A list subclass as Python pickles it with protocol 1 in fast mode;
        // _reconstructor(cls, list, state) gives the state.
        let before = b"ccopy_reg\n_reconstructor\n(c__main__\nL\nc__builtin__\nlist\n]";
        let inner = [&before[..], b"tR"].concat();
        let options = DeOptions::new().keep_restore_state();
        let value = value_from_slice(&nest(LIMIT, before, &inner, b"atR"), options.clone()).unwrap();
        assert_eq!(depth(&value), LIMIT);
        let result = value_from_slice(&nest(LIMIT + 1, before, &inner, b"atR"), options);
        assert_parse_error(result, "list subclass");
    }

    #[test]
    fn very_deep() {
        let n = 100_000;
        let tuples = nest(n, b"", b")", b"\x85");
        let lists = nest(n, b"]", b"]", b"a");
        let dicts = nest(n, b"}K\x00", b"}", b"s");
        for &(name, pickle) in &[("tuples", &tuples), ("lists", &lists), ("dicts", &dicts)] {
            assert_parse_error(value_from_slice(pickle, DeOptions::new()), name);
        }
        // Endings that drop (no STOP), print (APPEND to a tuple) or clone
        // (DUP) the deep value.
        for &end in &[&b""[..], b"Na", b"2N."] {
            let mut pickle = tuples.clone();
            pickle.pop();
            pickle.extend(end);
            assert_parse_error(value_from_slice(&pickle, DeOptions::new()), "tuples");
        }
        let pickle = memo_lists(n);
        assert_convert_error(value_from_slice(&pickle, DeOptions::new()), "Value");
        assert_convert_error(from_slice::<serde_json::Value>(&pickle, DeOptions::new()), "json");
        assert_convert_error(from_slice::<IgnoredAny>(&pickle, DeOptions::new()), "IgnoredAny");
        for &(name, make) in ENUMS {
            assert_convert_error(from_slice::<E>(&make(n), DeOptions::new()), name);
        }
    }

    #[test]
    fn wide() {
        // 1000 lists [{"Node": "Leaf"}] in a list: siblings don't add up.
        let mut pickle = b"\x80\x02]".to_vec();
        for _ in 0..1000 {
            pickle.extend(b"]}X\x04\x00\x00\x00NodeX\x04\x00\x00\x00Leafsaa");
        }
        pickle.push(b'.');
        value_from_slice(&pickle, DeOptions::new()).unwrap();
        from_slice::<serde_json::Value>(&pickle, DeOptions::new()).unwrap();
        from_slice::<IgnoredAny>(&pickle, DeOptions::new()).unwrap();
        assert_eq!(from_slice::<Vec<Vec<E>>>(&pickle, DeOptions::new()).unwrap().len(), 1000);
    }

    #[test]
    fn error_paths() {
        // The deepest value the parser keeps is one level over the limit:
        // drop it (no STOP), print it (APPEND to a tuple) and clone it (DUP).
        let mut pickle = nest(LIMIT + 1, b"", b")", b"\x85");
        pickle.pop();
        match value_from_slice(&pickle, DeOptions::new()) {
            Err(Error::Eval(ErrorCode::EOFWhileParsing, _)) => {}
            other => panic!("expected EOF error, got {:?}", other),
        }
        let append = [&pickle[..], b"Na."].concat();
        let expected = format!("{}{}", "Tuple([".repeat(LIMIT + 1), "])".repeat(LIMIT + 1));
        match value_from_slice(&append, DeOptions::new()) {
            Err(Error::Eval(ErrorCode::InvalidStackTop("list", got), _)) => assert_eq!(got, expected),
            other => panic!("expected stack error, got {:?}", other),
        }
        let dup = [&pickle[..], b"2N."].concat();
        assert_eq!(value_from_slice(&dup, DeOptions::new()).unwrap(), Value::None);
    }

    #[test]
    fn max_depth_option() {
        let options = DeOptions::new().max_depth(10);
        value_from_slice(&nest(10, b"", b")", b"\x85"), options.clone()).unwrap();
        assert_convert_error(value_from_slice(&nest(11, b"", b")", b"\x85"), options.clone()), "tuples");
        assert_parse_error(value_from_slice(&nest(12, b"", b")", b"\x85"), options.clone()), "tuples");
        from_slice::<IgnoredAny>(&memo_lists(10), options.clone()).unwrap();
        assert_convert_error(from_slice::<IgnoredAny>(&memo_lists(11), options.clone()), "lists");
        from_slice::<E>(&memo_enum_dicts(10), options.clone()).unwrap();
        assert_convert_error(from_slice::<E>(&memo_enum_dicts(11), options), "enum");

        let options = DeOptions::new().max_depth(150);
        value_from_slice(&nest(150, b"]", b"]", b"a"), options.clone()).unwrap();
        assert_convert_error(value_from_slice(&nest(151, b"]", b"]", b"a"), options.clone()), "lists");
        value_from_slice(&memo_lists(150), options.clone()).unwrap();
        assert_convert_error(value_from_slice(&memo_lists(151), options), "memoized lists");

        // Zero only allows scalars, not even empty containers or instances.
        let options = DeOptions::new().max_depth(0);
        assert_eq!(value_from_slice(b"N.", options.clone()).unwrap(), Value::None);
        for &pickle in &[&b")."[..], b"].", b"}.", b"\x8f.", b"(i__main__\nC\n."] {
            assert_convert_error(value_from_slice(pickle, options.clone()), "empty container");
        }
        // Bytes as value_to_vec writes them for protocol 2, with REDUCE.
        let pickle = b"\x80\x02c_codecs\nencode\nX\x03\x00\x00\x00abcX\x06\x00\x00\x00latin1\x86R.";
        assert_eq!(value_from_slice(pickle, options).unwrap(), Value::Bytes(b"abc".to_vec()));
    }

    #[test]
    fn memo_depth() {
        // A memoized list gets a deep tuple appended, then DUP copies it out
        // of the memo, so the copy is that deep too.
        let tuple = nest(LIMIT, b"", b")", b"\x85");
        let pickle = [&b"\x80\x04]\x94"[..], &tuple[2..tuple.len() - 1], b"a2\x85."].concat();
        assert_parse_error(value_from_slice(&pickle, DeOptions::new()), "DUP of memoized list");
        let pickle = [&b"\x80\x04]\x94"[..], &tuple[2..tuple.len() - 1], b"a2."].concat();
        assert_convert_error(value_from_slice(&pickle, DeOptions::new()), "memoized list");
    }

    #[test]
    fn depth_restored_after_error() {
        // The next pickle in a stream decodes after a too deep one.
        let mut stream = memo_lists(LIMIT + 1);
        stream.extend(memo_lists(LIMIT));
        let mut de = Deserializer::new(&stream[..], DeOptions::new());
        assert_convert_error(de.deserialize_value(), "Value");
        de.reset_memo();
        assert_eq!(depth(&de.deserialize_value().unwrap()), LIMIT);

        let mut de = Deserializer::new(&stream[..], DeOptions::new());
        assert_convert_error(IgnoredAny::deserialize(&mut de), "IgnoredAny");
        de.reset_memo();
        IgnoredAny::deserialize(&mut de).unwrap();

        let mut stream = memo_enum_dicts(LIMIT + 1);
        stream.extend(memo_enum_dicts(LIMIT));
        let mut de = Deserializer::new(&stream[..], DeOptions::new());
        assert_convert_error(E::deserialize(&mut de), "enum");
        de.reset_memo();
        assert_eq!(E::deserialize(&mut de).unwrap(), nodes(LIMIT));
    }
}
