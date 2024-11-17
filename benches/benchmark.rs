#![cfg(feature = "criterion-bench")]

use byteorder::{LittleEndian, WriteBytesExt};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use serde_pickle;
use serde_pickle::*;
use std::collections::BTreeMap;
use std::io::Read;

// TODO: These macros are redefined from tests
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

fn unpickle_list(c: &mut Criterion) {
    // Creates [[0], [1], [2], ...]
    // Start a list
    let mut buffer = b"\x80\x02]q\x00(".to_vec();
    for i in 0..1000 {
        // Insert an empty list (memoized)
        buffer.extend(b"]r");
        buffer.write_u32::<LittleEndian>(i + 1).unwrap();
        // Insert i as an integer
        buffer.push(b'M');
        buffer.write_u16::<LittleEndian>(i as u16).unwrap();
        // Append
        buffer.push(b'a');
    }
    // Append all
    buffer.extend(b"e.");

    c.bench_function("unpickle_list", |b| {
        b.iter(|| value_from_slice(&buffer, Default::default()).unwrap());
    });
}

fn unpickle_list_no_memo(c: &mut Criterion) {
    // Same as above, but doesn't use the memo
    let mut buffer = b"\x80\x02](".to_vec();
    for i in 0..1000 {
        buffer.extend(b"]M");
        buffer.write_u16::<LittleEndian>(i as u16).unwrap();
        buffer.push(b'a');
    }
    buffer.extend(b"e.");

    c.bench_function("unpickle_list_no_memo", |b| {
        b.iter(|| value_from_slice(&buffer, Default::default()).unwrap());
    });
}

fn unpickle_dict(c: &mut Criterion) {
    // Creates {0: "string", 1: "string", ...}
    let mut buffer = b"\x80\x03}q\x00(K\x00".to_vec();
    buffer.extend(b"X\x06\x00\x00\x00stringq\x01");
    for i in 0..1000 {
        buffer.push(b'M');
        buffer.write_u16::<LittleEndian>(i as u16).unwrap();
        buffer.extend(b"h\x01");
    }
    buffer.extend(b"u.");

    c.bench_function("unpickle_dict", |b| {
        b.iter(|| value_from_slice(&buffer, Default::default()).unwrap());
    });
}

fn unpickle_nested_list(c: &mut Criterion) {
    // Creates [[[[...]]]]
    let mut buffer = b"\x80\x02".to_vec();
    for i in 0..101 {
        buffer.extend(b"]r");
        buffer.write_u32::<LittleEndian>(i).unwrap();
    }
    for _ in 0..100 {
        buffer.push(b'a');
    }
    buffer.push(b'.');

    c.bench_function("unpickle_nested_list", |b| {
        b.iter(|| value_from_slice(&buffer, Default::default()).unwrap());
    });
}

fn unpickle_nested_list_no_memo(c: &mut Criterion) {
    // Creates [[[[...]]]] without using memo
    let mut buffer = b"\x80\x02".to_vec();
    for _ in 0..201 {
        buffer.extend(b"]");
    }
    for _ in 0..200 {
        buffer.push(b'a');
    }
    buffer.push(b'.');

    c.bench_function("unpickle_nested_list_no_memo", |b| {
        b.iter(|| value_from_slice(&buffer, Default::default()).unwrap());
    });
}

fn unpickle_simple_tuple(c: &mut Criterion) {
    let mut list = Vec::with_capacity(1000);
    for i in 0..1000 {
        list.push(pyobj!(i = i));
    }
    let tuple = Value::Tuple(list);
    let buffer = value_to_vec(&tuple, Default::default()).unwrap();

    c.bench_function("unpickle_simple_tuple", |b| {
        b.iter(|| value_from_slice(&buffer, Default::default()).unwrap());
    });
}

fn pickle_list(c: &mut Criterion) {
    let mut list = Vec::with_capacity(1000);
    for i in 0..1000 {
        list.push(pyobj!(l = [i = i]));
    }
    let list = Value::List(list);
    c.bench_function("pickle_list", |b| {
        b.iter(|| value_to_vec(&list, Default::default()).unwrap());
    });
}

fn pickle_dict(c: &mut Criterion) {
    let mut dict = BTreeMap::new();
    for i in 0..1000 {
        dict.insert(hpyobj!(i = i), pyobj!(l = [i = i]));
    }
    let dict = Value::Dict(dict);

    c.bench_function("pickle_dict", |b| {
        b.iter(|| value_to_vec(&dict, Default::default()).unwrap());
    });
}

fn bench_picklefile(c: &mut Criterion, filename: &str) {
    // Load the picklefile
    let mut contents = vec![];
    let mut f = std::fs::File::open(filename).unwrap();
    f.read_to_end(&mut contents).unwrap();

    // Run the benchmark
    c.bench_function(filename, |b| {
        b.iter(|| {
            serde_pickle::de::value_from_slice(black_box(&contents), serde_pickle::de::DeOptions::new())
                .unwrap()
        })
    });
}

pub fn criterion_benchmark(c: &mut Criterion) {
    bench_picklefile(c, "benches/data/biglist.pickle");
    bench_picklefile(c, "benches/data/manyrefs.pickle");
    bench_picklefile(c, "benches/data/manystrings.pickle");

    for i in 0..=5 {
        bench_picklefile(c, &format!("test/data/tests_py3_proto{}.pickle", i));
    }
    for i in 0..=2 {
        bench_picklefile(c, &format!("test/data/tests_py2_proto{}.pickle", i));
    }

    unpickle_list(c);
    unpickle_list_no_memo(c);
    unpickle_dict(c);
    unpickle_nested_list(c);
    unpickle_nested_list_no_memo(c);
    unpickle_simple_tuple(c);
    pickle_list(c);
    pickle_dict(c);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
