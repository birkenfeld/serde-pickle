// Copyright (c) 2015-2021 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

//! QuickCheck Arbitrary instance for Value, and associated helpers.

use std::i64;
use num_bigint::BigInt;
use quickcheck::{Arbitrary, Gen, empty_shrinker};
use rand::Rng;
use crate::{Value, HashableValue};

const MAX_DEPTH: u32 = 1;

fn gen_value<G: Gen>(g: &mut G, depth: u32) -> Value {
    let upper = if depth > 0 { 12 } else { 7 };
    match g.gen_range(0, upper) {
        // leaves
        0  => Value::None,
        1  => Value::Bool(Arbitrary::arbitrary(g)),
        2  => Value::I64(Arbitrary::arbitrary(g)),
        3  => Value::Int(gen_bigint(g)),
        4  => Value::F64(Arbitrary::arbitrary(g)),
        5  => Value::Bytes(Arbitrary::arbitrary(g)),
        6  => Value::String(Arbitrary::arbitrary(g)),
        // recursive variants
        7  => Value::List(gen_vec(g, depth - 1)),
        8  => Value::Tuple(gen_vec(g, depth - 1)),
        9  => Value::Set(gen_hvec(g, depth - 1).into_iter().collect()),
        10 => Value::FrozenSet(gen_hvec(g, depth - 1).into_iter().collect()),
        11 => { let kvec = gen_hvec(g, depth - 1);
                let vvec = gen_vec(g, depth - 1);
                Value::Dict(kvec.into_iter().zip(vvec).collect()) },
        _  => unreachable!(),
    }
}

fn gen_bigint<G: Gen>(g: &mut G) -> BigInt {
    // We have to construct a value outside of i64 range, since other values
    // are unpickled as i64s instead of big ints.
    let offset = BigInt::from(2) * BigInt::from(if g.gen() { i64::MIN } else { i64::MAX });
    offset + BigInt::from(g.gen::<i64>())
}

fn gen_vec<G: Gen>(g: &mut G, depth: u32) -> Vec<Value> {
    let size = { let s = g.size(); g.gen_range(0, s) };
    (0..size).map(|_| gen_value(g, depth)).collect()
}

fn gen_hvalue<G: Gen>(g: &mut G, depth: u32) -> HashableValue {
    let upper = if depth > 0 { 9 } else { 7 };
    match g.gen_range(0, upper) {
        // leaves
        0  => HashableValue::None,
        1  => HashableValue::Bool(Arbitrary::arbitrary(g)),
        2  => HashableValue::I64(Arbitrary::arbitrary(g)),
        3  => { // We have to construct a value outside of i64 range.
                let val: i64 = Arbitrary::arbitrary(g);
                let max = BigInt::from(i64::MAX);
                HashableValue::Int(BigInt::from(val) + BigInt::from(2) * max) },
        4  => HashableValue::F64(Arbitrary::arbitrary(g)),
        5  => HashableValue::Bytes(Arbitrary::arbitrary(g)),
        6  => HashableValue::String(Arbitrary::arbitrary(g)),
        // recursive variants
        7  => HashableValue::Tuple(gen_hvec(g, depth - 1)),
        8  => HashableValue::FrozenSet(gen_hvec(g, depth - 1).into_iter().collect()),
        _  => unreachable!(),
    }
}

fn gen_hvec<G: Gen>(g: &mut G, depth: u32) -> Vec<HashableValue> {
    let size = { let s = g.size(); g.gen_range(0, s) };
    (0..size).map(|_| gen_hvalue(g, depth)).collect()
}

impl Arbitrary for Value {
    fn arbitrary<G: Gen>(g: &mut G) -> Value {
        gen_value(g, MAX_DEPTH)
    }

    fn shrink(&self) -> Box<dyn Iterator<Item=Value>> {
        match *self {
            Value::None => empty_shrinker(),
            Value::Bool(v) => Box::new(Arbitrary::shrink(&v).map(Value::Bool)),
            Value::I64(v) => Box::new(Arbitrary::shrink(&v).map(Value::I64)),
            Value::Int(_) => empty_shrinker(),
            Value::F64(v) => Box::new(Arbitrary::shrink(&v).map(Value::F64)),
            Value::Bytes(ref v) => Box::new(Arbitrary::shrink(v).map(Value::Bytes)),
            Value::String(ref v) => Box::new(Arbitrary::shrink(v).map(Value::String)),
            Value::List(ref v) => Box::new(Arbitrary::shrink(v).map(Value::List)),
            Value::Tuple(ref v) => Box::new(Arbitrary::shrink(v).map(Value::List)),
            Value::Set(ref v) => Box::new(Arbitrary::shrink(v).map(Value::Set)),
            Value::FrozenSet(ref v) => Box::new(Arbitrary::shrink(v).map(Value::FrozenSet)),
            Value::Dict(ref v) => Box::new(Arbitrary::shrink(v).map(Value::Dict)),
        }
    }
}

impl Arbitrary for HashableValue {
    fn arbitrary<G: Gen>(g: &mut G) -> HashableValue {
        gen_hvalue(g, MAX_DEPTH)
    }

    fn shrink(&self) -> Box<dyn Iterator<Item=HashableValue>> {
        match *self {
            HashableValue::None => empty_shrinker(),
            HashableValue::Bool(v) => Box::new(Arbitrary::shrink(&v).map(HashableValue::Bool)),
            HashableValue::I64(v) => Box::new(Arbitrary::shrink(&v).map(HashableValue::I64)),
            HashableValue::Int(_) => empty_shrinker(),
            HashableValue::F64(v) => Box::new(Arbitrary::shrink(&v).map(HashableValue::F64)),
            HashableValue::Bytes(ref v) => Box::new(Arbitrary::shrink(v).map(HashableValue::Bytes)),
            HashableValue::String(ref v) => Box::new(Arbitrary::shrink(v).map(HashableValue::String)),
            HashableValue::Tuple(ref v) => Box::new(Arbitrary::shrink(v).map(HashableValue::Tuple)),
            HashableValue::FrozenSet(ref v) => Box::new(Arbitrary::shrink(v).map(HashableValue::FrozenSet)),
        }
    }
}
