// Copyright (c) 2015-2016 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

//! Serialization and deserialization for Python's pickle format
//!
//! # Pickle format
//!
//! Please see the [Python docs](http://docs.python.org/library/pickle) for
//! details on the pickle format.
//!
//! # Supported types
//!
//! Pickle is very powerful.  It is capable of serializing pretty arbitrary
//! graphs of Python objects, with most custom classes being serialized out
//! of the box.  Currently, this crate only supports Python's built-in types
//! that map easily to Rust constructs.  There are:
//!
//! * None
//! * Boolean (Rust `bool`)
//! * Integers (Rust `i64` or bigints from num)
//! * Floats (Rust `f64`)
//! * Strings (Rust `Vec<u8>`)
//! * Unicode strings (Rust `String`)
//! * Lists and tuples (Rust `Vec<Value>`)
//! * Sets and frozensets (Rust `HashSet<Value>`)
//! * Dictionaries (Rust `HashMap<Value, Value>`)
//!
//! # Exported API
//!
//! The library exports generic serde (de)serializing functions `to_*` and
//! `from_*`.  It also exports functions that produce or take only the specific
//! `Value` struct exposed by this library, which supports all built-in Python
//! types (notably, long integers and sets, which serde's generic types don't
//! handle).  These functions, called `value_from_*` and `value_to_*`, will
//! correctly (un)pickle these types.

#![cfg_attr(test, feature(test))]

extern crate serde;
extern crate num_bigint;
extern crate num_traits;
extern crate byteorder;

pub use self::ser::{
    Serializer,
    to_writer,
    to_vec,
    value_to_writer,
    value_to_vec,
};

pub use self::de::{
    Deserializer,
    from_iter,
    from_reader,
    from_slice,
    value_from_iter,
    value_from_reader,
    value_from_slice,
};

pub use self::value::{
    Value,
    HashableValue,
    to_value,
    from_value,
};

pub use self::error::{Error, ErrorCode, Result};

pub mod ser;
pub mod de;
pub mod error;
pub mod value;
mod consts;

#[cfg(test)]
#[path = "../test/mod.rs"]
mod test;
