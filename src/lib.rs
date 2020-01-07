// Copyright (c) 2015-2019 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

//! Serialization and deserialization for Python's pickle format
//!
//! # Pickle format
//!
//! Please see the [Python docs](http://docs.python.org/library/pickle) for
//! details on the Pickle format.
//!
//! This crate supports all Pickle protocols (0 to 4) when reading, and writing
//! protocol 2 (compatible with Python 2 and 3), or protocol 3 (compatible with
//! Python 3 only).
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
//! When deserializing, arbitrary Python objects saved using a pickled instance
//! dictionary or `__setstate__` are replaced by that state, since version
//! 0.5 of this library.
//!
//! *Note:* since Serde 1.0, fixed-size Rust arrays (which have type `[T; n]` or
//! `&[T; n]`) are treated as tuples when serializing.  In particular, this
//! means that bytes literals will also be serialized as a tuple of integers
//! unless you convert them into an unsized slice first (e.g. `&b"bytes"[..]`).
//!
//! # Exported API
//!
//! The library exports generic serde (de)serializing functions `to_*` and
//! `from_*`.  It also exports functions that produce or take only the specific
//! `Value` struct exposed by this library, which supports all built-in Python
//! types (notably, long integers and sets, which serde's generic types don't
//! handle).  These functions, called `value_from_*` and `value_to_*`, will
//! correctly (un)pickle these types.

#![cfg_attr(feature = "unstable", feature(test))]

pub use self::ser::{
    Serializer,
    to_writer,
    to_vec,
    value_to_writer,
    value_to_vec,
};

pub use self::de::{
    Deserializer,
    from_reader,
    from_slice,
    from_iter,
    value_from_reader,
    value_from_slice,
    value_from_iter,
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
mod value_impls;

#[cfg(test)]
#[path = "../test/mod.rs"]
mod test;
