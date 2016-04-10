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
//! * Boolean (Rust `bool`)
//! * Integers (Rust `i64` or bigints from num)
//! * Floats (Rust `f64`)
//! * Strings (Rust `Vec<u8>`)
//! * Unicode strings (Rust `String`)
//! * Lists and tuples (Rust `Vec<Value>`)
//! * Sets and frozensets (Rust `HashSet<Value>`)
//! * Dictionaries (Rust `HashMap<Value, Value>`)
//! * None

extern crate num_bigint;
extern crate num_traits;
extern crate serde;
extern crate byteorder;

pub use self::ser::{
    Serializer,
    to_writer,
    to_vec,
    value_to_writer,
    value_to_vec,
};

pub use self::de::{
    PickleReader,
    from_iter,
    from_reader,
    from_slice,
    from_str,
    value_from_iter,
    value_from_reader,
    value_from_slice,
    value_from_str,
};

pub use self::value::{
    Value,
    Deserializer,
    to_value,
    from_value,
};

pub use self::error::{Error, ErrorCode, Result};

pub mod ser;
pub mod de;
pub mod error;
pub mod value;
mod consts;
