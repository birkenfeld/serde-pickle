// Copyright (c) 2015-2021 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

//! Pickle serialization

use std::io;
use std::collections::BTreeSet;
use serde::ser;
use serde::ser::Serialize;
use byteorder::{LittleEndian, BigEndian, WriteBytesExt};
use num_bigint::BigInt;
use num_traits::Signed;

use super::consts::*;
use super::error::{Error, Result};
use super::value::{Value, HashableValue};

/// Supported pickle protocols for writing.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PickleProto {
    V2,
    V3,
}

impl Default for PickleProto {
    fn default() -> Self {
        Self::V3
    }
}

/// Options for serializing.
#[derive(Clone, Debug, Default)]
pub struct SerOptions {
    proto: PickleProto,
    compat_enum_repr: bool,
}

impl SerOptions {
    /// Construct with default options:
    ///
    /// - use pickle protocol v3
    /// - use the serde-standard Enum representation
    pub fn new() -> Self {
        Default::default()
    }

    /// Set the used pickle protocol to v2.
    pub fn proto_v2(mut self) -> Self {
        self.proto = PickleProto::V2;
        self
    }

    /// Switch Enum serialization to the representation used up to serde-pickle 0.6.
    ///
    /// "serde standard" representation (now default):
    /// ```text
    ///   Variant           ->  'Variant'
    ///   Variant(T)        ->  {'Variant': T}
    ///   Variant(T1, T2)   ->  {'Variant': [T1, T2]}
    ///   Variant { x: T }  ->  {'Variant': {'x': T}}
    /// ```
    ///
    /// "compat" representation:
    /// ```text
    ///   Variant           ->  ('Variant',)
    ///   Variant(T)        ->  ('Variant', T)
    ///   Variant(T1, T2)   ->  ('Variant', [T1, T2])
    ///   Variant { x: T }  ->  ('Variant', {'x': T})
    /// ```
    ///
    /// When deserializing, `serde-pickle` can handle both representations.
    pub fn compat_enum_repr(mut self) -> Self {
        self.compat_enum_repr = true;
        self
    }
}

/// A structure for serializing Rust values into a Pickle stream.
pub struct Serializer<W> {
    writer: W,
    options: SerOptions,
}

impl<W: io::Write> Serializer<W> {
    pub fn new(writer: W, options: SerOptions) -> Self {
        Serializer { writer, options }
    }

    /// Unwrap the `Writer` from the `Serializer`.
    pub fn into_inner(self) -> W {
        self.writer
    }

    #[inline]
    fn write_opcode(&mut self, opcode: u8) -> Result<()> {
        self.writer.write_all(&[opcode]).map_err(From::from)
    }

    fn serialize_hashable_value(&mut self, value: &HashableValue) -> Result<()> {
        use serde::Serializer;
        match *value {
            // Cases covered by the Serializer trait
            HashableValue::None    => self.serialize_unit(),
            HashableValue::Bool(b) => self.serialize_bool(b),
            HashableValue::I64(i)  => self.serialize_i64(i),
            HashableValue::F64(f)  => self.serialize_f64(f),
            HashableValue::Bytes(ref b) => self.serialize_bytes(b),
            HashableValue::String(ref s) => self.serialize_str(s),
            HashableValue::Int(ref i) => self.serialize_bigint(i),
            HashableValue::FrozenSet(ref s) => self.serialize_set(s, b"frozenset"),
            HashableValue::Tuple(ref t) =>
                self.serialize_tuplevalue(t, |slf, v| slf.serialize_hashable_value(v)),
        }
    }

    fn serialize_value(&mut self, value: &Value) -> Result<()> {
        use serde::Serializer;
        match *value {
            // Cases covered by the Serializer trait
            Value::None    => self.serialize_unit(),
            Value::Bool(b) => self.serialize_bool(b),
            Value::I64(i)  => self.serialize_i64(i),
            Value::F64(f)  => self.serialize_f64(f),
            Value::Bytes(ref b) => self.serialize_bytes(b),
            Value::String(ref s) => self.serialize_str(s),
            Value::List(ref l) => {
                self.write_opcode(EMPTY_LIST)?;
                for chunk in l.chunks(1000) {
                    self.write_opcode(MARK)?;
                    for item in chunk {
                        self.serialize_value(item)?;
                    }
                    self.write_opcode(APPENDS)?;
                }
                Ok(())
            },
            Value::Dict(ref d) => {
                self.write_opcode(EMPTY_DICT)?;
                self.write_opcode(MARK)?;
                for (n, (key, value)) in d.iter().enumerate() {
                    if n % 1000 == 999 {
                        self.write_opcode(SETITEMS)?;
                        self.write_opcode(MARK)?;
                    }
                    self.serialize_hashable_value(key)?;
                    self.serialize_value(value)?;
                }
                self.write_opcode(SETITEMS)?;
                Ok(())
            }

            // Others
            Value::Int(ref i) => {
                self.serialize_bigint(i)
            }
            Value::Tuple(ref t) => {
                self.serialize_tuplevalue(t, |slf, v| slf.serialize_value(v))
            },
            Value::Set(ref s) => {
                self.serialize_set(s, b"set")
            },
            Value::FrozenSet(ref s) => {
                self.serialize_set(s, b"frozenset")
            }
        }
    }

    fn serialize_bigint(&mut self, i: &BigInt) -> Result<()> {
        let bytes = if i.is_negative() {
            let n_bytes = i.to_bytes_le().1.len();
            let pos = i + (BigInt::from(1) << (n_bytes * 8));
            let mut bytes = pos.to_bytes_le().1;
            while bytes.len() < n_bytes {
                bytes.push(0x00);
            }
            if *bytes.last().unwrap() < 0x80 {
                bytes.push(0xff);
            }
            bytes
        } else {
            let mut bytes = i.to_bytes_le().1;
            if *bytes.last().unwrap() >= 0x80 {
                bytes.push(0x00);
            }
            bytes
        };
        if bytes.len() < 256 {
            self.write_opcode(LONG1)?;
            self.writer.write_u8(bytes.len() as u8)?;
        } else {
            self.write_opcode(LONG4)?;
            self.writer.write_u32::<LittleEndian>(bytes.len() as u32)?;
        }
        self.writer.write_all(&bytes).map_err(From::from)
    }

    fn serialize_tuplevalue<T, F>(&mut self, t: &[T], f: F) -> Result<()>
        where F: Fn(&mut Self, &T) -> Result<()>
    {
        if t.is_empty() {
            self.write_opcode(EMPTY_TUPLE)
        } else if t.len() == 1 {
            f(self, &t[0])?;
            self.write_opcode(TUPLE1)
        } else if t.len() == 2 {
            f(self, &t[0])?;
            f(self, &t[1])?;
            self.write_opcode(TUPLE2)
        } else if t.len() == 3 {
            f(self, &t[0])?;
            f(self, &t[1])?;
            f(self, &t[2])?;
            self.write_opcode(TUPLE3)
        } else {
            self.write_opcode(MARK)?;
            for item in t.iter() {
                f(self, item)?;
            }
            self.write_opcode(TUPLE)?;
            Ok(())
        }
    }

    fn serialize_set(&mut self, items: &BTreeSet<HashableValue>, name: &[u8]) -> Result<()> {
        self.write_opcode(GLOBAL)?;
        if self.options.proto == PickleProto::V3 {
            self.writer.write_all(b"builtins\n")?;
        } else {
            self.writer.write_all(b"__builtin__\n")?;
        }
        self.writer.write_all(name)?;
        self.writer.write_all(b"\n")?;
        self.write_opcode(EMPTY_LIST)?;
        self.write_opcode(MARK)?;
        for (n, item) in items.iter().enumerate() {
            if n % 1000 == 999 {
                self.write_opcode(APPENDS)?;
                self.write_opcode(MARK)?;
            }
            self.serialize_hashable_value(item)?;
        }
        self.write_opcode(APPENDS)?;
        self.write_opcode(TUPLE1)?;
        self.write_opcode(REDUCE)
    }
}

pub struct Compound<'a, W: io::Write + 'a> {
    ser: &'a mut Serializer<W>,
    state: Option<usize>,
}

impl<'a, W: io::Write> ser::SerializeSeq for Compound<'a, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        value.serialize(&mut *self.ser)?;
        // Batch appends as in Python pickle
        *self.state.as_mut().unwrap() += 1;
        if self.state.unwrap() == 1000 {
            self.ser.write_opcode(APPENDS)?;
            self.ser.write_opcode(MARK)?;
            self.state = Some(0);
        }
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<()> {
        if self.state.is_some() {
            self.ser.write_opcode(APPENDS)?;
        }
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeTuple for Compound<'a, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        value.serialize(&mut *self.ser)
    }

    #[inline]
    fn end(self) -> Result<()> {
        if self.state.is_some() {
            self.ser.write_opcode(TUPLE)?;
        }
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeTupleStruct for Compound<'a, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        ser::SerializeTuple::serialize_element(self, value)
    }

    #[inline]
    fn end(self) -> Result<()> {
        ser::SerializeTuple::end(self)
    }
}

impl<'a, W: io::Write> ser::SerializeTupleVariant for Compound<'a, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        value.serialize(&mut *self.ser)
    }

    #[inline]
    fn end(self) -> Result<()> {
        self.ser.write_opcode(APPENDS)?;
        if self.ser.options.compat_enum_repr {
            self.ser.write_opcode(TUPLE2)
        } else {
            self.ser.write_opcode(SETITEM)
        }
    }
}

impl<'a, W: io::Write> ser::SerializeMap for Compound<'a, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<()> {
        key.serialize(&mut *self.ser)
    }

    #[inline]
    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        value.serialize(&mut *self.ser)?;
        // Batch appends as in Python pickle
        *self.state.as_mut().unwrap() += 1;
        if self.state.unwrap() == 1000 {
            self.ser.write_opcode(SETITEMS)?;
            self.ser.write_opcode(MARK)?;
            self.state = Some(0);
        }
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<()> {
        if self.state.is_some() {
            self.ser.write_opcode(SETITEMS)?;
        }
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeStruct for Compound<'a, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T: Serialize + ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()> {
        ser::SerializeMap::serialize_key(self, key)?;
        ser::SerializeMap::serialize_value(self, value)
    }

    #[inline]
    fn end(self) -> Result<()> {
        ser::SerializeMap::end(self)
    }
}

impl<'a, W: io::Write> ser::SerializeStructVariant for Compound<'a, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T: Serialize + ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()> {
        ser::SerializeStruct::serialize_field(self, key, value)
    }

    #[inline]
    fn end(self) -> Result<()> {
        if self.state.is_some() {
            self.ser.write_opcode(SETITEMS)?;
        }
        if self.ser.options.compat_enum_repr {
            self.ser.write_opcode(TUPLE2)
        } else {
            self.ser.write_opcode(SETITEM)
        }
    }
}

impl<'a, W: io::Write> ser::Serializer for &'a mut Serializer<W> {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = Compound<'a, W>;
    type SerializeTuple = Self::SerializeSeq;
    type SerializeTupleStruct = Self::SerializeTuple;
    type SerializeTupleVariant = Self::SerializeTuple;
    type SerializeMap = Compound<'a, W>;
    type SerializeStruct = Self::SerializeMap;
    type SerializeStructVariant = Self::SerializeMap;

    #[inline]
    fn serialize_bool(self, value: bool) -> Result<()> {
        self.write_opcode(if value { NEWTRUE } else { NEWFALSE })
    }

    #[inline]
    fn serialize_i8(self, value: i8) -> Result<()> {
        if value > 0 {
            self.write_opcode(BININT1)?;
            self.writer.write_i8(value).map_err(From::from)
        } else {
            self.write_opcode(BININT)?;
            self.writer.write_i32::<LittleEndian>(value.into()).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_i16(self, value: i16) -> Result<()> {
        if value > 0 {
            self.write_opcode(BININT2)?;
            self.writer.write_i16::<LittleEndian>(value).map_err(From::from)
        } else {
            self.write_opcode(BININT)?;
            self.writer.write_i32::<LittleEndian>(value.into()).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_i32(self, value: i32) -> Result<()> {
        self.write_opcode(BININT)?;
        self.writer.write_i32::<LittleEndian>(value).map_err(From::from)
    }

    #[inline]
    fn serialize_i64(self, value: i64) -> Result<()> {
        if -0x8000_0000 <= value && value < 0x8000_0000 {
            self.write_opcode(BININT)?;
            self.writer.write_i32::<LittleEndian>(value as i32).map_err(From::from)
        } else {
            self.write_opcode(LONG1)?;
            self.writer.write_i8(8)?;
            self.writer.write_i64::<LittleEndian>(value).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_u8(self, value: u8) -> Result<()> {
        self.write_opcode(BININT1)?;
        self.writer.write_u8(value).map_err(From::from)
    }

    #[inline]
    fn serialize_u16(self, value: u16) -> Result<()> {
        self.write_opcode(BININT2)?;
        self.writer.write_u16::<LittleEndian>(value).map_err(From::from)
    }

    #[inline]
    fn serialize_u32(self, value: u32) -> Result<()> {
        if value < 0x8000_0000 {
            self.write_opcode(BININT)?;
            self.writer.write_u32::<LittleEndian>(value).map_err(From::from)
        } else {
            self.write_opcode(LONG1)?;
            self.writer.write_i8(5)?;
            self.writer.write_u32::<LittleEndian>(value)?;
            // The final byte has to be there, otherwise we'd get the unsigned
            // value interpreted as signed.
            self.writer.write_i8(0).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_u64(self, value: u64) -> Result<()> {
        if value < 0x8000_0000 {
            self.write_opcode(BININT)?;
            self.writer.write_u32::<LittleEndian>(value as u32).map_err(From::from)
        } else {
            self.write_opcode(LONG1)?;
            self.writer.write_i8(9)?;
            self.writer.write_u64::<LittleEndian>(value)?;
            // The final byte has to be there, otherwise we could get the
            // unsigned value interpreted as signed.
            self.writer.write_i8(0).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_f32(self, value: f32) -> Result<()> {
        self.write_opcode(BINFLOAT)?;
        // Yes, this one is big endian.
        self.writer.write_f64::<BigEndian>(value.into()).map_err(From::from)
    }

    #[inline]
    fn serialize_f64(self, value: f64) -> Result<()> {
        self.write_opcode(BINFLOAT)?;
        self.writer.write_f64::<BigEndian>(value).map_err(From::from)
    }

    #[inline]
    fn serialize_char(self, value: char) -> Result<()> {
        let mut string = String::with_capacity(4);  // longest utf-8 encoding
        string.push(value);
        self.serialize_str(&string)
    }

    #[inline]
    fn serialize_str(self, value: &str) -> Result<()> {
        self.write_opcode(BINUNICODE)?;
        self.writer.write_u32::<LittleEndian>(value.len() as u32)?;
        self.writer.write_all(value.as_bytes()).map_err(From::from)
    }

    #[inline]
    fn serialize_bytes(self, value: &[u8]) -> Result<()> {
        if self.options.proto == PickleProto::V3 {
            if value.len() < 256 {
                self.write_opcode(SHORT_BINBYTES)?;
                self.writer.write_u8(value.len() as u8)?;
            } else {
                self.write_opcode(BINBYTES)?;
                self.writer.write_u32::<LittleEndian>(value.len() as u32)?;
            }
            self.writer.write_all(value).map_err(From::from)
        } else {
            // We can't use the BINSTRING opcodes because they depend on the
            // str encoding in Unpickler, which varies between Py2 and Py3.
            // Instead, pickle the bytes as unicode codepoints and then encode
            // them as latin1 on unpickling to get the bytes (Python itself
            // does this trick)
            // TODO: we could keep track of 'codecs\nencode' and 'latin1' in
            // the memo rather than writing them out for each byte string
            self.write_opcode(GLOBAL)?;
            self.writer.write_all(b"_codecs\nencode\n")?;
            // BINUNICODE needs a utf8-encoded string, but we're pretending ours
            // has a latin1 encoding. Happily, the byte values of an encoded latin1
            // string match their codepoints. So converting to utf8 encoding is
            // as simple as interpreting each byte as a unicode codepoint and
            // then encoding as utf8 - https://stackoverflow.com/a/28175593/2352259
            let utf8_value: String = value.iter().map(|&c| c as char).collect();
            self.serialize_str(&utf8_value)?;
            self.serialize_str("latin1")?;
            self.write_opcode(TUPLE2)?;
            self.write_opcode(REDUCE).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_unit(self) -> Result<()> {
        // Although Python has an empty tuple, we use None here for compatibility
        // with other serialization formats.
        self.write_opcode(NONE)
    }

    #[inline]
    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
        self.write_opcode(NONE)
    }

    #[inline]
    fn serialize_unit_variant(self, _name: &'static str, _variant_index: u32, variant: &'static str)
                              -> Result<()> {
        self.serialize_str(variant)?;
        if self.options.compat_enum_repr {
            self.write_opcode(TUPLE1)
        } else {
            Ok(())
        }
    }

    #[inline]
    fn serialize_newtype_struct<T: Serialize + ?Sized>(self, _name: &'static str, value: &T) -> Result<()> {
        value.serialize(self)
    }

    #[inline]
    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _name: &'static str,
                                                        _variant_index: u32, variant: &'static str,
                                                        value: &T) -> Result<()> {
        if self.options.compat_enum_repr {
            self.serialize_str(variant)?;
            value.serialize(&mut *self)?;
            self.write_opcode(TUPLE2)
        } else {
            self.write_opcode(EMPTY_DICT)?;
            self.serialize_str(variant)?;
            value.serialize(&mut *self)?;
            self.write_opcode(SETITEM)
        }
    }

    #[inline]
    fn serialize_none(self) -> Result<()> {
        self.serialize_unit()
    }

    #[inline]
    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<()> {
        value.serialize(self)
    }

    #[inline]
    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        self.write_opcode(EMPTY_LIST)?;
        match len {
            Some(len) if len == 0 => Ok(Compound { ser: self, state: None }),
            _ => {
                self.write_opcode(MARK)?;
                Ok(Compound { ser: self, state: Some(0) })
            }
        }
    }

    #[inline]
    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        if len == 0 {
            self.write_opcode(EMPTY_TUPLE)?;
            Ok(Compound { ser: self, state: None })
        } else {
            self.write_opcode(MARK)?;
            Ok(Compound { ser: self, state: Some(0) })
        }
    }

    #[inline]
    fn serialize_tuple_struct(self, _name: &'static str, len: usize)
                              -> Result<Self::SerializeTupleStruct> {
        self.serialize_tuple(len)
    }

    #[inline]
    fn serialize_tuple_variant(self, _name: &'static str, _variant_index: u32, variant: &'static str,
                               _len: usize) -> Result<Self::SerializeTupleVariant> {
        if !self.options.compat_enum_repr {
            self.write_opcode(EMPTY_DICT)?;
        }
        self.serialize_str(variant)?;
        self.write_opcode(EMPTY_LIST)?;
        self.write_opcode(MARK)?;
        Ok(Compound { ser: self, state: None })
    }

    #[inline]
    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        self.write_opcode(EMPTY_DICT)?;
        match len {
            Some(len) if len == 0 => Ok(Compound { ser: self, state: None }),
            _ => {
                self.write_opcode(MARK)?;
                Ok(Compound { ser: self, state: Some(0) })
            }
        }
    }

    #[inline]
    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_map(Some(len))
    }

    #[inline]
    fn serialize_struct_variant(self, _name: &'static str, _variant_index: u32, variant: &'static str,
                                len: usize) -> Result<Self::SerializeStructVariant> {
        if !self.options.compat_enum_repr {
            self.write_opcode(EMPTY_DICT)?;
        }
        self.serialize_str(variant)?;
        self.serialize_map(Some(len))
    }
}

fn wrap_write<W: io::Write, F>(mut writer: W, inner: F, options: SerOptions) -> Result<()>
    where F: FnOnce(&mut Serializer<W>) -> Result<()>
{
    writer.write_all(&[PROTO])?;
    if options.proto == PickleProto::V3 {
        writer.write_all(b"\x03")?;
    } else {
        writer.write_all(b"\x02")?;
    }
    let mut ser = Serializer::new(writer, options);
    inner(&mut ser)?;
    let mut writer = ser.into_inner();
    writer.write_all(&[STOP]).map_err(From::from)
}


/// Encode the value into a pickle stream.
pub fn value_to_writer<W: io::Write>(writer: &mut W, value: &Value, options: SerOptions)
                                     -> Result<()> {
    wrap_write(writer, |ser| ser.serialize_value(value), options)
}

/// Encode the specified struct into a `[u8]` writer.
#[inline]
pub fn to_writer<W: io::Write, T: Serialize>(writer: &mut W, value: &T, options: SerOptions)
                                             -> Result<()> {
    wrap_write(writer, |ser| value.serialize(ser), options)
}

/// Encode the value into a `Vec<u8>` buffer.
#[inline]
pub fn value_to_vec(value: &Value, options: SerOptions) -> Result<Vec<u8>> {
    let mut writer = Vec::with_capacity(128);
    value_to_writer(&mut writer, value, options)?;
    Ok(writer)
}

/// Encode the specified struct into a `Vec<u8>` buffer.
#[inline]
pub fn to_vec<T: Serialize>(value: &T, options: SerOptions) -> Result<Vec<u8>> {
    let mut writer = Vec::with_capacity(128);
    to_writer(&mut writer, value, options)?;
    Ok(writer)
}
