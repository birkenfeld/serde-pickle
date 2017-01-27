// Copyright (c) 2015-2017 Georg Brandl.  Licensed under the Apache License,
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


/// A structure for serializing Rust values into a Pickle stream.
pub struct Serializer<W> {
    writer: W,
    use_proto_3: bool,
}

impl<W: io::Write> Serializer<W> {
    pub fn new(writer: W, use_proto_3: bool) -> Self {
        Serializer {
            writer: writer,
            use_proto_3: use_proto_3,
        }
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
                try!(self.write_opcode(EMPTY_LIST));
                for chunk in l.chunks(1000) {
                    try!(self.write_opcode(MARK));
                    for item in chunk {
                        try!(self.serialize_value(item));
                    }
                    try!(self.write_opcode(APPENDS));
                }
                Ok(())
            },
            Value::Dict(ref d) => {
                try!(self.write_opcode(EMPTY_DICT));
                try!(self.write_opcode(MARK));
                for (n, (key, value)) in d.iter().enumerate() {
                    if n % 1000 == 999 {
                        try!(self.write_opcode(SETITEMS));
                        try!(self.write_opcode(MARK));
                    }
                    try!(self.serialize_hashable_value(&key));
                    try!(self.serialize_value(value));
                }
                try!(self.write_opcode(SETITEMS));
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
            if bytes.last().unwrap() < &0x80 {
                bytes.push(0xff);
            }
            bytes
        } else {
            let mut bytes = i.to_bytes_le().1;
            if bytes.last().unwrap() >= &0x80 {
                bytes.push(0x00);
            }
            bytes
        };
        if bytes.len() < 256 {
            try!(self.write_opcode(LONG1));
            try!(self.writer.write_u8(bytes.len() as u8));
        } else {
            try!(self.write_opcode(LONG4));
            try!(self.writer.write_u32::<LittleEndian>(bytes.len() as u32));
        }
        self.writer.write_all(&bytes).map_err(From::from)
    }

    fn serialize_tuplevalue<T, F>(&mut self, t: &[T], f: F) -> Result<()>
        where F: Fn(&mut Self, &T) -> Result<()>
    {
        if t.is_empty() {
            self.write_opcode(EMPTY_TUPLE)
        } else if t.len() == 1 {
            try!(f(self, &t[0]));
            self.write_opcode(TUPLE1)
        } else if t.len() == 2 {
            try!(f(self, &t[0]));
            try!(f(self, &t[1]));
            self.write_opcode(TUPLE2)
        } else if t.len() == 3 {
            try!(f(self, &t[0]));
            try!(f(self, &t[1]));
            try!(f(self, &t[2]));
            self.write_opcode(TUPLE3)
        } else {
            try!(self.write_opcode(MARK));
            for item in t.iter() {
                try!(f(self, item));
            }
            try!(self.write_opcode(TUPLE));
            Ok(())
        }
    }

    fn serialize_set(&mut self, items: &BTreeSet<HashableValue>, name: &[u8]) -> Result<()> {
        try!(self.write_opcode(GLOBAL));
        if self.use_proto_3 {
            try!(self.writer.write(b"builtins\n"));
        } else {
            try!(self.writer.write(b"__builtin__\n"));
        }
        try!(self.writer.write_all(name));
        try!(self.writer.write(b"\n"));
        try!(self.write_opcode(EMPTY_LIST));
        try!(self.write_opcode(MARK));
        for (n, item) in items.iter().enumerate() {
            if n % 1000 == 999 {
                try!(self.write_opcode(APPENDS));
                try!(self.write_opcode(MARK));
            }
            try!(self.serialize_hashable_value(&item));
        }
        try!(self.write_opcode(APPENDS));
        try!(self.write_opcode(TUPLE1));
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
        try!(value.serialize(&mut *self.ser));
        // Batch appends as in Python pickle
        *self.state.as_mut().unwrap() += 1;
        if self.state.unwrap() == 1000 {
            try!(self.ser.write_opcode(APPENDS));
            try!(self.ser.write_opcode(MARK));
            self.state = Some(0);
        }
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<()> {
        if self.state.is_some() {
            try!(self.ser.write_opcode(APPENDS));
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
            try!(self.ser.write_opcode(TUPLE));
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
        try!(self.ser.write_opcode(APPENDS));
        self.ser.write_opcode(TUPLE2)
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
        try!(value.serialize(&mut *self.ser));
        // Batch appends as in Python pickle
        *self.state.as_mut().unwrap() += 1;
        if self.state.unwrap() == 1000 {
            try!(self.ser.write_opcode(SETITEMS));
            try!(self.ser.write_opcode(MARK));
            self.state = Some(0);
        }
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<()> {
        if self.state.is_some() {
            try!(self.ser.write_opcode(SETITEMS));
        }
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeStruct for Compound<'a, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T: Serialize + ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()> {
        try!(ser::SerializeMap::serialize_key(self, key));
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
            try!(self.ser.write_opcode(SETITEMS));
        }
        self.ser.write_opcode(TUPLE2)
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
            try!(self.write_opcode(BININT1));
            self.writer.write_i8(value).map_err(From::from)
        } else {
            try!(self.write_opcode(BININT));
            self.writer.write_i32::<LittleEndian>(value as i32).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_i16(self, value: i16) -> Result<()> {
        if value > 0 {
            try!(self.write_opcode(BININT2));
            self.writer.write_i16::<LittleEndian>(value).map_err(From::from)
        } else {
            try!(self.write_opcode(BININT));
            self.writer.write_i32::<LittleEndian>(value as i32).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_i32(self, value: i32) -> Result<()> {
        try!(self.write_opcode(BININT));
        self.writer.write_i32::<LittleEndian>(value).map_err(From::from)
    }

    #[inline]
    fn serialize_i64(self, value: i64) -> Result<()> {
        if -0x8000_0000 <= value && value < 0x8000_0000 {
            try!(self.write_opcode(BININT));
            self.writer.write_i32::<LittleEndian>(value as i32).map_err(From::from)
        } else {
            try!(self.write_opcode(LONG1));
            try!(self.writer.write_i8(8));
            self.writer.write_i64::<LittleEndian>(value).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_u8(self, value: u8) -> Result<()> {
        try!(self.write_opcode(BININT1));
        self.writer.write_u8(value).map_err(From::from)
    }

    #[inline]
    fn serialize_u16(self, value: u16) -> Result<()> {
        try!(self.write_opcode(BININT2));
        self.writer.write_u16::<LittleEndian>(value).map_err(From::from)
    }

    #[inline]
    fn serialize_u32(self, value: u32) -> Result<()> {
        if value < 0x8000_0000 {
            try!(self.write_opcode(BININT));
            self.writer.write_u32::<LittleEndian>(value).map_err(From::from)
        } else {
            try!(self.write_opcode(LONG1));
            try!(self.writer.write_i8(5));
            try!(self.writer.write_u32::<LittleEndian>(value));
            // The final byte has to be there, otherwise we'd get the unsigned
            // value interpreted as signed.
            self.writer.write_i8(0).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_u64(self, value: u64) -> Result<()> {
        if value < 0x8000_0000 {
            try!(self.write_opcode(BININT));
            self.writer.write_u32::<LittleEndian>(value as u32).map_err(From::from)
        } else {
            try!(self.write_opcode(LONG1));
            try!(self.writer.write_i8(9));
            try!(self.writer.write_u64::<LittleEndian>(value));
            // The final byte has to be there, otherwise we could get the
            // unsigned value interpreted as signed.
            self.writer.write_i8(0).map_err(From::from)
        }
    }

    #[inline]
    fn serialize_f32(self, value: f32) -> Result<()> {
        try!(self.write_opcode(BINFLOAT));
        // Yes, this one is big endian.
        self.writer.write_f64::<BigEndian>(value as f64).map_err(From::from)
    }

    #[inline]
    fn serialize_f64(self, value: f64) -> Result<()> {
        try!(self.write_opcode(BINFLOAT));
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
        try!(self.write_opcode(BINUNICODE));
        try!(self.writer.write_u32::<LittleEndian>(value.len() as u32));
        self.writer.write_all(value.as_bytes()).map_err(From::from)
    }

    #[inline]
    fn serialize_bytes(self, value: &[u8]) -> Result<()> {
        if value.len() < 256 {
            let op = if self.use_proto_3 { SHORT_BINBYTES } else { SHORT_BINSTRING };
            try!(self.write_opcode(op));
            try!(self.writer.write_u8(value.len() as u8));
        } else {
            let op = if self.use_proto_3 { BINBYTES } else { BINSTRING };
            try!(self.write_opcode(op));
            try!(self.writer.write_u32::<LittleEndian>(value.len() as u32));
        }
        self.writer.write_all(value).map_err(From::from)
    }

    #[inline]
    fn serialize_unit(self) -> Result<()> {
        // Although Python has an empty tuple, we use None here for compatibility
        // with other serialization formats.
        self.write_opcode(NONE)
    }

    #[inline]
    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
        self.write_opcode(EMPTY_TUPLE)
    }

    // We'll use tuples for serializing enums:
    // Variant             ('Variant',)
    // Variant(T)          ('Variant', T)
    // Variant(T1, T2)     ('Variant', [T1, T2])
    // Variant { x: T }    ('Variant', {'x': T})
    #[inline]
    fn serialize_unit_variant(self, _name: &str, _variant_index: usize, variant: &str)
                              -> Result<()> {
        try!(self.serialize_str(variant));
        self.write_opcode(TUPLE1)
    }

    #[inline]
    fn serialize_newtype_struct<T: Serialize + ?Sized>(self, _name: &'static str, value: &T) -> Result<()> {
        value.serialize(self)
    }

    #[inline]
    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _name: &str,
                                                        _variant_index: usize, variant: &str,
                                                        value: &T) -> Result<()> {
        try!(self.serialize_str(variant));
        try!(value.serialize(&mut *self));
        self.write_opcode(TUPLE2)
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
        try!(self.write_opcode(EMPTY_LIST));
        match len {
            Some(len) if len == 0 => Ok(Compound { ser: self, state: None }),
            _ => {
                try!(self.write_opcode(MARK));
                Ok(Compound { ser: self, state: Some(0) })
            }
        }
    }

    #[inline]
    fn serialize_seq_fixed_size(self, _len: usize) -> Result<Self::SerializeSeq> {
        try!(self.write_opcode(EMPTY_LIST));
        try!(self.write_opcode(MARK));
        Ok(Compound { ser: self, state: Some(0) })
    }

    #[inline]
    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        if len == 0 {
            try!(self.write_opcode(EMPTY_TUPLE));
            Ok(Compound { ser: self, state: None })
        } else {
            try!(self.write_opcode(MARK));
            Ok(Compound { ser: self, state: Some(0) })
        }
    }

    #[inline]
    fn serialize_tuple_struct(self, _name: &'static str, len: usize)
                              -> Result<Self::SerializeTupleStruct> {
        self.serialize_tuple(len)
    }

    #[inline]
    fn serialize_tuple_variant(self, _name: &str, _variant_index: usize, variant: &str,
                               _len: usize) -> Result<Self::SerializeTupleVariant> {
        try!(self.serialize_str(variant));
        try!(self.write_opcode(EMPTY_LIST));
        try!(self.write_opcode(MARK));
        Ok(Compound { ser: self, state: None })
    }

    #[inline]
    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        try!(self.write_opcode(EMPTY_DICT));
        match len {
            Some(len) if len == 0 => Ok(Compound { ser: self, state: None }),
            _ => {
                try!(self.write_opcode(MARK));
                Ok(Compound { ser: self, state: Some(0) })
            }
        }
    }

    #[inline]
    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_map(Some(len))
    }

    #[inline]
    fn serialize_struct_variant(self, _name: &str, _variant_index: usize, variant: &str,
                                len: usize) -> Result<Self::SerializeStructVariant> {
        try!(self.serialize_str(variant));
        self.serialize_map(Some(len))
    }
}

fn wrap_write<W: io::Write, F>(mut writer: W, inner: F, use_proto_3: bool) -> Result<()>
    where F: FnOnce(&mut Serializer<W>) -> Result<()>
{
    try!(writer.write_all(&[PROTO]));
    if use_proto_3 {
        try!(writer.write_all(b"\x03"));
    } else {
        try!(writer.write_all(b"\x02"));
    }
    let mut ser = Serializer::new(writer, use_proto_3);
    try!(inner(&mut ser));
    let mut writer = ser.into_inner();
    writer.write_all(&[STOP]).map_err(From::from)
}


/// Encode the value into a pickle stream.
pub fn value_to_writer<W: io::Write>(writer: &mut W, value: &Value, use_proto_3: bool)
                                     -> Result<()> {
    wrap_write(writer, |ser| ser.serialize_value(value), use_proto_3)
}

/// Encode the specified struct into a `[u8]` writer.
#[inline]
pub fn to_writer<W: io::Write, T: Serialize>(writer: &mut W, value: &T, use_proto_3: bool)
                                             -> Result<()> {
    wrap_write(writer, |ser| value.serialize(ser), use_proto_3)
}

/// Encode the value into a `Vec<u8>` buffer.
#[inline]
pub fn value_to_vec(value: &Value, use_proto_3: bool) -> Result<Vec<u8>> {
    let mut writer = Vec::with_capacity(128);
    try!(value_to_writer(&mut writer, value, use_proto_3));
    Ok(writer)
}

/// Encode the specified struct into a `Vec<u8>` buffer.
#[inline]
pub fn to_vec<T: Serialize>(value: &T, use_proto_3: bool) -> Result<Vec<u8>> {
    let mut writer = Vec::with_capacity(128);
    try!(to_writer(&mut writer, value, use_proto_3));
    Ok(writer)
}
