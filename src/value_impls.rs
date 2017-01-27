// Copyright (c) 2015-2017 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

//! Serializer/Deserializer implementations for `value::Value`.

use std::fmt;
use std::vec;
use std::result::Result as StdResult;
use std::collections::{btree_map, BTreeMap};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use serde::{ser, de};
use serde::ser::Serialize;
use serde::de::Visitor;

use value::{Value, HashableValue};
use error::{Error, ErrorCode, Result};

impl de::Deserialize for Value {
    #[inline]
    fn deserialize<D: de::Deserializer>(deser: D) -> StdResult<Value, D::Error> {
        struct ValueVisitor;

        impl Visitor for ValueVisitor {
            type Value = Value;

            #[inline]
            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a value")
            }

            #[inline]
            fn visit_bool<E>(self, value: bool) -> StdResult<Value, E> {
                Ok(Value::Bool(value))
            }

            #[inline]
            fn visit_i64<E>(self, value: i64) -> StdResult<Value, E> {
                Ok(Value::I64(value))
            }

            #[inline]
            fn visit_u64<E>(self, value: u64) -> StdResult<Value, E> {
                if value < 0x8000_0000_0000_0000 {
                    Ok(Value::I64(value as i64))
                } else {
                    Ok(Value::Int(BigInt::from(value)))
                }
            }

            #[inline]
            fn visit_f64<E>(self, value: f64) -> StdResult<Value, E> {
                Ok(Value::F64(value))
            }

            #[inline]
            fn visit_str<E: de::Error>(self, value: &str) -> StdResult<Value, E> {
                self.visit_string(String::from(value))
            }

            #[inline]
            fn visit_string<E>(self, value: String) -> StdResult<Value, E> {
                Ok(Value::String(value))
            }

            #[inline]
            fn visit_bytes<E: de::Error>(self, value: &[u8]) -> StdResult<Value, E> {
                self.visit_byte_buf(value.to_vec())
            }

            #[inline]
            fn visit_byte_buf<E: de::Error>(self, value: Vec<u8>) -> StdResult<Value, E> {
                Ok(Value::Bytes(value))
            }

            #[inline]
            fn visit_none<E>(self) -> StdResult<Value, E> {
                Ok(Value::None)
            }

            #[inline]
            fn visit_some<D: de::Deserializer>(self, deser: D) -> StdResult<Value, D::Error> {
                de::Deserialize::deserialize(deser)
            }

            #[inline]
            fn visit_unit<E>(self) -> StdResult<Value, E> {
                Ok(Value::None)
            }

            #[inline]
            fn visit_seq<V: de::SeqVisitor>(self, visitor: V) -> StdResult<Value, V::Error> {
                let values = try!(de::impls::VecVisitor::new().visit_seq(visitor));
                Ok(Value::List(values))
            }

            #[inline]
            fn visit_map<V: de::MapVisitor>(self, visitor: V) -> StdResult<Value, V::Error> {
                let values = try!(de::impls::BTreeMapVisitor::new().visit_map(visitor));
                Ok(Value::Dict(values))
            }
        }

        deser.deserialize(ValueVisitor)
    }
}

impl de::Deserialize for HashableValue {
    #[inline]
    fn deserialize<D: de::Deserializer>(deser: D) -> StdResult<HashableValue, D::Error> {
        struct ValueVisitor;

        impl Visitor for ValueVisitor {
            type Value = HashableValue;

            #[inline]
            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a hashable value")
            }

            #[inline]
            fn visit_bool<E>(self, value: bool) -> StdResult<HashableValue, E> {
                Ok(HashableValue::Bool(value))
            }

            #[inline]
            fn visit_i64<E>(self, value: i64) -> StdResult<HashableValue, E> {
                Ok(HashableValue::I64(value))
            }

            #[inline]
            fn visit_u64<E>(self, value: u64) -> StdResult<HashableValue, E> {
                if value < 0x8000_0000_0000_0000 {
                    Ok(HashableValue::I64(value as i64))
                } else {
                    Ok(HashableValue::Int(BigInt::from(value)))
                }
            }

            #[inline]
            fn visit_f64<E>(self, value: f64) -> StdResult<HashableValue, E> {
                Ok(HashableValue::F64(value))
            }

            #[inline]
            fn visit_str<E: de::Error>(self, value: &str) -> StdResult<HashableValue, E> {
                self.visit_string(String::from(value))
            }

            #[inline]
            fn visit_string<E>(self, value: String) -> StdResult<HashableValue, E> {
                Ok(HashableValue::String(value))
            }

            #[inline]
            fn visit_none<E>(self) -> StdResult<HashableValue, E> {
                Ok(HashableValue::None)
            }

            #[inline]
            fn visit_some<D: de::Deserializer>(self, deser: D) -> StdResult<HashableValue, D::Error> {
                de::Deserialize::deserialize(deser)
            }

            #[inline]
            fn visit_unit<E>(self) -> StdResult<HashableValue, E> {
                Ok(HashableValue::None)
            }

            #[inline]
            fn visit_seq<V: de::SeqVisitor>(self, visitor: V)
                                            -> StdResult<HashableValue, V::Error> {
                let values = try!(de::impls::VecVisitor::new().visit_seq(visitor));
                Ok(HashableValue::Tuple(values))
            }
        }

        deser.deserialize(ValueVisitor)
    }
}

/// Deserializes a decoded value into any serde supported value.
pub struct Deserializer {
    value: Option<Value>,
}

impl Deserializer {
    /// Creates a new deserializer instance for deserializing the specified JSON value.
    pub fn new(value: Value) -> Deserializer {
        Deserializer {
            value: Some(value),
        }
    }
}

impl<'a> de::Deserializer for &'a mut Deserializer {
    type Error = Error;

    fn deserialize<V: Visitor>(mut self, visitor: V) -> Result<V::Value> {
        let value = match self.value.take() {
            Some(value) => value,
            None => { return Err(Error::Syntax(ErrorCode::EOFWhileParsing)); }
        };

        match value {
            Value::None => visitor.visit_unit(),
            Value::Bool(v) => visitor.visit_bool(v),
            Value::I64(v) => visitor.visit_i64(v),
            Value::Int(v) => {
                if let Some(i) = v.to_i64() {
                    visitor.visit_i64(i)
                } else {
                    return Err(Error::Syntax(
                        ErrorCode::InvalidValue("integer too large".into())));
                }
            },
            Value::F64(v) => visitor.visit_f64(v),
            Value::Bytes(v) => visitor.visit_byte_buf(v),
            Value::String(v) => visitor.visit_string(v),
            Value::List(v) => {
                let len = v.len();
                visitor.visit_seq(SeqDeserializer {
                    de: &mut self,
                    iter: v.into_iter(),
                    len: len,
                })
            },
            Value::Tuple(v) => {
                visitor.visit_seq(SeqDeserializer {
                    de: &mut self,
                    len: v.len(),
                    iter: v.into_iter(),
                })
            }
            Value::Set(v) | Value::FrozenSet(v) => {
                let v: Vec<_> = v.into_iter().map(HashableValue::into_value).collect();
                visitor.visit_seq(SeqDeserializer {
                    de: &mut self,
                    len: v.len(),
                    iter: v.into_iter(),
                })
            },
            Value::Dict(v) => {
                let len = v.len();
                visitor.visit_map(MapDeserializer {
                    de: &mut self,
                    iter: v.into_iter(),
                    value: None,
                    len: len,
                })
            },
        }
    }

    #[inline]
    fn deserialize_option<V: Visitor>(self, visitor: V) -> Result<V::Value> {
        match self.value {
            Some(Value::None) => visitor.visit_none(),
            Some(_) => visitor.visit_some(self),
            None => Err(Error::Syntax(ErrorCode::EOFWhileParsing)),
        }
    }

    #[inline]
    fn deserialize_newtype_struct<V: Visitor>(self, _name: &str, visitor: V) -> Result<V::Value> {
        visitor.visit_newtype_struct(self)
    }

    #[inline]
    fn deserialize_enum<V: de::Visitor>(self, _name: &str,
                                        _variants: &'static [&'static str],
                                        visitor: V) -> Result<V::Value> {
        visitor.visit_enum(self)
    }

    forward_to_deserialize! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string unit seq
        seq_fixed_size bytes byte_buf map unit_struct tuple_struct struct
        struct_field tuple ignored_any
    }
}

impl<'a> de::EnumVisitor for &'a mut Deserializer {
    type Error = Error;
    type Variant = Self;

    fn visit_variant_seed<T: de::DeserializeSeed>(self, seed: T) -> Result<(T::Value, Self::Variant)> {
        match self.value.take() {
            Some(Value::Tuple(mut v)) => {
                if v.len() == 2 {
                    let args = v.pop();
                    self.value = v.pop();
                    let res = try!(seed.deserialize(&mut *self));
                    self.value = args;
                    Ok((res, self))
                } else {
                    self.value = v.pop();
                    let res = try!(seed.deserialize(&mut *self));
                    Ok((res, self))
                }
            }
            _ => Err(Error::Syntax(ErrorCode::Structure("enums must be tuples".into())))
        }
    }
}

impl<'a> de::VariantVisitor for &'a mut Deserializer {
    type Error = Error;

    fn visit_unit(self) -> Result<()> {
        Ok(())
    }

    fn visit_newtype_seed<T: de::DeserializeSeed>(self, seed: T) -> Result<T::Value> {
        seed.deserialize(self)
    }

    fn visit_tuple<V: Visitor>(self, _len: usize, visitor: V) -> Result<V::Value> {
        de::Deserializer::deserialize(self, visitor)
    }

    fn visit_struct<V: Visitor>(self, _fields: &'static [&'static str], visitor: V)
                                -> Result<V::Value> {
        de::Deserializer::deserialize(self, visitor)
    }
}

struct SeqDeserializer<'a> {
    de: &'a mut Deserializer,
    iter: vec::IntoIter<Value>,
    len: usize,
}

impl<'a> de::SeqVisitor for SeqDeserializer<'a> {
    type Error = Error;

    fn visit_seed<T: de::DeserializeSeed>(&mut self, seed: T) -> Result<Option<T::Value>> {
        match self.iter.next() {
            Some(value) => {
                self.len -= 1;
                self.de.value = Some(value);
                Ok(Some(try!(seed.deserialize(&mut *self.de))))
            }
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

struct MapDeserializer<'a> {
    de: &'a mut Deserializer,
    iter: btree_map::IntoIter<HashableValue, Value>,
    value: Option<Value>,
    len: usize,
}

impl<'a> de::MapVisitor for MapDeserializer<'a> {
    type Error = Error;

    fn visit_key_seed<T: de::DeserializeSeed>(&mut self, seed: T) -> Result<Option<T::Value>> {
        match self.iter.next() {
            Some((key, value)) => {
                self.len -= 1;
                self.value = Some(value);
                self.de.value = Some(key.into_value());
                Ok(Some(try!(seed.deserialize(&mut *self.de))))
            }
            None => Ok(None),
        }
    }

    fn visit_value_seed<T: de::DeserializeSeed>(&mut self, seed: T) -> Result<T::Value> {
        let value = self.value.take().unwrap();
        self.de.value = Some(value);
        Ok(try!(seed.deserialize(&mut *self.de)))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}


/// Create a `serde::Serializer` that serializes a `Serialize`e into a `Value`.
pub struct Serializer;

impl Default for Serializer {
    fn default() -> Self {
        Serializer
    }
}

pub struct SerializeSeq<'a> {
    ser: &'a mut Serializer,
    state: Vec<Value>,
}

impl<'a> ser::SerializeSeq for SerializeSeq<'a> {
    type Ok = Value;
    type Error = Error;

    #[inline]
    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        self.state.push(try!(value.serialize(&mut *self.ser)));
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Value> {
        Ok(Value::List(self.state))
    }
}

impl<'a> ser::SerializeTuple for SerializeSeq<'a> {
    type Ok = Value;
    type Error = Error;

    #[inline]
    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    #[inline]
    fn end(self) -> Result<Value> {
        Ok(Value::Tuple(self.state))
    }
}

impl<'a> ser::SerializeTupleStruct for SerializeSeq<'a> {
    type Ok = Value;
    type Error = Error;

    #[inline]
    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    #[inline]
    fn end(self) -> Result<Value> {
        Ok(Value::Tuple(self.state))
    }
}

pub struct SerializeTupleVariant<'a> {
    ser: &'a mut Serializer,
    variant: &'a str,
    state: Vec<Value>,
}

impl<'a> ser::SerializeTupleVariant for SerializeTupleVariant<'a> {
    type Ok = Value;
    type Error = Error;

    #[inline]
    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        self.state.push(try!(value.serialize(&mut *self.ser)));
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Value> {
        let var = try!(self.variant.serialize(&mut *self.ser));
        let seq = Value::List(self.state);
        Ok(Value::Tuple(vec![var, seq]))
    }
}

pub struct SerializeMap<'a> {
    ser: &'a mut Serializer,
    variant: &'a str,
    key: Option<HashableValue>,
    state: BTreeMap<HashableValue, Value>,
}

impl<'a> ser::SerializeMap for SerializeMap<'a> {
    type Ok = Value;
    type Error = Error;

    #[inline]
    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<()> {
        let key = try!(key.serialize(&mut *self.ser));
        self.key = Some(try!(key.into_hashable()));
        Ok(())
    }

    #[inline]
    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
        let value = try!(value.serialize(&mut *self.ser));
        let key = self.key.take().unwrap();
        self.state.insert(key, value);
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Value> {
        Ok(Value::Dict(self.state))
    }
}

impl<'a> ser::SerializeStruct for SerializeMap<'a> {
    type Ok = Value;
    type Error = Error;

    #[inline]
    fn serialize_field<T: Serialize + ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()> {
        let key = try!(key.serialize(&mut *self.ser));
        let key = try!(key.into_hashable());
        let value = try!(value.serialize(&mut *self.ser));
        self.state.insert(key, value);
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Value> {
        Ok(Value::Dict(self.state))
    }
}

impl<'a> ser::SerializeStructVariant for SerializeMap<'a> {
    type Ok = Value;
    type Error = Error;

    #[inline]
    fn serialize_field<T: Serialize + ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()> {
        ser::SerializeStruct::serialize_field(self, key, value)
    }

    #[inline]
    fn end(self) -> Result<Value> {
        let var = try!(self.variant.serialize(&mut *self.ser));
        let map = Value::Dict(self.state);
        Ok(Value::Tuple(vec![var, map]))
    }
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = Value;
    type Error = Error;

    type SerializeSeq = SerializeSeq<'a>;
    type SerializeTuple = Self::SerializeSeq;
    type SerializeTupleStruct = Self::SerializeSeq;
    type SerializeTupleVariant = SerializeTupleVariant<'a>;
    type SerializeMap = SerializeMap<'a>;
    type SerializeStruct = Self::SerializeMap;
    type SerializeStructVariant = Self::SerializeMap;

    #[inline]
    fn serialize_bool(self, value: bool) -> Result<Value> {
        Ok(Value::Bool(value))
    }

    #[inline]
    fn serialize_i8(self, v: i8) -> Result<Value> {
        self.serialize_i64(v as i64)
    }

    #[inline]
    fn serialize_i16(self, v: i16) -> Result<Value> {
        self.serialize_i64(v as i64)
    }

    #[inline]
    fn serialize_i32(self, v: i32) -> Result<Value> {
        self.serialize_i64(v as i64)
    }

    #[inline]
    fn serialize_i64(self, value: i64) -> Result<Value> {
        Ok(Value::I64(value))
    }

    #[inline]
    fn serialize_u8(self, v: u8) -> Result<Value> {
        self.serialize_u64(v as u64)
    }

    #[inline]
    fn serialize_u16(self, v: u16) -> Result<Value> {
        self.serialize_u64(v as u64)
    }

    #[inline]
    fn serialize_u32(self, v: u32) -> Result<Value> {
        self.serialize_u64(v as u64)
    }

    #[inline]
    fn serialize_u64(self, value: u64) -> Result<Value> {
        Ok(if value < 0x8000_0000_0000_0000 {
            Value::I64(value as i64)
        } else {
            Value::Int(BigInt::from(value))
        })
    }

    #[inline]
    fn serialize_f32(self, value: f32) -> Result<Value> {
        self.serialize_f64(value as f64)
    }

    #[inline]
    fn serialize_f64(self, value: f64) -> Result<Value> {
        Ok(Value::F64(value))
    }

    #[inline]
    fn serialize_char(self, value: char) -> Result<Value> {
        let mut s = String::new();
        s.push(value);
        Ok(Value::String(s))
    }

    #[inline]
    fn serialize_str(self, value: &str) -> Result<Value> {
        Ok(Value::String(String::from(value)))
    }

    #[inline]
    fn serialize_bytes(self, value: &[u8]) -> Result<Value> {
        Ok(Value::Bytes(value.to_vec()))
    }

    #[inline]
    fn serialize_unit(self) -> Result<Value> {
        Ok(Value::None)
    }

    #[inline]
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Value> {
        Ok(Value::Tuple(vec![]))
    }

    #[inline]
    fn serialize_unit_variant(self, _name: &str, _variant_index: usize, variant: &str)
                              -> Result<Value> {
        Ok(Value::Tuple(vec![Value::String(variant.into())]))
    }

    #[inline]
    fn serialize_newtype_struct<T: Serialize + ?Sized>(self, _name: &'static str, value: &T)
                                                       -> Result<Value> {
        value.serialize(self)
    }

    #[inline]
    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _name: &str, _variant_index: usize,
                                                        variant: &str, value: &T) -> Result<Value> {
        Ok(Value::Tuple(vec![Value::String(variant.into()),
                             try!(to_value(&value))]))
    }

    #[inline]
    fn serialize_none(self) -> Result<Value> {
        self.serialize_unit()
    }

    #[inline]
    fn serialize_some<V: Serialize + ?Sized>(self, value: &V) -> Result<Value> {
        value.serialize(self)
    }

    #[inline]
    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SerializeSeq { ser: self, state: vec![] })
    }

    #[inline]
    fn serialize_seq_fixed_size(self, len: usize) -> Result<Self::SerializeSeq> {
        Ok(SerializeSeq { ser: self, state: Vec::with_capacity(len) })
    }

    #[inline]
    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq_fixed_size(len)
    }

    #[inline]
    fn serialize_tuple_struct(self, _name: &'static str, len: usize)
                              -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq_fixed_size(len)
    }

    #[inline]
    fn serialize_tuple_variant(self, _name: &str, _variant_index: usize, variant: &'a str,
                               len: usize) -> Result<Self::SerializeTupleVariant> {
        Ok(SerializeTupleVariant { ser: self, variant: variant, state: Vec::with_capacity(len) })
    }

    #[inline]
    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(SerializeMap { ser: self, variant: "", key: None, state: BTreeMap::new() })
    }

    #[inline]
    fn serialize_struct(self, _name: &'static str, _len: usize)
                        -> Result<Self::SerializeStruct> {
        Ok(SerializeMap { ser: self, variant: "", key: None, state: BTreeMap::new() })
    }

    #[inline]
    fn serialize_struct_variant(self, _name: &str, _variant_index: usize, variant: &'a str,
                                _len: usize) -> Result<Self::SerializeStructVariant> {
        Ok(SerializeMap { ser: self, variant: variant, key: None, state: BTreeMap::new() })
    }
}


/// Serialize any serde serializable object into a `value::Value`.
pub fn to_value<T: Serialize + ?Sized>(value: &T) -> Result<Value> {
    value.serialize(&mut Serializer)
}

/// Deserialize a `value::Value` from any serde deserializable object.
pub fn from_value<T: de::Deserialize>(value: Value) -> Result<T> {
    let mut de = Deserializer::new(value);
    de::Deserialize::deserialize(&mut de)
}
