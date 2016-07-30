// Copyright (c) 2015-2016 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

//! Serializer/Deserializer implementations for `value::Value`.

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
    fn deserialize<D: de::Deserializer>(deser: &mut D) -> StdResult<Value, D::Error> {
        struct ValueVisitor;

        impl Visitor for ValueVisitor {
            type Value = Value;

            #[inline]
            fn visit_bool<E>(&mut self, value: bool) -> StdResult<Value, E> {
                Ok(Value::Bool(value))
            }

            #[inline]
            fn visit_i64<E>(&mut self, value: i64) -> StdResult<Value, E> {
                Ok(Value::I64(value))
            }

            #[inline]
            fn visit_u64<E>(&mut self, value: u64) -> StdResult<Value, E> {
                if value < 0x8000_0000_0000_0000 {
                    Ok(Value::I64(value as i64))
                } else {
                    Ok(Value::Int(BigInt::from(value)))
                }
            }

            #[inline]
            fn visit_f64<E>(&mut self, value: f64) -> StdResult<Value, E> {
                Ok(Value::F64(value))
            }

            #[inline]
            fn visit_str<E: de::Error>(&mut self, value: &str) -> StdResult<Value, E> {
                self.visit_string(String::from(value))
            }

            #[inline]
            fn visit_string<E>(&mut self, value: String) -> StdResult<Value, E> {
                Ok(Value::String(value))
            }

            #[inline]
            fn visit_bytes<E: de::Error>(&mut self, value: &[u8]) -> StdResult<Value, E> {
                self.visit_byte_buf(value.to_vec())
            }

            #[inline]
            fn visit_byte_buf<E: de::Error>(&mut self, value: Vec<u8>) -> StdResult<Value, E> {
                Ok(Value::Bytes(value))
            }

            #[inline]
            fn visit_none<E>(&mut self) -> StdResult<Value, E> {
                Ok(Value::None)
            }

            #[inline]
            fn visit_some<D: de::Deserializer>(&mut self, deser: &mut D)
                                               -> StdResult<Value, D::Error> {
                de::Deserialize::deserialize(deser)
            }

            #[inline]
            fn visit_unit<E>(&mut self) -> StdResult<Value, E> {
                Ok(Value::None)
            }

            #[inline]
            fn visit_seq<V: de::SeqVisitor>(&mut self, visitor: V) -> StdResult<Value, V::Error> {
                let values = try!(de::impls::VecVisitor::new().visit_seq(visitor));
                Ok(Value::List(values))
            }

            #[inline]
            fn visit_map<V: de::MapVisitor>(&mut self, visitor: V) -> StdResult<Value, V::Error> {
                let values = try!(de::impls::BTreeMapVisitor::new().visit_map(visitor));
                Ok(Value::Dict(values))
            }
        }

        deser.deserialize(ValueVisitor)
    }
}

impl de::Deserialize for HashableValue {
    #[inline]
    fn deserialize<D: de::Deserializer>(deser: &mut D) -> StdResult<HashableValue, D::Error> {
        struct ValueVisitor;

        impl Visitor for ValueVisitor {
            type Value = HashableValue;

            #[inline]
            fn visit_bool<E>(&mut self, value: bool) -> StdResult<HashableValue, E> {
                Ok(HashableValue::Bool(value))
            }

            #[inline]
            fn visit_i64<E>(&mut self, value: i64) -> StdResult<HashableValue, E> {
                Ok(HashableValue::I64(value))
            }

            #[inline]
            fn visit_u64<E>(&mut self, value: u64) -> StdResult<HashableValue, E> {
                if value < 0x8000_0000_0000_0000 {
                    Ok(HashableValue::I64(value as i64))
                } else {
                    Ok(HashableValue::Int(BigInt::from(value)))
                }
            }

            #[inline]
            fn visit_f64<E>(&mut self, value: f64) -> StdResult<HashableValue, E> {
                Ok(HashableValue::F64(value))
            }

            #[inline]
            fn visit_str<E: de::Error>(&mut self, value: &str) -> StdResult<HashableValue, E> {
                self.visit_string(String::from(value))
            }

            #[inline]
            fn visit_string<E>(&mut self, value: String) -> StdResult<HashableValue, E> {
                Ok(HashableValue::String(value))
            }

            #[inline]
            fn visit_none<E>(&mut self) -> StdResult<HashableValue, E> {
                Ok(HashableValue::None)
            }

            #[inline]
            fn visit_some<D: de::Deserializer>(&mut self, deser: &mut D)
                                               -> StdResult<HashableValue, D::Error> {
                de::Deserialize::deserialize(deser)
            }

            #[inline]
            fn visit_unit<E>(&mut self) -> StdResult<HashableValue, E> {
                Ok(HashableValue::None)
            }

            #[inline]
            fn visit_seq<V: de::SeqVisitor>(&mut self, visitor: V)
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

impl de::Deserializer for Deserializer {
    type Error = Error;

    fn deserialize<V: Visitor>(&mut self, mut visitor: V) -> Result<V::Value> {
        let value = match self.value.take() {
            Some(value) => value,
            None => { return Err(de::Error::end_of_stream()); }
        };

        match value {
            Value::None => visitor.visit_unit(),
            Value::Bool(v) => visitor.visit_bool(v),
            Value::I64(v) => visitor.visit_i64(v),
            Value::Int(v) => {
                if let Some(i) = v.to_i64() {
                    visitor.visit_i64(i)
                } else {
                    return Err(de::Error::invalid_value("integer too large"));
                }
            },
            Value::F64(v) => visitor.visit_f64(v),
            Value::Bytes(v) => visitor.visit_byte_buf(v),
            Value::String(v) => visitor.visit_string(v),
            Value::List(v) => {
                let len = v.len();
                visitor.visit_seq(SeqDeserializer {
                    de: self,
                    iter: v.into_iter(),
                    len: len,
                })
            },
            Value::Tuple(v) => {
                visitor.visit_seq(SeqDeserializer {
                    de: self,
                    len: v.len(),
                    iter: v.into_iter(),
                })
            }
            Value::Set(v) | Value::FrozenSet(v) => {
                let v: Vec<_> = v.into_iter().map(HashableValue::into_value).collect();
                visitor.visit_seq(SeqDeserializer {
                    de: self,
                    len: v.len(),
                    iter: v.into_iter(),
                })
            },
            Value::Dict(v) => {
                let len = v.len();
                visitor.visit_map(MapDeserializer {
                    de: self,
                    iter: v.into_iter(),
                    value: None,
                    len: len,
                })
            },
        }
    }

    #[inline]
    fn deserialize_bool<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_usize<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_u64(visitor)
    }

    #[inline]
    fn deserialize_u8<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_u64(visitor)
    }

    #[inline]
    fn deserialize_u16<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_u64(visitor)
    }

    #[inline]
    fn deserialize_u32<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_u64(visitor)
    }

    #[inline]
    fn deserialize_u64<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_isize<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_i64(visitor)
    }

    #[inline]
    fn deserialize_i8<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_i64(visitor)
    }

    #[inline]
    fn deserialize_i16<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_i64(visitor)
    }

    #[inline]
    fn deserialize_i32<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_i64(visitor)
    }

    #[inline]
    fn deserialize_i64<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_f32<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_f64(visitor)
    }

    #[inline]
    fn deserialize_f64<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_char<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_f64(visitor)
    }

    #[inline]
    fn deserialize_str<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_string<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_unit<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_option<V: Visitor>(&mut self, mut visitor: V) -> Result<V::Value> {
        match self.value {
            Some(Value::None) => visitor.visit_none(),
            Some(_) => visitor.visit_some(self),
            None => Err(de::Error::end_of_stream()),
        }
    }

    #[inline]
    fn deserialize_seq<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_seq_fixed_size<V: Visitor>(&mut self, _len: usize, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_bytes<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize_seq(visitor)
    }

    #[inline]
    fn deserialize_map<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_unit_struct<V: Visitor>(&mut self, _name: &str, visitor: V) -> Result<V::Value> {
        self.deserialize_unit(visitor)
    }

    #[inline]
    fn deserialize_newtype_struct<V: Visitor>(&mut self, _name: &str, mut visitor: V) -> Result<V::Value> {
        visitor.visit_newtype_struct(self)
    }

    #[inline]
    fn deserialize_tuple_struct<V: Visitor>(&mut self, _name: &'static str,
                                            _len: usize, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_struct<V: Visitor>(&mut self, _name: &'static str,
                                      _fields: &'static [&'static str], visitor: V) -> Result<V::Value> {
        self.deserialize_map(visitor)
    }

    #[inline]
    fn deserialize_struct_field<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }

    #[inline]
    fn deserialize_tuple<V: Visitor>(&mut self, _len: usize, visitor: V) -> Result<V::Value> {
        self.deserialize_seq(visitor)
    }

    #[inline]
    fn deserialize_enum<V: de::EnumVisitor>(&mut self, _name: &str,
                                            _variants: &'static [&'static str],
                                            mut visitor: V) -> Result<V::Value> {
        visitor.visit(self)
    }

    #[inline]
    fn deserialize_ignored_any<V: Visitor>(&mut self, visitor: V) -> Result<V::Value> {
        self.deserialize(visitor)
    }
}

impl de::VariantVisitor for Deserializer {
    type Error = Error;

    fn visit_variant<T: de::Deserialize>(&mut self) -> Result<T> {
        match self.value.take() {
            Some(Value::Tuple(mut v)) => {
                if v.len() == 2 {
                    let args = v.pop();
                    self.value = v.pop();
                    let res = de::Deserialize::deserialize(self);
                    self.value = args;
                    res
                } else {
                    self.value = v.pop();
                    de::Deserialize::deserialize(self)
                }
            }
            _ => Err(Error::Syntax(ErrorCode::Custom("enums must be tuples".into())))
        }
    }

    fn visit_unit(&mut self) -> Result<()> {
        Ok(())
    }

    fn visit_newtype<T: de::Deserialize>(&mut self) -> Result<T> {
        de::Deserialize::deserialize(self)
    }

    fn visit_tuple<V: Visitor>(&mut self, _len: usize, visitor: V) -> Result<V::Value> {
        de::Deserializer::deserialize(self, visitor)
    }

    fn visit_struct<V: Visitor>(&mut self, _fields: &'static [&'static str], visitor: V)
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

    fn visit<T: de::Deserialize>(&mut self) -> Result<Option<T>> {
        match self.iter.next() {
            Some(value) => {
                self.len -= 1;
                self.de.value = Some(value);
                Ok(Some(try!(de::Deserialize::deserialize(self.de))))
            }
            None => Ok(None),
        }
    }

    fn end(&mut self) -> Result<()> {
        if self.len == 0 {
            Ok(())
        } else {
            Err(de::Error::invalid_length(self.len))
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

    fn visit_key<T: de::Deserialize>(&mut self) -> Result<Option<T>> {
        match self.iter.next() {
            Some((key, value)) => {
                self.len -= 1;
                self.value = Some(value);
                self.de.value = Some(key.into_value());
                Ok(Some(try!(de::Deserialize::deserialize(self.de))))
            }
            None => Ok(None),
        }
    }

    fn visit_value<T: de::Deserialize>(&mut self) -> Result<T> {
        let value = self.value.take().unwrap();
        self.de.value = Some(value);
        Ok(try!(de::Deserialize::deserialize(self.de)))
    }

    fn end(&mut self) -> Result<()> {
        if self.len == 0 {
            Ok(())
        } else {
            Err(de::Error::invalid_length(self.len))
        }
    }

    fn missing_field<V: de::Deserialize>(&mut self, field: &'static str) -> Result<V> {
        Err(de::Error::missing_field(field))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}


/// Create a `serde::Serializer` that serializes a `Serialize`e into a `Value`.
pub struct Serializer {
    values: Vec<Value>,
}

impl Serializer {
    /// Construct a new `Serializer`.
    pub fn new() -> Serializer {
        Serializer {
            values: Vec::with_capacity(16),
        }
    }

    /// Unwrap the `Serializer` and return the `Value`.
    pub fn finish(mut self) -> Result<Value> {
        self.values.pop().ok_or_else(|| ser::Error::custom("expected a value"))
    }
}

impl Default for Serializer {
    fn default() -> Self {
        Self::new()
    }
}

impl ser::Serializer for Serializer {
    type Error = Error;
    type SeqState = Serializer;
    type TupleState = Self::SeqState;
    type MapState = (BTreeMap<HashableValue, Value>, Serializer);
    type StructState = Self::MapState;
    type TupleStructState = Self::TupleState;
    type StructVariantState = Self::MapState;
    type TupleVariantState = Self::TupleState;

    #[inline]
    fn serialize_bool(&mut self, value: bool) -> Result<()> {
        self.values.push(Value::Bool(value));
        Ok(())
    }

    #[inline]
    fn serialize_isize(&mut self, v: isize) -> Result<()> {
        self.serialize_i64(v as i64)
    }

    #[inline]
    fn serialize_i8(&mut self, v: i8) -> Result<()> {
        self.serialize_i64(v as i64)
    }

    #[inline]
    fn serialize_i16(&mut self, v: i16) -> Result<()> {
        self.serialize_i64(v as i64)
    }

    #[inline]
    fn serialize_i32(&mut self, v: i32) -> Result<()> {
        self.serialize_i64(v as i64)
    }

    #[inline]
    fn serialize_i64(&mut self, value: i64) -> Result<()> {
        self.values.push(Value::I64(value));
        Ok(())
    }

    #[inline]
    fn serialize_usize(&mut self, v: usize) -> Result<()> {
        self.serialize_u64(v as u64)
    }

    #[inline]
    fn serialize_u8(&mut self, v: u8) -> Result<()> {
        self.serialize_u64(v as u64)
    }

    #[inline]
    fn serialize_u16(&mut self, v: u16) -> Result<()> {
        self.serialize_u64(v as u64)
    }

    #[inline]
    fn serialize_u32(&mut self, v: u32) -> Result<()> {
        self.serialize_u64(v as u64)
    }

    #[inline]
    fn serialize_u64(&mut self, value: u64) -> Result<()> {
        if value < 0x8000_0000_0000_0000 {
            self.values.push(Value::I64(value as i64));
        } else {
            self.values.push(Value::Int(BigInt::from(value)));
        }
        Ok(())
    }

    #[inline]
    fn serialize_f32(&mut self, value: f32) -> Result<()> {
        self.serialize_f64(value as f64)
    }

    #[inline]
    fn serialize_f64(&mut self, value: f64) -> Result<()> {
        self.values.push(Value::F64(value));
        Ok(())
    }

    #[inline]
    fn serialize_char(&mut self, value: char) -> Result<()> {
        let mut s = String::new();
        s.push(value);
        self.values.push(Value::String(s));
        Ok(())
    }

    #[inline]
    fn serialize_str(&mut self, value: &str) -> Result<()> {
        self.values.push(Value::String(String::from(value)));
        Ok(())
    }

    #[inline]
    fn serialize_bytes(&mut self, value: &[u8]) -> Result<()> {
        self.values.push(Value::Bytes(value.to_vec()));
        Ok(())
    }

    #[inline]
    fn serialize_none(&mut self) -> Result<()> {
        self.serialize_unit()
    }

    #[inline]
    fn serialize_some<V: Serialize>(&mut self, value: V) -> Result<()> {
        value.serialize(self)
    }

    #[inline]
    fn serialize_unit(&mut self) -> Result<()> {
        self.values.push(Value::None);
        Ok(())
    }

    #[inline]
    fn serialize_unit_variant(&mut self, _name: &str, _variant_index: usize, variant: &str)
                              -> Result<()> {
        self.values.push(Value::Tuple(vec![Value::String(variant.into())]));
        Ok(())
    }

    #[inline]
    fn serialize_newtype_variant<T: Serialize>(&mut self, _name: &str, _variant_index: usize,
                                               variant: &str, value: T) -> Result<()> {
        self.values.push(Value::Tuple(vec![Value::String(variant.into()),
                                           try!(to_value(&value))]));
        Ok(())
    }

    #[inline]
    fn serialize_tuple_variant(&mut self, _name: &str, _variant_index: usize, variant: &str,
                               len: usize) -> Result<Serializer> {
        try!(self.serialize_str(variant));
        self.serialize_seq_fixed_size(len)
    }

    #[inline]
    fn serialize_tuple_variant_elt<T: Serialize>(&mut self, state: &mut Serializer,
                                                 value: T) -> Result<()> {
        self.serialize_seq_elt(state, value)
    }

    #[inline]
    fn serialize_tuple_variant_end(&mut self, state: Serializer) -> Result<()> {
        try!(self.serialize_seq_end(state));
        let seq = self.values.pop().unwrap();
        let var = self.values.pop().unwrap();
        self.values.push(Value::Tuple(vec![var, seq]));
        Ok(())
    }

    #[inline]
    fn serialize_struct_variant(&mut self, _name: &str, _variant_index: usize, variant: &str,
                                len: usize) -> Result<Self::MapState> {
        try!(self.serialize_str(variant));
        self.serialize_map(Some(len))
    }

    #[inline]
    fn serialize_struct_variant_elt<T: Serialize>(&mut self, state: &mut Self::MapState,
                                                  key: &'static str, value: T) -> Result<()> {
        try!(self.serialize_map_key(state, key));
        self.serialize_map_value(state, value)
    }

    #[inline]
    fn serialize_struct_variant_end(&mut self, state: Self::MapState) -> Result<()> {
        try!(self.serialize_map_end(state));
        let map = self.values.pop().unwrap();
        let var = self.values.pop().unwrap();
        self.values.push(Value::Tuple(vec![var, map]));
        Ok(())
    }

    #[inline]
    fn serialize_struct(&mut self, _name: &'static str, len: usize) -> Result<Self::MapState> {
        self.serialize_map(Some(len))
    }

    #[inline]
    fn serialize_struct_elt<T: Serialize>(&mut self, state: &mut Self::MapState,
                                          key: &'static str, value: T) -> Result<()> {
        try!(self.serialize_map_key(state, key));
        self.serialize_map_value(state, value)
    }

    #[inline]
    fn serialize_struct_end(&mut self, state: Self::MapState) -> Result<()> {
        self.serialize_map_end(state)
    }

    #[inline]
    fn serialize_unit_struct(&mut self, _name: &'static str) -> Result<()> {
        self.values.push(Value::Tuple(vec![]));
        Ok(())
    }

    #[inline]
    fn serialize_newtype_struct<T: Serialize>(&mut self, _name: &'static str, value: T) -> Result<()> {
        value.serialize(self)
    }

    #[inline]
    fn serialize_tuple_struct(&mut self, _name: &'static str, len: usize) -> Result<Serializer> {
        self.serialize_tuple(len)
    }

    #[inline]
    fn serialize_tuple_struct_elt<T: Serialize>(&mut self, state: &mut Serializer, value: T) -> Result<()> {
        self.serialize_tuple_elt(state, value)
    }

    #[inline]
    fn serialize_tuple_struct_end(&mut self, state: Serializer) -> Result<()> {
        self.serialize_tuple_end(state)
    }

    #[inline]
    fn serialize_tuple(&mut self, _len: usize) -> Result<Serializer> {
        Ok(Serializer::new())
    }

    #[inline]
    fn serialize_tuple_elt<T: Serialize>(&mut self, state: &mut Serializer, value: T) -> Result<()> {
        value.serialize(state)
    }

    #[inline]
    fn serialize_tuple_end(&mut self, state: Serializer) -> Result<()> {
        self.values.push(Value::Tuple(state.values));
        Ok(())
    }

    #[inline]
    fn serialize_seq_fixed_size(&mut self, len: usize) -> Result<Serializer> {
        self.serialize_seq(Some(len))
    }

    #[inline]
    fn serialize_seq(&mut self, _len: Option<usize>) -> Result<Serializer> {
        Ok(Serializer::new())
    }

    #[inline]
    fn serialize_seq_elt<T: Serialize>(&mut self, state: &mut Serializer, value: T) -> Result<()> {
        value.serialize(state)
    }

    #[inline]
    fn serialize_seq_end(&mut self, state: Serializer) -> Result<()> {
        self.values.push(Value::List(state.values));
        Ok(())
    }

    #[inline]
    fn serialize_map(&mut self, _len: Option<usize>) -> Result<Self::MapState> {
        Ok((BTreeMap::new(), Serializer::new()))
    }

    #[inline]
    fn serialize_map_key<T: Serialize>(&mut self, state: &mut Self::MapState, key: T) -> Result<()> {
        key.serialize(&mut state.1)
    }

    #[inline]
    fn serialize_map_value<T: Serialize>(&mut self, state: &mut Self::MapState, value: T) -> Result<()> {
        try!(value.serialize(&mut state.1));
        let value = state.1.values.pop().unwrap();
        let key = try!(state.1.values.pop().unwrap().into_hashable());
        state.0.insert(key, value);
        Ok(())
    }

    #[inline]
    fn serialize_map_end(&mut self, state: Self::MapState) -> Result<()> {
        self.values.push(Value::Dict(state.0));
        Ok(())
    }
}


/// Serialize any serde serializable object into a `value::Value`.
pub fn to_value<T: Serialize + ?Sized>(value: &T) -> Result<Value> {
    let mut ser = Serializer::new();
    value.serialize(&mut ser).ok().unwrap();
    ser.finish()
}

/// Deserialize a `value::Value` from any serde deserializable object.
pub fn from_value<T: de::Deserialize>(value: Value) -> Result<T> {
    let mut de = Deserializer::new(value);
    de::Deserialize::deserialize(&mut de)
}
