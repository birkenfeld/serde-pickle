//! Python values

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::collections::btree_map;
use std::vec;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use serde::{ser, de};

use error::Error;

#[derive(Clone, Debug)]
pub enum Value {
    /// None
    None,
    /// Boolean
    Bool(bool),
    /// Normal-sized integer
    I64(i64),
    /// Big integer
    Int(BigInt),
    /// Float
    F64(f64),
    /// Bytestring
    Bytes(Vec<u8>),
    /// Unicode string
    String(String),
    /// List
    List(Vec<Value>),
    /// Tuple
    Tuple(Box<[Value]>),
    /// Set
    Set(BTreeSet<HashableValue>),
    /// Frozen (immutable) set
    FrozenSet(BTreeSet<HashableValue>),
    /// Dictionary
    Dict(BTreeMap<HashableValue, Value>),
}

#[derive(Clone, Debug)]
pub enum HashableValue {
    /// None
    None,
    /// Boolean
    Bool(bool),
    /// Normal-sized integer
    I64(i64),
    /// Big integer
    Int(BigInt),
    /// Float
    F64(f64),
    /// Bytestring
    Bytes(Vec<u8>),
    /// Unicode string
    String(String),
    /// Tuple
    Tuple(Box<[HashableValue]>),
    /// Frozen (immutable) set
    FrozenSet(BTreeSet<HashableValue>),
}

impl Value {
    pub fn to_hashable(self) -> Option<HashableValue> {
        match self {
            Value::None         => Some(HashableValue::None),
            Value::Bool(b)      => Some(HashableValue::Bool(b)),
            Value::I64(i)       => Some(HashableValue::I64(i)),
            Value::Int(i)       => Some(HashableValue::Int(i)),
            Value::F64(f)       => Some(HashableValue::F64(f)),
            Value::Bytes(b)     => Some(HashableValue::Bytes(b)),
            Value::String(s)    => Some(HashableValue::String(s)),
            Value::FrozenSet(v) => Some(HashableValue::FrozenSet(v)),
            Value::Tuple(v)     => values_to_hashable(v).map(HashableValue::Tuple),
            _                   => None
        }
    }
}

impl HashableValue {
    pub fn to_value(self) -> Value {
        match self {
            HashableValue::None         => Value::None,
            HashableValue::Bool(b)      => Value::Bool(b),
            HashableValue::I64(i)       => Value::I64(i),
            HashableValue::Int(i)       => Value::Int(i),
            HashableValue::F64(f)       => Value::F64(f),
            HashableValue::Bytes(b)     => Value::Bytes(b),
            HashableValue::String(s)    => Value::String(s),
            HashableValue::FrozenSet(v) => Value::FrozenSet(v),
            HashableValue::Tuple(v)     => Value::Tuple(hashable_to_values(v)),
        }
    }
}

fn values_to_hashable(values: Box<[Value]>) -> Option<Box<[HashableValue]>> {
    values.into_vec()
          .into_iter()
          .map(Value::to_hashable)
          .collect::<Option<Vec<_>>>()
          .map(Vec::into_boxed_slice)
}

fn hashable_to_values(values: Box<[HashableValue]>) -> Box<[Value]> {
    values.into_vec()
          .into_iter()
          .map(HashableValue::to_value)
          .collect::<Vec<_>>()
          .into_boxed_slice()
}

impl PartialEq for HashableValue {
    fn eq(&self, other: &HashableValue) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for HashableValue {}

impl PartialOrd for HashableValue {
    fn partial_cmp(&self, other: &HashableValue) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Implement a (more or less) consistent ordering for HashableValues
/// so that they can be added to dictionaries and sets.
///
/// This is done similar to Python 2's ordering of different types.
impl Ord for HashableValue {
    fn cmp(&self, other: &HashableValue) -> Ordering {
        use self::HashableValue::*;
        match *self {
            None => match *other {
                None => Ordering::Equal,
                _    => Ordering::Less
            },
            Bool(b) => match *other {
                Bool(b2) => b.cmp(&b2),
                None     => Ordering::Greater,
                _        => Ordering::Less
            },
            I64(i) => match *other {
                None         => Ordering::Greater,
                Bool(b)      => i.cmp(&(b as i64)),
                I64(i2)      => i.cmp(&i2),
                Int(ref bi)  => BigInt::from(i).cmp(bi),
                F64(f)       => i.cmp(&(f as i64)),
                _            => Ordering::Less
            },
            Int(ref bi) => match *other {
                None         => Ordering::Greater,
                Bool(b)      => bi.cmp(&BigInt::from(b as i64)),
                I64(i)       => bi.cmp(&BigInt::from(i)),
                Int(ref bi2) => bi.cmp(bi2),
                F64(f)       => bi.cmp(&BigInt::from(f as i64)),
                _            => Ordering::Less
            },
            F64(f) => match *other {
                None         => Ordering::Greater,
                Bool(b)      => float_ord(f, b as i64 as f64),
                I64(i)       => float_ord(f, i as f64),
                Int(ref bi)  => BigInt::from(f as i64).cmp(bi),
                F64(f2)      => float_ord(f, f2),
                _            => Ordering::Less
            },
            Bytes(ref bs) => match *other {
                String(_) | FrozenSet(_) |
                Tuple(_)       => Ordering::Less,
                Bytes(ref bs2) => bs.cmp(bs2),
                _              => Ordering::Greater
            },
            String(ref s) => match *other {
                FrozenSet(_) |
                Tuple(_)       => Ordering::Less,
                String(ref s2) => s.cmp(s2),
                _              => Ordering::Greater
            },
            FrozenSet(ref s) => match *other {
                Tuple(_)          => Ordering::Less,
                FrozenSet(ref s2) => s.cmp(s2),
                _                 => Ordering::Greater
            },
            Tuple(ref t) => match *other {
                Tuple(ref t2) => t.cmp(t2),
                _             => Ordering::Greater
            },
        }
    }
}

/// A reasonable total ordering for floats.
fn float_ord(f: f64, g: f64) -> Ordering {
    match f.partial_cmp(&g) {
        Some(o) => o,
        None    => Ordering::Less
    }
}

impl de::Deserialize for Value {
    #[inline]
    fn deserialize<D>(deser: &mut D) -> Result<Value, D::Error> where D: de::Deserializer {
        struct ValueVisitor;

        impl de::Visitor for ValueVisitor {
            type Value = Value;

            #[inline]
            fn visit_bool<E>(&mut self, value: bool) -> Result<Value, E> {
                Ok(Value::Bool(value))
            }

            #[inline]
            fn visit_i64<E>(&mut self, value: i64) -> Result<Value, E> {
                Ok(Value::I64(value))
            }

            #[inline]
            fn visit_u64<E>(&mut self, value: u64) -> Result<Value, E> {
                if value < 0x8000_0000_0000_0000 {
                    Ok(Value::I64(value as i64))
                } else {
                    Ok(Value::Int(BigInt::from(value)))
                }
            }

            #[inline]
            fn visit_f64<E>(&mut self, value: f64) -> Result<Value, E> {
                Ok(Value::F64(value))
            }

            #[inline]
            fn visit_str<E: de::Error>(&mut self, value: &str) -> Result<Value, E> {
                self.visit_string(String::from(value))
            }

            #[inline]
            fn visit_string<E>(&mut self, value: String) -> Result<Value, E> {
                Ok(Value::String(value))
            }

            #[inline]
            fn visit_none<E>(&mut self) -> Result<Value, E> {
                Ok(Value::None)
            }

            #[inline]
            fn visit_some<D>(&mut self, deser: &mut D) -> Result<Value, D::Error>
                where D: de::Deserializer,
            {
                de::Deserialize::deserialize(deser)
            }

            #[inline]
            fn visit_unit<E>(&mut self) -> Result<Value, E> {
                Ok(Value::None)
            }

            #[inline]
            fn visit_seq<V>(&mut self, visitor: V) -> Result<Value, V::Error>
                where V: de::SeqVisitor,
            {
                let values = try!(de::impls::VecVisitor::new().visit_seq(visitor));
                Ok(Value::List(values))
            }

            #[inline]
            fn visit_map<V>(&mut self, visitor: V) -> Result<Value, V::Error>
                where V: de::MapVisitor,
            {
                let values = try!(de::impls::BTreeMapVisitor::new().visit_map(visitor));
                Ok(Value::Dict(values))
            }
        }

        deser.deserialize(ValueVisitor)
    }
}

impl de::Deserialize for HashableValue {
    #[inline]
    fn deserialize<D>(deser: &mut D) -> Result<HashableValue, D::Error> where D: de::Deserializer {
        struct ValueVisitor;

        impl de::Visitor for ValueVisitor {
            type Value = HashableValue;

            #[inline]
            fn visit_bool<E>(&mut self, value: bool) -> Result<HashableValue, E> {
                Ok(HashableValue::Bool(value))
            }

            #[inline]
            fn visit_i64<E>(&mut self, value: i64) -> Result<HashableValue, E> {
                Ok(HashableValue::I64(value))
            }

            #[inline]
            fn visit_u64<E>(&mut self, value: u64) -> Result<HashableValue, E> {
                if value < 0x8000_0000_0000_0000 {
                    Ok(HashableValue::I64(value as i64))
                } else {
                    Ok(HashableValue::Int(BigInt::from(value)))
                }
            }

            #[inline]
            fn visit_f64<E>(&mut self, value: f64) -> Result<HashableValue, E> {
                Ok(HashableValue::F64(value))
            }

            #[inline]
            fn visit_str<E: de::Error>(&mut self, value: &str) -> Result<HashableValue, E> {
                self.visit_string(String::from(value))
            }

            #[inline]
            fn visit_string<E>(&mut self, value: String) -> Result<HashableValue, E> {
                Ok(HashableValue::String(value))
            }

            #[inline]
            fn visit_none<E>(&mut self) -> Result<HashableValue, E> {
                Ok(HashableValue::None)
            }

            #[inline]
            fn visit_some<D>(&mut self, deser: &mut D) -> Result<HashableValue, D::Error>
                where D: de::Deserializer,
            {
                de::Deserialize::deserialize(deser)
            }

            #[inline]
            fn visit_unit<E>(&mut self) -> Result<HashableValue, E> {
                Ok(HashableValue::None)
            }

            #[inline]
            fn visit_seq<V>(&mut self, visitor: V) -> Result<HashableValue, V::Error>
                where V: de::SeqVisitor,
            {
                let values = try!(de::impls::VecVisitor::new().visit_seq(visitor));
                Ok(HashableValue::Tuple(values.into_boxed_slice()))
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

    fn deserialize<V>(&mut self, mut visitor: V) -> Result<V::Value, Error>
        where V: de::Visitor
    {
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
                let v = v.into_vec();
                visitor.visit_seq(SeqDeserializer {
                    de: self,
                    len: v.len(),
                    iter: v.into_iter(),
                })
            }
            Value::Set(v) | Value::FrozenSet(v) => {
                let v: Vec<_> = v.into_iter().map(HashableValue::to_value).collect();
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
    fn deserialize_option<V>(&mut self, mut visitor: V) -> Result<V::Value, Error>
        where V: de::Visitor,
    {
        match self.value {
            Some(Value::None) => visitor.visit_none(),
            Some(_) => visitor.visit_some(self),
            None => Err(de::Error::end_of_stream()),
        }
    }
}

struct SeqDeserializer<'a> {
    de: &'a mut Deserializer,
    iter: vec::IntoIter<Value>,
    len: usize,
}

impl<'a> de::Deserializer for SeqDeserializer<'a> {
    type Error = Error;

    #[inline]
    fn deserialize<V>(&mut self, mut visitor: V) -> Result<V::Value, Error>
        where V: de::Visitor,
    {
        visitor.visit_seq(self)
    }
}

impl<'a> de::SeqVisitor for SeqDeserializer<'a> {
    type Error = Error;

    fn visit<T>(&mut self) -> Result<Option<T>, Error>
        where T: de::Deserialize
    {
        match self.iter.next() {
            Some(value) => {
                self.len -= 1;
                self.de.value = Some(value);
                Ok(Some(try!(de::Deserialize::deserialize(self.de))))
            }
            None => Ok(None),
        }
    }

    fn end(&mut self) -> Result<(), Error> {
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

impl<'a> de::Deserializer for MapDeserializer<'a> {
    type Error = Error;

    #[inline]
    fn deserialize<V>(&mut self, mut visitor: V) -> Result<V::Value, Error>
        where V: de::Visitor,
    {
        visitor.visit_map(self)
    }
}

impl<'a> de::MapVisitor for MapDeserializer<'a> {
    type Error = Error;

    fn visit_key<T>(&mut self) -> Result<Option<T>, Error>
        where T: de::Deserialize
    {
        match self.iter.next() {
            Some((key, value)) => {
                self.len -= 1;
                self.value = Some(value);
                self.de.value = Some(key.to_value());
                Ok(Some(try!(de::Deserialize::deserialize(self.de))))
            }
            None => Ok(None),
        }
    }

    fn visit_value<T>(&mut self) -> Result<T, Error>
        where T: de::Deserialize
    {
        let value = self.value.take().unwrap();
        self.de.value = Some(value);
        Ok(try!(de::Deserialize::deserialize(self.de)))
    }

    fn end(&mut self) -> Result<(), Error> {
        if self.len == 0 {
            Ok(())
        } else {
            Err(de::Error::invalid_length(self.len))
        }
    }

    fn missing_field<V>(&mut self, field: &'static str) -> Result<V, Error>
        where V: de::Deserialize,
    {
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
    pub fn unwrap(mut self) -> Value {
        self.values.pop().expect("expected a value")
    }
}

impl ser::Serializer for Serializer {
    type Error = Error;

    #[inline]
    fn serialize_bool(&mut self, value: bool) -> Result<(), Error> {
        self.values.push(Value::Bool(value));
        Ok(())
    }

    #[inline]
    fn serialize_i64(&mut self, value: i64) -> Result<(), Error> {
        self.values.push(Value::I64(value));
        Ok(())
    }

    #[inline]
    fn serialize_u64(&mut self, value: u64) -> Result<(), Error> {
        if value < 0x8000_0000_0000_0000 {
            self.values.push(Value::I64(value as i64));
        } else {
            self.values.push(Value::Int(BigInt::from(value)));
        }
        Ok(())
    }

    #[inline]
    fn serialize_f64(&mut self, value: f64) -> Result<(), Error> {
        self.values.push(Value::F64(value));
        Ok(())
    }

    #[inline]
    fn serialize_char(&mut self, value: char) -> Result<(), Error> {
        let mut s = String::new();
        s.push(value);
        self.values.push(Value::String(s));
        Ok(())
    }

    #[inline]
    fn serialize_str(&mut self, value: &str) -> Result<(), Error> {
        self.values.push(Value::String(String::from(value)));
        Ok(())
    }

    #[inline]
    fn serialize_bytes(&mut self, value: &[u8]) -> Result<(), Error> {
        self.values.push(Value::Bytes(value.to_vec()));
        Ok(())
    }

    #[inline]
    fn serialize_none(&mut self) -> Result<(), Error> {
        self.serialize_unit()
    }

    #[inline]
    fn serialize_some<V>(&mut self, value: V) -> Result<(), Error>
        where V: ser::Serialize,
    {
        value.serialize(self)
    }

    #[inline]
    fn serialize_unit(&mut self) -> Result<(), Error> {
        self.values.push(Value::None);
        Ok(())
    }

    #[inline]
    fn serialize_unit_variant(&mut self, _name: &str, _variant_index: usize, variant: &str)
                              -> Result<(), Error> {
        self.values.push(Value::Tuple(Box::new([Value::String(variant.into())])));
        Ok(())
    }

    #[inline]
    fn serialize_newtype_variant<T>(&mut self, _name: &str, _variant_index: usize, variant: &str,
                                    value: T) -> Result<(), Error> where T: ser::Serialize {
        self.values.push(Value::Tuple(Box::new([Value::String(variant.into()),
                                         to_value(&value)])));
        Ok(())
    }

    #[inline]
    fn serialize_tuple_variant<V>(&mut self, _name: &str, _variant_index: usize, variant: &str,
                                  visitor: V) -> Result<(), Error> where V: ser::SeqVisitor {
        let mut ser = Serializer::new();
        try!(ser.serialize_seq(visitor));
        self.values.push(Value::Tuple(Box::new([Value::String(variant.into()), ser.unwrap()])));
        Ok(())
    }

    #[inline]
    fn serialize_struct_variant<V>(&mut self, _name: &str, _variant_index: usize, variant: &str,
                                   visitor: V) -> Result<(), Error> where V: ser::MapVisitor {
        let mut ser = Serializer::new();
        try!(ser.serialize_map(visitor));
        self.values.push(Value::Tuple(Box::new([Value::String(variant.into()), ser.unwrap()])));
        Ok(())
    }

    #[inline]
    fn serialize_seq<V>(&mut self, mut visitor: V) -> Result<(), Error> where V: ser::SeqVisitor {
        let mut ser = Serializer::new();
        while let Some(()) = try!(visitor.visit(&mut ser)) { }
        self.values.push(Value::List(ser.values));
        Ok(())
    }

    #[inline]
    fn serialize_seq_elt<T>(&mut self, value: T) -> Result<(), Error>
        where T: ser::Serialize,
    {
        value.serialize(self)
    }

    #[inline]
    fn serialize_map<V>(&mut self, mut visitor: V) -> Result<(), Error> where V: ser::MapVisitor {
        let mut ser = Serializer::new();
        while let Some(()) = try!(visitor.visit(&mut ser)) { }
        let mut map = BTreeMap::new();
        let mut iter = ser.values.into_iter();
        while let Some(key) = iter.next() {
            let value = iter.next().unwrap();
            let key = match key.to_hashable() {
                Some(v) => v,
                None => return Err(ser::Error::custom("Map key not valid in Python dict")),
            };
            map.insert(key, value);
        }
        self.values.push(Value::Dict(map));
        Ok(())
    }

    #[inline]
    fn serialize_map_elt<K, V>(&mut self, key: K, value: V)
                               -> Result<(), Error> where K: ser::Serialize, V: ser::Serialize {
        try!(key.serialize(self));
        value.serialize(self)
    }
}


pub fn to_value<T: ser::Serialize + ?Sized>(value: &T) -> Value {
    let mut ser = Serializer::new();
    value.serialize(&mut ser).ok().unwrap();
    ser.unwrap()
}

pub fn from_value<T: de::Deserialize>(value: Value) -> Result<T, Error> {
    let mut de = Deserializer::new(value);
    de::Deserialize::deserialize(&mut de)
}
