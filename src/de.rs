// Copyright (c) 2015-2016 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

//! # Pickle deserialization
//!
//! Note: Serde's interface doesn't support all of Python's primitive types.  In
//! order to deserialize a pickle stream to `value::Value`, use the
//! `value_from_*` functions exported here, not the generic `from_*` functions.

use std::io;
use std::mem;
use std::str;
use std::char;
use std::vec;
use std::collections::BTreeMap;
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use byteorder::{ByteOrder, BigEndian, LittleEndian};
use serde::de;

use super::error::{Error, ErrorCode, Result};
use super::consts::*;
use super::value;

type MemoId = u32;

#[derive(Clone, Debug, PartialEq)]
enum Global {
    Set,         // builtins/__builtin__.set
    Frozenset,   // builtins/__builtin__.frozenset
    Encode,      // _codecs.encode
}

/// Our intermediate representation of a value.
///
/// The most striking difference to `value::Value` is that it contains a variant
/// for "MemoRef", which references values put into the "memo" map, and a variant
/// for module globals that we support.
///
/// We also don't use sets and maps at the Rust level, since they are not
/// needed: nothing is ever looked up in them at this stage, and Vecs are much
/// tighter in memory.
#[derive(Clone, Debug, PartialEq)]
enum Value {
    MemoRef(MemoId),
    Global(Global),
    None,
    Bool(bool),
    I64(i64),
    Int(BigInt),
    F64(f64),
    Bytes(Vec<u8>),
    String(String),
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Set(Vec<Value>),
    FrozenSet(Vec<Value>),
    Dict(Vec<(Value, Value)>),
}

struct CharIter<Iter: Iterator<Item=io::Result<u8>>> {
    rdr: Iter,
    pos: usize,
}

impl<Iter: Iterator<Item=io::Result<u8>>> Iterator for CharIter<Iter> {
    type Item = io::Result<u8>;
    fn next(&mut self) -> Option<io::Result<u8>> {
        self.pos += 1;
        self.rdr.next()
    }
}

impl<Iter: Iterator<Item=io::Result<u8>>> CharIter<Iter> {
    fn new(rdr: Iter) -> CharIter<Iter> {
        CharIter {
            rdr: rdr,
            pos: 0,
        }
    }

    fn pos(&self) -> usize { self.pos }
}

/// Decodes pickle streams into values.
pub struct Deserializer<Iter: Iterator<Item=io::Result<u8>>> {
    rdr: CharIter<Iter>,
    ch: Option<u8>,
    value: Option<Value>,
    memo: BTreeMap<MemoId, Value>,
    memo_refs: BTreeMap<MemoId, i32>,
    stack: Vec<Value>,
    stacks: Vec<Vec<Value>>,
    decode_strings: bool,
}

impl<Iter> Deserializer<Iter>
    where Iter: Iterator<Item=io::Result<u8>>
{
    pub fn new(rdr: Iter, decode_strings: bool) -> Deserializer<Iter> {
        Deserializer {
            rdr: CharIter::new(rdr),
            ch: None,
            value: None,
            memo: BTreeMap::new(),
            memo_refs: BTreeMap::new(),
            stack: Vec::with_capacity(128),
            stacks: Vec::with_capacity(16),
            decode_strings: decode_strings,
        }
    }

    pub fn end(&mut self) -> Result<()> {
        match self.ch.take() {
            Some(_) => self.error(ErrorCode::TrailingBytes),
            None => match self.rdr.next() {
                Some(Err(err)) => Err(Error::Io(err)),
                Some(Ok(_)) => self.error(ErrorCode::TrailingBytes),
                None => Ok(())
            }
        }
    }

    fn get_next_value(&mut self) -> Result<Value> {
        match self.value.take() {
            Some(v) => Ok(v),
            None => self.parse_value(),
        }
    }

    fn parse_value(&mut self) -> Result<Value> {
        loop {
            match try!(self.read_byte()) {
                // Specials
                PROTO => {
                    // Ignore this, as it is only important for instances (read the version byte).
                    try!(self.read_byte());
                }
                FRAME => {
                    // We'll ignore framing. But we still have to gobble up the length.
                    try!(self.read_bytes(8));
                }
                STOP => return self.pop(),
                MARK => {
                    let stack = mem::replace(&mut self.stack, Vec::with_capacity(128));
                    self.stacks.push(stack);
                }
                POP => {
                    if self.stack.is_empty() {
                        try!(self.pop_mark());
                    } else {
                        try!(self.pop());
                    }
                },
                POP_MARK => { try!(self.pop_mark()); },
                DUP => { let top = try!(self.top()).clone(); self.stack.push(top); },

                // Memo saving ops
                PUT => {
                    let bytes = try!(self.read_line());
                    let memo_id = match str::from_utf8(&bytes).unwrap_or("").parse() {
                        Ok(v) => v,
                        Err(_) => return self.error(ErrorCode::InvalidLiteral(bytes)),
                    };
                    try!(self.memoize(memo_id));
                }
                BINPUT => {
                    let memo_id = try!(self.read_byte());
                    try!(self.memoize(memo_id as MemoId));
                }
                LONG_BINPUT => {
                    let bytes = try!(self.read_bytes(4));
                    let memo_id = LittleEndian::read_u32(&bytes);
                    try!(self.memoize(memo_id as MemoId));
                }
                MEMOIZE => {
                    let memo_id = self.memo.len();
                    try!(self.memoize(memo_id as MemoId));
                }

                // Memo getting ops
                GET => {
                    let bytes = try!(self.read_line());
                    let memo_id = match str::from_utf8(&bytes).unwrap_or("").parse() {
                        Ok(v) => v,
                        Err(_) => return self.error(ErrorCode::InvalidLiteral(bytes)),
                    };
                    self.push_memo_ref(memo_id);
                }
                BINGET => {
                    let memo_id = try!(self.read_byte()) as MemoId;
                    self.push_memo_ref(memo_id);
                }
                LONG_BINGET => {
                    let bytes = try!(self.read_bytes(4));
                    let memo_id = LittleEndian::read_u32(&bytes);
                    self.push_memo_ref(memo_id as MemoId);
                }

                // Singletons
                NONE => self.stack.push(Value::None),
                NEWFALSE => self.stack.push(Value::Bool(false)),
                NEWTRUE => self.stack.push(Value::Bool(true)),

                // ASCII-formatted numbers
                INT => {
                    let line = try!(self.read_line());
                    // Handle protocol 1 way of spelling true/false
                    if line == b"00" {
                        self.stack.push(Value::Bool(false))
                    } else if line == b"01" {
                        self.stack.push(Value::Bool(true))
                    } else {
                        match str::from_utf8(&line).unwrap_or("").parse::<i64>() {
                            Ok(i)  => self.stack.push(Value::I64(i)),
                            Err(_) => return self.error(ErrorCode::InvalidLiteral(line.into()))
                        }
                    }
                }
                LONG => {
                    let mut line = try!(self.read_line());
                    // Remove "L" suffix.
                    if line.last() == Some(&b'L') { line.pop(); }
                    match BigInt::parse_bytes(&line, 10) {
                        Some(i)  => self.stack.push(Value::Int(i)),
                        None => return self.error(ErrorCode::InvalidLiteral(line.into()))
                    }
                }
                FLOAT => {
                    let line = try!(self.read_line());
                    match str::from_utf8(&line).unwrap_or("").parse::<f64>() {
                        Ok(f)  => self.stack.push(Value::F64(f)),
                        Err(_) => return self.error(ErrorCode::InvalidLiteral(line.into()))
                    }
                }

                // Until-EOL strings
                STRING => {
                    let line = try!(self.read_line());
                    // Remove quotes.
                    let slice = if line.len() >= 2 && line[0] == line[line.len() - 1] &&
                        (line[0] == b'"' || line[0] == b'\'') {
                            &line[1..line.len() - 1]
                        } else { &*line };
                    let string = try!(self.decode_escaped_string(slice));
                    self.stack.push(string);
                }
                UNICODE => {
                    let line = try!(self.read_line());
                    let string = try!(self.decode_escaped_unicode(&line));
                    self.stack.push(string);
                }

                // Binary-coded numbers
                BINFLOAT => {
                    let bytes = try!(self.read_bytes(8));
                    self.stack.push(Value::F64(BigEndian::read_f64(&bytes)));
                }
                BININT => {
                    let bytes = try!(self.read_bytes(4));
                    self.stack.push(Value::I64(LittleEndian::read_i32(&bytes) as i64));
                }
                BININT1 => {
                    let byte = try!(self.read_byte());
                    self.stack.push(Value::I64(byte as i64));
                }
                BININT2 => {
                    let bytes = try!(self.read_bytes(2));
                    self.stack.push(Value::I64(LittleEndian::read_u16(&bytes) as i64));
                }

                // Length-prefixed longs
                LONG1 => {
                    let bytes = try!(self.read_u8_prefixed_bytes());
                    let long = self.decode_long(bytes);
                    self.stack.push(long);
                }
                LONG4 => {
                    let bytes = try!(self.read_i32_prefixed_bytes());
                    let long = self.decode_long(bytes);
                    self.stack.push(long);
                }

                // Length-prefixed (byte)strings
                SHORT_BINBYTES => {
                    let string = try!(self.read_u8_prefixed_bytes());
                    self.stack.push(Value::Bytes(string));
                }
                BINBYTES => {
                    let string = try!(self.read_u32_prefixed_bytes());
                    self.stack.push(Value::Bytes(string));
                }
                BINBYTES8 => {
                    let string = try!(self.read_u64_prefixed_bytes());
                    self.stack.push(Value::Bytes(string));
                }
                SHORT_BINSTRING => {
                    let string = try!(self.read_u8_prefixed_bytes());
                    let decoded = try!(self.decode_string(string));
                    self.stack.push(decoded);
                }
                BINSTRING => {
                    let string = try!(self.read_i32_prefixed_bytes());
                    let decoded = try!(self.decode_string(string));
                    self.stack.push(decoded);
                }
                SHORT_BINUNICODE => {
                    let string = try!(self.read_u8_prefixed_bytes());
                    let decoded = try!(self.decode_unicode(string));
                    self.stack.push(decoded);
                }
                BINUNICODE => {
                    let string = try!(self.read_u32_prefixed_bytes());
                    let decoded = try!(self.decode_unicode(string));
                    self.stack.push(decoded);
                }
                BINUNICODE8 => {
                    let string = try!(self.read_u64_prefixed_bytes());
                    let decoded = try!(self.decode_unicode(string));
                    self.stack.push(decoded);
                }

                // Containers
                EMPTY_TUPLE => self.stack.push(Value::Tuple(Vec::new())),
                TUPLE1 => {
                    let item = try!(self.pop());
                    self.stack.push(Value::Tuple(vec![item]));
                }
                TUPLE2 => {
                    let item2 = try!(self.pop());
                    let item1 = try!(self.pop());
                    self.stack.push(Value::Tuple(vec![item1, item2]));
                }
                TUPLE3 => {
                    let item3 = try!(self.pop());
                    let item2 = try!(self.pop());
                    let item1 = try!(self.pop());
                    self.stack.push(Value::Tuple(vec![item1, item2, item3]));
                }
                TUPLE => {
                    let items = try!(self.pop_mark());
                    self.stack.push(Value::Tuple(items));
                }
                EMPTY_LIST => self.stack.push(Value::List(vec![])),
                LIST => {
                    let items = try!(self.pop_mark());
                    self.stack.push(Value::List(items));
                }
                APPEND => {
                    let pos = self.rdr.pos();
                    let value = try!(self.pop());
                    let top = try!(self.top());
                    if let &mut Value::List(ref mut list) = top {
                        list.push(value);
                    } else {
                        return Self::stack_error("list", top, pos);
                    }
                }
                APPENDS => {
                    let pos = self.rdr.pos();
                    let items = try!(self.pop_mark());
                    let top = try!(self.top());
                    if let &mut Value::List(ref mut list) = top {
                        list.extend(items);
                    } else {
                        return Self::stack_error("list", top, pos);
                    }
                }
                EMPTY_DICT => self.stack.push(Value::Dict(Vec::new())),
                DICT => {
                    let items = try!(self.pop_mark());
                    let mut dict = Vec::with_capacity(items.len() / 2);
                    let mut key = None;
                    for value in items {
                        match key.take() {
                            None      => key = Some(value),
                            Some(key) => { dict.push((key, value)); }
                        }
                    }
                    self.stack.push(Value::Dict(dict));
                }
                SETITEM => {
                    let pos = self.rdr.pos();
                    let value = try!(self.pop());
                    let key = try!(self.pop());
                    let top = try!(self.top());
                    if let &mut Value::Dict(ref mut dict) = top {
                        dict.push((key, value));
                    } else {
                        return Self::stack_error("dict", top, pos);
                    }
                }
                SETITEMS => {
                    let pos = self.rdr.pos();
                    let items = try!(self.pop_mark());
                    let top = try!(self.top());
                    if let &mut Value::Dict(ref mut dict) = top {
                        let mut key = None;
                        for value in items {
                            match key.take() {
                                None      => key = Some(value),
                                Some(key) => { dict.push((key, value)); }
                            }
                        }
                    } else {
                        return Self::stack_error("dict", top, pos);
                    }
                }
                EMPTY_SET => self.stack.push(Value::Set(Vec::new())),
                FROZENSET => {
                    let items = try!(self.pop_mark());
                    self.stack.push(Value::FrozenSet(items));
                }
                ADDITEMS => {
                    let pos = self.rdr.pos();
                    let items = try!(self.pop_mark());
                    let top = try!(self.top());
                    if let &mut Value::Set(ref mut set) = top {
                        set.extend(items);
                    } else {
                        return Self::stack_error("set", top, pos);
                    }
                }

                // Arbitrary module globals, used here for unpickling set and frozenset
                GLOBAL => {
                    let modname = try!(self.read_line());
                    let globname = try!(self.read_line());
                    try!(self.handle_global(modname, globname));
                }
                STACK_GLOBAL => {
                    let globname = match try!(self.pop()) {
                        Value::String(string) => string.into_bytes(),
                        other => return Self::stack_error("string", &other, self.rdr.pos()),
                    };
                    let modname = match try!(self.pop()) {
                        Value::String(string) => string.into_bytes(),
                        other => return Self::stack_error("string", &other, self.rdr.pos()),
                    };
                    try!(self.handle_global(modname, globname));
                }
                REDUCE => {
                    let mut argtuple = match try!(self.pop_resolve()) {
                        Value::Tuple(args) => args,
                        other => return Self::stack_error("tuple", &other, self.rdr.pos()),
                    };
                    let global = try!(self.pop_resolve());
                    match global {
                        Value::Global(Global::Set) => {
                            match self.resolve(argtuple.pop()) {
                                Some(Value::List(items)) =>
                                    self.stack.push(Value::Set(items)),
                                _ => return self.error(ErrorCode::InvalidValue("set() arg".into())),
                            }
                        }
                        Value::Global(Global::Frozenset) => {
                            match self.resolve(argtuple.pop()) {
                                Some(Value::List(items)) =>
                                    self.stack.push(Value::FrozenSet(items)),
                                _ => return self.error(ErrorCode::InvalidValue("set() arg".into())),
                            }
                        }
                        Value::Global(Global::Encode) => {
                            // Byte object encoded as _codecs.encode(x, 'latin1')
                            match self.resolve(argtuple.pop()) {  // Encoding, always latin1
                                Some(Value::String(_)) => { }
                                _ => return self.error(ErrorCode::InvalidValue("encode() arg".into())),
                            }
                            match self.resolve(argtuple.pop()) {
                                Some(Value::String(s)) => {
                                    // Now we have to convert the string to latin-1
                                    // encoded bytes.  It never contains codepoints
                                    // above 0xff.
                                    let bytes = s.chars().map(|ch| ch as u8).collect();
                                    self.stack.push(Value::Bytes(bytes));
                                }
                                _ => return self.error(ErrorCode::InvalidValue("encode() arg".into())),
                            }
                        }
                        other => return Self::stack_error("global reference", &other, self.rdr.pos()),
                    }
                }

                // Unsupported (object building, and memoizing) opcodes
                code => return self.error(ErrorCode::Unsupported(code as char))
            }
        }
    }

    fn pop(&mut self) -> Result<Value> {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None    => self.error(ErrorCode::StackUnderflow)
        }
    }

    fn pop_resolve(&mut self) -> Result<Value> {
        let top = self.stack.pop();
        match self.resolve(top) {
            Some(v) => Ok(v),
            None    => self.error(ErrorCode::StackUnderflow)
        }
    }

    fn top(&mut self) -> Result<&mut Value> {
        match self.stack.last_mut() {
            None => Err(Error::Eval(ErrorCode::StackUnderflow, self.rdr.pos())),
            // Since some operations like APPEND do things to the stack top, we
            // need to provide the reference to the "real" object here, not the
            // MemoRef variant.
            Some(&mut Value::MemoRef(n)) =>
                self.memo.get_mut(&n).ok_or(Error::Syntax(ErrorCode::MissingMemo(n))),
            Some(other_value) => Ok(other_value)
        }
    }

    fn pop_mark(&mut self) -> Result<Vec<Value>> {
        match self.stacks.pop() {
            Some(new) => Ok(mem::replace(&mut self.stack, new)),
            None      => self.error(ErrorCode::StackUnderflow)
        }
    }

    fn push_memo_ref(&mut self, memo_id: MemoId) {
        self.stack.push(Value::MemoRef(memo_id));
        let count = self.memo_refs.entry(memo_id).or_insert(0);
        *count = *count + 1;
    }

    fn memoize(&mut self, memo_id: MemoId) -> Result<()> {
        // Memoize the current stack top with the given ID.  Moves the actual
        // object into the memo, and saves a reference on the stack instead.
        let mut item = try!(self.pop());
        if let Value::MemoRef(id) = item {
            // TODO: is this even possible?
            item = try!(self.memo.get(&id).ok_or(
                Error::Eval(ErrorCode::MissingMemo(id), self.rdr.pos()))).clone();
        }
        self.memo.insert(memo_id, item);
        self.push_memo_ref(memo_id);
        Ok(())
    }

    fn resolve(&mut self, maybe_memo: Option<Value>) -> Option<Value> {
        // Resolve memo reference during stream decoding.
        match maybe_memo {
            Some(Value::MemoRef(id)) => {
                if let Some(count) = self.memo_refs.get_mut(&id) {
                    *count = *count - 1;
                }
                // We can't remove it from the memo here, since we haven't
                // decoded the whole stream yet and there may be further
                // references to the value.
                self.memo.get(&id).map(Clone::clone)
            }
            other => other
        }
    }

    fn resolve_recursive<T, F>(&mut self, id: MemoId, f: F) -> Result<T>
        where F: Fn(&mut Self, Value) -> Result<T>
    {
        // Resolve memo reference during Value deserializing.
        //
        // Take the value from the memo while visiting it.  This prevents us
        // from trying to depickle recursive structures, which we can't do
        // because our Values aren't references.
        let value = match self.memo.remove(&id) {
            Some(value) => value,
            None => return Err(Error::Syntax(ErrorCode::Recursive)),
        };
        let new_count = if let Some(count) = self.memo_refs.get_mut(&id) {
            *count = *count - 1;
            *count
        } else { 0 };
        if new_count <= 0 {
            f(self, value)
            // No need to put it back.
        } else {
            let result = f(self, value.clone());
            self.memo.insert(id, value);
            result
        }
    }

    fn read_byte(&mut self) -> Result<u8> {
        match self.ch.take() {
            Some(ch) => Ok(ch),
            None => match self.rdr.next() {
                Some(Err(err)) => Err(Error::Io(err)),
                Some(Ok(ch)) => Ok(ch),
                None => self.error(ErrorCode::EOFWhileParsing)
            }
        }
    }

    fn read_line(&mut self) -> Result<Vec<u8>> {
        let mut result = Vec::with_capacity(16);
        loop {
            match try!(self.read_byte()) {
                b'\n' => {
                    if result.last() == Some(&b'\r') { result.pop(); }
                    return Ok(result)
                }
                ch    => result.push(ch)
            }
        }
    }

    fn read_bytes(&mut self, n: u64) -> Result<Vec<u8>> {
        (0..n).map(|_| self.read_byte()).collect()
    }

    fn read_i32_prefixed_bytes(&mut self) -> Result<Vec<u8>> {
        let lenbytes = try!(self.read_bytes(4));
        match LittleEndian::read_i32(&lenbytes) {
            0          => Ok(vec![]),
            l if l < 0 => self.error(ErrorCode::NegativeLength),
            l          => self.read_bytes(l as u64)
        }
    }

    fn read_u64_prefixed_bytes(&mut self) -> Result<Vec<u8>> {
        let lenbytes = try!(self.read_bytes(8));
        self.read_bytes(LittleEndian::read_u64(&lenbytes))
    }

    fn read_u32_prefixed_bytes(&mut self) -> Result<Vec<u8>> {
        let lenbytes = try!(self.read_bytes(4));
        self.read_bytes(LittleEndian::read_u32(&lenbytes) as u64)
    }

    fn read_u8_prefixed_bytes(&mut self) -> Result<Vec<u8>> {
        let lenbyte = try!(self.read_byte());
        self.read_bytes(lenbyte as u64)
    }

    fn decode_string(&self, string: Vec<u8>) -> Result<Value> {
        if self.decode_strings {
            self.decode_unicode(string)
        } else {
            Ok(Value::Bytes(string))
        }
    }

    fn decode_escaped_string(&self, s: &[u8]) -> Result<Value> {
        // These are encoded with "normal" Python string escape rules.
        let mut result = Vec::with_capacity(s.len());
        let mut iter = s.iter();
        while let Some(&b) = iter.next() {
            match b {
                b'\\' => match iter.next() {
                    Some(&b'\\') => result.push(b'\\'),
                    Some(&b'a') => result.push(b'\x07'),
                    Some(&b'b') => result.push(b'\x08'),
                    Some(&b't') => result.push(b'\x09'),
                    Some(&b'n') => result.push(b'\x0a'),
                    Some(&b'v') => result.push(b'\x0b'),
                    Some(&b'f') => result.push(b'\x0c'),
                    Some(&b'r') => result.push(b'\x0d'),
                    Some(&b'x') => {
                        match iter.next()
                                  .and_then(|&ch1| (ch1 as char).to_digit(16))
                                  .and_then(|v1| iter.next()
                                            .and_then(|&ch2| (ch2 as char).to_digit(16))
                                            .map(|v2| 16*(v1 as u8) + (v2 as u8)))
                        {
                            Some(v) => result.push(v),
                            None => return self.error(ErrorCode::InvalidLiteral(s.into()))
                        }
                    },
                    _ => return self.error(ErrorCode::InvalidLiteral(s.into())),
                },
                _ => result.push(b)
            }
        }
        self.decode_string(result)
    }

    fn decode_unicode(&self, string: Vec<u8>) -> Result<Value> {
        match String::from_utf8(string) {
            Ok(v)  => Ok(Value::String(v)),
            Err(_) => self.error(ErrorCode::StringNotUTF8)
        }
    }

    fn decode_escaped_unicode(&self, s: &[u8]) -> Result<Value> {
        // These are encoded with "raw-unicode-escape", which only knows
        // the \uXXXX and \UYYYYYYYY escapes.  The backslash is escaped
        // in this way, too.
        let mut result = String::with_capacity(s.len());
        let mut iter = s.iter();
        while let Some(&b) = iter.next() {
            match b {
                b'\\' => {
                    let nescape = match iter.next() {
                        Some(&b'u') => 4,
                        Some(&b'U') => 8,
                        _ => return self.error(ErrorCode::InvalidLiteral(s.into())),
                    };
                    let mut accum = 0;
                    for _i in 0..nescape {
                        accum *= 16;
                        match iter.next().and_then(|&ch| (ch as char).to_digit(16)) {
                            Some(v) => accum += v,
                            None => return self.error(ErrorCode::InvalidLiteral(s.into()))
                        }
                    }
                    match char::from_u32(accum) {
                        Some(v) => result.push(v),
                        None => return self.error(ErrorCode::InvalidLiteral(s.into()))
                    }
                }
                _ => result.push(b as char)
            }
        }
        Ok(Value::String(result))
    }

    fn decode_long(&self, bytes: Vec<u8>) -> Value {
        // BigInt::from_bytes_le doesn't like a sign bit in the bytes, therefore
        // we have to extract that ourselves and do the two-s complement.
        let negative = (bytes.len() > 0) && (bytes[bytes.len() - 1] & 0x80 != 0);
        let mut val = BigInt::from_bytes_le(Sign::Plus, &bytes);
        if negative {
            val = val - (BigInt::from(1) << (bytes.len() * 8));
        }
        Value::Int(val)
    }

    fn handle_global(&mut self, modname: Vec<u8>, globname: Vec<u8>) -> Result<()> {
        match (&*modname, &*globname) {
            (b"__builtin__", b"set") => self.stack.push(Value::Global(Global::Set)),
            (b"builtins", b"set") => self.stack.push(Value::Global(Global::Set)),
            (b"__builtin__", b"frozenset") => self.stack.push(Value::Global(Global::Frozenset)),
            (b"builtins", b"frozenset") => self.stack.push(Value::Global(Global::Frozenset)),
            (b"_codecs", b"encode") => self.stack.push(Value::Global(Global::Encode)),
            _ => return self.error(ErrorCode::UnsupportedGlobal(modname, globname)),
        }
        Ok(())
    }

    fn stack_error<T>(what: &'static str, value: &Value, pos: usize) -> Result<T> {
        let it = format!("{:?}", value);
        Err(Error::Eval(ErrorCode::InvalidStackTop(what, it), pos))
    }

    fn error<T>(&self, reason: ErrorCode) -> Result<T> {
        Err(Error::Eval(reason, self.rdr.pos()))
    }

    fn deserialize_value(&mut self, value: Value) -> Result<value::Value> {
        match value {
            Value::None => Ok(value::Value::None),
            Value::Bool(v) => Ok(value::Value::Bool(v)),
            Value::I64(v) => Ok(value::Value::I64(v)),
            Value::Int(v) => {
                if let Some(i) = v.to_i64() {
                    Ok((value::Value::I64(i)))
                } else {
                    Ok((value::Value::Int(v)))
                }
            },
            Value::F64(v) => Ok(value::Value::F64(v)),
            Value::Bytes(v) => Ok(value::Value::Bytes(v)),
            Value::String(v) => Ok(value::Value::String(v)),
            Value::List(v) => {
                let new_list = try!(v.into_iter().map(|v| self.deserialize_value(v)).collect());
                Ok(value::Value::List(new_list))
            },
            Value::Tuple(v) => {
                let new_list: Vec<_> = try!(v.into_iter().map(|v| self.deserialize_value(v)).collect());
                Ok(value::Value::Tuple(new_list))
            },
            Value::Set(v) => {
                let new_list = try!(v.into_iter().map(|v| self.deserialize_value(v)
                                                      .and_then(|rv| rv.to_hashable())).collect());
                Ok(value::Value::Set(new_list))
            },
            Value::FrozenSet(v) => {
                let new_list = try!(v.into_iter().map(|v| self.deserialize_value(v)
                                                      .and_then(|rv| rv.to_hashable())).collect());
                Ok(value::Value::FrozenSet(new_list))
            },
            Value::Dict(v) => {
                let mut map = BTreeMap::new();
                for (key, value) in v {
                    let real_key = try!(self.deserialize_value(key).and_then(|rv| rv.to_hashable()));
                    let real_value = try!(self.deserialize_value(value));
                    map.insert(real_key, real_value);
                }
                Ok(value::Value::Dict(map))
            },
            Value::MemoRef(memo_id) => {
                self.resolve_recursive(memo_id, |slf, value| slf.deserialize_value(value))
            },
            Value::Global(_) => Err(Error::Syntax(ErrorCode::UnresolvedGlobal)),
        }
    }
}

impl<Iter> de::Deserializer for Deserializer<Iter>
    where Iter: Iterator<Item=io::Result<u8>>
{
    type Error = Error;

    fn deserialize<V>(&mut self, mut visitor: V) -> Result<V::Value>
        where V: de::Visitor
    {
        let value = try!(self.get_next_value());
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
                visitor.visit_seq(SeqVisitor {
                    de: self,
                    iter: v.into_iter(),
                    len: len,
                })
            },
            Value::Tuple(v) => {
                visitor.visit_seq(SeqVisitor {
                    de: self,
                    len: v.len(),
                    iter: v.into_iter(),
                })
            }
            Value::Set(v) | Value::FrozenSet(v) => {
                visitor.visit_seq(SeqVisitor {
                    de: self,
                    len: v.len(),
                    iter: v.into_iter(),
                })
            },
            Value::Dict(v) => {
                let len = v.len();
                visitor.visit_map(MapVisitor {
                    de: self,
                    iter: v.into_iter(),
                    value: None,
                    len: len,
                })
            },
            Value::MemoRef(memo_id) => {
                self.resolve_recursive(memo_id, |slf, value| {
                    slf.value = Some(value);
                    de::Deserialize::deserialize(slf)
                })
            },
            Value::Global(_) => Err(Error::Syntax(ErrorCode::UnresolvedGlobal)),
        }
    }

    #[inline]
    fn deserialize_option<V>(&mut self, mut visitor: V) -> Result<V::Value>
        where V: de::Visitor,
    {
        let value = try!(self.get_next_value());
        match value {
            Value::None => visitor.visit_none(),
            _           => {
                self.value = Some(value);
                visitor.visit_some(self)
            }
        }
    }

    #[inline]
    fn deserialize_unit_struct<V>(&mut self, _name: &str, visitor: V)
                                  -> Result<V::Value> where V: de::Visitor {
        self.deserialize_unit(visitor)
    }

    #[inline]
    fn deserialize_newtype_struct<V>(&mut self, _name: &str, mut visitor: V)
                                     -> Result<V::Value> where V: de::Visitor {
        visitor.visit_newtype_struct(self)
    }

    #[inline]
    fn deserialize_enum<V>(&mut self, _name: &str, _variants: &'static [&'static str],
                           mut visitor: V) -> Result<V::Value> where V: de::EnumVisitor {
        visitor.visit(self)
    }
}

impl<Iter> de::VariantVisitor for Deserializer<Iter>
    where Iter: Iterator<Item=io::Result<u8>>
{
    type Error = Error;

    fn visit_variant<V>(&mut self) -> Result<V> where V: de::Deserialize {
        let value = try!(self.get_next_value());
        match value {
            Value::Tuple(mut v) => {
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

    fn visit_newtype<T>(&mut self) -> Result<T> where T: de::Deserialize {
        de::Deserialize::deserialize(self)
    }

    fn visit_tuple<V>(&mut self, _len: usize, visitor: V) -> Result<V::Value> where V: de::Visitor {
        de::Deserializer::deserialize(self, visitor)
    }

    fn visit_struct<V>(&mut self, _fields: &'static [&'static str], visitor: V)
                       -> Result<V::Value> where V: de::Visitor {
        de::Deserializer::deserialize(self, visitor)
    }
}

struct SeqVisitor<'a, Iter>
    where Iter: Iterator<Item=io::Result<u8>> + 'a
{
    de: &'a mut Deserializer<Iter>,
    iter: vec::IntoIter<Value>,
    len: usize,
}

impl<'a, Iter> de::SeqVisitor for SeqVisitor<'a, Iter>
    where Iter: Iterator<Item=io::Result<u8>> + 'a
{
    type Error = Error;

    fn visit<T>(&mut self) -> Result<Option<T>>
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

struct MapVisitor<'a, Iter>
    where Iter: Iterator<Item=io::Result<u8>> + 'a
{
    de: &'a mut Deserializer<Iter>,
    iter: vec::IntoIter<(Value, Value)>,
    value: Option<Value>,
    len: usize,
}

impl<'a, Iter> de::MapVisitor for MapVisitor<'a, Iter>
    where Iter: Iterator<Item=io::Result<u8>> + 'a
{
    type Error = Error;

    fn visit_key<T>(&mut self) -> Result<Option<T>>
        where T: de::Deserialize
    {
        match self.iter.next() {
            Some((key, value)) => {
                self.len -= 1;
                self.value = Some(value);
                self.de.value = Some(key);
                Ok(Some(try!(de::Deserialize::deserialize(self.de))))
            }
            None => Ok(None),
        }
    }

    fn visit_value<T>(&mut self) -> Result<T>
        where T: de::Deserialize
    {
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

    fn missing_field<V>(&mut self, field: &'static str) -> Result<V>
        where V: de::Deserialize,
    {
        Err(de::Error::missing_field(field))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}


/// Decodes a value directly from an iterator.
pub fn from_iter<I, T>(iter: I) -> Result<T>
    where I: Iterator<Item=io::Result<u8>>,
          T: de::Deserialize
{
    let mut de = Deserializer::new(iter, false);
    let value = try!(de::Deserialize::deserialize(&mut de));
    // Make sure the whole stream has been consumed.
    try!(de.end());
    Ok(value)
}

/// Decodes a value from a `std::io::Read`.
pub fn from_reader<R: io::Read, T: de::Deserialize>(rdr: R) -> Result<T> {
    from_iter(rdr.bytes())
}

/// Decodes a value from a byte slice `&[u8]`.
pub fn from_slice<T: de::Deserialize>(v: &[u8]) -> Result<T> {
    from_iter(v.iter().map(|byte| Ok(*byte)))
}

/// Decodes a value directly from an iterator.
pub fn value_from_iter<I>(iter: I) -> Result<value::Value>
    where I: Iterator<Item=io::Result<u8>>
{
    let mut de = Deserializer::new(iter, false);
    let intermediate_value = try!(de.parse_value());
    let value = try!(de.deserialize_value(intermediate_value));
    // Make sure the whole stream has been consumed.
    try!(de.end());
    Ok(value)
}

/// Decodes a value from a `std::io::Read`.
pub fn value_from_reader<R: io::Read>(rdr: R) -> Result<value::Value> {
    value_from_iter(rdr.bytes())
}

/// Decodes a value from a byte slice `&[u8]`.
pub fn value_from_slice(v: &[u8]) -> Result<value::Value> {
    value_from_iter(v.iter().map(|byte| Ok(*byte)))
}
