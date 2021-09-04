// Copyright (c) 2015-2021 Georg Brandl.  Licensed under the Apache License,
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
use std::io::{BufReader, BufRead, Read};
use std::str::FromStr;
use std::collections::BTreeMap;
use std::iter::FusedIterator;
use serde::{de, forward_to_deserialize_any};
use serde::de::Visitor;
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use byteorder::{ByteOrder, BigEndian, LittleEndian};
use iter_read::{IterRead, IterReadItem};

use super::error::{Error, ErrorCode, Result};
use super::consts::*;
use super::value;

type MemoId = u32;

#[derive(Clone, Debug, PartialEq)]
enum Global {
    Set,         // builtins/__builtin__.set
    Frozenset,   // builtins/__builtin__.frozenset
    Bytearray,   // builtins/__builtin__.bytearray
    List,        // builtins/__builtin__.list
    Int,         // builtins/__builtin__.int
    Encode,      // _codecs.encode
    Other,       // anything else (may be a classobj that is later discarded)
}

/// Our intermediate representation of a value.
///
/// The most striking difference to `value::Value` is that it contains a variant
/// for `MemoRef`, which references values put into the "memo" map, and a variant
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

/// Options for deserializing.
#[derive(Clone, Debug, Default)]
pub struct DeOptions {
    decode_strings: bool,
    replace_unresolved_globals: bool,
}

impl DeOptions {
    /// Construct with default options:
    ///
    /// - don't decode strings saved as STRING opcodes (only protocols 0-2) as UTF-8
    /// - don't replace unresolvable globals by `None`
    pub fn new() -> Self {
        Default::default()
    }

    /// Activate decoding strings saved as STRING.
    pub fn decode_strings(mut self) -> Self {
        self.decode_strings = true;
        self
    }

    /// Activate replacing unresolved globals by `None`.
    pub fn replace_unresolved_globals(mut self) -> Self {
        self.replace_unresolved_globals = true;
        self
    }
}

/// Decodes pickle streams into values.
pub struct Deserializer<R: Read> {
    rdr: BufReader<R>,
    options: DeOptions,
    pos: usize,
    value: Option<Value>,                  // next value to deserialize
    memo: BTreeMap<MemoId, (Value, i32)>,  // pickle memo (value, number of refs)
    stack: Vec<Value>,                     // topmost items on the stack
    stacks: Vec<Vec<Value>>,               // items further down the stack, between MARKs
}

impl<R: Read> Deserializer<R> {
    /// Construct a new Deserializer.
    pub fn new(rdr: R, options: DeOptions) -> Deserializer<R> {
        Deserializer {
            rdr: BufReader::new(rdr),
            pos: 0,
            value: None,
            memo: BTreeMap::new(),
            stack: Vec::with_capacity(128),
            stacks: Vec::with_capacity(16),
            options,
        }
    }

    /// Decode a Value from this pickle.  This is different from going through
    /// the generic serde `deserialize`, since it preserves some types that are
    /// not in the serde data model, such as big integers.
    pub fn deserialize_value(&mut self) -> Result<value::Value> {
        let internal_value = self.parse_value()?;
        self.convert_value(internal_value)
    }

    /// Get the next value to deserialize, either by parsing the pickle stream
    /// or from `self.value`.
    fn get_next_value(&mut self) -> Result<Value> {
        match self.value.take() {
            Some(v) => Ok(v),
            None => self.parse_value(),
        }
    }

    /// Parse a value from the underlying stream.  This will consume the whole
    /// pickle until the STOP opcode.
    fn parse_value(&mut self) -> Result<Value> {
        // Clear memo, to allow reading multiple pickle dump calls to
        // a single stream.
        self.memo.clear();

        loop {
            match self.read_byte()? {
                // Specials
                PROTO => {
                    // Ignore this, as it is only important for instances (read
                    // the version byte).
                    self.read_byte()?;
                }
                FRAME => {
                    // We'll ignore framing. But we still have to gobble up the length.
                    self.read_bytes(8)?;
                }
                STOP => return self.pop(),
                MARK => {
                    let stack = mem::replace(&mut self.stack, Vec::with_capacity(128));
                    self.stacks.push(stack);
                }
                POP => {
                    if self.stack.is_empty() {
                        self.pop_mark()?;
                    } else {
                        self.pop()?;
                    }
                },
                POP_MARK => { self.pop_mark()?; },
                DUP => { let top = self.top()?.clone(); self.stack.push(top); },

                // Memo saving ops
                PUT => {
                    let bytes = self.read_line()?;
                    let memo_id = self.parse_ascii(bytes)?;
                    self.memoize(memo_id)?;
                }
                BINPUT => {
                    let memo_id = self.read_byte()?;
                    self.memoize(memo_id.into())?;
                }
                LONG_BINPUT => {
                    let bytes = self.read_bytes(4)?;
                    let memo_id = LittleEndian::read_u32(&bytes);
                    self.memoize(memo_id)?;
                }
                MEMOIZE => {
                    let memo_id = self.memo.len();
                    self.memoize(memo_id as MemoId)?;
                }

                // Memo getting ops
                GET => {
                    let bytes = self.read_line()?;
                    let memo_id = self.parse_ascii(bytes)?;
                    self.push_memo_ref(memo_id)?;
                }
                BINGET => {
                    let memo_id = self.read_byte()?;
                    self.push_memo_ref(memo_id.into())?;
                }
                LONG_BINGET => {
                    let bytes = self.read_bytes(4)?;
                    let memo_id = LittleEndian::read_u32(&bytes);
                    self.push_memo_ref(memo_id)?;
                }

                // Singletons
                NONE => self.stack.push(Value::None),
                NEWFALSE => self.stack.push(Value::Bool(false)),
                NEWTRUE => self.stack.push(Value::Bool(true)),

                // ASCII-formatted numbers
                INT => {
                    let line = self.read_line()?;
                    let val = self.decode_text_int(line)?;
                    self.stack.push(val);
                }
                LONG => {
                    let line = self.read_line()?;
                    let long = self.decode_text_long(line)?;
                    self.stack.push(long);
                }
                FLOAT => {
                    let line = self.read_line()?;
                    let f = self.parse_ascii(line)?;
                    self.stack.push(Value::F64(f));
                }

                // ASCII-formatted strings
                STRING => {
                    let line = self.read_line()?;
                    let string = self.decode_escaped_string(&line)?;
                    self.stack.push(string);
                }
                UNICODE => {
                    let line = self.read_line()?;
                    let string = self.decode_escaped_unicode(&line)?;
                    self.stack.push(string);
                }

                // Binary-coded numbers
                BINFLOAT => {
                    let bytes = self.read_bytes(8)?;
                    self.stack.push(Value::F64(BigEndian::read_f64(&bytes)));
                }
                BININT => {
                    let bytes = self.read_bytes(4)?;
                    self.stack.push(Value::I64(LittleEndian::read_i32(&bytes).into()));
                }
                BININT1 => {
                    let byte = self.read_byte()?;
                    self.stack.push(Value::I64(byte.into()));
                }
                BININT2 => {
                    let bytes = self.read_bytes(2)?;
                    self.stack.push(Value::I64(LittleEndian::read_u16(&bytes).into()));
                }
                LONG1 => {
                    let bytes = self.read_u8_prefixed_bytes()?;
                    let long = self.decode_binary_long(bytes);
                    self.stack.push(long);
                }
                LONG4 => {
                    let bytes = self.read_i32_prefixed_bytes()?;
                    let long = self.decode_binary_long(bytes);
                    self.stack.push(long);
                }

                // Length-prefixed (byte)strings
                SHORT_BINBYTES => {
                    let string = self.read_u8_prefixed_bytes()?;
                    self.stack.push(Value::Bytes(string));
                }
                BINBYTES => {
                    let string = self.read_u32_prefixed_bytes()?;
                    self.stack.push(Value::Bytes(string));
                }
                BINBYTES8 => {
                    let string = self.read_u64_prefixed_bytes()?;
                    self.stack.push(Value::Bytes(string));
                }
                SHORT_BINSTRING => {
                    let string = self.read_u8_prefixed_bytes()?;
                    let decoded = self.decode_string(string)?;
                    self.stack.push(decoded);
                }
                BINSTRING => {
                    let string = self.read_i32_prefixed_bytes()?;
                    let decoded = self.decode_string(string)?;
                    self.stack.push(decoded);
                }
                SHORT_BINUNICODE => {
                    let string = self.read_u8_prefixed_bytes()?;
                    let decoded = self.decode_unicode(string)?;
                    self.stack.push(decoded);
                }
                BINUNICODE => {
                    let string = self.read_u32_prefixed_bytes()?;
                    let decoded = self.decode_unicode(string)?;
                    self.stack.push(decoded);
                }
                BINUNICODE8 => {
                    let string = self.read_u64_prefixed_bytes()?;
                    let decoded = self.decode_unicode(string)?;
                    self.stack.push(decoded);
                }
                BYTEARRAY8 => {
                    let string = self.read_u64_prefixed_bytes()?;
                    self.stack.push(Value::Bytes(string));
                }

                // Tuples
                EMPTY_TUPLE => self.stack.push(Value::Tuple(Vec::new())),
                TUPLE1 => {
                    let item = self.pop()?;
                    self.stack.push(Value::Tuple(vec![item]));
                 }
                 TUPLE2 => {
                    let item2 = self.pop()?;
                    let item1 = self.pop()?;
                    self.stack.push(Value::Tuple(vec![item1, item2]));
                 }
                 TUPLE3 => {
                    let item3 = self.pop()?;
                    let item2 = self.pop()?;
                    let item1 = self.pop()?;
                    self.stack.push(Value::Tuple(vec![item1, item2, item3]));
                }
                TUPLE => {
                    let items = self.pop_mark()?;
                    self.stack.push(Value::Tuple(items));
                }

                // Lists
                EMPTY_LIST => self.stack.push(Value::List(Vec::new())),
                LIST => {
                    let items = self.pop_mark()?;
                    self.stack.push(Value::List(items));
                }
                APPEND => {
                    let value = self.pop()?;
                    self.modify_list(|list| list.push(value))?;
                }
                APPENDS => {
                    let items = self.pop_mark()?;
                    self.modify_list(|list| list.extend(items))?;
                }

                // Dicts
                EMPTY_DICT => self.stack.push(Value::Dict(Vec::new())),
                DICT => {
                    let items = self.pop_mark()?;
                    let mut dict = Vec::with_capacity(items.len() / 2);
                    Self::extend_dict(&mut dict, items);
                    self.stack.push(Value::Dict(dict));
                }
                SETITEM => {
                    let value = self.pop()?;
                    let key = self.pop()?;
                    self.modify_dict(|dict| dict.push((key, value)))?;
                }
                SETITEMS => {
                    let items = self.pop_mark()?;
                    self.modify_dict(|dict| Self::extend_dict(dict, items))?;
                }

                // Sets and frozensets
                EMPTY_SET => self.stack.push(Value::Set(Vec::new())),
                FROZENSET => {
                    let items = self.pop_mark()?;
                    self.stack.push(Value::FrozenSet(items));
                }
                ADDITEMS => {
                    let items = self.pop_mark()?;
                    self.modify_set(|set| set.extend(items))?;
                }

                // Arbitrary module globals, used here for unpickling set and frozenset
                // from protocols < 4
                GLOBAL => {
                    let modname = self.read_line()?;
                    let globname = self.read_line()?;
                    let value = self.decode_global(modname, globname)?;
                    self.stack.push(value);
                }
                STACK_GLOBAL => {
                    let globname = match self.pop_resolve()? {
                        Value::String(string) => string.into_bytes(),
                        other => return Self::stack_error("string", &other, self.pos),
                    };
                    let modname = match self.pop_resolve()? {
                        Value::String(string) => string.into_bytes(),
                        other => return Self::stack_error("string", &other, self.pos),
                    };
                    let value = self.decode_global(modname, globname)?;
                    self.stack.push(value);
                }
                REDUCE => {
                    let argtuple = match self.pop_resolve()? {
                        Value::Tuple(args) => args,
                        other => return Self::stack_error("tuple", &other, self.pos),
                    };
                    let global = self.pop_resolve()?;
                    self.reduce_global(global, argtuple)?;
                }

                // Arbitrary classes - make a best effort attempt to recover some data
                INST => {
                    // pop module name and class name
                    for _ in 0..2 {
                        self.read_line()?;
                    }
                    // pop arguments to init
                    self.pop_mark()?;
                    // push empty dictionary instead of the class instance
                    self.stack.push(Value::Dict(Vec::new()));
                }
                OBJ => {
                    // pop arguments to init
                    self.pop_mark()?;
                    // pop class object
                    self.pop()?;
                    self.stack.push(Value::Dict(Vec::new()));
                }
                NEWOBJ => {
                    // pop arguments and class object
                    for _ in 0..2 {
                        self.pop()?;
                    }
                    self.stack.push(Value::Dict(Vec::new()));
                }
                NEWOBJ_EX => {
                    // pop keyword args, arguments and class object
                    for _ in 0..3 {
                        self.pop()?;
                    }
                    self.stack.push(Value::Dict(Vec::new()));
                }
                BUILD => {
                    // The top-of-stack for BUILD is used either as the instance __dict__,
                    // or an argument for __setstate__, in which case it can be *any* type
                    // of object.  In both cases, we just replace the standin.
                    let state = self.pop()?;
                    self.pop()?;  // remove the object standin
                    self.stack.push(state);
                }

                // Unsupported opcodes
                code => return self.error(ErrorCode::Unsupported(code as char))
            }
        }
    }

    // Pop the stack top item.
    fn pop(&mut self) -> Result<Value> {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None    => self.error(ErrorCode::StackUnderflow)
        }
    }

    // Pop the stack top item, and resolve it if it is a memo reference.
    fn pop_resolve(&mut self) -> Result<Value> {
        let top = self.stack.pop();
        match self.resolve(top) {
            Some(v) => Ok(v),
            None    => self.error(ErrorCode::StackUnderflow)
        }
    }

    // Pop all topmost stack items until the next MARK.
    fn pop_mark(&mut self) -> Result<Vec<Value>> {
        match self.stacks.pop() {
            Some(new) => Ok(mem::replace(&mut self.stack, new)),
            None      => self.error(ErrorCode::StackUnderflow)
        }
    }

    // Mutably view the stack top item.
    fn top(&mut self) -> Result<&mut Value> {
        match self.stack.last_mut() {
            // Since some operations like APPEND do things to the stack top, we
            // need to provide the reference to the "real" object here, not the
            // MemoRef variant.
            Some(&mut Value::MemoRef(n)) =>
                self.memo.get_mut(&n)
                         .map(|&mut (ref mut v, _)| v)
                         .ok_or_else(|| Error::Syntax(ErrorCode::MissingMemo(n))),
            Some(other_value) => Ok(other_value),
            None => Err(Error::Eval(ErrorCode::StackUnderflow, self.pos)),
        }
    }

    // Pushes a memo reference on the stack, and increases the usage counter.
    fn push_memo_ref(&mut self, memo_id: MemoId) -> Result<()> {
        self.stack.push(Value::MemoRef(memo_id));
        match self.memo.get_mut(&memo_id) {
            Some(&mut (_, ref mut count)) => { *count += 1; Ok(()) }
            None => Err(Error::Eval(ErrorCode::MissingMemo(memo_id), self.pos)),
        }
    }

    // Memoize the current stack top with the given ID.  Moves the actual
    // object into the memo, and saves a reference on the stack instead.
    fn memoize(&mut self, memo_id: MemoId) -> Result<()> {
        let mut item = self.pop()?;
        if let Value::MemoRef(id) = item {
            // TODO: is this even possible?
            item = match self.memo.get(&id) {
                Some(&(ref v, _)) => v.clone(),
                None => return Err(Error::Eval(ErrorCode::MissingMemo(id), self.pos)),
            };
        }
        self.memo.insert(memo_id, (item, 1));
        self.stack.push(Value::MemoRef(memo_id));
        Ok(())
    }

    // Resolve memo reference during stream decoding.
    fn resolve(&mut self, maybe_memo: Option<Value>) -> Option<Value> {
        match maybe_memo {
            Some(Value::MemoRef(id)) => {
                self.memo.get_mut(&id).map(|&mut (ref val, ref mut count)| {
                    // We can't remove it from the memo here, since we haven't
                    // decoded the whole stream yet and there may be further
                    // references to the value.
                    *count -= 1;
                    val.clone()
                })
            },
            other => other
        }
    }

    // Resolve memo reference during Value deserializing.
    fn resolve_recursive<T, U, F>(&mut self, id: MemoId, u: U, f: F) -> Result<T>
        where F: FnOnce(&mut Self, U, Value) -> Result<T>
    {
        // Take the value from the memo while visiting it.  This prevents us
        // from trying to depickle recursive structures, which we can't do
        // because our Values aren't references.
        let (value, mut count) = match self.memo.remove(&id) {
            Some(entry) => entry,
            None => return Err(Error::Syntax(ErrorCode::Recursive)),
        };
        count -= 1;
        if count <= 0 {
            f(self, u, value)
            // No need to put it back.
        } else {
            let result = f(self, u, value.clone());
            self.memo.insert(id, (value, count));
            result
        }
    }

    /// Assert that we reached the end of the stream.
    pub fn end(&mut self) -> Result<()> {
        let mut buf = [0];
        match self.rdr.read(&mut buf) {
            Err(err) => Err(Error::Io(err)),
            Ok(1) => self.error(ErrorCode::TrailingBytes),
            _ => Ok(())
        }
    }

    fn read_line(&mut self) -> Result<Vec<u8>> {
        let mut buf = Vec::with_capacity(16);
        match self.rdr.read_until(b'\n', &mut buf) {
            Ok(_) => {
                self.pos += buf.len();
                buf.pop(); // remove newline
                if buf.last() == Some(&b'\r') { buf.pop(); }
                Ok(buf)
            },
            Err(err) => Err(Error::Io(err))
        }
    }

    #[inline]
    fn read_byte(&mut self) -> Result<u8> {
        let mut buf = [0];
        match self.rdr.read(&mut buf) {
            Ok(1) => { self.pos += 1; Ok(buf[0]) },
            Ok(_) => self.error(ErrorCode::EOFWhileParsing),
            Err(err) => Err(Error::Io(err)),
        }
    }

    #[inline]
    fn read_bytes(&mut self, n: usize) -> Result<Vec<u8>> {
        let mut buf = Vec::new();
        match self.rdr.by_ref().take(n as u64).read_to_end(&mut buf) {
            Ok(m) if n == m => { self.pos += n; Ok(buf) },
            Ok(_) => self.error(ErrorCode::EOFWhileParsing),
            Err(err) => Err(Error::Io(err)),
        }
    }

    fn read_i32_prefixed_bytes(&mut self) -> Result<Vec<u8>> {
        let lenbytes = self.read_bytes(4)?;
        match LittleEndian::read_i32(&lenbytes) {
            0          => Ok(vec![]),
            l if l < 0 => self.error(ErrorCode::NegativeLength),
            l          => self.read_bytes(l as usize)
        }
    }

    fn read_u64_prefixed_bytes(&mut self) -> Result<Vec<u8>> {
        let lenbytes = self.read_bytes(8)?;
        self.read_bytes(LittleEndian::read_u64(&lenbytes) as usize)
    }

    fn read_u32_prefixed_bytes(&mut self) -> Result<Vec<u8>> {
        let lenbytes = self.read_bytes(4)?;
        self.read_bytes(LittleEndian::read_u32(&lenbytes) as usize)
    }

    fn read_u8_prefixed_bytes(&mut self) -> Result<Vec<u8>> {
        let lenbyte = self.read_byte()?;
        self.read_bytes(lenbyte as usize)
    }

    // Parse an expected ASCII literal from the stream or raise an error.
    fn parse_ascii<T: FromStr>(&self, bytes: Vec<u8>) -> Result<T> {
        match str::from_utf8(&bytes).unwrap_or("").parse() {
            Ok(v) => Ok(v),
            Err(_) => self.error(ErrorCode::InvalidLiteral(bytes)),
        }
    }

    // Decode a text-encoded integer.
    fn decode_text_int(&self, line: Vec<u8>) -> Result<Value> {
        // Handle protocol 1 way of spelling true/false
        Ok(if line == b"00" {
            Value::Bool(false)
        } else if line == b"01" {
            Value::Bool(true)
        } else {
            let i = self.parse_ascii(line)?;
            Value::I64(i)
        })
    }

    // Decode a text-encoded long integer.
    fn decode_text_long(&self, mut line: Vec<u8>) -> Result<Value> {
        // Remove "L" suffix.
        if line.last() == Some(&b'L') { line.pop(); }
        match BigInt::parse_bytes(&line, 10) {
            Some(i)  => Ok(Value::Int(i)),
            None => self.error(ErrorCode::InvalidLiteral(line))
        }
    }

    // Decode an escaped string.  These are encoded with "normal" Python string
    // escape rules.
    fn decode_escaped_string(&self, slice: &[u8]) -> Result<Value> {
        // Remove quotes if they appear.
        let slice = if (slice.len() >= 2) &&
            (slice[0] == slice[slice.len() - 1]) &&
            (slice[0] == b'"' || slice[0] == b'\'')
        {
            &slice[1..slice.len() - 1]
        } else {
            slice
        };
        let mut result = Vec::with_capacity(slice.len());
        let mut iter = slice.iter();
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
                            None => return self.error(ErrorCode::InvalidLiteral(slice.into()))
                        }
                    },
                    _ => return self.error(ErrorCode::InvalidLiteral(slice.into())),
                },
                _ => result.push(b)
            }
        }
        self.decode_string(result)
    }

    // Decode escaped Unicode strings. These are encoded with "raw-unicode-escape",
    // which only knows the \uXXXX and \UYYYYYYYY escapes. The backslash is escaped
    // in this way, too.
    fn decode_escaped_unicode(&self, s: &[u8]) -> Result<Value> {
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

    // Decode a string - either as Unicode or as bytes.
    fn decode_string(&self, string: Vec<u8>) -> Result<Value> {
        if self.options.decode_strings {
            self.decode_unicode(string)
        } else {
            Ok(Value::Bytes(string))
        }
    }

    // Decode a Unicode string from UTF-8.
    fn decode_unicode(&self, string: Vec<u8>) -> Result<Value> {
        match String::from_utf8(string) {
            Ok(v)  => Ok(Value::String(v)),
            Err(_) => self.error(ErrorCode::StringNotUTF8)
        }
    }

    // Decode a binary-encoded long integer.
    fn decode_binary_long(&self, bytes: Vec<u8>) -> Value {
        // BigInt::from_bytes_le doesn't like a sign bit in the bytes, therefore
        // we have to extract that ourselves and do the two-s complement.
        let negative = !bytes.is_empty() && (bytes[bytes.len() - 1] & 0x80 != 0);
        let mut val = BigInt::from_bytes_le(Sign::Plus, &bytes);
        if negative {
            val -= BigInt::from(1) << (bytes.len() * 8);
        }
        Value::Int(val)
    }

    // Modify the stack-top list.
    fn modify_list<F>(&mut self, f: F) -> Result<()>
        where F: FnOnce(&mut Vec<Value>)
    {
        let pos = self.pos;
        let top = self.top()?;
        if let Value::List(ref mut list) = *top {
            f(list);
            Ok(())
        } else {
            Self::stack_error("list", top, pos)
        }
    }

    // Push items from a (key, value, key, value) flattened list onto a (key, value) vec.
    fn extend_dict(dict: &mut Vec<(Value, Value)>, items: Vec<Value>) {
        let mut key = None;
        for value in items {
            match key.take() {
                None      => key = Some(value),
                Some(key) => dict.push((key, value))
            }
        }
    }

    // Modify the stack-top dict.
    fn modify_dict<F>(&mut self, f: F) -> Result<()>
        where F: FnOnce(&mut Vec<(Value, Value)>)
    {
        let pos = self.pos;
        let top = self.top()?;
        if let Value::Dict(ref mut dict) = *top {
            f(dict);
            Ok(())
        } else {
            Self::stack_error("dict", top, pos)
        }
    }

    // Modify the stack-top set.
    fn modify_set<F>(&mut self, f: F) -> Result<()>
        where F: FnOnce(&mut Vec<Value>)
    {
        let pos = self.pos;
        let top = self.top()?;
        if let Value::Set(ref mut set) = *top {
            f(set);
            Ok(())
        } else {
            Self::stack_error("set", top, pos)
        }
    }

    // Push the Value::Global referenced by modname and globname.
    fn decode_global(&mut self, modname: Vec<u8>, globname: Vec<u8>) -> Result<Value> {
        let value = match (&*modname, &*globname) {
            (b"_codecs", b"encode") => Value::Global(Global::Encode),
            (b"__builtin__", b"set") | (b"builtins", b"set") =>
                Value::Global(Global::Set),
            (b"__builtin__", b"frozenset") | (b"builtins", b"frozenset") =>
                Value::Global(Global::Frozenset),
            (b"__builtin__", b"list") | (b"builtins", b"list") =>
                Value::Global(Global::List),
            (b"__builtin__", b"bytearray") | (b"builtins", b"bytearray") =>
                Value::Global(Global::Bytearray),
            (b"__builtin__", b"int") | (b"builtins", b"int") =>
                Value::Global(Global::Int),
            _ => Value::Global(Global::Other),
        };
        Ok(value)
    }

    // Handle the REDUCE opcode for the few Global objects we support.
    fn reduce_global(&mut self, global: Value, mut argtuple: Vec<Value>) -> Result<()> {
        match global {
            Value::Global(Global::Set) => {
                match self.resolve(argtuple.pop()) {
                    Some(Value::List(items)) => {
                        self.stack.push(Value::Set(items));
                        Ok(())
                    }
                    _ => self.error(ErrorCode::InvalidValue("set() arg".into())),
                }
            }
            Value::Global(Global::Frozenset) => {
                match self.resolve(argtuple.pop()) {
                    Some(Value::List(items)) => {
                        self.stack.push(Value::FrozenSet(items));
                        Ok(())
                    }
                    _ => self.error(ErrorCode::InvalidValue("frozenset() arg".into())),
                }
            }
            Value::Global(Global::Bytearray) => {
                // On Py2, the call is encoded as bytearray(u"foo", "latin-1").
                argtuple.truncate(1);
                match self.resolve(argtuple.pop()) {
                    Some(Value::Bytes(bytes)) => {
                        self.stack.push(Value::Bytes(bytes));
                        Ok(())
                    }
                    Some(Value::String(string)) => {
                        // The code points in the string are actually bytes values.
                        // So we need to collect them individually.
                        self.stack.push(Value::Bytes(
                            string.chars().map(|ch| ch as u32 as u8).collect()));
                        Ok(())
                    }
                    _ => self.error(ErrorCode::InvalidValue("bytearray() arg".into())),
                }
            }
            Value::Global(Global::List) => {
                match self.resolve(argtuple.pop()) {
                    Some(Value::List(items)) => {
                        self.stack.push(Value::List(items));
                        Ok(())
                    }
                    _ => self.error(ErrorCode::InvalidValue("list() arg".into())),
                }
            }
            Value::Global(Global::Int) => {
                match self.resolve(argtuple.pop()) {
                    Some(Value::Int(integer)) => {
                        self.stack.push(Value::Int(integer));
                        Ok(())
                    }
                    _ => self.error(ErrorCode::InvalidValue("int() arg".into())),
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
                        Ok(())
                    }
                    _ => self.error(ErrorCode::InvalidValue("encode() arg".into())),
                }
            }
            Value::Global(Global::Other) => {
                // Anything else; just keep it on the stack as an opaque object.
                // If it is a class object, it will get replaced later when the
                // class is instantiated.
                self.stack.push(Value::Global(Global::Other));
                Ok(())
            }
            other => Self::stack_error("global reference", &other, self.pos),
        }
    }

    fn stack_error<T>(what: &'static str, value: &Value, pos: usize) -> Result<T> {
        let it = format!("{:?}", value);
        Err(Error::Eval(ErrorCode::InvalidStackTop(what, it), pos))
    }

    fn error<T>(&self, reason: ErrorCode) -> Result<T> {
        Err(Error::Eval(reason, self.pos))
    }

    fn convert_value(&mut self, value: Value) -> Result<value::Value> {
        match value {
            Value::None => Ok(value::Value::None),
            Value::Bool(v) => Ok(value::Value::Bool(v)),
            Value::I64(v) => Ok(value::Value::I64(v)),
            Value::Int(v) => {
                if let Some(i) = v.to_i64() {
                    Ok(value::Value::I64(i))
                } else {
                    Ok(value::Value::Int(v))
                }
            },
            Value::F64(v) => Ok(value::Value::F64(v)),
            Value::Bytes(v) => Ok(value::Value::Bytes(v)),
            Value::String(v) => Ok(value::Value::String(v)),
            Value::List(v) => {
                let new = v.into_iter().map(|v| self.convert_value(v))
                                       .collect::<Result<_>>();
                Ok(value::Value::List(new?))
            },
            Value::Tuple(v) => {
                let new = v.into_iter().map(|v| self.convert_value(v))
                                       .collect::<Result<_>>();
                Ok(value::Value::Tuple(new?))
            },
            Value::Set(v) => {
                let new = v.into_iter().map(|v| self.convert_value(v)
                                            .and_then(|rv| rv.into_hashable()))
                                       .collect::<Result<_>>();
                Ok(value::Value::Set(new?))
            },
            Value::FrozenSet(v) => {
                let new = v.into_iter().map(|v| self.convert_value(v)
                                            .and_then(|rv| rv.into_hashable()))
                                       .collect::<Result<_>>();
                Ok(value::Value::FrozenSet(new?))
            },
            Value::Dict(v) => {
                let mut map = BTreeMap::new();
                for (key, value) in v {
                    let real_key = self.convert_value(key).and_then(|rv| rv.into_hashable())?;
                    let real_value = self.convert_value(value)?;
                    map.insert(real_key, real_value);
                }
                Ok(value::Value::Dict(map))
            },
            Value::MemoRef(memo_id) => {
                self.resolve_recursive(memo_id, (), |slf, (), value| slf.convert_value(value))
            },
            Value::Global(_) => {
                if self.options.replace_unresolved_globals {
                    Ok(value::Value::None)
                } else {
                    Err(Error::Syntax(ErrorCode::UnresolvedGlobal))
                }
            },
        }
    }
}

impl<'de: 'a, 'a, R: Read> de::Deserializer<'de> for &'a mut Deserializer<R> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(mut self, visitor: V) -> Result<V::Value> {
        let value = self.get_next_value()?;
        match value {
            Value::None => visitor.visit_unit(),
            Value::Bool(v) => visitor.visit_bool(v),
            Value::I64(v) => visitor.visit_i64(v),
            Value::Int(v) => {
                if let Some(i) = v.to_i64() {
                    visitor.visit_i64(i)
                } else {
                    return Err(Error::Syntax(ErrorCode::InvalidValue("integer too large".into())));
                }
            },
            Value::F64(v) => visitor.visit_f64(v),
            Value::Bytes(v) => visitor.visit_byte_buf(v),
            Value::String(v) => visitor.visit_string(v),
            Value::List(v) => {
                let len = v.len();
                visitor.visit_seq(SeqAccess {
                    de: &mut self,
                    iter: v.into_iter(),
                    len,
                })
            },
            Value::Tuple(v) => {
                visitor.visit_seq(SeqAccess {
                    len: v.len(),
                    iter: v.into_iter(),
                    de: &mut self,
                })
            }
            Value::Set(v) | Value::FrozenSet(v) => {
                visitor.visit_seq(SeqAccess {
                    de: &mut self,
                    len: v.len(),
                    iter: v.into_iter(),
                })
            },
            Value::Dict(v) => {
                let len = v.len();
                visitor.visit_map(MapAccess {
                    de: &mut self,
                    iter: v.into_iter(),
                    value: None,
                    len,
                })
            },
            Value::MemoRef(memo_id) => {
                self.resolve_recursive(memo_id, visitor, |slf, visitor, value| {
                    slf.value = Some(value);
                    slf.deserialize_any(visitor)
                })
            },
            Value::Global(_) => {
                if self.options.replace_unresolved_globals {
                    visitor.visit_unit()
                } else {
                    Err(Error::Syntax(ErrorCode::UnresolvedGlobal))
                }
            },
        }
    }

    #[inline]
    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value> {
        let value = self.get_next_value()?;
        match value {
            Value::None => visitor.visit_none(),
            _           => {
                self.value = Some(value);
                visitor.visit_some(self)
            }
        }
    }

    #[inline]
    fn deserialize_newtype_struct<V: Visitor<'de>>(self, _name: &'static str, visitor: V) -> Result<V::Value> {
        visitor.visit_newtype_struct(self)
    }

    #[inline]
    fn deserialize_enum<V: Visitor<'de>>(mut self, _name: &'static str, _variants: &'static [&'static str],
                                    visitor: V) -> Result<V::Value> {
        visitor.visit_enum(VariantAccess { de: &mut self })
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string unit seq
        bytes byte_buf map tuple_struct struct identifier
        tuple ignored_any unit_struct
    }
}

struct VariantAccess<'a, R: Read + 'a> {
    de: &'a mut Deserializer<R>,
}

impl<'de: 'a, 'a, R: Read + 'a> de::EnumAccess<'de> for VariantAccess<'a, R> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V: de::DeserializeSeed<'de>>(self, seed: V) -> Result<(V::Value, Self)> {
        let value = self.de.get_next_value()?;
        match value {
            Value::Tuple(mut v) => {
                if v.len() == 2 {
                    let args = v.pop();
                    self.de.value = v.pop();
                    let val = seed.deserialize(&mut *self.de)?;
                    self.de.value = args;
                    Ok((val, self))
                } else {
                    self.de.value = v.pop();
                    let val = seed.deserialize(&mut *self.de)?;
                    Ok((val, self))
                }
            }
            Value::Dict(mut v) => {
                if v.len() != 1 {
                    Err(Error::Syntax(ErrorCode::Structure("enum variants must \
                                                            have one dict entry".into())))
                } else {
                    let (name, args) = v.pop().unwrap();
                    self.de.value = Some(name);
                    let val = seed.deserialize(&mut *self.de)?;
                    self.de.value = Some(args);
                    Ok((val, self))
                }
            }
            Value::MemoRef(memo_id) => {
                self.de.resolve_recursive(memo_id, (), |slf, (), value| {
                    slf.value = Some(value);
                    Ok(())
                })?;
                // retry with memo resolved
                self.variant_seed(seed)
            }
            s @ Value::String(_) => {
                self.de.value = Some(s);
                let val = seed.deserialize(&mut *self.de)?;
                Ok((val, self))
            }
            _ => Err(Error::Syntax(ErrorCode::Structure("enums must be represented as \
                                                         dicts or tuples".into())))
        }
    }
}

impl<'de: 'a, 'a, R: Read + 'a> de::VariantAccess<'de> for VariantAccess<'a, R> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        Ok(())
    }

    fn newtype_variant_seed<T: de::DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value> {
        seed.deserialize(self.de)
    }

    fn tuple_variant<V: Visitor<'de>>(self, _len: usize, visitor: V) -> Result<V::Value> {
        de::Deserializer::deserialize_any(self.de, visitor)
    }

    fn struct_variant<V: Visitor<'de>>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value> {
        de::Deserializer::deserialize_any(self.de, visitor)
    }
}

struct SeqAccess<'a, R: Read + 'a> {
    de: &'a mut Deserializer<R>,
    iter: vec::IntoIter<Value>,
    len: usize,
}

impl<'de: 'a, 'a, R: Read> de::SeqAccess<'de> for SeqAccess<'a, R> {
    type Error = Error;

    fn next_element_seed<T: de::DeserializeSeed<'de>>(&mut self, seed: T) -> Result<Option<T::Value>> {
        match self.iter.next() {
            Some(value) => {
                self.len -= 1;
                self.de.value = Some(value);
                Ok(Some(seed.deserialize(&mut *self.de)?))
            }
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.len)
    }
}

struct MapAccess<'a, R: Read + 'a> {
    de: &'a mut Deserializer<R>,
    iter: vec::IntoIter<(Value, Value)>,
    value: Option<Value>,
    len: usize,
}

impl<'de: 'a, 'a, R: Read> de::MapAccess<'de> for MapAccess<'a, R> {
    type Error = Error;

    fn next_key_seed<T: de::DeserializeSeed<'de>>(&mut self, seed: T) -> Result<Option<T::Value>> {
        match self.iter.next() {
            Some((key, value)) => {
                self.len -= 1;
                self.value = Some(value);
                self.de.value = Some(key);
                Ok(Some(seed.deserialize(&mut *self.de)?))
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<T: de::DeserializeSeed<'de>>(&mut self, seed: T) -> Result<T::Value> {
        let value = self.value.take().unwrap();
        self.de.value = Some(value);
        Ok(seed.deserialize(&mut *self.de)?)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.len)
    }
}


/// Decodes a value from a `std::io::Read`.
pub fn from_reader<'de, R: io::Read, T: de::Deserialize<'de>>(rdr: R, options: DeOptions) -> Result<T> {
    let mut de = Deserializer::new(rdr, options);
    let value = de::Deserialize::deserialize(&mut de)?;
    // Make sure the whole stream has been consumed.
    de.end()?;
    Ok(value)
}

/// Decodes a value from a byte slice `&[u8]`.
pub fn from_slice<'de, T: de::Deserialize<'de>>(v: &[u8], options: DeOptions) -> Result<T> {
    from_reader(io::Cursor::new(v), options)
}

/// Decodes a value from any iterator supported as a reader.
pub fn from_iter<'de, E, I, T>(it: I, options: DeOptions) -> Result<T>
where E: IterReadItem, I: FusedIterator<Item=E>, T: de::Deserialize<'de>
{
    from_reader(IterRead::new(it), options)
}

/// Decodes a value from a `std::io::Read`.
pub fn value_from_reader<R: io::Read>(rdr: R, options: DeOptions) -> Result<value::Value> {
    let mut de = Deserializer::new(rdr, options);
    let value = de.deserialize_value()?;
    de.end()?;
    Ok(value)
}

/// Decodes a value from a byte slice `&[u8]`.
pub fn value_from_slice(v: &[u8], options: DeOptions) -> Result<value::Value> {
    value_from_reader(io::Cursor::new(v), options)
}

/// Decodes a value from any iterator supported as a reader.
pub fn value_from_iter<E, I>(it: I, options: DeOptions) -> Result<value::Value>
where E: IterReadItem, I: FusedIterator<Item=E>
{
    value_from_reader(IterRead::new(it), options)
}
