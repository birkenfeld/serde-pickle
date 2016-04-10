//! # Pickle deserialization
//!
//! Note: Pickles are not a declarative format, but a program for a stack-based
//! VM.  Each value that is decoded is simply put on the stack, and some
//! operations pop items from the stack and construct new data with them.
//!
//! This means that we cannot decode pickles directly with the serde visitor,
//! since we don't know e.g. when a map starts.  Instead, we have to interpret
//! the pickle into an intermediate representation of Python objects (i.e.
//! `value::Value`) and can then deserialize this into other serde-supported
//! data types.
//!
//! In turn, this means that using the generic `from_` functions with
//! `value::Value` as the output type will construct Values, and then let
//! serde de- and reconstruct them.  Don't do that, use the `value_from_`
//! functions instead.

use std::io;
use std::mem;
use std::str;
use std::char;
use std::collections::{BTreeMap, BTreeSet};
use num_bigint::{BigInt, Sign};
use byteorder::{ByteOrder, BigEndian, LittleEndian};
use serde::de;

use super::error::{Error, ErrorCode, Result};
use super::consts::*;
use super::value::{Value, HashableValue, from_value};

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

/// Decodes pickle streams into Values.
pub struct PickleReader<Iter: Iterator<Item=io::Result<u8>>> {
    rdr: CharIter<Iter>,
    ch: Option<u8>,
    stack: Vec<Value>,
    stacks: Vec<Vec<Value>>,
    decode_strings: bool,
}

impl<Iter> PickleReader<Iter>
    where Iter: Iterator<Item=io::Result<u8>>
{
    pub fn new(rdr: Iter, decode_strings: bool) -> PickleReader<Iter> {
        PickleReader {
            rdr: CharIter::new(rdr),
            ch: None,
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

    fn parse(&mut self) -> Result<Value> {
        loop {
            match try!(self.read_byte()) {
                // Specials
                STOP => return self.pop(),
                POP => {
                    if self.stack.is_empty() {
                        try!(self.pop_mark());
                    } else {
                        try!(self.pop());
                    }
                },
                POP_MARK => { try!(self.pop_mark()); },
                DUP => { let top = try!(self.top()).clone(); self.stack.push(top); },
                MARK => {
                    let stack = mem::replace(&mut self.stack, Vec::with_capacity(128));
                    self.stacks.push(stack);
                }
                PROTO => {
                    // Ignore this, as it is only important for instances (read the version byte).
                    try!(self.read_byte());
                }
                FRAME => {
                    // We'll ignore framing for now. But we still have to gobble up the length.
                    try!(self.read_bytes(8));
                }

                // Memo ops: ignore (trying to get the memo will error out)
                PUT => { try!(self.read_line()); },
                BINPUT => { try!(self.read_byte()); },
                LONG_BINPUT => { try!(self.read_bytes(4)); },
                MEMOIZE => { },

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
                        } else { &line };
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
                EMPTY_TUPLE => self.stack.push(Value::Tuple(Box::new([]))),
                TUPLE1 => {
                    let item = try!(self.pop());
                    self.stack.push(Value::Tuple(Box::new([item])));
                }
                TUPLE2 => {
                    let item2 = try!(self.pop());
                    let item1 = try!(self.pop());
                    self.stack.push(Value::Tuple(Box::new([item1, item2])));
                }
                TUPLE3 => {
                    let item3 = try!(self.pop());
                    let item2 = try!(self.pop());
                    let item1 = try!(self.pop());
                    self.stack.push(Value::Tuple(Box::new([item1, item2, item3])));
                }
                TUPLE => {
                    let items = try!(self.pop_mark());
                    self.stack.push(Value::Tuple(items.into_boxed_slice()));
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
                        return Err(Error::Eval(ErrorCode::InvalidStackTop, pos));
                    }
                }
                APPENDS => {
                    let pos = self.rdr.pos();
                    let items = try!(self.pop_mark());
                    let top = try!(self.top());
                    if let &mut Value::List(ref mut list) = top {
                        list.extend(items);
                    } else {
                        return Err(Error::Eval(ErrorCode::InvalidStackTop, pos));
                    }
                }
                EMPTY_DICT => self.stack.push(Value::Dict(BTreeMap::new())),
                DICT => {
                    let pos = self.rdr.pos();
                    let items = try!(self.pop_mark());
                    let mut dict = BTreeMap::new();
                    let mut key = None;
                    for value in items {
                        match key.take() {
                            None      => key = Some(value),
                            Some(key) => { dict.insert(try!(make_hashable(key, pos)), value); }
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
                        dict.insert(try!(make_hashable(key, pos)), value);
                    } else {
                        return Err(Error::Eval(ErrorCode::InvalidStackTop, pos));
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
                                Some(key) => { dict.insert(try!(make_hashable(key, pos)), value); }
                            }
                        }
                    } else {
                        return Err(Error::Eval(ErrorCode::InvalidStackTop, pos));
                    }
                }
                EMPTY_SET => self.stack.push(Value::Set(BTreeSet::new())),
                FROZENSET => {
                    let pos = self.rdr.pos();
                    let items = try!(self.pop_mark());
                    let mut set = BTreeSet::new();
                    for item in items {
                        set.insert(try!(make_hashable(item, pos)));
                    }
                    self.stack.push(Value::FrozenSet(set));
                }
                ADDITEMS => {
                    let pos = self.rdr.pos();
                    let items = try!(self.pop_mark());
                    let top = try!(self.top());
                    if let &mut Value::Set(ref mut set) = top {
                        for item in items {
                            set.insert(try!(make_hashable(item, pos)));
                        }
                    } else {
                        return Err(Error::Eval(ErrorCode::InvalidStackTop, pos));
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
                        _ => return self.error(ErrorCode::InvalidStackTop),
                    };
                    let modname = match try!(self.pop()) {
                        Value::String(string) => string.into_bytes(),
                        _ => return self.error(ErrorCode::InvalidStackTop),
                    };
                    try!(self.handle_global(modname, globname));
                }
                REDUCE => {
                    let mut argtuple = match try!(self.pop()) {
                        Value::Tuple(args) => args.into_vec(),
                        _ => return self.error(ErrorCode::InvalidStackTop),
                    };
                    let callable = try!(self.pop());  // actually an object we construct
                    match callable {
                        Value::Set(mut set) => {
                            match argtuple.pop() {
                                Some(Value::List(items)) => {
                                    let pos = self.rdr.pos();
                                    for item in items {
                                        set.insert(try!(make_hashable(item, pos)));
                                    }
                                    self.stack.push(Value::Set(set));
                                }
                                _ => return self.error(ErrorCode::InvalidStackTop),
                            }
                        }
                        Value::FrozenSet(mut set) => {
                            match argtuple.pop() {
                                Some(Value::List(items)) => {
                                    let pos = self.rdr.pos();
                                    for item in items {
                                        set.insert(try!(make_hashable(item, pos)));
                                    }
                                    self.stack.push(Value::FrozenSet(set));
                                }
                                _ => return self.error(ErrorCode::InvalidStackTop),
                            }
                        }
                        Value::Bytes(mut bytes) => {
                            // Byte object encoded as _codecs.encode(x, 'latin1')
                            println!("{:?}", argtuple);
                            match argtuple.pop() {  // Encoding, always latin1
                                Some(Value::String(_)) => { }
                                _ => return self.error(ErrorCode::InvalidStackTop),
                            }
                            match argtuple.pop() {
                                Some(Value::String(s)) => {
                                    // Now we have to convert the string to latin-1
                                    // encoded bytes.  It never contains codepoints
                                    // above 0xff.
                                    for ch in s.chars() {
                                        bytes.push(ch as u8);
                                    }
                                    self.stack.push(Value::Bytes(bytes));
                                }
                                _ => return self.error(ErrorCode::InvalidStackTop),
                            }
                        }
                        _ => return self.error(ErrorCode::InvalidStackTop),
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

    fn top(&mut self) -> Result<&mut Value> {
        if self.stack.is_empty() {
            return self.error(ErrorCode::StackUnderflow);
        }
        return Ok(self.stack.last_mut().unwrap());
    }

    fn pop_mark(&mut self) -> Result<Vec<Value>> {
        match self.stacks.pop() {
            Some(new) => Ok(mem::replace(&mut self.stack, new)),
            None      => self.error(ErrorCode::StackUnderflow)
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
            (b"__builtin__", b"set") =>
                self.stack.push(Value::Set(BTreeSet::new())),
            (b"__builtin__", b"frozenset") =>
                self.stack.push(Value::FrozenSet(BTreeSet::new())),
            (b"_codecs", b"encode") =>
                self.stack.push(Value::Bytes(Vec::new())),
            _ => return self.error(ErrorCode::Unsupported(GLOBAL as char))
        }
        Ok(())
    }

    fn error<T>(&self, reason: ErrorCode) -> Result<T> {
        Err(Error::Eval(reason, self.rdr.pos()))
    }
}

fn make_hashable(value: Value, pos: usize) -> Result<HashableValue> {
    match value.to_hashable() {
        Some(v) => Ok(v),
        None    => Err(Error::Eval(ErrorCode::ValueNotHashable, pos))
    }
}

/// Decodes a value directly from an iterator.
pub fn value_from_iter<I>(iter: I) -> Result<Value>
    where I: Iterator<Item=io::Result<u8>>
{
    let mut pr = PickleReader::new(iter, false);
    let value = try!(pr.parse());
    // Make sure the whole stream has been consumed.
    try!(pr.end());
    Ok(value)
}

/// Decodes a value from a `std::io::Read`.
pub fn value_from_reader<R: io::Read>(rdr: R) -> Result<Value> {
    value_from_iter(rdr.bytes())
}

/// Decodes a value from a byte slice `&[u8]`.
pub fn value_from_slice(v: &[u8]) -> Result<Value> {
    value_from_iter(v.iter().map(|byte| Ok(*byte)))
}

/// Decodes a json value from a `&str`.
pub fn value_from_str(s: &str) -> Result<Value> {
    value_from_slice(s.as_bytes())
}

/// Decodes a value directly from an iterator.
pub fn from_iter<I, T>(iter: I) -> Result<T>
    where I: Iterator<Item=io::Result<u8>>,
          T: de::Deserialize
{
    from_value(try!(value_from_iter(iter)))
}

/// Decodes a value from a `std::io::Read`.
pub fn from_reader<R: io::Read, T: de::Deserialize>(rdr: R) -> Result<T> {
    from_value(try!(value_from_reader(rdr)))
}

/// Decodes a value from a byte slice `&[u8]`.
pub fn from_slice<T: de::Deserialize>(v: &[u8]) -> Result<T> {
    from_value(try!(value_from_slice(v)))
}

/// Decodes a json value from a `&str`.
pub fn from_str<T: de::Deserialize>(s: &str) -> Result<T> {
    from_value(try!(value_from_str(s)))
}
