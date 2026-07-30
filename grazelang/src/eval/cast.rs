use std::{
    borrow::Cow,
    char::DecodeUtf16Error,
    fmt::{Debug, Write},
    vec,
};

use arcstr::ArcStr as IString;
use grazelang_types::project_json::Sb3PrimitiveOrBool;
use serde::{Deserialize, Serialize};

pub trait ScratchVmToNumber {
    /// Equivalent to `Cast.toNumber` in scratch-vm
    fn to_number(&self) -> f64;
}

pub trait ScratchVmToBoolean {
    /// Equivalent to `Cast.toBoolean` in scratch-vm
    fn to_boolean(&self) -> bool;
}

pub trait ScratchVmToString {
    /// Equivalent to `Cast.toString` in scratch-vm
    fn to_js_string(self) -> JsOwnedStringData;

    /// Equivalent to `Cast.toString` in scratch-vm but tries to avoid allocation
    fn to_js_cow_str(&self) -> Cow<'_, JsStringData>;

    fn write_to_js_string(&self, string: &mut JsOwnedStringData);
}

pub trait ScratchVmCompare {
    /// Equivalent to `Cast.compare` in scratch-vm
    fn compare(&self, other: &Self) -> f64;
}

pub trait ScratchVmIsInt {
    /// Equivalent to `Cast.isInt` in scratch-vm
    fn is_int(&self) -> bool;
}

pub type JsOwnedStringData = Vec<u16>;

#[derive(Debug, PartialEq)]
pub struct U16Sink<'a> {
    pub data: &'a mut JsOwnedStringData,
}

impl Write for U16Sink<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.data.extend(s.encode_utf16());
        Ok(())
    }
}

pub type JsStringData = [u16];

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum JsPrimitive {
    JsString(JsOwnedStringData),
    String(String),
    /// Only here to reduce allocations
    IString(IString),
    Number(f64),
    Bool(bool),
}

pub fn try_convert_f64_into_i128(value: f64) -> Option<i128> {
    (value.is_finite()
        && value.fract() == 0.0
        && value >= i128::MIN as f64
        && value <= i128::MAX as f64)
        .then_some(value as i128)
}

impl From<JsPrimitive> for Sb3PrimitiveOrBool {
    fn from(value: JsPrimitive) -> Self {
        match value {
            JsPrimitive::JsString(value) => {
                Sb3PrimitiveOrBool::String(String::from_utf16_lossy(&value))
            }
            JsPrimitive::String(value) => Sb3PrimitiveOrBool::String(value),
            JsPrimitive::IString(value) => Sb3PrimitiveOrBool::String(value.to_string()),
            JsPrimitive::Number(value) => {
                if let Some(value) = try_convert_f64_into_i128(value) {
                    if let Ok(value) = value.try_into() {
                        Sb3PrimitiveOrBool::Int(value)
                    } else {
                        Sb3PrimitiveOrBool::Int128(value)
                    }
                } else {
                    Sb3PrimitiveOrBool::Float(value)
                }
            }
            JsPrimitive::Bool(value) => Sb3PrimitiveOrBool::Bool(value),
        }
    }
}

impl From<Sb3PrimitiveOrBool> for JsPrimitive {
    fn from(value: Sb3PrimitiveOrBool) -> Self {
        match value {
            Sb3PrimitiveOrBool::String(value) => JsPrimitive::String(value),
            Sb3PrimitiveOrBool::Int128(value) => JsPrimitive::String(value.to_string()),
            Sb3PrimitiveOrBool::Int(value) => JsPrimitive::String(value.to_string()),
            Sb3PrimitiveOrBool::Float(value) => JsPrimitive::Number(value),
            Sb3PrimitiveOrBool::Bool(value) => JsPrimitive::Bool(value),
        }
    }
}

impl ScratchVmToNumber for JsPrimitive {
    fn to_number(&self) -> f64 {
        fn convert_str_to_number(value: &str) -> f64 {
            let value =
                parse_ecmascript_string_numeric_literal::parse_string_numeric_literal(value);
            if value.is_nan() {
                return 0.0;
            }
            value
        }
        match self {
            JsPrimitive::JsString(value) => convert_str_to_number(&String::from_utf16_lossy(value)),
            JsPrimitive::String(value) => convert_str_to_number(value),
            JsPrimitive::IString(value) => convert_str_to_number(value),
            JsPrimitive::Number(value) if value.is_nan() => 0.0,
            JsPrimitive::Number(value) => *value,
            JsPrimitive::Bool(value) => (*value).into(),
        }
    }
}

impl ScratchVmToBoolean for JsPrimitive {
    fn to_boolean(&self) -> bool {
        fn convert_str_to_bool(value: &str) -> bool {
            match value {
                "" | "0" => false,
                value if value.eq_ignore_ascii_case("false") => false,
                _ => true,
            }
        }
        match self {
            JsPrimitive::JsString(value) => convert_str_to_bool(&String::from_utf16_lossy(value)),
            JsPrimitive::String(value) => convert_str_to_bool(value),
            JsPrimitive::IString(value) => convert_str_to_bool(value),
            JsPrimitive::Number(value) => (!value.is_nan()) && *value != 0.0,
            JsPrimitive::Bool(value) => *value,
        }
    }
}

impl ScratchVmToString for JsPrimitive {
    fn to_js_string(self) -> JsOwnedStringData {
        match self {
            JsPrimitive::JsString(value) => value,
            JsPrimitive::String(value) => value.encode_utf16().collect(),
            JsPrimitive::IString(value) => value.encode_utf16().collect(),
            JsPrimitive::Number(value) => {
                ryu_js::Buffer::new().format(value).encode_utf16().collect()
            }
            JsPrimitive::Bool(value) => {
                if value {
                    vec![b't' as u16, b'r' as u16, b'u' as u16, b'e' as u16]
                } else {
                    vec![
                        b'f' as u16,
                        b'a' as u16,
                        b'l' as u16,
                        b's' as u16,
                        b'e' as u16,
                    ]
                }
            }
        }
    }

    fn to_js_cow_str(&self) -> Cow<'_, JsStringData> {
        match self {
            JsPrimitive::JsString(value) => Cow::Borrowed(value),
            JsPrimitive::String(value) => Cow::Owned(value.encode_utf16().collect()),
            JsPrimitive::IString(value) => Cow::Owned(value.encode_utf16().collect()),
            JsPrimitive::Number(value) => Cow::Owned(
                ryu_js::Buffer::new()
                    .format(*value)
                    .encode_utf16()
                    .collect(),
            ),
            JsPrimitive::Bool(value) => Cow::Borrowed({
                if *value {
                    &[b't' as u16, b'r' as u16, b'u' as u16, b'e' as u16]
                } else {
                    &[
                        b'f' as u16,
                        b'a' as u16,
                        b'l' as u16,
                        b's' as u16,
                        b'e' as u16,
                    ]
                }
            }),
        }
    }

    fn write_to_js_string(&self, string: &mut JsOwnedStringData) {
        match self {
            JsPrimitive::JsString(value) => string.extend(value),
            JsPrimitive::String(value) => string.extend(value.encode_utf16()),
            JsPrimitive::IString(value) => string.extend(value.encode_utf16()),
            JsPrimitive::Number(value) => {
                string.extend(ryu_js::Buffer::new().format(*value).encode_utf16())
            }
            JsPrimitive::Bool(value) => write!(U16Sink { data: string }, "{}", value).unwrap(),
        }
    }
}

impl ScratchVmCompare for JsPrimitive {
    fn compare(&self, other: &Self) -> f64 {
        fn convert_to_number_and_ws(value: &JsPrimitive) -> (f64, bool) {
            use parse_ecmascript_string_numeric_literal::parse_string_numeric_literal_and_is_ws;
            match value {
                JsPrimitive::JsString(value) => {
                    parse_string_numeric_literal_and_is_ws(&String::from_utf16_lossy(value))
                }
                JsPrimitive::String(value) => parse_string_numeric_literal_and_is_ws(value),
                JsPrimitive::IString(value) => parse_string_numeric_literal_and_is_ws(value),
                JsPrimitive::Number(value) => (*value, false),
                JsPrimitive::Bool(value) => ((*value).into(), false),
            }
        }
        let (mut num_1, ws_1) = convert_to_number_and_ws(self);
        let (mut num_2, ws_2) = convert_to_number_and_ws(other);
        if ws_1 {
            num_1 = f64::NAN;
        } else if ws_2 {
            num_2 = f64::NAN;
        }
        if num_1.is_nan() || num_2.is_nan() {
            let str_1 = self.to_js_cow_str();
            let str_2 = other.to_js_cow_str();
            if str_1 == str_2 {
                return 0.0;
            }
            let mut iter_a = char::decode_utf16(str_1.iter().cloned()).to_lowercase();
            let mut iter_b = char::decode_utf16(str_2.iter().cloned()).to_lowercase();
            loop {
                let val_a = iter_a.next();
                let val_b = iter_b.next();
                return match (val_a, val_b) {
                    (Some(a), Some(b)) => match a.cmp(&b) {
                        std::cmp::Ordering::Equal => continue,
                        std::cmp::Ordering::Less => -1.0,
                        std::cmp::Ordering::Greater => 1.0,
                    },
                    (None, None) => 0.0,
                    (None, Some(_)) => -1.0,
                    (Some(_), None) => 1.0,
                };
            }
        }
        if (num_1 == f64::INFINITY && num_2 == f64::INFINITY)
            || (num_1 == f64::NEG_INFINITY && num_2 == f64::NEG_INFINITY)
        {
            return 0.0;
        }
        num_1 - num_2
    }
}

impl ScratchVmIsInt for JsPrimitive {
    fn is_int(&self) -> bool {
        match self {
            JsPrimitive::JsString(value) => !value.contains(&(b'.' as u16)),
            JsPrimitive::String(value) => !value.contains('.'),
            JsPrimitive::IString(value) => !value.contains('.'),
            JsPrimitive::Number(value) => {
                value.is_nan() || try_convert_f64_into_i128(*value).is_some()
            }
            JsPrimitive::Bool(_) => true,
        }
    }
}

pub mod parse_ecmascript_string_numeric_literal {
    pub fn ecmascript_is_str_white_space_char(value: char) -> bool {
        matches!(
            value as u32,
            0x0009..=0x000D | 0xFEFF | 0x0020 | 0x00A0 | 0x1680 | 0x2000
                ..=0x200A | 0x202F | 0x205F | 0x3000 | 0x2028 | 0x2029
        )
    }
    pub fn parse_string_numeric_literal_and_is_ws(value: &str) -> (f64, bool) {
        let value = value.trim_matches(ecmascript_is_str_white_space_char);
        match value {
            "" => return (0.0, true),
            "Infinity" | "+Infinity" => return (f64::INFINITY, false),
            "-Infinity" => return (f64::NEG_INFINITY, false),
            _ => (),
        }
        let base = {
            let mut i = value.bytes();
            match i.next().unwrap() {
                b'0' => {
                    if let Some(c) = i.next() {
                        match c {
                            b'b' | b'B' => Some(2),
                            b'o' | b'O' => Some(8),
                            b'x' | b'X' => Some(16),
                            _ => None,
                        }
                    } else {
                        return (0.0, false);
                    }
                }
                b'+' | b'-' | b'.' | b'1'..=b'9' => None,
                _ => return (f64::NAN, false),
            }
        };
        if let Some(base) = base {
            let value = &value[2..];
            if value.is_empty() {
                return (f64::NAN, false);
            }
            return (
                u32::from_str_radix(value, base)
                    .map(Into::into)
                    .unwrap_or_else(|_| {
                        let mut current_value = 0.0_f64;
                        for c in value.chars() {
                            if let Some(c) = c.to_digit(base) {
                                current_value = current_value.mul_add(base as f64, c as f64);
                            } else {
                                return f64::NAN;
                            }
                        }
                        current_value
                    }),
                false,
            );
        }
        (fast_float2::parse(value).unwrap_or(f64::NAN), false)
    }
    pub fn parse_string_numeric_literal(value: &str) -> f64 {
        parse_string_numeric_literal_and_is_ws(value).0
    }
}

pub trait ToLowercaseU16: Iterator<Item = Result<char, DecodeUtf16Error>> + Sized {
    fn to_lowercase(self) -> ToLowercaseU16Iterator<Self> {
        ToLowercaseU16Iterator {
            iterator: self,
            buf: Default::default(),
            buf_idx: 0,
            buf_len: 0,
        }
    }
}

impl<I> ToLowercaseU16 for I where I: Iterator<Item = Result<char, DecodeUtf16Error>> {}

pub struct ToLowercaseU16Iterator<I>
where
    I: Iterator<Item = Result<char, DecodeUtf16Error>>,
{
    pub iterator: I,
    pub buf: [u16; 8],
    pub buf_len: usize,
    pub buf_idx: usize,
}

impl<I> Iterator for ToLowercaseU16Iterator<I>
where
    I: Iterator<Item = Result<char, DecodeUtf16Error>>,
{
    type Item = u16;
    fn next(&mut self) -> Option<Self::Item> {
        if self.buf_idx < self.buf_len {
            let value = self.buf[self.buf_idx];
            self.buf_idx += 1;
            return Some(value);
        }
        self.buf_len = match self.iterator.next()? {
            Ok(value) => {
                let mut index = 0;
                for c in value.to_lowercase() {
                    index += c.encode_utf16(&mut self.buf[index..]).len();
                }
                index
            }
            Err(value) => {
                self.buf[0] = value.unpaired_surrogate();
                1
            }
        };
        self.buf_idx = 1;
        Some(self.buf[0])
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.iterator.size_hint();
        (lower, upper.map(|value| value.saturating_mul(8)))
    }
}
