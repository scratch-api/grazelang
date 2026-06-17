use std::{
    borrow::Cow,
    fmt::{Debug, Write},
};

use arcstr::ArcStr as IString;
use grazelang_library::project_json::Sb3Primitive;
use serde::{Deserialize, Serialize};

pub trait ScratchVmToNumber {
    /// Equivalent to `Cast.toNumber` in scratch-vm
    fn to_number(self) -> f64;
}

pub trait ScratchVmToBoolean {
    /// Equivalent to `Cast.toBoolean` in scratch-vm
    fn to_boolean(self) -> bool;
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
    fn compare(self, other: Self) -> f64;
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
    Boolean(bool),
}

pub fn try_convert_f64_into_i128(value: f64) -> Option<i128> {
    (value.is_finite()
        && value.fract() == 0.0
        && value >= i128::MIN as f64
        && value <= i128::MAX as f64)
        .then_some(value as i128)
}

impl From<JsPrimitive> for Sb3Primitive {
    fn from(value: JsPrimitive) -> Self {
        match value {
            JsPrimitive::JsString(value) => Sb3Primitive::String(String::from_utf16_lossy(&value)),
            JsPrimitive::String(value) => Sb3Primitive::String(value),
            JsPrimitive::IString(value) => Sb3Primitive::String(value.to_string()),
            JsPrimitive::Number(value) => {
                if let Some(value) = try_convert_f64_into_i128(value) {
                    if let Ok(value) = value.try_into() {
                        Sb3Primitive::Int(value)
                    } else {
                        Sb3Primitive::Int128(value)
                    }
                } else {
                    Sb3Primitive::Float(value)
                }
            }
            JsPrimitive::Boolean(value) => Sb3Primitive::String(value.to_string()),
        }
    }
}

impl From<Sb3Primitive> for JsPrimitive {
    fn from(value: Sb3Primitive) -> Self {
        match value {
            Sb3Primitive::String(value) => JsPrimitive::String(value),
            Sb3Primitive::Int128(value) => JsPrimitive::String(value.to_string()),
            Sb3Primitive::Int(value) => JsPrimitive::String(value.to_string()),
            Sb3Primitive::Float(value) => JsPrimitive::Number(value),
        }
    }
}

impl ScratchVmToNumber for &JsPrimitive {
    fn to_number(self) -> f64 {
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
            JsPrimitive::Boolean(value) => (*value).into(),
        }
    }
}

impl ScratchVmToBoolean for &JsPrimitive {
    fn to_boolean(self) -> bool {
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
            JsPrimitive::Boolean(value) => *value,
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
            JsPrimitive::Boolean(value) => {
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
            JsPrimitive::Boolean(value) => Cow::Borrowed({
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
            JsPrimitive::Number(value) => string.extend(ryu_js::Buffer::new()
                .format(*value)
                .encode_utf16()),
            JsPrimitive::Boolean(value) => write!(U16Sink { data: string }, "{}", value)
                .expect("a formatting trait implementation returned an error when the underlying stream did not"),
        }
    }
}

impl ScratchVmCompare for &JsPrimitive {
    fn compare(self, other: Self) -> f64 {
        fn convert_to_number_and_ws(value: &JsPrimitive) -> (f64, bool) {
            use parse_ecmascript_string_numeric_literal::parse_string_numeric_literal_and_is_ws;
            match value {
                JsPrimitive::JsString(value) => {
                    parse_string_numeric_literal_and_is_ws(&String::from_utf16_lossy(value))
                }
                JsPrimitive::String(value) => parse_string_numeric_literal_and_is_ws(value),
                JsPrimitive::IString(value) => parse_string_numeric_literal_and_is_ws(value),
                JsPrimitive::Number(value) => (*value, false),
                JsPrimitive::Boolean(value) => ((*value).into(), false),
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
            let chars_1 = char::decode_utf16(str_1.iter().cloned())
                .map(|r| r.unwrap_or(char::REPLACEMENT_CHARACTER))
                .flat_map(|c| c.to_lowercase());
            let chars_2 = char::decode_utf16(str_2.iter().cloned())
                .map(|r| r.unwrap_or(char::REPLACEMENT_CHARACTER))
                .flat_map(|c| c.to_lowercase());
            let ordering = chars_1.cmp(chars_2);
            return match ordering {
                std::cmp::Ordering::Less => -1.0,
                std::cmp::Ordering::Equal => 0.0,
                std::cmp::Ordering::Greater => 1.0,
            };
        }
        if (num_1 == f64::INFINITY && num_2 == f64::INFINITY)
            || (num_1 == f64::NEG_INFINITY && num_2 == f64::NEG_INFINITY)
        {
            return 0.0;
        }
        num_1 - num_2
    }
}

pub mod parse_ecmascript_string_numeric_literal {
    #[cfg(feature = "include_ecmascript_string_numeric_literal_cst")]
    pub mod string_numeric_literal_cst {
        pub trait BooleanType {}
        pub struct TrueType;
        impl BooleanType for TrueType {}
        pub enum FalseType {}
        impl BooleanType for FalseType {}
        type Opt<T> = Option<Box<T>>;
        type Ref<T> = Box<T>;
        // https://tc39.es/ecma262/#sec-tonumber-applied-to-the-string-type
        // StringNumericLiteral :::
        //   StrWhiteSpaceopt
        //   StrWhiteSpaceopt StrNumericLiteral StrWhiteSpaceopt
        pub enum StringNumericLiteral {
            A(Opt<StrWhiteSpace>),
            B(Opt<StrWhiteSpace>, StrNumericLiteral, Opt<StrWhiteSpace>),
        }
        // StrWhiteSpace :::
        //   StrWhiteSpaceChar StrWhiteSpaceopt
        pub struct StrWhiteSpace(pub StrWhiteSpaceChar, pub Opt<StrWhiteSpace>);
        // StrWhiteSpaceChar :::
        //   WhiteSpace
        //   LineTerminator
        pub enum StrWhiteSpaceChar {
            A(WhiteSpace),
            B(LineTerminator),
        }
        // StrNumericLiteral :::
        //   StrDecimalLiteral
        //   NonDecimalIntegerLiteral[~Sep]
        pub enum StrNumericLiteral {
            A(StrDecimalLiteral),
            B(NonDecimalIntegerLiteral<FalseType>),
        }
        // StrDecimalLiteral :::
        //   StrUnsignedDecimalLiteral
        //   + StrUnsignedDecimalLiteral
        //   - StrUnsignedDecimalLiteral
        pub enum StrDecimalLiteral {
            A(StrUnsignedDecimalLiteral),
            B(CharPlus, StrUnsignedDecimalLiteral),
            C(CharMinus, StrUnsignedDecimalLiteral),
        }
        // StrUnsignedDecimalLiteral :::
        //   Infinity
        //   DecimalDigits[~Sep] . DecimalDigits[~Sep]opt ExponentPart[~Sep]opt
        //   . DecimalDigits[~Sep] ExponentPart[~Sep]opt
        //   DecimalDigits[~Sep] ExponentPart[~Sep]opt
        pub enum StrUnsignedDecimalLiteral {
            A(Infinity),
            B(
                DecimalDigits<FalseType>,
                CharDot,
                Opt<DecimalDigits<FalseType>>,
                Opt<ExponentPart<FalseType>>,
            ),
            C(
                CharDot,
                DecimalDigits<FalseType>,
                Opt<ExponentPart<FalseType>>,
            ),
            D(DecimalDigits<FalseType>, Opt<ExponentPart<FalseType>>),
        }
        // NonDecimalIntegerLiteral[Sep] ::
        //   BinaryIntegerLiteral[?Sep]
        //   OctalIntegerLiteral[?Sep]
        //   HexIntegerLiteral[?Sep]
        pub enum NonDecimalIntegerLiteral<Sep: BooleanType> {
            A(BinaryIntegerLiteral<Sep>),
            B(OctalIntegerLiteral<Sep>),
            C(HexIntegerLiteral<Sep>),
        }
        // NumericLiteralSeparator ::
        //   _
        pub struct NumericLiteralSeparator(pub CharUnderscore);
        // NumericLiteral ::
        //   DecimalLiteral
        //   DecimalBigIntegerLiteral
        //   NonDecimalIntegerLiteral[+Sep]
        //   NonDecimalIntegerLiteral[+Sep] BigIntLiteralSuffix
        //   LegacyOctalIntegerLiteral
        pub enum NumericLiteral {
            A(DecimalLiteral),
            B(DecimalBigIntegerLiteral),
            C(NonDecimalIntegerLiteral<TrueType>),
            D(NonDecimalIntegerLiteral<TrueType>, BigIntLiteralSuffix),
            E(LegacyOctalIntegerLiteral),
        }
        // DecimalBigIntegerLiteral ::
        //   0 BigIntLiteralSuffix
        //   NonZeroDigit DecimalDigits[+Sep]opt BigIntLiteralSuffix
        //   NonZeroDigit NumericLiteralSeparator DecimalDigits[+Sep] BigIntLiteralSuffix
        pub enum DecimalBigIntegerLiteral {
            A(CharZero, BigIntLiteralSuffix),
            B(
                NonZeroDigit,
                Opt<DecimalDigits<TrueType>>,
                BigIntLiteralSuffix,
            ),
            C(
                NonZeroDigit,
                NumericLiteralSeparator,
                DecimalDigits<TrueType>,
                BigIntLiteralSuffix,
            ),
        }
        // BigIntLiteralSuffix ::
        //   n
        pub struct BigIntLiteralSuffix(pub CharN);
        // DecimalLiteral ::
        //   DecimalIntegerLiteral . DecimalDigits[+Sep]opt ExponentPart[+Sep]opt
        //   . DecimalDigits[+Sep] ExponentPart[+Sep]opt
        //   DecimalIntegerLiteral ExponentPart[+Sep]opt
        pub enum DecimalLiteral {
            A(
                DecimalIntegerLiteral,
                CharDot,
                Opt<DecimalDigits<TrueType>>,
                Opt<ExponentPart<TrueType>>,
            ),
            B(
                CharDot,
                DecimalDigits<TrueType>,
                Opt<ExponentPart<TrueType>>,
            ),
            C(DecimalIntegerLiteral, Opt<ExponentPart<TrueType>>),
        }
        // DecimalIntegerLiteral ::
        //   0
        //   NonZeroDigit
        //   NonZeroDigit NumericLiteralSeparatoropt DecimalDigits[+Sep]
        //   NonOctalDecimalIntegerLiteral
        pub enum DecimalIntegerLiteral {
            A(CharZero),
            B(NonZeroDigit),
            C(
                NonZeroDigit,
                Opt<NumericLiteralSeparator>,
                DecimalDigits<TrueType>,
            ),
            D(NonOctalDecimalIntegerLiteral),
        }
        // DecimalDigits[Sep] ::
        //   DecimalDigit
        //   DecimalDigits[?Sep] DecimalDigit
        //   [+Sep] DecimalDigits[+Sep] NumericLiteralSeparator DecimalDigit
        pub enum DecimalDigits<Sep: BooleanType> {
            A(DecimalDigit),
            B(Ref<DecimalDigits<Sep>>, DecimalDigit),
            C(
                Sep,
                Ref<DecimalDigits<Sep>>,
                NumericLiteralSeparator,
                DecimalDigit,
            ),
        }
        // DecimalDigit :: one of
        //   0 1 2 3 4 5 6 7 8 9
        pub enum DecimalDigit {
            A(CharZero),
            B(CharOne),
            C(CharTwo),
            D(CharThree),
            E(CharFour),
            F(CharFive),
            G(CharSix),
            H(CharSeven),
            I(CharEight),
            J(CharNine),
        }
        // NonZeroDigit :: one of
        //   1 2 3 4 5 6 7 8 9
        pub enum NonZeroDigit {
            A(CharOne),
            B(CharTwo),
            C(CharThree),
            D(CharFour),
            E(CharFive),
            F(CharSix),
            G(CharSeven),
            H(CharEight),
            I(CharNine),
        }
        // ExponentPart[Sep] ::
        //   ExponentIndicator SignedInteger[?Sep]
        pub struct ExponentPart<Sep: BooleanType>(pub SignedInteger<Sep>);
        // ExponentIndicator :: one of
        //   e E
        pub enum ExponentIndicator {
            A(CharE),
            B(CharCapitalE),
        }
        // SignedInteger[Sep] ::
        //   DecimalDigits[?Sep]
        //   + DecimalDigits[?Sep]
        //   - DecimalDigits[?Sep]
        pub enum SignedInteger<Sep: BooleanType> {
            A(DecimalDigits<Sep>),
            B(CharPlus, DecimalDigits<Sep>),
            C(CharMinus, DecimalDigits<Sep>),
        }
        // BinaryIntegerLiteral[Sep] ::
        //   0b BinaryDigits[?Sep]
        //   0B BinaryDigits[?Sep]
        pub enum BinaryIntegerLiteral<Sep: BooleanType> {
            A(CharZero, CharB, BinaryDigits<Sep>),
            B(CharZero, CharCapitalB, BinaryDigits<Sep>),
        }
        // BinaryDigits[Sep] ::
        //   BinaryDigit
        //   BinaryDigits[?Sep] BinaryDigit
        //   [+Sep] BinaryDigits[+Sep] NumericLiteralSeparator BinaryDigit
        pub enum BinaryDigits<Sep: BooleanType> {
            A(BinaryDigit),
            B(Ref<BinaryDigits<Sep>>, BinaryDigit),
            C(
                Sep,
                Ref<BinaryDigits<Sep>>,
                NumericLiteralSeparator,
                BinaryDigit,
            ),
        }
        // BinaryDigit :: one of
        //   0 1
        pub enum BinaryDigit {
            A(CharZero),
            B(CharOne),
        }
        // OctalIntegerLiteral[Sep] ::
        //   0o OctalDigits[?Sep]
        //   0O OctalDigits[?Sep]
        pub enum OctalIntegerLiteral<Sep: BooleanType> {
            A(CharZero, CharO, OctalDigits<Sep>),
            B(CharZero, CharCapitalO, OctalDigits<Sep>),
        }
        // OctalDigits[Sep] ::
        //   OctalDigit
        //   OctalDigits[?Sep] OctalDigit
        //   [+Sep] OctalDigits[+Sep] NumericLiteralSeparator OctalDigit
        pub enum OctalDigits<Sep: BooleanType> {
            A(OctalDigit),
            B(Ref<OctalDigits<Sep>>, OctalDigit),
            C(
                Sep,
                Ref<OctalDigits<Sep>>,
                NumericLiteralSeparator,
                OctalDigit,
            ),
        }
        // LegacyOctalIntegerLiteral ::
        //   0 OctalDigit
        //   LegacyOctalIntegerLiteral OctalDigit
        pub enum LegacyOctalIntegerLiteral {
            A(CharZero, OctalDigit),
            B(Ref<LegacyOctalIntegerLiteral>, OctalDigit),
        }
        // NonOctalDecimalIntegerLiteral ::
        //   0 NonOctalDigit
        //   LegacyOctalLikeDecimalIntegerLiteral NonOctalDigit
        //   NonOctalDecimalIntegerLiteral DecimalDigit
        pub enum NonOctalDecimalIntegerLiteral {
            A(CharZero, NonOctalDigit),
            B(LegacyOctalLikeDecimalIntegerLiteral, NonOctalDigit),
            C(Ref<NonOctalDecimalIntegerLiteral>, DecimalDigit),
        }
        // LegacyOctalLikeDecimalIntegerLiteral ::
        //   0 OctalDigit
        //   LegacyOctalLikeDecimalIntegerLiteral OctalDigit
        pub enum LegacyOctalLikeDecimalIntegerLiteral {
            A(CharZero, OctalDigit),
            B(Ref<LegacyOctalLikeDecimalIntegerLiteral>, OctalDigit),
        }
        // OctalDigit :: one of
        //   0 1 2 3 4 5 6 7
        pub enum OctalDigit {
            A(CharZero),
            B(CharOne),
            C(CharTwo),
            D(CharThree),
            E(CharFour),
            F(CharFive),
            G(CharSix),
            H(CharSeven),
        }
        // NonOctalDigit :: one of
        //   8 9
        pub enum NonOctalDigit {
            A(CharEight),
            B(CharNine),
        }
        // HexIntegerLiteral[Sep] ::
        //   0x HexDigits[?Sep]
        //   0X HexDigits[?Sep]
        pub enum HexIntegerLiteral<Sep: BooleanType> {
            A(CharZero, CharX, HexDigits<Sep>),
            B(CharZero, CharCapitalX, HexDigits<Sep>),
        }
        // HexDigits[Sep] ::
        //   HexDigit
        //   HexDigits[?Sep] HexDigit
        //   [+Sep] HexDigits[+Sep] NumericLiteralSeparator HexDigit
        pub enum HexDigits<Sep: BooleanType> {
            A(HexDigit),
            B(Ref<HexDigits<Sep>>, HexDigit),
            C(Sep, Ref<HexDigits<Sep>>, NumericLiteralSeparator, HexDigit),
        }
        // HexDigit :: one of
        //   0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F
        pub enum HexDigit {
            A(CharZero),
            B(CharOne),
            C(CharTwo),
            D(CharThree),
            E(CharFour),
            F(CharFive),
            G(CharSix),
            H(CharSeven),
            I(CharEight),
            J(CharNine),
            K(CharA),
            L(CharB),
            M(CharC),
            N(CharD),
            O(CharE),
            P(CharF),
            Q(CharCapitalA),
            R(CharCapitalB),
            S(CharCapitalC),
            T(CharCapitalD),
            U(CharCapitalE),
            V(CharCapitalF),
        }
        pub struct CharZero;
        pub struct CharOne;
        pub struct CharTwo;
        pub struct CharThree;
        pub struct CharFour;
        pub struct CharFive;
        pub struct CharSix;
        pub struct CharSeven;
        pub struct CharEight;
        pub struct CharNine;
        pub struct CharPlus;
        pub struct CharMinus;
        pub struct CharDot;
        pub struct CharN;
        pub struct CharUnderscore;
        pub struct CharB;
        pub struct CharCapitalB;
        pub struct CharO;
        pub struct CharCapitalO;
        pub struct CharX;
        pub struct CharCapitalX;
        pub struct CharE;
        pub struct CharCapitalE;
        pub struct CharA;
        pub struct CharCapitalA;
        pub struct CharC;
        pub struct CharCapitalC;
        pub struct CharD;
        pub struct CharCapitalD;
        pub struct CharF;
        pub struct CharCapitalF;
        pub struct WhiteSpace;
        pub struct LineTerminator;
        pub struct Infinity;
    }
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
