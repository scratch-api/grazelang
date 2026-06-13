use std::borrow::Cow;

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
    fn to_string_js(self) -> String;

    /// Equivalent to `Cast.toString` in scratch-vm but tries to avoid allocation
    fn to_cow_str_js(&self) -> Cow<'_, str>;
}

pub trait ScratchVmCompare {
    /// Equivalent to `Cast.compare` in scratch-vm
    fn compare(self, other: Self) -> f64;
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum JsPrimitive {
    String(String),
    /// Only here to reduce allocations
    IString(IString),
    Number(f64),
    Boolean(bool),
}

impl From<JsPrimitive> for Sb3Primitive {
    fn from(value: JsPrimitive) -> Self {
        match value {
            JsPrimitive::String(value) => Sb3Primitive::String(value),
            JsPrimitive::IString(value) => Sb3Primitive::String(value.to_string()),
            JsPrimitive::Number(value) => Sb3Primitive::Float(value),
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
            // TODO: Implement ECMAScript specification for number coersion of String
            value.parse().unwrap_or(0.0)
        }
        match self {
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
            JsPrimitive::String(value) => convert_str_to_bool(value),
            JsPrimitive::IString(value) => convert_str_to_bool(value),
            JsPrimitive::Number(value) => (!value.is_nan()) && *value != 0.0,
            JsPrimitive::Boolean(value) => *value,
        }
    }
}

impl ScratchVmToString for JsPrimitive {
    fn to_string_js(self) -> String {
        match self {
            JsPrimitive::String(value) => value,
            JsPrimitive::IString(value) => value.to_string(),
            JsPrimitive::Number(value) => ryu_js::Buffer::new().format(value).to_string(),
            JsPrimitive::Boolean(value) => value.to_string(),
        }
    }

    fn to_cow_str_js(&self) -> Cow<'_, str> {
        match self {
            JsPrimitive::String(value) => Cow::Borrowed(value),
            JsPrimitive::IString(value) => Cow::Borrowed(value),
            JsPrimitive::Number(value) => {
                Cow::Owned(ryu_js::Buffer::new().format(*value).to_string())
            }
            JsPrimitive::Boolean(value) => Cow::Owned(value.to_string()),
        }
    }
}

impl ScratchVmCompare for &JsPrimitive {
    fn compare(self, other: Self) -> f64 {
        todo!()
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
            C(CharDot, DecimalDigits<FalseType>, Opt<ExponentPart<FalseType>>),
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
            B(NonZeroDigit, Opt<DecimalDigits<TrueType>>, BigIntLiteralSuffix),
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
            B(CharDot, DecimalDigits<TrueType>, Opt<ExponentPart<TrueType>>),
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
        pub struct ExponentPart<Sep: BooleanType>(pub ExponentIndicator, pub SignedInteger<Sep>);
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
            Zero(CharZero),
            One(CharOne),
            Two(CharTwo),
            Three(CharThree),
            Four(CharFour),
            Five(CharFive),
            Six(CharSix),
            Seven(CharSeven),
        }
        // NonOctalDigit :: one of
        //   8 9
        pub enum NonOctalDigit {
            Eight(CharEight),
            Nine(CharNine),
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
            C(
                Sep,
                Ref<HexDigits<Sep>>,
                NumericLiteralSeparator,
                HexDigit,
            ),
        }
        // HexDigit :: one of
        //   0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F
        pub enum HexDigit {
            Zero(CharZero),
            One(CharOne),
            Two(CharTwo),
            Three(CharThree),
            Four(CharFour),
            Five(CharFive),
            Six(CharSix),
            Seven(CharSeven),
            Eight(CharEight),
            Nine(CharNine),
            A(CharA),
            B(CharB),
            C(CharC),
            D(CharD),
            E(CharE),
            F(CharF),
            CapitalA(CharCapitalA),
            CapitalB(CharCapitalB),
            CapitalC(CharCapitalC),
            CapitalD(CharCapitalD),
            CapitalE(CharCapitalE),
            CapitalF(CharCapitalF),
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
    pub fn parse_string_numeric_literal(value: &str) -> f64 {
        todo!()
    }
}
