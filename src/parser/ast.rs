use serde::{Deserialize, Serialize};
use arcstr::ArcStr as IString; // Immutable string

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    BinOp(Literal, BinOp, Literal)
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    And,
    Or,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual
}

impl BinOp {
    fn get_precedence() -> u8 {
        1
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    String(IString),
    DecimalInt(IString),
    DecimalFloat(IString),
    HexadecimalInt(IString),
    OctalInt(IString),
    BinaryInt(IString)
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParseError {
    UnexpectedEndOfInput,
    UnexpectedToken,
    LexerStuck,
}