use logos::Logos;
use serde::{Deserialize, Serialize};
use arcstr::ArcStr as IString; // Immutable string

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal)
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Logos)]
pub enum ParseError {
    UnexpectedEndOfInput
}