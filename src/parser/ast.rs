use serde::{Deserialize, Serialize};
use arcstr::ArcStr as IString; // Immutable string

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    BinOp(Box<Expression>, BinOp, Box<Expression>)
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

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Associativity {
    Left,
    NotLeft
}

impl BinOp {
    pub fn get_precedence(&self) -> (u8, Associativity) {
        use Associativity::Left as L;
        match self {
            BinOp::Plus => (2, L),
            BinOp::Minus => (2, L),
            BinOp::Times => (3, L),
            BinOp::Div => (3, L),
            BinOp::Mod => (3, L),
            BinOp::And => (0, L),
            BinOp::Or => (0, L),
            BinOp::Equals => (1, L),
            BinOp::NotEquals => (1, L),
            BinOp::LessThan => (1, L),
            BinOp::GreaterThan => (1, L),
            BinOp::LessThanOrEqual => (1, L),
            BinOp::GreaterThanOrEqual => (1, L),
        }
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
pub struct Identifier {
    pub scope: Vec<IString>,
    pub names: Vec<IString>
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParseError {
    UnexpectedEndOfInput,
    UnexpectedToken,
    LexerStuck,
}