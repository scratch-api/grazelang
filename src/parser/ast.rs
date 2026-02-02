use serde::{Deserialize, Serialize};
use arcstr::ArcStr as IString; // Immutable string
use crate::lexer::PosRange;

pub trait GetPos {
    fn get_position<'a>(&'a self) -> &'a PosRange;
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    FormattedString(Vec<FormattedStringContent>, PosRange),
    BinOp(Box<Expression>, BinOp, Box<Expression>, PosRange),
    UnOp(UnOp, Box<Expression>, PosRange),
    Identifier(Identifier),
    Call(Identifier, Vec<Expression>, PosRange),
    GetItem(Identifier, Box<Expression>, PosRange),
    Parentheses(Box<Expression>, PosRange)
}

impl GetPos for Expression {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            Expression::Literal(l) => l.get_position(),
            Expression::FormattedString(_, p) => p,
            Expression::BinOp(_, _, _, p) => p,
            Expression::UnOp(_, _, p) => p,
            Expression::Identifier(i) => i.get_position(),
            Expression::Call(_, _, p) => p,
            Expression::GetItem(_, _, p) => p,
            Expression::Parentheses(_, p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinOp {
    Plus(PosRange),
    Minus(PosRange),
    Times(PosRange),
    Div(PosRange),
    Mod(PosRange),
    Join(PosRange),
    And(PosRange),
    Or(PosRange),
    Equals(PosRange),
    NotEquals(PosRange),
    LessThan(PosRange),
    GreaterThan(PosRange),
    LessThanOrEqual(PosRange),
    GreaterThanOrEqual(PosRange),
}

impl GetPos for BinOp {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            BinOp::Plus(p) => p,
            BinOp::Minus(p) => p,
            BinOp::Times(p) => p,
            BinOp::Div(p) => p,
            BinOp::Mod(p) => p,
            BinOp::Join(p) => p,
            BinOp::And(p) => p,
            BinOp::Or(p) => p,
            BinOp::Equals(p) => p,
            BinOp::NotEquals(p) => p,
            BinOp::LessThan(p) => p,
            BinOp::GreaterThan(p) => p,
            BinOp::LessThanOrEqual(p) => p,
            BinOp::GreaterThanOrEqual(p) => p,
        }
    }
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
            BinOp::Plus(_) => (3, L),
            BinOp::Minus(_) => (3, L),
            BinOp::Times(_) => (4, L),
            BinOp::Div(_) => (4, L),
            BinOp::Mod(_) => (4, L),
            BinOp::Join(_) => (2, L),
            BinOp::And(_) => (0, L),
            BinOp::Or(_) => (0, L),
            BinOp::Equals(_) => (1, L),
            BinOp::NotEquals(_) => (1, L),
            BinOp::LessThan(_) => (1, L),
            BinOp::GreaterThan(_) => (1, L),
            BinOp::LessThanOrEqual(_) => (1, L),
            BinOp::GreaterThanOrEqual(_) => (1, L),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnOp {
    Minus(PosRange),
    Not(PosRange),
    Exp(PosRange),
    Pow(PosRange)
}

impl GetPos for UnOp {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            UnOp::Minus(p) => p,
            UnOp::Not(p) => p,
            UnOp::Exp(p) => p,
            UnOp::Pow(p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    String(IString, PosRange),
    DecimalInt(IString, PosRange),
    DecimalFloat(IString, PosRange),
    HexadecimalInt(IString, PosRange),
    OctalInt(IString, PosRange),
    BinaryInt(IString, PosRange)
}

impl GetPos for Literal {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            Literal::String(_, p) => p,
            Literal::DecimalInt(_, p) => p,
            Literal::DecimalFloat(_, p) => p,
            Literal::HexadecimalInt(_, p) => p,
            Literal::OctalInt(_, p) => p,
            Literal::BinaryInt(_, p) => p,
        }
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FormattedStringContent {
    Expression(Expression),
    String(IString, PosRange)
}

impl GetPos for FormattedStringContent {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            FormattedStringContent::Expression(expression) => expression.get_position(),
            FormattedStringContent::String(_, p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Identifier {
    pub scope: Vec<(IString, PosRange)>,
    pub names: Vec<(IString, PosRange)>,
    pub pos_range: PosRange
}

impl GetPos for Identifier {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.pos_range
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParseError {
    UnexpectedEndOfInput,
    UnexpectedToken(PosRange),
    LexerStuck(PosRange),
}