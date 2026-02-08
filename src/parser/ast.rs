use crate::{lexer::PosRange};
use arcstr::ArcStr as IString; // Immutable string
use serde::{Deserialize, Serialize};

pub trait GetPos {
    fn get_position<'a>(&'a self) -> &'a PosRange;
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    DataDeclaration(LetKeyword, MultiDataDeclaration, PosRange),
    Assignment(Identifier, Expression, PosRange),
    SetItem(Identifier, Expression, Expression, PosRange),
    Call(Identifier, Vec<Expression>, PosRange),
    ControlBlock(Identifier, Expression, CodeBlock, PosRange),
    Forever(CodeBlock, PosRange),
    IfElse(Vec<(Expression, CodeBlock)>, Option<CodeBlock>, PosRange),
    Semicolon(PosRange),
}

impl GetPos for Statement {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            Statement::DataDeclaration(_, _, p) => p,
            Statement::Assignment(_, _, p) => p,
            Statement::SetItem(_, _, _, p) => p,
            Statement::Call(_, _, p) => p,
            Statement::ControlBlock(_, _, _, p) => p,
            Statement::Forever(_, p) => p,
            Statement::IfElse(_, _, p) => p,
            Statement::Semicolon(p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LetKeyword(pub PosRange);

impl GetPos for LetKeyword {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarsKeyword(pub PosRange);

impl GetPos for VarsKeyword {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListsKeyword(pub PosRange);

impl GetPos for ListsKeyword {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarKeyword(pub PosRange);

impl GetPos for VarKeyword {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListKeyword(pub PosRange);

impl GetPos for ListKeyword {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SingleDataDeclarationType {
    Unset,
    Var(PosRange),
    List(PosRange)
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MultiDataDeclaration {
    Mixed(
        DataDeclarationScope,
        LeftParens,
        Vec<SingleDataDeclaration>,
        RightParens,
        PosRange,
    ),
    Vars(
        DataDeclarationScope,
        VarsKeyword,
        LeftBrace,
        Vec<SingleDataDeclaration>,
        RightBrace,
        PosRange,
    ),
    Lists(
        DataDeclarationScope,
        ListsKeyword,
        LeftBrace,
        Vec<SingleDataDeclaration>,
        RightBrace,
        PosRange,
    ),
    Single(SingleDataDeclaration),
}

impl GetPos for MultiDataDeclaration {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            MultiDataDeclaration::Mixed(_, _, _, _, p) => p,
            MultiDataDeclaration::Vars(_, _, _, _, _, p) => p,
            MultiDataDeclaration::Lists(_, _, _, _, _, p) => p,
            MultiDataDeclaration::Single(d) => d.get_position(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DataDeclarationScope {
    Unset,
    Global(PosRange),
    Local(PosRange),
    Cloud(PosRange),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AssignmentOperator {
    Set(PosRange),
    None,
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ListEntry {
    Expression(Expression),
    Unwrap(Literal, PosRange)
}

impl GetPos for ListEntry {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            ListEntry::Expression(l) => l.get_position(),
            ListEntry::Unwrap(_, p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SingleDataDeclaration {
    Variable(
        Option<VarKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        Identifier,
        AssignmentOperator,
        Expression,
        PosRange,
    ),
    EmptyVariable(
        Option<VarKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        Identifier,
        PosRange,
    ),
    List(
        Option<ListKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        Identifier,
        LeftBrace,
        Vec<ListEntry>,
        RightBrace,
        PosRange,
    ),
    EmptyList(
        Option<ListKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        Identifier,
        PosRange,
    ),
}

impl GetPos for SingleDataDeclaration {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            SingleDataDeclaration::Variable(_, _, _, _, _, _, p) => p,
            SingleDataDeclaration::EmptyVariable(_, _, _, _, p) => p,
            SingleDataDeclaration::List(_, _, _, _, _, _, _, p) => p,
            SingleDataDeclaration::EmptyList(_, _, _, _, p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CodeBlock {
    pub left_brace: LeftBrace,
    pub statements: Vec<Statement>,
    pub right_brace: RightBrace,
    pub pos_range: PosRange,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LeftBrace(pub PosRange);

impl GetPos for LeftBrace {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RightBrace(pub PosRange);

impl GetPos for RightBrace {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

impl GetPos for CodeBlock {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.pos_range
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    FormattedString(Vec<FormattedStringContent>, PosRange),
    BinOp(Box<Expression>, BinOp, Box<Expression>, PosRange),
    UnOp(UnOp, Box<Expression>, PosRange),
    Identifier(Identifier),
    Call(
        Identifier,
        LeftParens,
        Vec<Expression>,
        RightParens,
        PosRange,
    ),
    GetItem(
        Identifier,
        LeftBracket,
        Box<Expression>,
        RightBracket,
        PosRange,
    ),
    Parentheses(LeftParens, Box<Expression>, RightParens, PosRange),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LeftParens(pub PosRange);

impl GetPos for LeftParens {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RightParens(pub PosRange);

impl GetPos for RightParens {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LeftBracket(pub PosRange);

impl GetPos for LeftBracket {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RightBracket(pub PosRange);

impl GetPos for RightBracket {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

impl GetPos for Expression {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            Expression::Literal(l) => l.get_position(),
            Expression::FormattedString(_, p) => p,
            Expression::BinOp(_, _, _, p) => p,
            Expression::UnOp(_, _, p) => p,
            Expression::Identifier(i) => i.get_position(),
            Expression::Call(_, _, _, _, p) => p,
            Expression::GetItem(_, _, _, _, p) => p,
            Expression::Parentheses(_, _, _, p) => p,
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
    NotLeft,
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
    Pow(PosRange),
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
    BinaryInt(IString, PosRange),
    EmptyExpression(LeftParens, RightParens, PosRange),
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
            Literal::EmptyExpression(_, _, p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FormattedStringContent {
    Expression(Expression),
    String(IString, PosRange),
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
    pub pos_range: PosRange,
}

impl GetPos for Identifier {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.pos_range
    }
}

impl Identifier {
    pub fn is_forever() -> bool {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CanonicalIdentifier {
    pub name: IString,
    pub pos_range: PosRange,
}

impl GetPos for CanonicalIdentifier {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.pos_range
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParseError {
    UnexpectedEndOfInput,
    UnexpectedToken(&'static str, &'static str, PosRange),
    LexerStuck(PosRange),
}
