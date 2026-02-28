use crate::lexer::PosRange;
use arcstr::ArcStr as IString; // Immutable string
use serde::{Deserialize, Serialize};

pub trait GetPos {
    fn get_position<'a>(&'a self) -> &'a PosRange;
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GrazeProgram(pub Vec<TopLevelStatement>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TopLevelStatement {
    Stage(StageKeyword, StageCodeBlock, Option<Semicolon>, PosRange),
    Sprite(SpriteKeyword, Option<CanonicalIdentifier>, Identifier, SpriteCodeBlock, Option<Semicolon>, PosRange),
    BroadcastDeclaration(BroadcastKeyword, Option<CanonicalIdentifier>, Identifier, Semicolon, PosRange),
    // Mod(),
    EmptyStatement(Semicolon),
}

impl GetPos for TopLevelStatement {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            TopLevelStatement::Stage(_, _, _, p) => p,
            TopLevelStatement::Sprite(_, _, _, _, _, p) => p,
            TopLevelStatement::BroadcastDeclaration(_, _, _, _, p) => p,
            TopLevelStatement::EmptyStatement(p) => &p.0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SpriteKeyword(pub PosRange);

impl GetPos for SpriteKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StageKeyword(pub PosRange);

impl GetPos for StageKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CostumeKeyword(pub PosRange);

impl GetPos for CostumeKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BroadcastKeyword(pub PosRange);

impl GetPos for BroadcastKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BackdropKeyword(pub PosRange);

impl GetPos for BackdropKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SoundKeyword(pub PosRange);

impl GetPos for SoundKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StageStatement {
    DataDeclaration(LetKeyword, DataDeclaration, Semicolon, PosRange),
    BackdropDeclaration(BackdropKeyword, AssetDeclaration, Semicolon, PosRange),
    SoundDeclaration(SoundKeyword, AssetDeclaration, Semicolon, PosRange),
    SingleInputHatStatement(
        Identifier,
        Expression,
        CodeBlock,
        Option<Semicolon>,
        PosRange,
    ),
    MultiInputHatStatement(
        Identifier,
        LeftParens,
        Vec<(Expression, Option<Comma>)>,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        PosRange,
    ),
    // Mod(),
    EmptyStatement(Semicolon),
}

impl GetPos for StageStatement {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            StageStatement::DataDeclaration(_, _, _, p) => p,
            StageStatement::BackdropDeclaration(_, _, _, p) => p,
            StageStatement::SoundDeclaration(_, _, _, p) => p,
            StageStatement::SingleInputHatStatement(_, _, _, _, p) => p,
            StageStatement::MultiInputHatStatement(_, _, _, _, _, _, p) => p,
            StageStatement::EmptyStatement(p) => &p.0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SpriteStatement {
    DataDeclaration(LetKeyword, DataDeclaration, Semicolon, PosRange),
    CostumeDeclaration(CostumeKeyword, AssetDeclaration, Semicolon, PosRange),
    SoundDeclaration(SoundKeyword, AssetDeclaration, Semicolon, PosRange),
    SingleInputHatStatement(
        Identifier,
        Expression,
        CodeBlock,
        Option<Semicolon>,
        PosRange,
    ),
    MultiInputHatStatement(
        Identifier,
        LeftParens,
        Vec<(Expression, Option<Comma>)>,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        PosRange,
    ),
    // Mod(),
    EmptyStatement(Semicolon),
}

impl GetPos for SpriteStatement {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            SpriteStatement::DataDeclaration(_, _, _, p) => p,
            SpriteStatement::CostumeDeclaration(_, _, _, p) => p,
            SpriteStatement::SoundDeclaration(_, _, _, p) => p,
            SpriteStatement::SingleInputHatStatement(_, _, _, _, p) => p,
            SpriteStatement::MultiInputHatStatement(_, _, _, _, _, _, p) => p,
            SpriteStatement::EmptyStatement(p) => &p.0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AssetDeclaration {
    Multiple(
        LeftParens,
        Vec<(SingleAssetDeclaration, Option<Comma>)>,
        RightParens,
        PosRange,
    ),
    Single(SingleAssetDeclaration),
}

impl GetPos for AssetDeclaration {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            AssetDeclaration::Multiple(_, _, _, p) => p,
            AssetDeclaration::Single(d) => d.get_position(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SingleAssetDeclaration(
    pub Option<CanonicalIdentifier>,
    pub Identifier,
    pub LeftParens,
    pub Literal,
    pub RightParens,
    pub PosRange,
);

impl GetPos for SingleAssetDeclaration {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.5
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    DataDeclaration(LetKeyword, DataDeclaration, Semicolon, PosRange),
    Assignment(
        Identifier,
        NormalAssignmentOperator,
        Expression,
        Semicolon,
        PosRange,
    ),
    ListAssignment(
        Identifier,
        NormalAssignmentOperator,
        LeftBrace,
        Vec<(ListEntry, Option<Comma>)>,
        RightBrace,
        Semicolon,
        PosRange,
    ),
    SetItem(
        Identifier,
        LeftBracket,
        Expression,
        RightBracket,
        NormalAssignmentOperator,
        Expression,
        Semicolon,
        PosRange,
    ),
    Call(
        Identifier,
        LeftParens,
        Vec<(Expression, Option<Comma>)>,
        RightParens,
        Semicolon,
        PosRange,
    ),
    SingleInputControl(
        Identifier,
        Expression,
        CodeBlock,
        Option<Semicolon>,
        PosRange,
    ),
    MultiInputControl(
        Identifier,
        LeftParens,
        Vec<(Expression, Option<Comma>)>,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        PosRange,
    ),
    Forever(Identifier, CodeBlock, Option<Semicolon>, PosRange),
    IfElse(
        (Identifier, Expression, CodeBlock),
        Vec<(Identifier, Identifier, Expression, CodeBlock)>,
        Option<(Identifier, CodeBlock)>,
        Option<Semicolon>,
        PosRange,
    ),
    EmptyStatement(Semicolon),
}

impl GetPos for Statement {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            Statement::DataDeclaration(_, _, _, p) => p,
            Statement::Assignment(_, _, _, _, p) => p,
            Statement::ListAssignment(_, _, _, _, _, _, p) => p,
            Statement::SetItem(_, _, _, _, _, _, _, p) => p,
            Statement::Call(_, _, _, _, _, p) => p,
            Statement::SingleInputControl(_, _, _, _, p) => p,
            Statement::MultiInputControl(_, _, _, _, _, _, p) => p,
            Statement::Forever(_, _, _, p) => p,
            Statement::IfElse(_, _, _, _, p) => p,
            Statement::EmptyStatement(p) => &p.0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LetKeyword(pub PosRange);

impl GetPos for LetKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarsKeyword(pub PosRange);

impl GetPos for VarsKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListsKeyword(pub PosRange);

impl GetPos for ListsKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarKeyword(pub PosRange);

impl GetPos for VarKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListKeyword(pub PosRange);

impl GetPos for ListKeyword {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SingleDataDeclarationType {
    Unset,
    Var(PosRange),
    List(PosRange),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DataDeclaration {
    Mixed(
        DataDeclarationScope,
        LeftParens,
        Vec<(SingleDataDeclaration, Option<Comma>)>,
        RightParens,
        PosRange,
    ),
    Vars(
        DataDeclarationScope,
        VarsKeyword,
        LeftBrace,
        Vec<(SingleDataDeclaration, Option<Comma>)>,
        RightBrace,
        PosRange,
    ),
    Lists(
        DataDeclarationScope,
        ListsKeyword,
        LeftBrace,
        Vec<(SingleDataDeclaration, Option<Comma>)>,
        RightBrace,
        PosRange,
    ),
    Single(SingleDataDeclaration),
}

impl GetPos for DataDeclaration {
    fn get_position<'a>(&'a self) -> &'a PosRange {
        match self {
            DataDeclaration::Mixed(_, _, _, _, p) => p,
            DataDeclaration::Vars(_, _, _, _, _, p) => p,
            DataDeclaration::Lists(_, _, _, _, _, p) => p,
            DataDeclaration::Single(d) => d.get_position(),
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
pub struct NormalAssignmentOperator(pub PosRange);

impl GetPos for NormalAssignmentOperator {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ListEntry {
    Expression(Expression),
    Unwrap(Literal, PosRange),
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
        NormalAssignmentOperator,
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
        NormalAssignmentOperator,
        LeftBrace,
        Vec<(ListEntry, Option<Comma>)>,
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
            SingleDataDeclaration::List(_, _, _, _, _, _, _, _, p) => p,
            SingleDataDeclaration::EmptyList(_, _, _, _, p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Comma(pub PosRange);

impl GetPos for Comma {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LeftBrace(pub PosRange);

impl GetPos for LeftBrace {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RightBrace(pub PosRange);

impl GetPos for RightBrace {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StageCodeBlock {
    pub left_brace: LeftBrace,
    pub statements: Vec<StageStatement>,
    pub right_brace: RightBrace,
    pub pos_range: PosRange,
}

impl GetPos for StageCodeBlock {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.pos_range
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SpriteCodeBlock {
    pub left_brace: LeftBrace,
    pub statements: Vec<SpriteStatement>,
    pub right_brace: RightBrace,
    pub pos_range: PosRange,
}

impl GetPos for SpriteCodeBlock {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.pos_range
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CodeBlock {
    pub left_brace: LeftBrace,
    pub statements: Vec<Statement>,
    pub right_brace: RightBrace,
    pub pos_range: PosRange,
}

impl GetPos for CodeBlock {
    #[inline]
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
        Vec<(Expression, Option<Comma>)>,
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
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RightParens(pub PosRange);

impl GetPos for RightParens {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LeftBracket(pub PosRange);

impl GetPos for LeftBracket {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RightBracket(pub PosRange);

impl GetPos for RightBracket {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.0
    }
}
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Semicolon(pub PosRange);

impl GetPos for Semicolon {
    #[inline]
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

/// What counts as `scope`, what counts as `names`?
///
/// When there is only one segment: it counts as `names`.
///
/// When there are multiple segments: everything before the first dot is in `scope`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Identifier {
    pub scope: Vec<(IString, PosRange)>,
    pub names: Vec<(IString, PosRange)>,
    pub pos_range: PosRange,
}

impl GetPos for Identifier {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.pos_range
    }
}

impl Identifier {
    pub fn is_forever(&self) -> bool {
        todo!()
    }
    pub fn is_if(&self) -> bool {
        todo!()
    }
    pub fn is_else(&self) -> bool {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CanonicalIdentifier {
    pub name: IString,
    pub pos_range: PosRange,
}

impl GetPos for CanonicalIdentifier {
    #[inline]
    fn get_position<'a>(&'a self) -> &'a PosRange {
        &self.pos_range
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParseError {
    UnexpectedEndOfInput(&'static str),
    UnexpectedToken(&'static str, &'static str, &'static str, PosRange),
    LexerStuck(&'static str, PosRange),
}
