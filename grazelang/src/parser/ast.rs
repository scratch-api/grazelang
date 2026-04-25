use std::collections::HashMap;

use crate::{
    codegen::project_json::{Sb3Primitive, Sb3PrimitiveBlock},
    lexer::PosRange,
};
use arcstr::ArcStr as IString; // Immutable string
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub trait GetPos {
    fn get_position(&self) -> &PosRange;
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GrazeProgram(pub Vec<TopLevelStatement>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TopLevelStatement {
    Stage(StageKeyword, StageCodeBlock, Option<Semicolon>, PosRange),
    Sprite(
        SpriteKeyword,
        Option<CanonicalIdentifier>,
        Identifier,
        SpriteCodeBlock,
        Option<Semicolon>,
        PosRange,
    ),
    BroadcastDeclaration(
        BroadcastKeyword,
        Option<CanonicalIdentifier>,
        Identifier,
        Semicolon,
        PosRange,
    ),
    EmptyStatement(Semicolon),
}

impl GetPos for TopLevelStatement {
    fn get_position(&self) -> &PosRange {
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
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StageKeyword(pub PosRange);

impl GetPos for StageKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CostumeKeyword(pub PosRange);

impl GetPos for CostumeKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BroadcastKeyword(pub PosRange);

impl GetPos for BroadcastKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BackdropKeyword(pub PosRange);

impl GetPos for BackdropKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SoundKeyword(pub PosRange);

impl GetPos for SoundKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProcKeyword(pub PosRange);

impl GetPos for ProcKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WarpSpecifier {
    pub is_warp: bool,
    pub pos_range: PosRange,
}

impl GetPos for WarpSpecifier {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.pos_range
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum CustomBlockParamKindValue {
    Number,
    String,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CustomBlockParamKind {
    pub kind: CustomBlockParamKindValue,
    pub pos_range: PosRange,
}

impl GetPos for CustomBlockParamKind {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.pos_range
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StageStatement {
    DataDeclaration(LetKeyword, DataDeclaration, Semicolon, PosRange),
    BackdropDeclaration(BackdropKeyword, AssetDeclaration, Semicolon, PosRange),
    SoundDeclaration(SoundKeyword, AssetDeclaration, Semicolon, PosRange),
    NoInputHatStatement(Identifier, CodeBlock, Option<Semicolon>, PosRange),
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
    CustomBlockDefinition(
        Option<WarpSpecifier>,
        ProcKeyword,
        Option<CanonicalIdentifier>,
        Identifier,
        LeftParens,
        Vec<(
            Option<CustomBlockParamKind>,
            Option<CanonicalIdentifier>,
            Identifier,
            Option<Comma>,
        )>,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        PosRange,
    ),
    IsolatedBlock(CodeBlock, Option<Semicolon>, PosRange),
    IsolatedExpression(
        LeftParens,
        Expression,
        RightParens,
        Option<Semicolon>,
        PosRange,
    ),
    EmptyStatement(Semicolon),
}

impl GetPos for StageStatement {
    fn get_position(&self) -> &PosRange {
        match self {
            StageStatement::DataDeclaration(_, _, _, p) => p,
            StageStatement::BackdropDeclaration(_, _, _, p) => p,
            StageStatement::SoundDeclaration(_, _, _, p) => p,
            StageStatement::NoInputHatStatement(_, _, _, p) => p,
            StageStatement::SingleInputHatStatement(_, _, _, _, p) => p,
            StageStatement::MultiInputHatStatement(_, _, _, _, _, _, p) => p,
            StageStatement::CustomBlockDefinition(_, _, _, _, _, _, _, _, _, p) => p,
            StageStatement::IsolatedBlock(_, _, p) => p,
            StageStatement::IsolatedExpression(_, _, _, _, p) => p,
            StageStatement::EmptyStatement(p) => &p.0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SpriteStatement {
    DataDeclaration(LetKeyword, DataDeclaration, Semicolon, PosRange),
    CostumeDeclaration(CostumeKeyword, AssetDeclaration, Semicolon, PosRange),
    SoundDeclaration(SoundKeyword, AssetDeclaration, Semicolon, PosRange),
    NoInputHatStatement(Identifier, CodeBlock, Option<Semicolon>, PosRange),
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
    CustomBlockDefinition(
        Option<WarpSpecifier>,
        ProcKeyword,
        Option<CanonicalIdentifier>,
        Identifier,
        LeftParens,
        Vec<(
            Option<CustomBlockParamKind>,
            Option<CanonicalIdentifier>,
            Identifier,
            Option<Comma>,
        )>,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        PosRange,
    ),
    IsolatedBlock(CodeBlock, Option<Semicolon>, PosRange),
    IsolatedExpression(
        LeftParens,
        Expression,
        RightParens,
        Option<Semicolon>,
        PosRange,
    ),
    EmptyStatement(Semicolon),
}

impl GetPos for SpriteStatement {
    fn get_position(&self) -> &PosRange {
        match self {
            SpriteStatement::DataDeclaration(_, _, _, p) => p,
            SpriteStatement::CostumeDeclaration(_, _, _, p) => p,
            SpriteStatement::SoundDeclaration(_, _, _, p) => p,
            SpriteStatement::NoInputHatStatement(_, _, _, p) => p,
            SpriteStatement::SingleInputHatStatement(_, _, _, _, p) => p,
            SpriteStatement::MultiInputHatStatement(_, _, _, _, _, _, p) => p,
            SpriteStatement::CustomBlockDefinition(_, _, _, _, _, _, _, _, _, p) => p,
            SpriteStatement::IsolatedBlock(_, _, p) => p,
            SpriteStatement::IsolatedExpression(_, _, _, _, p) => p,
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
    fn get_position(&self) -> &PosRange {
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
    pub (IString, PosRange),
    pub RightParens,
    pub PosRange,
);

impl GetPos for SingleAssetDeclaration {
    #[inline]
    fn get_position(&self) -> &PosRange {
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
        LeftBracket,
        Vec<(ListEntry, Option<Comma>)>,
        RightBracket,
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
        (SyntacticIf, Expression, CodeBlock),
        Vec<(SyntacticElse, SyntacticIf, Expression, CodeBlock)>,
        Option<(SyntacticElse, CodeBlock)>,
        Option<Semicolon>,
        PosRange,
    ),
    EmptyStatement(Semicolon),
}

impl GetPos for Statement {
    fn get_position(&self) -> &PosRange {
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
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarsKeyword(pub PosRange);

impl GetPos for VarsKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListsKeyword(pub PosRange);

impl GetPos for ListsKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarKeyword(pub PosRange);

impl GetPos for VarKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListKeyword(pub PosRange);

impl GetPos for ListKeyword {
    #[inline]
    fn get_position(&self) -> &PosRange {
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
    Single(Box<SingleDataDeclaration>),
}

impl GetPos for DataDeclaration {
    fn get_position(&self) -> &PosRange {
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
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ListEntry {
    Expression(Expression),
    Unwrap(Literal, PosRange),
}

impl GetPos for ListEntry {
    fn get_position(&self) -> &PosRange {
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
        LeftBracket,
        Vec<(ListEntry, Option<Comma>)>,
        RightBracket,
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
    fn get_position(&self) -> &PosRange {
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
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LeftBrace(pub PosRange);

impl GetPos for LeftBrace {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RightBrace(pub PosRange);

impl GetPos for RightBrace {
    #[inline]
    fn get_position(&self) -> &PosRange {
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
    fn get_position(&self) -> &PosRange {
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
    fn get_position(&self) -> &PosRange {
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
    fn get_position(&self) -> &PosRange {
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
    GetLetter(
        Box<Expression>,
        LetterAccessLeftBracket,
        Box<Expression>,
        RightBracket,
        PosRange,
    ),
    Parentheses(LeftParens, Box<Expression>, RightParens, PosRange),
}

impl Expression {
    pub fn is_empty(&self) -> bool {
        matches!(self, Expression::Literal(Literal::EmptyExpression(..)))
    }

    pub fn calculate_value(&self) -> Sb3Primitive {
        match self {
            Expression::Literal(literal) => {
                Sb3Primitive::String(literal.get_string_value().to_string())
            }
            // TODO: try to calculate the value or warn the user
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LeftParens(pub PosRange);

impl GetPos for LeftParens {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RightParens(pub PosRange);

impl GetPos for RightParens {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LeftBracket(pub PosRange);

impl GetPos for LeftBracket {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RightBracket(pub PosRange);

impl GetPos for RightBracket {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LetterAccessLeftBracket(pub PosRange);

impl GetPos for LetterAccessLeftBracket {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Semicolon(pub PosRange);

impl GetPos for Semicolon {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

impl GetPos for Expression {
    fn get_position(&self) -> &PosRange {
        match self {
            Expression::Literal(l) => l.get_position(),
            Expression::FormattedString(_, p) => p,
            Expression::BinOp(_, _, _, p) => p,
            Expression::UnOp(_, _, p) => p,
            Expression::Identifier(i) => i.get_position(),
            Expression::Call(_, _, _, _, p) => p,
            Expression::GetItem(_, _, _, _, p) => p,
            Expression::GetLetter(_, _, _, _, p) => p,
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
    fn get_position(&self) -> &PosRange {
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BinOpDescriptor {
    pub opcode: String,
    pub operand_a_input_name: String,
    pub operand_b_input_name: String,
    pub operand_a_default: Option<Sb3PrimitiveBlock>,
    pub operand_b_default: Option<Sb3PrimitiveBlock>,
    pub is_negated: bool,
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

    pub fn get_descriptor(&self) -> BinOpDescriptor {
        match self {
            BinOp::Plus(_) => BinOpDescriptor {
                opcode: "operator_add".to_string(),
                operand_a_input_name: "NUM1".to_string(),
                operand_b_input_name: "NUM2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("".into()),
                is_negated: false,
            },
            BinOp::Minus(_) => BinOpDescriptor {
                opcode: "operator_subtract".to_string(),
                operand_a_input_name: "NUM1".to_string(),
                operand_b_input_name: "NUM2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("".into()),
                is_negated: false,
            },
            BinOp::Times(_) => BinOpDescriptor {
                opcode: "operator_multiply".to_string(),
                operand_a_input_name: "NUM1".to_string(),
                operand_b_input_name: "NUM2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("".into()),
                is_negated: false,
            },
            BinOp::Div(_) => BinOpDescriptor {
                opcode: "operator_divide".to_string(),
                operand_a_input_name: "NUM1".to_string(),
                operand_b_input_name: "NUM2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("".into()),
                is_negated: false,
            },
            BinOp::Mod(_) => BinOpDescriptor {
                opcode: "operator_mod".to_string(),
                operand_a_input_name: "NUM1".to_string(),
                operand_b_input_name: "NUM2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("".into()),
                is_negated: false,
            },
            BinOp::Join(_) => BinOpDescriptor {
                opcode: "operator_join".to_string(),
                operand_a_input_name: "STRING1".to_string(),
                operand_b_input_name: "STRING2".to_string(),
                operand_a_default: Some("apple ".into()),
                operand_b_default: Some("banana".into()),
                is_negated: false,
            },
            BinOp::And(_) => BinOpDescriptor {
                opcode: "operator_and".to_string(),
                operand_a_input_name: "OPERAND1".to_string(),
                operand_b_input_name: "OPERAND2".to_string(),
                operand_a_default: None,
                operand_b_default: None,
                is_negated: false,
            },
            BinOp::Or(_) => BinOpDescriptor {
                opcode: "operator_or".to_string(),
                operand_a_input_name: "OPERAND1".to_string(),
                operand_b_input_name: "OPERAND2".to_string(),
                operand_a_default: None,
                operand_b_default: None,
                is_negated: false,
            },
            BinOp::Equals(_) => BinOpDescriptor {
                opcode: "operator_equals".to_string(),
                operand_a_input_name: "OPERAND1".to_string(),
                operand_b_input_name: "OPERAND2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("50".into()),
                is_negated: false,
            },
            BinOp::NotEquals(_) => BinOpDescriptor {
                opcode: "operator_equals".to_string(),
                operand_a_input_name: "OPERAND1".to_string(),
                operand_b_input_name: "OPERAND2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("50".into()),
                is_negated: true,
            },
            BinOp::LessThan(_) => BinOpDescriptor {
                opcode: "operator_lt".to_string(),
                operand_a_input_name: "OPERAND1".to_string(),
                operand_b_input_name: "OPERAND2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("50".into()),
                is_negated: false,
            },
            BinOp::GreaterThanOrEqual(_) => BinOpDescriptor {
                opcode: "operator_lt".to_string(),
                operand_a_input_name: "OPERAND1".to_string(),
                operand_b_input_name: "OPERAND2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("50".into()),
                is_negated: true,
            },
            BinOp::GreaterThan(_) => BinOpDescriptor {
                opcode: "operator_gt".to_string(),
                operand_a_input_name: "OPERAND1".to_string(),
                operand_b_input_name: "OPERAND2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("50".into()),
                is_negated: false,
            },
            BinOp::LessThanOrEqual(_) => BinOpDescriptor {
                opcode: "operator_gt".to_string(),
                operand_a_input_name: "OPERAND1".to_string(),
                operand_b_input_name: "OPERAND2".to_string(),
                operand_a_default: Some("".into()),
                operand_b_default: Some("50".into()),
                is_negated: true,
            },
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
    fn get_position(&self) -> &PosRange {
        match self {
            UnOp::Minus(p) => p,
            UnOp::Not(p) => p,
            UnOp::Exp(p) => p,
            UnOp::Pow(p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnOpDescriptor {
    pub opcode: String,
    pub operand_input_name: String,
    pub extra_inputs: HashMap<String, crate::codegen::project_json::Sb3PrimitiveBlock>,
    pub field_values: HashMap<String, crate::codegen::project_json::Sb3FieldValue>,
    pub default: Option<crate::codegen::project_json::Sb3PrimitiveBlock>,
}

impl UnOp {
    pub fn get_descriptor(&self) -> UnOpDescriptor {
        use crate::codegen::project_json::{Sb3FieldValue, Sb3PrimitiveBlock};

        match self {
            UnOp::Minus(_) => UnOpDescriptor {
                opcode: "operator_subtract".to_string(),
                operand_input_name: "NUM2".to_string(),
                extra_inputs: HashMap::from([(
                    "NUM1".to_string(),
                    Sb3PrimitiveBlock::Number("".into()),
                )]),
                field_values: HashMap::new(),
                default: Some(Sb3PrimitiveBlock::Number("".into())),
            },
            UnOp::Not(_) => UnOpDescriptor {
                opcode: "operator_not".to_string(),
                operand_input_name: "OPERAND".to_string(),
                extra_inputs: HashMap::new(),
                field_values: HashMap::new(),
                default: None,
            },
            UnOp::Exp(_) => UnOpDescriptor {
                opcode: "operator_mathop".to_string(),
                operand_input_name: "NUM".to_string(),
                extra_inputs: HashMap::new(),
                field_values: HashMap::from([(
                    "OPERATOR".to_string(),
                    Sb3FieldValue::Normal("e ^".into()),
                )]),
                default: Some(Sb3PrimitiveBlock::Number("".into())),
            },
            UnOp::Pow(_) => UnOpDescriptor {
                opcode: "operator_mathop".to_string(),
                operand_input_name: "NUM".to_string(),
                extra_inputs: HashMap::new(),
                field_values: HashMap::from([(
                    "OPERATOR".to_string(),
                    Sb3FieldValue::Normal("10 ^".into()),
                )]),
                default: Some(Sb3PrimitiveBlock::Number("".into())),
            },
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

const EMPTY_ISTRING_REF: &IString = &arcstr::literal!("");

impl Literal {
    pub fn get_string_value(&self) -> &IString {
        match self {
            Literal::String(value, _) => value,
            Literal::DecimalInt(value, _) => value,
            Literal::DecimalFloat(value, _) => value,
            Literal::HexadecimalInt(value, _) => value,
            Literal::OctalInt(value, _) => value,
            Literal::BinaryInt(value, _) => value,
            Literal::EmptyExpression(_, _, _) => EMPTY_ISTRING_REF,
        }
    }
    pub fn cast_to_string(&self) -> IString {
        match self {
            Literal::String(value, _) => value.clone(),
            Literal::DecimalInt(value, _) => value.clone(),
            Literal::DecimalFloat(value, _) => value.clone(),
            Literal::HexadecimalInt(value, _) => value.clone(),
            Literal::OctalInt(value, _) => value.clone(),
            Literal::BinaryInt(value, _) => value.clone(),
            Literal::EmptyExpression(_, _, _) => arcstr::literal!(""),
        }
    }
}

impl From<&Literal> for Sb3Primitive {
    fn from(value: &Literal) -> Self {
        value.get_string_value().into()
    }
}

impl From<&Literal> for Sb3PrimitiveBlock {
    fn from(value: &Literal) -> Self {
        Sb3PrimitiveBlock::String(value.into())
    }
}

impl GetPos for Literal {
    fn get_position(&self) -> &PosRange {
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
    fn get_position(&self) -> &PosRange {
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
    fn get_position(&self) -> &PosRange {
        &self.pos_range
    }
}

impl Identifier {
    pub fn to_single(&self) -> Option<&(IString, PosRange)> {
        match (self.scope.len(), self.names.len()) {
            (0, 1) => self.names.first(),
            (1, 0) => self.scope.first(),
            _ => None,
        }
    }
}

impl Identifier {
    pub fn to_syntactic_if(&self) -> Option<SyntacticIf> {
        if self.names.len() != 1 || !self.scope.is_empty() {
            return None;
        }
        self.names.first().and_then(|(value, pos_range)| {
            (value.as_str() == "if").then_some(SyntacticIf(*pos_range))
        })
    }

    pub fn to_syntactic_else(&self) -> Option<SyntacticElse> {
        if self.names.len() != 1 || !self.scope.is_empty() {
            return None;
        }
        self.names.first().and_then(|(value, pos_range)| {
            (value.as_str() == "else").then_some(SyntacticElse(*pos_range))
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SyntacticIf(pub PosRange);

impl GetPos for SyntacticIf {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SyntacticElse(pub PosRange);

impl GetPos for SyntacticElse {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CanonicalIdentifier {
    pub name: IString,
    pub pos_range: PosRange,
}

impl GetPos for CanonicalIdentifier {
    #[inline]
    fn get_position(&self) -> &PosRange {
        &self.pos_range
    }
}

#[derive(Debug, Clone, PartialEq, Error, Serialize, Deserialize)]
pub enum ParseError {
    #[error(
        "the lexer reached the end of input without the parser completing (context: {context})"
    )]
    UnexpectedEndOfInput { context: &'static str },
    #[error("unexpected token at {pos_range:?}, expected {expected} (context: {context})")]
    UnexpectedToken {
        expected: &'static str,
        message: &'static str,
        context: &'static str,
        found: crate::lexer::Token,
        pos_range: PosRange,
    },
    #[error("the lexer got stuck after the token at {pos_range:?} (context: {context})")]
    LexerStuck {
        context: &'static str,
        pos_range: PosRange,
    },
}
