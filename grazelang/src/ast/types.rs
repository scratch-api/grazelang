use std::collections::HashMap;

use arcstr::ArcStr as IString;

use grazelang_types::project_json::Sb3PrimitiveBlock;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GrazeProgram(pub Vec<TopLevelStatement>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TopLevelStatement {
    Stage {
        code_block: StageCodeBlock,
    },
    Sprite {
        canonical_identifier: Option<CanonicalIdentifier>,
        identifier: SingleIdentifier,
        code_block: SpriteCodeBlock,
    },
    BroadcastDeclaration {
        canonical_identifier: Option<CanonicalIdentifier>,
        identifier: SingleIdentifier,
    },
    UseStatement(UseStatementContent),
    UseExtensionStatement(UseStatementContent),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WarpSpecifier {
    pub is_warp: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UseStatementContent {
    SingleUse {
        identifier: Identifier,
        rename: Option<SingleIdentifier>,
    },
    MultiUse {
        root: Identifier,
        content: Vec<UseStatementContent>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CustomBlockParamKind {
    Number,
    String,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StageStatement {
    DataDeclaration(DataDeclaration),
    BackdropDeclaration(AssetDeclaration),
    SoundDeclaration(AssetDeclaration),
    HatStatement {
        hat_function: Identifier,
        arguments: Vec<Expression>,
        code_block: CodeBlock,
    },
    CustomBlockDefinition {
        is_warp: WarpSpecifier,
        canonical_identifier: Option<CanonicalIdentifier>,
        identifier: SingleIdentifier,
        parameters: Vec<(
            Option<CustomBlockParamKind>,
            Option<CanonicalIdentifier>,
            SingleIdentifier,
        )>,
        code_block: CodeBlock,
    },
    IsolatedBlock(CodeBlock),
    IsolatedExpression(Expression),
    ConfigStatement(Vec<DictionaryEntry>),
    MonitorDeclaration {
        value: MonitorValue,
        configuration: Vec<DictionaryEntry>,
    },
    UseStatement(UseStatementContent),
    UseExtensionStatement(UseStatementContent),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SpriteStatement {
    DataDeclaration(DataDeclaration),
    CostumeDeclaration(AssetDeclaration),
    SoundDeclaration(AssetDeclaration),
    HatStatement {
        hat_function: Identifier,
        arguments: Vec<Expression>,
        code_block: CodeBlock,
    },
    CustomBlockDefinition {
        is_warp: WarpSpecifier,
        canonical_identifier: Option<CanonicalIdentifier>,
        identifier: SingleIdentifier,
        parameters: Vec<(
            Option<CustomBlockParamKind>,
            Option<CanonicalIdentifier>,
            SingleIdentifier,
        )>,
        code_block: CodeBlock,
    },
    IsolatedBlock(CodeBlock),
    IsolatedExpression(Expression),
    ConfigStatement(Vec<DictionaryEntry>),
    MonitorDeclaration {
        value: MonitorValue,
        configuration: Vec<DictionaryEntry>,
    },
    UseStatement(UseStatementContent),
    UseExtensionStatement(UseStatementContent),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AssetDeclaration {
    Multiple(Vec<SingleAssetDeclaration>),
    Single(SingleAssetDeclaration),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SingleAssetDeclaration {
    pub canonical_identifier: Option<CanonicalIdentifier>,
    pub identifier: SingleIdentifier,
    pub value: SingleAssetDeclarationValue,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SingleAssetDeclarationValue {
    Simple(IString),
    Dictionary(Vec<DictionaryEntry>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    DataDeclaration(DataDeclaration),
    Assignment {
        target: Identifier,
        value: Expression,
    },
    ListAssignment {
        target: Identifier,
        value: Vec<ListEntry>,
    },
    SetItem {
        list: Identifier,
        item: Expression,
        value: Expression,
    },
    Call {
        function: Identifier,
        arguments: Vec<Expression>,
    },
    Control {
        control_function: Identifier,
        arguments: Vec<Expression>,
        code_block: CodeBlock,
    },
    IfElse {
        first_branch: (Expression, CodeBlock),
        alternative_branches: Vec<(Expression, CodeBlock)>,
        else_branch: Option<CodeBlock>,
    },
    UseStatement(UseStatementContent),
    UseExtensionStatement(UseStatementContent),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SingleDataDeclarationType {
    Var,
    List,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DataDeclaration {
    Mixed {
        scope: DataDeclarationScope,
        declarations: Vec<SingleDataDeclaration>,
    },
    Vars {
        scope: DataDeclarationScope,
        declarations: Vec<SingleDataDeclaration>,
    },
    Lists {
        scope: DataDeclarationScope,
        declarations: Vec<SingleDataDeclaration>,
    },
    Single(Box<SingleDataDeclaration>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DataDeclarationScope {
    Global,
    Local,
    Cloud,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DictionaryEntry {
    pub identifier: SingleIdentifier,
    pub value: DictionaryValue,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DictionaryValue {
    Primitive(Literal),
    Dictionary(Vec<DictionaryEntry>),
    List(Vec<DictionaryValue>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DictionaryValueLiteralOrList {
    Literal(Literal),
    List(Vec<DictionaryValue>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MonitorValue {
    Identifier(Identifier),
    Call {
        function: Identifier,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ListEntry {
    Expression(Expression),
    Unwrap(Literal),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SingleDataDeclaration {
    Variable {
        scope: DataDeclarationScope,
        canonical_identifier: Option<CanonicalIdentifier>,
        identifier: SingleIdentifier,
        value: Option<Expression>,
    },
    List {
        scope: DataDeclarationScope,
        canonical_identifier: Option<CanonicalIdentifier>,
        identifier: SingleIdentifier,
        value: Vec<ListEntry>,
    },
    FileList {
        scope: DataDeclarationScope,
        canonical_identifier: Option<CanonicalIdentifier>,
        identifier: SingleIdentifier,
        source: Expression,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StageCodeBlock {
    pub statements: Vec<StageStatement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SpriteCodeBlock {
    pub statements: Vec<SpriteStatement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CodeBlock {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    FormattedString(Vec<FormattedStringContent>),
    BinOp {
        operator: BinOp,
        left_operand: Box<Expression>,
        right_operand: Box<Expression>,
    },
    UnOp {
        operator: UnOp,
        operand: Box<Expression>,
    },
    Identifier(Identifier),
    Call {
        function: Identifier,
        arguments: Vec<Expression>,
    },
    GetItem {
        list: Identifier,
        item: Box<Expression>,
    },
    GetLetter {
        expression: Box<Expression>,
        letter: Box<Expression>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    Join,
    Contains,
    And,
    Or,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

pub(crate) use crate::parser::cst::Associativity;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BinOpDescriptor {
    pub opcode: String,
    pub operand_a_input_name: String,
    pub operand_b_input_name: String,
    pub operand_a_default: Option<Sb3PrimitiveBlock>,
    pub operand_b_default: Option<Sb3PrimitiveBlock>,
    pub is_negated: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum UnOp {
    Minus,
    Not,
    Exp,
    Pow,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnOpDescriptor {
    pub opcode: String,
    pub operand_input_name: String,
    pub extra_inputs: HashMap<String, grazelang_types::project_json::Sb3PrimitiveBlock>,
    pub field_values: HashMap<String, grazelang_types::project_json::Sb3FieldValue>,
    pub default: Option<grazelang_types::project_json::Sb3PrimitiveBlock>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    String(IString),
    DecimalInt(IString),
    DecimalFloat(IString),
    HexadecimalInt(IString),
    OctalInt(IString),
    BinaryInt(IString),
    Bool(bool),
    EmptyExpression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FormattedStringContent {
    Expression(Box<Expression>),
    String(IString),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Identifier {
    pub path: Vec<SingleIdentifier>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SingleIdentifier {
    pub value: IString,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CanonicalIdentifier {
    pub name: IString,
}

// TODO: Implement AST into CST parser
// Issue: #105

// TODO: Implement AST unparsing
// Issue: #104

// TODO: Implement CST into AST converter
// Issue: #103
