use std::collections::HashMap;

use crate::{
    eval::{call::ConstantExprFunction, cast::JsPrimitive},
    lexer::SourceSpan,
    messages::ConstantExprEvaluationError,
};
use arcstr::ArcStr as IString; // Immutable string
use grazelang_library::{
    ConstantExprLibraryItemValue,
    project_json::{Sb3Primitive, Sb3PrimitiveBlock},
};
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub trait GetPos {
    fn get_source_span(&self) -> &SourceSpan;

    fn range_to<T>(&self, other: &T) -> SourceSpan
    where
        T: GetPos,
    {
        self.range_to_end(other.get_source_span().0.1)
    }

    fn range_to_end(&self, end: (usize, usize)) -> SourceSpan {
        let own_position = self.get_source_span();
        ((own_position.0.0, end), own_position.1)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GrazeProgram(pub Vec<TopLevelStatement>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TopLevelStatement {
    Stage(StageKeyword, StageCodeBlock, Option<Semicolon>, SourceSpan),
    Sprite(
        SpriteKeyword,
        Option<CanonicalIdentifier>,
        Identifier,
        SpriteCodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    BroadcastDeclaration(
        BroadcastKeyword,
        Option<CanonicalIdentifier>,
        Identifier,
        Semicolon,
        SourceSpan,
    ),
    EmptyStatement(Semicolon),
    InvalidStatement(SourceSpan),
}

impl GetPos for TopLevelStatement {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            TopLevelStatement::Stage(_, _, _, p) => p,
            TopLevelStatement::Sprite(_, _, _, _, _, p) => p,
            TopLevelStatement::BroadcastDeclaration(_, _, _, _, p) => p,
            TopLevelStatement::EmptyStatement(p) => &p.0,
            TopLevelStatement::InvalidStatement(p) => p,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SpriteKeyword(pub SourceSpan);

impl GetPos for SpriteKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct StageKeyword(pub SourceSpan);

impl GetPos for StageKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct CostumeKeyword(pub SourceSpan);

impl GetPos for CostumeKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BroadcastKeyword(pub SourceSpan);

impl GetPos for BroadcastKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct BackdropKeyword(pub SourceSpan);

impl GetPos for BackdropKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SoundKeyword(pub SourceSpan);

impl GetPos for SoundKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ProcKeyword(pub SourceSpan);

impl GetPos for ProcKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WarpSpecifier {
    pub is_warp: bool,
    pub source_span: SourceSpan,
}

impl GetPos for WarpSpecifier {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum CustomBlockParamKindValue {
    Number,
    String,
    Boolean,
}

// TODO: Use CommaSeparated
// Issue: #60

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CommaSeparated<T> {
    pub values: Vec<(T, Comma)>,
    pub tail_value: T,
    pub source_span: SourceSpan,
}

impl<T> GetPos for CommaSeparated<T> {
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CustomBlockParamKind {
    pub kind: CustomBlockParamKindValue,
    pub source_span: SourceSpan,
}

impl GetPos for CustomBlockParamKind {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

type CustomBlockParams = Vec<(
    Option<CustomBlockParamKind>,
    Option<CanonicalIdentifier>,
    Identifier,
    Option<Comma>,
)>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StageStatement {
    DataDeclaration(LetKeyword, DataDeclaration, Semicolon, SourceSpan),
    BackdropDeclaration(BackdropKeyword, AssetDeclaration, Semicolon, SourceSpan),
    SoundDeclaration(SoundKeyword, AssetDeclaration, Semicolon, SourceSpan),
    NoInputHatStatement(Identifier, CodeBlock, Option<Semicolon>, SourceSpan),
    SingleInputHatStatement(
        Identifier,
        Expression,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    MultiInputHatStatement(
        Identifier,
        LeftParens,
        Vec<(Expression, Option<Comma>)>, // Use CommaSeparated
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    CustomBlockDefinition(
        Option<WarpSpecifier>,
        ProcKeyword,
        Option<CanonicalIdentifier>,
        Identifier,
        LeftParens,
        CustomBlockParams,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    IsolatedBlock(CodeBlock, Option<Semicolon>, SourceSpan),
    IsolatedExpression(
        LeftParens,
        Expression,
        RightParens,
        Option<Semicolon>,
        SourceSpan,
    ),
    EmptyStatement(Semicolon),
    InvalidStatement(SourceSpan),
}

impl GetPos for StageStatement {
    fn get_source_span(&self) -> &SourceSpan {
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
            StageStatement::InvalidStatement(p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SpriteStatement {
    DataDeclaration(LetKeyword, DataDeclaration, Semicolon, SourceSpan),
    CostumeDeclaration(CostumeKeyword, AssetDeclaration, Semicolon, SourceSpan),
    SoundDeclaration(SoundKeyword, AssetDeclaration, Semicolon, SourceSpan),
    NoInputHatStatement(Identifier, CodeBlock, Option<Semicolon>, SourceSpan),
    SingleInputHatStatement(
        Identifier,
        Expression,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    MultiInputHatStatement(
        Identifier,
        LeftParens,
        Vec<(Expression, Option<Comma>)>, // Use CommaSeparated
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    CustomBlockDefinition(
        Option<WarpSpecifier>,
        ProcKeyword,
        Option<CanonicalIdentifier>,
        Identifier,
        LeftParens,
        CustomBlockParams,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    IsolatedBlock(CodeBlock, Option<Semicolon>, SourceSpan),
    IsolatedExpression(
        LeftParens,
        Expression,
        RightParens,
        Option<Semicolon>,
        SourceSpan,
    ),
    EmptyStatement(Semicolon),
    InvalidStatement(SourceSpan),
}

impl GetPos for SpriteStatement {
    fn get_source_span(&self) -> &SourceSpan {
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
            SpriteStatement::InvalidStatement(p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AssetDeclaration {
    Multiple(
        LeftParens,
        Vec<(SingleAssetDeclaration, Option<Comma>)>, // Use CommaSeparated
        RightParens,
        SourceSpan,
    ),
    Single(SingleAssetDeclaration),
}

impl GetPos for AssetDeclaration {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            AssetDeclaration::Multiple(_, _, _, p) => p,
            AssetDeclaration::Single(d) => d.get_source_span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SingleAssetDeclaration(
    pub Option<CanonicalIdentifier>,
    pub Identifier,
    pub SingleAssetDeclarationValue,
    pub SourceSpan,
);

impl GetPos for SingleAssetDeclaration {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.3
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SingleAssetDeclarationValue {
    Simple(LeftParens, (IString, SourceSpan), RightParens, SourceSpan),
    FlatDictionary(
        LeftBrace,
        Vec<(Identifier, NormalAssignmentOperator, Literal, Option<Comma>)>,
        RightBrace,
        SourceSpan,
    ),
}

impl GetPos for SingleAssetDeclarationValue {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            SingleAssetDeclarationValue::Simple(_, _, _, source_span)
            | SingleAssetDeclarationValue::FlatDictionary(_, _, _, source_span) => source_span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    DataDeclaration(LetKeyword, DataDeclaration, Semicolon, SourceSpan),
    Assignment(
        Identifier,
        NormalAssignmentOperator,
        Expression,
        Semicolon,
        SourceSpan,
    ),
    ListAssignment(
        Identifier,
        NormalAssignmentOperator,
        LeftBracket,
        Vec<(ListEntry, Option<Comma>)>, // Use CommaSeparated
        RightBracket,
        Semicolon,
        SourceSpan,
    ),
    SetItem(
        Identifier,
        LeftBracket,
        Expression,
        RightBracket,
        NormalAssignmentOperator,
        Expression,
        Semicolon,
        SourceSpan,
    ),
    Call(
        Identifier,
        LeftParens,
        Vec<(Expression, Option<Comma>)>, // Use CommaSeparated
        RightParens,
        Semicolon,
        SourceSpan,
    ),
    SingleInputControl(
        Identifier,
        Expression,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    MultiInputControl(
        Identifier,
        LeftParens,
        Vec<(Expression, Option<Comma>)>, // Use CommaSeparated
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    Forever(Identifier, CodeBlock, Option<Semicolon>, SourceSpan),
    IfElse(
        (SyntacticIf, Expression, CodeBlock),
        Vec<(SyntacticElse, SyntacticIf, Expression, CodeBlock)>,
        Option<(SyntacticElse, CodeBlock)>,
        Option<Semicolon>,
        SourceSpan,
    ),
    EmptyStatement(Semicolon),
    InvalidStatement(SourceSpan),
}

impl GetPos for Statement {
    fn get_source_span(&self) -> &SourceSpan {
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
            Statement::InvalidStatement(p) => p,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LetKeyword(pub SourceSpan);

impl GetPos for LetKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct VarsKeyword(pub SourceSpan);

impl GetPos for VarsKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ListsKeyword(pub SourceSpan);

impl GetPos for ListsKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct VarKeyword(pub SourceSpan);

impl GetPos for VarKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ListKeyword(pub SourceSpan);

impl GetPos for ListKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SingleDataDeclarationType {
    Unset,
    Var(SourceSpan),
    List(SourceSpan),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DataDeclaration {
    Mixed(
        DataDeclarationScope,
        LeftParens,
        Vec<(SingleDataDeclaration, Option<Comma>)>, // Use CommaSeparated
        RightParens,
        SourceSpan,
    ),
    Vars(
        DataDeclarationScope,
        VarsKeyword,
        LeftBrace,
        Vec<(SingleDataDeclaration, Option<Comma>)>, // Use CommaSeparated
        RightBrace,
        SourceSpan,
    ),
    Lists(
        DataDeclarationScope,
        ListsKeyword,
        LeftBrace,
        Vec<(SingleDataDeclaration, Option<Comma>)>, // Use CommaSeparated
        RightBrace,
        SourceSpan,
    ),
    Single(Box<SingleDataDeclaration>),
}

impl GetPos for DataDeclaration {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            DataDeclaration::Mixed(_, _, _, _, p) => p,
            DataDeclaration::Vars(_, _, _, _, _, p) => p,
            DataDeclaration::Lists(_, _, _, _, _, p) => p,
            DataDeclaration::Single(d) => d.get_source_span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DataDeclarationScope {
    Unset,
    Global(SourceSpan),
    Local(SourceSpan),
    Cloud(SourceSpan),
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct NormalAssignmentOperator(pub SourceSpan);

impl GetPos for NormalAssignmentOperator {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ListEntry {
    Expression(Expression),
    Unwrap(Literal, SourceSpan),
}

impl GetPos for ListEntry {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            ListEntry::Expression(l) => l.get_source_span(),
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
        SourceSpan,
    ),
    EmptyVariable(
        Option<VarKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        Identifier,
        SourceSpan,
    ),
    List(
        Option<ListKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        Identifier,
        NormalAssignmentOperator,
        LeftBracket,
        Vec<(ListEntry, Option<Comma>)>, // Use CommaSeparated
        RightBracket,
        SourceSpan,
    ),
    EmptyList(
        Option<ListKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        Identifier,
        SourceSpan,
    ),
}

impl GetPos for SingleDataDeclaration {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            SingleDataDeclaration::Variable(_, _, _, _, _, _, p) => p,
            SingleDataDeclaration::EmptyVariable(_, _, _, _, p) => p,
            SingleDataDeclaration::List(_, _, _, _, _, _, _, _, p) => p,
            SingleDataDeclaration::EmptyList(_, _, _, _, p) => p,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Comma(pub SourceSpan);

impl GetPos for Comma {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LeftBrace(pub SourceSpan);

impl GetPos for LeftBrace {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct RightBrace(pub SourceSpan);

impl GetPos for RightBrace {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StageCodeBlock {
    pub left_brace: LeftBrace,
    pub statements: Vec<StageStatement>,
    pub right_brace: RightBrace,
    pub source_span: SourceSpan,
}

impl GetPos for StageCodeBlock {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SpriteCodeBlock {
    pub left_brace: LeftBrace,
    pub statements: Vec<SpriteStatement>,
    pub right_brace: RightBrace,
    pub source_span: SourceSpan,
}

impl GetPos for SpriteCodeBlock {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CodeBlock {
    pub left_brace: LeftBrace,
    pub statements: Vec<Statement>,
    pub right_brace: RightBrace,
    pub source_span: SourceSpan,
}

impl GetPos for CodeBlock {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    FormattedString(Vec<FormattedStringContent>, SourceSpan),
    BinOp(Box<Expression>, BinOp, Box<Expression>, SourceSpan),
    UnOp(UnOp, Box<Expression>, SourceSpan),
    Identifier(Identifier),
    Call(
        Identifier,
        LeftParens,
        Vec<(Expression, Option<Comma>)>, // Use CommaSeparated
        RightParens,
        SourceSpan,
    ),
    GetItem(
        Identifier,
        LeftBracket,
        Box<Expression>,
        RightBracket,
        SourceSpan,
    ),
    GetLetter(
        Box<Expression>,
        LetterAccessLeftBracket,
        Box<Expression>,
        RightBracket,
        SourceSpan,
    ),
    Parentheses(LeftParens, Box<Expression>, RightParens, SourceSpan),
}

impl Expression {
    pub fn is_empty(&self) -> bool {
        matches!(self, Expression::Literal(Literal::EmptyExpression(..)))
    }

    pub fn calculate_value_js(&self) -> Result<JsPrimitive, ConstantExprEvaluationError> {
        match self {
            Expression::Literal(literal) => Ok(Sb3Primitive::from(literal).into()),
            Expression::BinOp(expr_a, bin_op, expr_b, _) => {
                Ok(bin_op
                    .apply_operation(expr_a.calculate_value_js()?, expr_b.calculate_value_js()?))
            }
            Expression::UnOp(un_op, expr, _) => {
                Ok(un_op.apply_operation(expr.calculate_value_js()?))
            }
            Expression::Parentheses(_, expr, _, _) => expr.calculate_value_js(),
            Expression::GetLetter(string, _, index, _, _) => {
                use crate::eval::cast::{
                    JsPrimitive, ScratchVmToNumber, ScratchVmToString, try_convert_f64_into_i128,
                };
                let string_js = string.calculate_value_js()?;
                let string = string_js.to_js_cow_str();
                let index = index.calculate_value_js()?.to_number() - 1.0;
                let Ok(index) =
                    usize::try_from(if let Some(it) = try_convert_f64_into_i128(index.floor()) {
                        it
                    } else {
                        return Ok(JsPrimitive::IString(EMPTY_ISTRING_REF.clone()));
                    })
                else {
                    return Ok(JsPrimitive::IString(EMPTY_ISTRING_REF.clone()));
                };
                Ok(string
                    .get(index)
                    .map(|value| JsPrimitive::JsString(vec![*value]))
                    .unwrap_or_else(|| JsPrimitive::IString(EMPTY_ISTRING_REF.clone())))
            }
            Expression::FormattedString(content, _) => {
                use crate::eval::cast::ScratchVmToString;
                Ok(JsPrimitive::JsString(content.iter().try_fold(
                    Vec::<u16>::new(),
                    |mut current, value| {
                        match value {
                            FormattedStringContent::Expression(expression) => {
                                expression
                                    .calculate_value_js()?
                                    .write_to_js_string(&mut current);
                            }
                            FormattedStringContent::String(value, _) => {
                                current.extend(value.encode_utf16())
                            }
                        }
                        Ok(current)
                    },
                )?))
            }
            Expression::Call(identifier, _, exprs, _, source_span) => {
                let library_item = crate::library::const_expr_lookup(
                    identifier
                        .path
                        .iter()
                        .chain(identifier.fields.iter())
                        .map(|(next, _)| next as &str),
                )
                .map_err(|err| match err {
                    crate::library::ConstExpLookupError::NotFound => {
                        ConstantExprEvaluationError::ConstIdentifierDoesNotExist {
                            identifier: identifier.clone(),
                        }
                    }
                    crate::library::ConstExpLookupError::UsedSuper => {
                        ConstantExprEvaluationError::ConstIdentifierUsedSupper {
                            identifier: identifier.clone(),
                        }
                    }
                })?;

                let Some(ConstantExprLibraryItemValue::Function(function_id, _)) =
                    library_item.value
                else {
                    return Err(match library_item.value {
                        Some(ConstantExprLibraryItemValue::AssociatedItem(_)) => {
                            ConstantExprEvaluationError::NotConstFunctionButValue {
                                identifier: identifier.clone(),
                            }
                        }
                        None => ConstantExprEvaluationError::NotConstFunctionButNamespace {
                            identifier: identifier.clone(),
                        },
                        Some(ConstantExprLibraryItemValue::Function(_, _)) => unreachable!(),
                    });
                };

                let Ok(function) = ConstantExprFunction::try_from(function_id) else {
                    return Err(ConstantExprEvaluationError::NotConstFunctionButNamespace {
                        identifier: identifier.clone(),
                    });
                };

                function.apply(exprs.iter().map(|(value, _)| value), *source_span)
            }
            Expression::Identifier(identifier) => {
                let value = crate::library::const_expr_lookup(
                    identifier
                        .path
                        .iter()
                        .chain(identifier.fields.iter())
                        .map(|(next, _)| next as &str),
                )
                .map_err(|err| match err {
                    crate::library::ConstExpLookupError::NotFound => {
                        ConstantExprEvaluationError::ConstIdentifierDoesNotExist {
                            identifier: identifier.clone(),
                        }
                    }
                    crate::library::ConstExpLookupError::UsedSuper => {
                        ConstantExprEvaluationError::ConstIdentifierUsedSupper {
                            identifier: identifier.clone(),
                        }
                    }
                })?;
                let Ok(function) =
                    (match &value.value {
                        Some(ConstantExprLibraryItemValue::Function(function, true)) => {
                            ConstantExprFunction::try_from(*function)
                        }
                        Some(ConstantExprLibraryItemValue::Function(_, _)) => {
                            return Err(ConstantExprEvaluationError::NotSingletonConstFunction {
                                identifier: identifier.clone(),
                            });
                        }
                        Some(ConstantExprLibraryItemValue::AssociatedItem(_)) => {
                            return Err(
                                ConstantExprEvaluationError::NotSingletonConstFunctionButValue {
                                    identifier: identifier.clone(),
                                },
                            );
                        }
                        None => return Err(
                            ConstantExprEvaluationError::NotSingletonConstFunctionButNamespace {
                                identifier: identifier.clone(),
                            },
                        ),
                    })
                else {
                    return Err(ConstantExprEvaluationError::NotConstFunctionButNamespace {
                        identifier: identifier.clone(),
                    });
                };
                function.apply(std::iter::empty(), *identifier.get_source_span())
            }
            Expression::GetItem(identifier, _, _, _, _) => {
                Err(ConstantExprEvaluationError::ConstExprListAccess {
                    identifier: identifier.clone(),
                })
            }
        }
    }

    pub fn calculate_value(&self) -> Result<Sb3Primitive, ConstantExprEvaluationError> {
        if let Expression::Literal(literal) = self {
            return Ok(literal.into());
        }
        self.calculate_value_js().map(Into::into)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LeftParens(pub SourceSpan);

impl GetPos for LeftParens {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct RightParens(pub SourceSpan);

impl GetPos for RightParens {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LeftBracket(pub SourceSpan);

impl GetPos for LeftBracket {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct RightBracket(pub SourceSpan);

impl GetPos for RightBracket {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LetterAccessLeftBracket(pub SourceSpan);

impl GetPos for LetterAccessLeftBracket {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Semicolon(pub SourceSpan);

impl GetPos for Semicolon {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

impl GetPos for Expression {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            Expression::Literal(l) => l.get_source_span(),
            Expression::FormattedString(_, p) => p,
            Expression::BinOp(_, _, _, p) => p,
            Expression::UnOp(_, _, p) => p,
            Expression::Identifier(i) => i.get_source_span(),
            Expression::Call(_, _, _, _, p) => p,
            Expression::GetItem(_, _, _, _, p) => p,
            Expression::GetLetter(_, _, _, _, p) => p,
            Expression::Parentheses(_, _, _, p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinOp {
    Plus(SourceSpan),
    Minus(SourceSpan),
    Times(SourceSpan),
    Div(SourceSpan),
    Mod(SourceSpan),
    Join(SourceSpan),
    And(SourceSpan),
    Or(SourceSpan),
    Equals(SourceSpan),
    NotEquals(SourceSpan),
    LessThan(SourceSpan),
    GreaterThan(SourceSpan),
    LessThanOrEqual(SourceSpan),
    GreaterThanOrEqual(SourceSpan),
}

impl GetPos for BinOp {
    fn get_source_span(&self) -> &SourceSpan {
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
                operand_a_default: Some(Sb3PrimitiveBlock::Number("".into())),
                operand_b_default: Some(Sb3PrimitiveBlock::Number("".into())),
                is_negated: false,
            },
            BinOp::Minus(_) => BinOpDescriptor {
                opcode: "operator_subtract".to_string(),
                operand_a_input_name: "NUM1".to_string(),
                operand_b_input_name: "NUM2".to_string(),
                operand_a_default: Some(Sb3PrimitiveBlock::Number("".into())),
                operand_b_default: Some(Sb3PrimitiveBlock::Number("".into())),
                is_negated: false,
            },
            BinOp::Times(_) => BinOpDescriptor {
                opcode: "operator_multiply".to_string(),
                operand_a_input_name: "NUM1".to_string(),
                operand_b_input_name: "NUM2".to_string(),
                operand_a_default: Some(Sb3PrimitiveBlock::Number("".into())),
                operand_b_default: Some(Sb3PrimitiveBlock::Number("".into())),
                is_negated: false,
            },
            BinOp::Div(_) => BinOpDescriptor {
                opcode: "operator_divide".to_string(),
                operand_a_input_name: "NUM1".to_string(),
                operand_b_input_name: "NUM2".to_string(),
                operand_a_default: Some(Sb3PrimitiveBlock::Number("".into())),
                operand_b_default: Some(Sb3PrimitiveBlock::Number("".into())),
                is_negated: false,
            },
            BinOp::Mod(_) => BinOpDescriptor {
                opcode: "operator_mod".to_string(),
                operand_a_input_name: "NUM1".to_string(),
                operand_b_input_name: "NUM2".to_string(),
                operand_a_default: Some(Sb3PrimitiveBlock::Number("".into())),
                operand_b_default: Some(Sb3PrimitiveBlock::Number("".into())),
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
    Minus(SourceSpan),
    Not(SourceSpan),
    Exp(SourceSpan),
    Pow(SourceSpan),
}

impl GetPos for UnOp {
    fn get_source_span(&self) -> &SourceSpan {
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
    pub extra_inputs: HashMap<String, grazelang_library::project_json::Sb3PrimitiveBlock>,
    pub field_values: HashMap<String, grazelang_library::project_json::Sb3FieldValue>,
    pub default: Option<grazelang_library::project_json::Sb3PrimitiveBlock>,
}

impl UnOp {
    pub fn get_descriptor(&self) -> UnOpDescriptor {
        use grazelang_library::project_json::{Sb3FieldValue, Sb3PrimitiveBlock};

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
    String(IString, SourceSpan),
    DecimalInt(IString, SourceSpan),
    DecimalFloat(IString, SourceSpan),
    HexadecimalInt(IString, SourceSpan),
    OctalInt(IString, SourceSpan),
    BinaryInt(IString, SourceSpan),
    EmptyExpression(LeftParens, RightParens, SourceSpan),
}

pub const EMPTY_ISTRING_REF: &IString = &arcstr::literal!("");

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
        match value {
            Literal::DecimalInt(string, _) => {
                let mut int = 0_i128;
                let mut digits = 0;
                let mut i = string.chars();
                let is_negative = if string.starts_with('-') {
                    i.next();
                    true
                } else {
                    false
                };
                loop {
                    if let Some(c) = i.next() {
                        if (digits == 0 && c == '0') || c == '_' {
                            continue;
                        }
                        digits += 1;
                        int = if let Some(n) = int
                            .checked_mul(10)
                            .and_then(|n| n.checked_add(c.to_digit(10).unwrap() as i128))
                        {
                            n
                        } else {
                            return string.replace('_', "").into();
                        };
                    } else {
                        let int = if is_negative { -int } else { int };
                        if let Ok(int) = int.try_into() {
                            return Self::Int(int);
                        }
                        return Self::Int128(int);
                    }
                }
            }
            Literal::DecimalFloat(string, _) => {
                // Does not convert into f64 in order to preserve representation
                return string.replace('_', "").into();
            }
            Literal::HexadecimalInt(string, _) => {
                let mut int = 0_u128;
                let mut digits = 0;
                let mut i = string.chars();
                i.next();
                i.next();
                loop {
                    if let Some(c) = i.next() {
                        if (digits == 0 && c == '0') || c == '_' {
                            continue;
                        }
                        digits += 1;
                        if digits > 32 {
                            return string.replace('_', "").into();
                        }
                        int = (int << 4) | (c.to_digit(16).unwrap() as u128);
                    } else if let Ok(int) = i128::try_from(int) {
                        if let Ok(int) = int.try_into() {
                            return Self::Int(int);
                        }
                        return Self::Int128(int);
                    } else {
                        return string.replace('_', "").into();
                    }
                }
            }
            Literal::OctalInt(string, _) => {
                let mut int = 0_u128;
                let mut digits = 0;
                let mut i = string.chars();
                i.next();
                i.next();
                loop {
                    if let Some(c) = i.next() {
                        if (digits == 0 && c == '0') || c == '_' {
                            continue;
                        }
                        digits += 1;
                        if digits > 43 || int > (u128::MAX >> 3) {
                            return string.replace('_', "").into();
                        }
                        int = (int << 3) | (c.to_digit(8).unwrap() as u128);
                    } else if let Ok(int) = i128::try_from(int) {
                        if let Ok(int) = int.try_into() {
                            return Self::Int(int);
                        }
                        return Self::Int128(int);
                    } else {
                        return string.replace('_', "").into();
                    }
                }
            }
            Literal::BinaryInt(string, _) => {
                let mut int = 0_u128;
                let mut digits = 0;
                let mut i = string.chars();
                i.next();
                i.next();
                loop {
                    if let Some(c) = i.next() {
                        if (digits == 0 && c == '0') || c == '_' {
                            continue;
                        }
                        digits += 1;
                        if digits > 128 {
                            return string.replace('_', "").into();
                        }
                        int = (int << 1) | (c.to_digit(2).unwrap() as u128);
                    } else if let Ok(int) = i128::try_from(int) {
                        if let Ok(int) = int.try_into() {
                            return Self::Int(int);
                        }
                        return Self::Int128(int);
                    } else {
                        return string.replace('_', "").into();
                    }
                }
            }
            _ => (),
        }
        value.get_string_value().into()
    }
}

impl From<&Literal> for Sb3PrimitiveBlock {
    fn from(value: &Literal) -> Self {
        Sb3PrimitiveBlock::String(value.into())
    }
}

impl GetPos for Literal {
    fn get_source_span(&self) -> &SourceSpan {
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
    String(IString, SourceSpan),
}

impl GetPos for FormattedStringContent {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            FormattedStringContent::Expression(expression) => expression.get_source_span(),
            FormattedStringContent::String(_, p) => p,
        }
    }
}

/// Anything before a dot is a path
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Identifier {
    pub path: Vec<(IString, SourceSpan)>,   // abc::def
    pub fields: Vec<(IString, SourceSpan)>, // .ghi.jkl
    pub source_span: SourceSpan,
}

impl GetPos for Identifier {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

impl Identifier {
    pub fn to_single(&self) -> Option<&(IString, SourceSpan)> {
        match (self.path.len(), self.fields.len()) {
            (0, 1) => self.fields.first(),
            (1, 0) => self.path.first(),
            _ => None,
        }
    }
}

impl Identifier {
    pub fn to_syntactic_if(&self) -> Option<SyntacticIf> {
        self.to_single().and_then(|(value, source_span)| {
            (value.as_str() == "if").then_some(SyntacticIf(*source_span))
        })
    }

    pub fn to_syntactic_else(&self) -> Option<SyntacticElse> {
        self.to_single().and_then(|(value, source_span)| {
            (value.as_str() == "else").then_some(SyntacticElse(*source_span))
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SyntacticIf(pub SourceSpan);

impl GetPos for SyntacticIf {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SyntacticElse(pub SourceSpan);

impl GetPos for SyntacticElse {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CanonicalIdentifier {
    pub name: IString,
    pub source_span: SourceSpan,
}

impl GetPos for CanonicalIdentifier {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("the lexer reached the end of input without the parser completing")]
    UnexpectedEndOfInput {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[error("unexpected token at {source_span:?}, expected {expected}")]
    UnexpectedToken {
        expected: IString,
        message: IString,
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        found: crate::lexer::Token,
        source_span: SourceSpan,
    },
    #[error("the lexer got stuck after the token at {source_span:?}")]
    LexerStuck {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[error("tried to declare a local symbol in stage at {source_span:?}")]
    LocalSymbolInStage {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[error("tried to peek back at the beginning of the content")]
    PeekedBackAtBeginning {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[error("tried to shadow symbol {symbol}")]
    ShadowedSymbol {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        symbol: IString,
        source_span: SourceSpan,
    },
    #[error("tried to name a symbol \"super\"")]
    SymbolNamedSuper {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[error("expected key {key:?} in flat dictionary")]
    MissingFlatDictionaryEntry {
        key: IString,
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[error("unexpected key {key:?} in flat dictionary")]
    UnknownFlatDictionaryEntry {
        key: IString,
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[error("repeated key {key:?} in flat dictionary")]
    RepeatedFlatDictionaryEntry {
        key: IString,
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[error("key {key:?} with value {value:?} in flat dictionary has an incorrect type")]
    IncorrectFlatDictionaryEntryType {
        key: IString,
        value: Box<Literal>,
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[error("the expression {expression:?} is not calculatable by graze, {source}")]
    InvalidConstantExpression {
        expression: Box<Expression>,
        #[source]
        source: ConstantExprEvaluationError,
    },
    #[error("{source}")]
    IoError {
        #[source]
        source: std::rc::Rc<std::io::Error>,
        source_span: SourceSpan,
    },
}

impl GetPos for ParseError {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            ParseError::UnexpectedEndOfInput {
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::UnexpectedToken {
                expected: _,
                message: _,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                found: _,
                source_span,
            } => source_span,
            ParseError::LexerStuck {
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::LocalSymbolInStage {
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::PeekedBackAtBeginning {
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::ShadowedSymbol {
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                symbol: _,
                source_span,
            } => source_span,
            ParseError::SymbolNamedSuper {
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::MissingFlatDictionaryEntry {
                key: _,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::UnknownFlatDictionaryEntry {
                key: _,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::RepeatedFlatDictionaryEntry {
                key: _,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::IncorrectFlatDictionaryEntryType {
                key: _,
                value: _,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::InvalidConstantExpression {
                expression,
                source: _,
            } => expression.get_source_span(),
            ParseError::IoError {
                source: _,
                source_span,
            } => source_span,
        }
    }
}
