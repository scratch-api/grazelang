use std::{borrow::Cow, collections::HashMap, fmt::Display, matches};

use crate::{
    eval::{
        call::ConstantExprFunction,
        cast::JsPrimitive,
        ops::{ConstantExprBinOp, ConstantExprUnOp},
    },
    lexer::SourceSpan,
    messages::types::{ConstantExprEvaluationError, GetLintId},
};
use arcstr::ArcStr as IString; // Immutable string
use grazelang_types::{
    ConstantExprLibraryItemValue,
    project_json::{Sb3Primitive, Sb3PrimitiveBlock, Sb3PrimitiveOrBool},
};
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub const EMPTY_ISTRING_REF: &IString = &arcstr::literal!("");
pub const FALSE_ISTRING_REF: &IString = &arcstr::literal!("false");
pub const TRUE_ISTRING_REF: &IString = &arcstr::literal!("true");

pub trait GetPos {
    fn get_source_span(&self) -> &SourceSpan;

    #[inline]
    fn span_to<T>(&self, other: &T) -> SourceSpan
    where
        T: GetPos,
    {
        self.span_to_end(other.get_source_span().0.1)
    }

    fn span_to_end(&self, end: (usize, usize)) -> SourceSpan {
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
        SingleIdentifier,
        SpriteCodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    BroadcastDeclaration(
        BroadcastKeyword,
        Option<CanonicalIdentifier>,
        SingleIdentifier,
        Semicolon,
        SourceSpan,
    ),
    UseStatement(
        UseKeyword,
        Option<ExtensionKeyword>,
        UseStatementContent,
        Semicolon,
        SourceSpan,
    ),
    EmptyStatement(Semicolon),
    Invalid(SourceSpan),
}

impl GetPos for TopLevelStatement {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            TopLevelStatement::Stage(_, _, _, p) => p,
            TopLevelStatement::Sprite(_, _, _, _, _, p) => p,
            TopLevelStatement::BroadcastDeclaration(_, _, _, _, p) => p,
            TopLevelStatement::UseStatement(_, _, _, _, p) => p,
            TopLevelStatement::EmptyStatement(p) => &p.0,
            TopLevelStatement::Invalid(p) => p,
        }
    }
}

impl InvalidVariantFromSourceSpan for TopLevelStatement {
    #[inline]
    fn invalid_variant_from_source_span(source_span: SourceSpan) -> Self {
        Self::Invalid(source_span)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SpriteKeyword(pub SourceSpan);

impl FromSourceSpan for SpriteKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for SpriteKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct StageKeyword(pub SourceSpan);

impl FromSourceSpan for StageKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for StageKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct CostumeKeyword(pub SourceSpan);

impl FromSourceSpan for CostumeKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for CostumeKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BroadcastKeyword(pub SourceSpan);

impl FromSourceSpan for BroadcastKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for BroadcastKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UseKeyword(pub SourceSpan);

impl FromSourceSpan for UseKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for UseKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExtensionKeyword(pub SourceSpan);

impl FromSourceSpan for ExtensionKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for ExtensionKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConfigKeyword(pub SourceSpan);

impl FromSourceSpan for ConfigKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for ConfigKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MonitorKeyword(pub SourceSpan);

impl FromSourceSpan for MonitorKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for MonitorKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AsKeyword(pub SourceSpan);

impl FromSourceSpan for AsKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for AsKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct BackdropKeyword(pub SourceSpan);

impl FromSourceSpan for BackdropKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for BackdropKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SoundKeyword(pub SourceSpan);

impl FromSourceSpan for SoundKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for SoundKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ProcKeyword(pub SourceSpan);

impl FromSourceSpan for ProcKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CommaSeparated<T> {
    pub values: Vec<(T, Comma)>,
    pub tail_value: Option<Box<T>>,
    pub source_span: SourceSpan,
}

impl<T> CommaSeparated<T> {
    pub fn len(&self) -> usize {
        self.values.len() + self.tail_value.is_some() as usize
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty() && self.tail_value.is_none()
    }
}

pub struct BorrowedCommaSeparatedIterator<'a, T> {
    pub comma_separated: &'a CommaSeparated<T>,
    pub index: usize,
}

impl<'a, T> Iterator for BorrowedCommaSeparatedIterator<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let cs_len = self.comma_separated.values.len();
        if self.index < cs_len {
            let value = &self.comma_separated.values[self.index].0;
            self.index += 1;
            return Some(value);
        }
        if self.index == cs_len {
            let value = self.comma_separated.tail_value.as_ref().map(Box::as_ref);
            self.index += 1;
            return value;
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.comma_separated.len() - self.index;
        (len, Some(len))
    }
}

impl<T> CommaSeparated<T> {
    pub fn iter(&self) -> BorrowedCommaSeparatedIterator<'_, T> {
        self.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a CommaSeparated<T> {
    type Item = &'a T;
    type IntoIter = BorrowedCommaSeparatedIterator<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        BorrowedCommaSeparatedIterator {
            comma_separated: self,
            index: 0,
        }
    }
}
pub struct CommaSeparatedIterator<T> {
    pub values_iterator: <Vec<(T, Comma)> as IntoIterator>::IntoIter,
    pub tail_value: Option<T>,
    pub index: usize,
}

impl<T> Iterator for CommaSeparatedIterator<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.values_iterator
            .next()
            .map(|value| value.0)
            .or_else(|| self.tail_value.take())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.values_iterator.size_hint();
        (
            lower.saturating_add(self.tail_value.is_some() as usize),
            upper.map(|value| value.saturating_add(self.tail_value.is_some() as usize)),
        )
    }
}

impl<T> IntoIterator for CommaSeparated<T> {
    type Item = T;
    type IntoIter = CommaSeparatedIterator<T>;
    fn into_iter(self) -> Self::IntoIter {
        CommaSeparatedIterator {
            values_iterator: self.values.into_iter(),
            tail_value: self.tail_value.map(|value| *value),
            index: 0,
        }
    }
}

impl<T> GetPos for CommaSeparated<T> {
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UseStatementContent {
    SingleUse {
        identifier: Identifier,
        rename: Option<(AsKeyword, SingleIdentifier)>,
        source_span: SourceSpan,
    },
    MultiUse {
        root: Identifier,
        left_brace: LeftBrace,
        content: CommaSeparated<UseStatementContent>,
        right_brace: RightBrace,
        source_span: SourceSpan,
    },
    Invalid(SourceSpan),
}

impl GetPos for UseStatementContent {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            UseStatementContent::SingleUse {
                identifier: _,
                rename: _,
                source_span,
            }
            | UseStatementContent::MultiUse {
                root: _,
                left_brace: _,
                content: _,
                right_brace: _,
                source_span,
            }
            | UseStatementContent::Invalid(source_span) => source_span,
        }
    }
}

impl InvalidVariantFromSourceSpan for UseStatementContent {
    #[inline]
    fn invalid_variant_from_source_span(source_span: SourceSpan) -> Self {
        Self::Invalid(source_span)
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
    SingleIdentifier,
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
        CommaSeparated<Expression>,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    CustomBlockDefinition(
        Option<WarpSpecifier>,
        ProcKeyword,
        Option<CanonicalIdentifier>,
        SingleIdentifier,
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
    ConfigStatement(
        ConfigKeyword,
        LeftBrace,
        CommaSeparated<DictionaryEntry>,
        RightBrace,
        SourceSpan,
    ),
    MonitorDeclaration(
        MonitorKeyword,
        MonitorValue,
        LeftBrace,
        CommaSeparated<DictionaryEntry>,
        RightBrace,
        SourceSpan,
    ),
    UseStatement(
        UseKeyword,
        Option<ExtensionKeyword>,
        UseStatementContent,
        Semicolon,
        SourceSpan,
    ),
    EmptyStatement(Semicolon),
    Invalid(SourceSpan),
}

pub type CustomBlockDefinition = (
    Option<WarpSpecifier>,
    ProcKeyword,
    Option<CanonicalIdentifier>,
    SingleIdentifier,
    LeftParens,
    Vec<(
        Option<CustomBlockParamKind>,
        Option<CanonicalIdentifier>,
        SingleIdentifier,
        Option<Comma>,
    )>,
    RightParens,
    CodeBlock,
    Option<Semicolon>,
    SourceSpan,
);

impl GetPos for StageStatement {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            StageStatement::DataDeclaration(_, _, _, p)
            | StageStatement::BackdropDeclaration(_, _, _, p)
            | StageStatement::SoundDeclaration(_, _, _, p)
            | StageStatement::NoInputHatStatement(_, _, _, p)
            | StageStatement::SingleInputHatStatement(_, _, _, _, p)
            | StageStatement::MultiInputHatStatement(_, _, _, _, _, _, p)
            | StageStatement::CustomBlockDefinition(_, _, _, _, _, _, _, _, _, p)
            | StageStatement::IsolatedBlock(_, _, p)
            | StageStatement::IsolatedExpression(_, _, _, _, p)
            | StageStatement::ConfigStatement(_, _, _, _, p)
            | StageStatement::MonitorDeclaration(_, _, _, _, _, p)
            | StageStatement::UseStatement(_, _, _, _, p)
            | StageStatement::Invalid(p) => p,
            StageStatement::EmptyStatement(p) => &p.0,
        }
    }
}

impl InvalidVariantFromSourceSpan for StageStatement {
    #[inline]
    fn invalid_variant_from_source_span(source_span: SourceSpan) -> Self {
        Self::Invalid(source_span)
    }
}

impl ConfigStatementFromContent for StageStatement {
    #[inline]
    fn config_statement_from_content(
        config_keyword: ConfigKeyword,
        left_brace: LeftBrace,
        items: CommaSeparated<DictionaryEntry>,
        right_brace: RightBrace,
        source_span: SourceSpan,
    ) -> Self {
        Self::ConfigStatement(config_keyword, left_brace, items, right_brace, source_span)
    }
}

impl MonitorDeclarationFromContent for StageStatement {
    fn monitor_statement_from_content(
        monitor_keyword: MonitorKeyword,
        monitor_value: MonitorValue,
        left_brace: LeftBrace,
        items: CommaSeparated<DictionaryEntry>,
        right_brace: RightBrace,
        source_span: SourceSpan,
    ) -> Self {
        Self::MonitorDeclaration(
            monitor_keyword,
            monitor_value,
            left_brace,
            items,
            right_brace,
            source_span,
        )
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
        CommaSeparated<Expression>,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    ),
    CustomBlockDefinition(
        Option<WarpSpecifier>,
        ProcKeyword,
        Option<CanonicalIdentifier>,
        SingleIdentifier,
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
    ConfigStatement(
        ConfigKeyword,
        LeftBrace,
        CommaSeparated<DictionaryEntry>,
        RightBrace,
        SourceSpan,
    ),
    MonitorDeclaration(
        MonitorKeyword,
        MonitorValue,
        LeftBrace,
        CommaSeparated<DictionaryEntry>,
        RightBrace,
        SourceSpan,
    ),
    UseStatement(
        UseKeyword,
        Option<ExtensionKeyword>,
        UseStatementContent,
        Semicolon,
        SourceSpan,
    ),
    EmptyStatement(Semicolon),
    Invalid(SourceSpan),
}

impl GetPos for SpriteStatement {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            SpriteStatement::DataDeclaration(_, _, _, p)
            | SpriteStatement::CostumeDeclaration(_, _, _, p)
            | SpriteStatement::SoundDeclaration(_, _, _, p)
            | SpriteStatement::NoInputHatStatement(_, _, _, p)
            | SpriteStatement::SingleInputHatStatement(_, _, _, _, p)
            | SpriteStatement::MultiInputHatStatement(_, _, _, _, _, _, p)
            | SpriteStatement::CustomBlockDefinition(_, _, _, _, _, _, _, _, _, p)
            | SpriteStatement::IsolatedBlock(_, _, p)
            | SpriteStatement::IsolatedExpression(_, _, _, _, p)
            | SpriteStatement::ConfigStatement(_, _, _, _, p)
            | SpriteStatement::MonitorDeclaration(_, _, _, _, _, p)
            | SpriteStatement::UseStatement(_, _, _, _, p)
            | SpriteStatement::Invalid(p) => p,
            SpriteStatement::EmptyStatement(p) => &p.0,
        }
    }
}

impl InvalidVariantFromSourceSpan for SpriteStatement {
    #[inline]
    fn invalid_variant_from_source_span(source_span: SourceSpan) -> Self {
        Self::Invalid(source_span)
    }
}

impl ConfigStatementFromContent for SpriteStatement {
    #[inline]
    fn config_statement_from_content(
        config_keyword: ConfigKeyword,
        left_brace: LeftBrace,
        items: CommaSeparated<DictionaryEntry>,
        right_brace: RightBrace,
        source_span: SourceSpan,
    ) -> Self {
        Self::ConfigStatement(config_keyword, left_brace, items, right_brace, source_span)
    }
}

impl MonitorDeclarationFromContent for SpriteStatement {
    fn monitor_statement_from_content(
        monitor_keyword: MonitorKeyword,
        monitor_value: MonitorValue,
        left_brace: LeftBrace,
        items: CommaSeparated<DictionaryEntry>,
        right_brace: RightBrace,
        source_span: SourceSpan,
    ) -> Self {
        Self::MonitorDeclaration(
            monitor_keyword,
            monitor_value,
            left_brace,
            items,
            right_brace,
            source_span,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AssetDeclaration {
    Multiple(
        LeftParens,
        CommaSeparated<SingleAssetDeclaration>,
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
    pub SingleIdentifier,
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
    // TODO: Use `Expression` for SingleAssetDeclarationValue::Simple::1
    // Issue: #92
    Dictionary(
        LeftBrace,
        CommaSeparated<DictionaryEntry>,
        RightBrace,
        SourceSpan,
    ),
}

impl GetPos for SingleAssetDeclarationValue {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            SingleAssetDeclarationValue::Simple(_, _, _, source_span)
            | SingleAssetDeclarationValue::Dictionary(_, _, _, source_span) => source_span,
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
        CommaSeparated<ListEntry>,
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
        CommaSeparated<Expression>,
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
        CommaSeparated<Expression>,
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
    UseStatement(
        UseKeyword,
        Option<ExtensionKeyword>,
        UseStatementContent,
        Semicolon,
        SourceSpan,
    ),
    EmptyStatement(Semicolon),
    Invalid(SourceSpan),
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
            Statement::UseStatement(_, _, _, _, p) => p,
            Statement::EmptyStatement(p) => &p.0,
            Statement::Invalid(p) => p,
        }
    }
}

impl InvalidVariantFromSourceSpan for Statement {
    #[inline]
    fn invalid_variant_from_source_span(source_span: SourceSpan) -> Self {
        Self::Invalid(source_span)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LetKeyword(pub SourceSpan);

impl FromSourceSpan for LetKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for LetKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct VarsKeyword(pub SourceSpan);

impl FromSourceSpan for VarsKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for VarsKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ListsKeyword(pub SourceSpan);

impl FromSourceSpan for ListsKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for ListsKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct VarKeyword(pub SourceSpan);

impl FromSourceSpan for VarKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for VarKeyword {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ListKeyword(pub SourceSpan);

impl FromSourceSpan for ListKeyword {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

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
        CommaSeparated<SingleDataDeclaration>,
        RightParens,
        SourceSpan,
    ),
    Vars(
        DataDeclarationScope,
        VarsKeyword,
        LeftBrace,
        CommaSeparated<SingleDataDeclaration>,
        RightBrace,
        SourceSpan,
    ),
    Lists(
        DataDeclarationScope,
        ListsKeyword,
        LeftBrace,
        CommaSeparated<SingleDataDeclaration>,
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

impl FromSourceSpan for NormalAssignmentOperator {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for NormalAssignmentOperator {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[expect(clippy::large_enum_variant)]
pub enum DictionaryEntry {
    Valid(SingleIdentifier, Colon, DictionaryValue, SourceSpan),
    Invalid(SourceSpan),
}

impl GetPos for DictionaryEntry {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            DictionaryEntry::Valid(_, _, _, p) | DictionaryEntry::Invalid(p) => p,
        }
    }
}

impl InvalidVariantFromSourceSpan for DictionaryEntry {
    fn invalid_variant_from_source_span(source_span: SourceSpan) -> Self {
        Self::Invalid(source_span)
    }
}

impl DictionaryEntry {
    pub fn to_valid(&self) -> Option<(&SingleIdentifier, &Colon, &DictionaryValue, &SourceSpan)> {
        match self {
            DictionaryEntry::Valid(single_identifier, colon, value, p) => {
                Some((single_identifier, colon, value, p))
            }
            DictionaryEntry::Invalid(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DictionaryValue {
    Primitive(Literal),
    Dictionary(
        LeftBrace,
        CommaSeparated<DictionaryEntry>,
        RightBrace,
        SourceSpan,
    ),
    List(
        LeftBracket,
        CommaSeparated<DictionaryValue>,
        RightBracket,
        SourceSpan,
    ),
    Invalid(SourceSpan),
}

impl GetPos for DictionaryValue {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            DictionaryValue::Primitive(literal) => literal.get_source_span(),
            DictionaryValue::Dictionary(_, _, _, p)
            | DictionaryValue::List(_, _, _, p)
            | DictionaryValue::Invalid(p) => p,
        }
    }
}

impl InvalidVariantFromSourceSpan for DictionaryValue {
    fn invalid_variant_from_source_span(source_span: SourceSpan) -> Self {
        Self::Invalid(source_span)
    }
}

impl DictionaryValue {
    pub fn as_literal(&self) -> Result<&Literal, DictionaryTypeError> {
        match self {
            DictionaryValue::Primitive(literal) => Ok(literal),
            DictionaryValue::List(..) => Err(DictionaryTypeError::ListAsPrimitive {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Dictionary(..) => Err(DictionaryTypeError::DictAsPrimitive {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Invalid(source_span) => Err(DictionaryTypeError::InvalidValue {
                source_span: *source_span,
            }),
        }
    }

    pub fn to_literal(self) -> Result<Literal, DictionaryTypeError> {
        match self {
            DictionaryValue::Primitive(literal) => Ok(literal),
            DictionaryValue::List(..) => Err(DictionaryTypeError::ListAsPrimitive {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Dictionary(..) => Err(DictionaryTypeError::DictAsPrimitive {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Invalid(source_span) => {
                Err(DictionaryTypeError::InvalidValue { source_span })
            }
        }
    }

    pub fn as_list(&self) -> Result<&CommaSeparated<DictionaryValue>, DictionaryTypeError> {
        match self {
            DictionaryValue::List(_, items, _, _) => Ok(items),
            DictionaryValue::Primitive(..) => Err(DictionaryTypeError::PrimitiveAsList {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Dictionary(..) => Err(DictionaryTypeError::DictAsList {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Invalid(source_span) => Err(DictionaryTypeError::InvalidValue {
                source_span: *source_span,
            }),
        }
    }

    pub fn to_list(self) -> Result<CommaSeparated<DictionaryValue>, DictionaryTypeError> {
        match self {
            DictionaryValue::List(_, items, _, _) => Ok(items),
            DictionaryValue::Primitive(..) => Err(DictionaryTypeError::PrimitiveAsList {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Dictionary(..) => Err(DictionaryTypeError::DictAsList {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Invalid(source_span) => {
                Err(DictionaryTypeError::InvalidValue { source_span })
            }
        }
    }

    pub fn as_dict(&self) -> Result<&CommaSeparated<DictionaryEntry>, DictionaryTypeError> {
        match self {
            DictionaryValue::Dictionary(_, items, _, _) => Ok(items),
            DictionaryValue::Primitive(..) => Err(DictionaryTypeError::PrimitiveAsDict {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::List(..) => Err(DictionaryTypeError::ListAsDict {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Invalid(source_span) => Err(DictionaryTypeError::InvalidValue {
                source_span: *source_span,
            }),
        }
    }

    pub fn to_dict(self) -> Result<CommaSeparated<DictionaryEntry>, DictionaryTypeError> {
        match self {
            DictionaryValue::Dictionary(_, items, _, _) => Ok(items),
            DictionaryValue::Primitive(..) => Err(DictionaryTypeError::PrimitiveAsDict {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::List(..) => Err(DictionaryTypeError::ListAsDict {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Invalid(source_span) => {
                Err(DictionaryTypeError::InvalidValue { source_span })
            }
        }
    }

    pub fn as_literal_or_list(
        &self,
    ) -> Result<BorrowedDictionaryValueLiteralOrList<'_>, DictionaryTypeError> {
        match self {
            DictionaryValue::Primitive(literal) => {
                Ok(BorrowedDictionaryValueLiteralOrList::Literal(literal))
            }
            DictionaryValue::List(_, items, _, _) => {
                Ok(BorrowedDictionaryValueLiteralOrList::List(items))
            }
            DictionaryValue::Dictionary(..) => Err(DictionaryTypeError::CannotBeDict {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Invalid(source_span) => Err(DictionaryTypeError::InvalidValue {
                source_span: *source_span,
            }),
        }
    }

    pub fn to_literal_or_list(self) -> Result<DictionaryValueLiteralOrList, DictionaryTypeError> {
        match self {
            DictionaryValue::Primitive(literal) => {
                Ok(DictionaryValueLiteralOrList::Literal(literal))
            }
            DictionaryValue::List(_, items, _, _) => Ok(DictionaryValueLiteralOrList::List(items)),
            DictionaryValue::Dictionary(..) => Err(DictionaryTypeError::CannotBeDict {
                source_span: *self.get_source_span(),
            }),
            DictionaryValue::Invalid(source_span) => {
                Err(DictionaryTypeError::InvalidValue { source_span })
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DictionaryValueLiteralOrList {
    Literal(Literal),
    List(CommaSeparated<DictionaryValue>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BorrowedDictionaryValueLiteralOrList<'a> {
    Literal(&'a Literal),
    List(&'a CommaSeparated<DictionaryValue>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MonitorValue {
    Identifier(Identifier),
    Call(
        Identifier,
        LeftParens,
        CommaSeparated<Identifier>,
        RightParens,
        SourceSpan,
    ),
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Colon(pub SourceSpan);

impl FromSourceSpan for Colon {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for Colon {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Dot(pub SourceSpan);

impl FromSourceSpan for Dot {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for Dot {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct DoubleColon(pub SourceSpan);

impl FromSourceSpan for DoubleColon {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for DoubleColon {
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
        SingleIdentifier,
        NormalAssignmentOperator,
        Expression,
        SourceSpan,
    ),
    EmptyVariable(
        Option<VarKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        SingleIdentifier,
        SourceSpan,
    ),
    List(
        Option<ListKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        SingleIdentifier,
        NormalAssignmentOperator,
        LeftBracket,
        CommaSeparated<ListEntry>,
        RightBracket,
        SourceSpan,
    ),
    // FileList(
    //     Option<ListKeyword>,
    //     DataDeclarationScope,
    //     Option<CanonicalIdentifier>,
    //     SingleIdentifier,
    //     NormalAssignmentOperator,
    //     FileKeyword,
    //     Expression,
    //     SourceSpan,
    // )
    EmptyList(
        Option<ListKeyword>,
        DataDeclarationScope,
        Option<CanonicalIdentifier>,
        SingleIdentifier,
        SourceSpan,
    ),
}

// TODO: Allow loading list content from a resource file
// Syntax:
// ```rust
// let list list_name = file "path/to/list";
// ```
// Issue: #91

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

impl FromSourceSpan for Comma {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for Comma {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LeftBrace(pub SourceSpan);

impl FromSourceSpan for LeftBrace {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for LeftBrace {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct RightBrace(pub SourceSpan);

impl FromSourceSpan for RightBrace {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

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
        CommaSeparated<Expression>,
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
            Expression::Literal(literal) => Ok(Sb3PrimitiveOrBool::from(literal).into()),
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
                    identifier.iter().map(|value| value.value.as_str()),
                )
                .map_err(|err| match err {
                    crate::library::ConstExpLookupError::NotFound => {
                        ConstantExprEvaluationError::ConstIdentifierDoesNotExist {
                            identifier: Box::new(identifier.clone()),
                        }
                    }
                    crate::library::ConstExpLookupError::UsedSuper => {
                        ConstantExprEvaluationError::ConstIdentifierUsedSuper {
                            identifier: Box::new(identifier.clone()),
                        }
                    }
                })?;

                let Some(ConstantExprLibraryItemValue::Function(function_id, _)) =
                    library_item.value
                else {
                    return Err(match library_item.value {
                        Some(ConstantExprLibraryItemValue::AssociatedItem(_)) => {
                            ConstantExprEvaluationError::ConstValueNotCallable {
                                identifier: Box::new(identifier.clone()),
                            }
                        }
                        None => ConstantExprEvaluationError::ConstNamespaceNotCallable {
                            identifier: Box::new(identifier.clone()),
                        },
                        Some(ConstantExprLibraryItemValue::Function(_, _)) => unreachable!(),
                    });
                };

                let Ok(function) = ConstantExprFunction::try_from(function_id) else {
                    return Err(ConstantExprEvaluationError::ConstNamespaceNotCallable {
                        identifier: Box::new(identifier.clone()),
                    });
                };

                function.apply(exprs.iter(), *source_span)
            }
            Expression::Identifier(identifier) => {
                let value = crate::library::const_expr_lookup(
                    identifier.iter().map(|value| value.value.as_str()),
                )
                .map_err(|err| match err {
                    crate::library::ConstExpLookupError::NotFound => {
                        ConstantExprEvaluationError::ConstIdentifierDoesNotExist {
                            identifier: Box::new(identifier.clone()),
                        }
                    }
                    crate::library::ConstExpLookupError::UsedSuper => {
                        ConstantExprEvaluationError::ConstIdentifierUsedSuper {
                            identifier: Box::new(identifier.clone()),
                        }
                    }
                })?;
                let Ok(function) = (match &value.value {
                    Some(ConstantExprLibraryItemValue::Function(function, true)) => {
                        ConstantExprFunction::try_from(*function)
                    }
                    Some(ConstantExprLibraryItemValue::Function(_, _)) => {
                        return Err(ConstantExprEvaluationError::ConstFunctionNotSingleton {
                            identifier: Box::new(identifier.clone()),
                        });
                    }
                    Some(ConstantExprLibraryItemValue::AssociatedItem(_)) => {
                        return Err(ConstantExprEvaluationError::ConstValueNotJsPrimitive {
                            identifier: Box::new(identifier.clone()),
                        });
                    }
                    None => {
                        return Err(ConstantExprEvaluationError::ConstNamespaceNotJsPrimitive {
                            identifier: Box::new(identifier.clone()),
                        });
                    }
                }) else {
                    return Err(ConstantExprEvaluationError::ConstNamespaceNotCallable {
                        identifier: Box::new(identifier.clone()),
                    });
                };
                function.apply(std::iter::empty(), *identifier.get_source_span())
            }
            Expression::GetItem(_, _, _, _, _) => {
                Err(ConstantExprEvaluationError::ConstExprListAccess {
                    expression: Box::new(self.clone()),
                })
            }
        }
    }

    pub fn calculate_value(&self) -> Result<Sb3PrimitiveOrBool, ConstantExprEvaluationError> {
        if let Expression::Literal(literal) = self {
            return Ok(literal.into());
        }
        self.calculate_value_js().map(Into::into)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LeftParens(pub SourceSpan);

impl FromSourceSpan for LeftParens {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for LeftParens {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct RightParens(pub SourceSpan);

impl FromSourceSpan for RightParens {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for RightParens {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LeftBracket(pub SourceSpan);

impl FromSourceSpan for LeftBracket {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for LeftBracket {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct RightBracket(pub SourceSpan);

impl FromSourceSpan for RightBracket {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for RightBracket {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct LetterAccessLeftBracket(pub SourceSpan);

impl FromSourceSpan for LetterAccessLeftBracket {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for LetterAccessLeftBracket {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Semicolon(pub SourceSpan);

impl FromSourceSpan for Semicolon {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

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
    Contains(SourceSpan),
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
            BinOp::Contains(p) => p,
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
            BinOp::Contains(_) => (2, L),
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
            BinOp::Contains(_) => BinOpDescriptor {
                opcode: "operator_contains".to_string(),
                operand_a_input_name: "STRING1".to_string(),
                operand_b_input_name: "STRING2".to_string(),
                operand_a_default: Some("apple".into()),
                operand_b_default: Some("a".into()),
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
    pub extra_inputs: HashMap<String, grazelang_types::project_json::Sb3PrimitiveBlock>,
    pub field_values: HashMap<String, grazelang_types::project_json::Sb3FieldValue>,
    pub default: Option<grazelang_types::project_json::Sb3PrimitiveBlock>,
}

impl UnOp {
    pub fn get_descriptor(&self) -> UnOpDescriptor {
        use grazelang_types::project_json::{Sb3FieldValue, Sb3PrimitiveBlock};

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
    Bool(bool, SourceSpan),
    EmptyExpression(LeftParens, RightParens, SourceSpan),
}

impl Literal {
    pub fn get_non_empty(self) -> Option<Self> {
        match self {
            Literal::EmptyExpression(..) => None,
            _ => Some(self),
        }
    }

    pub fn get_string_value(&self) -> &IString {
        match self {
            Literal::String(value, _) => value,
            Literal::DecimalInt(value, _) => value,
            Literal::DecimalFloat(value, _) => value,
            Literal::HexadecimalInt(value, _) => value,
            Literal::OctalInt(value, _) => value,
            Literal::BinaryInt(value, _) => value,
            Literal::Bool(value, _) => {
                if *value {
                    TRUE_ISTRING_REF
                } else {
                    FALSE_ISTRING_REF
                }
            }
            Literal::EmptyExpression(_, _, _) => EMPTY_ISTRING_REF,
        }
    }

    pub fn cast_to_string(&self) -> IString {
        self.get_string_value().clone()
    }

    pub fn get_json_string(&self) -> Cow<'_, str> {
        Cow::Borrowed(match self {
            Literal::String(value, _) => return Cow::Owned(serde_json::to_string(value).unwrap()),
            Literal::DecimalInt(value, _) => value,
            Literal::DecimalFloat(value, _) => value,
            _ => {
                return Cow::Owned(serde_json::to_string(&Sb3PrimitiveOrBool::from(self)).unwrap());
            }
        })
    }
}

impl From<&Literal> for Sb3PrimitiveOrBool {
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
            Literal::Bool(value, _) => return Self::Bool(*value),
            _ => (),
        }
        value.get_string_value().into()
    }
}

impl From<&Literal> for Sb3Primitive {
    fn from(value: &Literal) -> Self {
        let prim_or_bool: Sb3PrimitiveOrBool = value.into();
        prim_or_bool.into()
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
            Literal::Bool(_, p) => p,
            Literal::EmptyExpression(_, _, p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FormattedStringContent {
    Expression(Box<Expression>),
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
    pub root: SingleIdentifier,
    pub path: Vec<(DoubleColon, SingleIdentifier)>,
    pub fields: Vec<(Dot, SingleIdentifier)>,
    pub source_span: SourceSpan,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.root)?;
        for (_, segment) in &self.path {
            write!(f, "::{}", segment.value)?;
        }
        for (_, segment) in &self.fields {
            write!(f, ".{}", segment.value)?;
        }
        Ok(())
    }
}

impl GetPos for Identifier {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Debug, Clone)]
pub enum OwnedIdentifierIterator {
    Root(
        SingleIdentifier,
        Vec<(DoubleColon, SingleIdentifier)>,
        Vec<(Dot, SingleIdentifier)>,
    ),
    Path(
        <Vec<(DoubleColon, SingleIdentifier)> as IntoIterator>::IntoIter,
        Vec<(Dot, SingleIdentifier)>,
    ),
    Fields(<Vec<(Dot, SingleIdentifier)> as IntoIterator>::IntoIter),
}

impl Iterator for OwnedIdentifierIterator {
    type Item = SingleIdentifier;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            OwnedIdentifierIterator::Root(_, path, fields) => {
                let path = std::mem::take(path);
                let fields = std::mem::take(fields);
                let OwnedIdentifierIterator::Root(value, _, _) = std::mem::replace(
                    self,
                    OwnedIdentifierIterator::Path(path.into_iter(), fields),
                ) else {
                    unreachable!()
                };
                Some(value)
            }
            OwnedIdentifierIterator::Path(path, fields) => {
                if let Some((_, value)) = path.next() {
                    return Some(value);
                }
                let fields = std::mem::take(fields);
                *self = OwnedIdentifierIterator::Fields(fields.into_iter());
                self.next()
            }
            OwnedIdentifierIterator::Fields(fields) => fields.next().map(|(_, value)| value),
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Serialize, Deserialize)]
pub enum BorrowedIdentifierIteratorState {
    #[default]
    Root,
    Path(usize),
    Fields(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BorrowedIdentifierIterator<'a> {
    pub identifier: &'a Identifier,
    pub state: BorrowedIdentifierIteratorState,
}

impl<'a> Iterator for BorrowedIdentifierIterator<'a> {
    type Item = &'a SingleIdentifier;
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.state {
            BorrowedIdentifierIteratorState::Root => {
                self.state = BorrowedIdentifierIteratorState::Path(0);
                Some(&self.identifier.root)
            }
            BorrowedIdentifierIteratorState::Path(idx) => {
                if *idx >= self.identifier.path.len() {
                    self.state = BorrowedIdentifierIteratorState::Fields(0);
                    return self.next();
                }
                let value = &self.identifier.path[*idx].1;
                *idx += 1;
                Some(value)
            }
            BorrowedIdentifierIteratorState::Fields(idx) => {
                if *idx >= self.identifier.fields.len() {
                    return None;
                }
                let value = &self.identifier.fields[*idx].1;
                *idx += 1;
                Some(value)
            }
        }
    }
}

impl<'a> IntoIterator for &'a Identifier {
    type Item = &'a SingleIdentifier;
    type IntoIter = BorrowedIdentifierIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        BorrowedIdentifierIterator {
            identifier: self,
            state: Default::default(),
        }
    }
}

impl IntoIterator for Identifier {
    type Item = SingleIdentifier;
    type IntoIter = OwnedIdentifierIterator;

    fn into_iter(self) -> Self::IntoIter {
        OwnedIdentifierIterator::Root(self.root, self.path, self.fields)
    }
}

impl Identifier {
    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        self.into_iter()
    }
}

impl Identifier {
    pub fn to_single(&self) -> Option<&SingleIdentifier> {
        if let (0, 0) = (self.path.len(), self.fields.len()) {
            Some(&self.root)
        } else {
            None
        }
    }
}

impl Identifier {
    pub fn to_syntactic_if(&self) -> Option<SyntacticIf> {
        self.to_single()
            .and_then(|SingleIdentifier { value, source_span }| {
                (value.as_str() == "if").then_some(SyntacticIf(*source_span))
            })
    }
}

impl SingleIdentifier {
    pub fn to_syntactic_if(&self) -> Option<SyntacticIf> {
        (self.value.as_str() == "if").then_some(SyntacticIf(self.source_span))
    }

    pub fn to_syntactic_else(&self) -> Option<SyntacticElse> {
        (self.value.as_str() == "else").then_some(SyntacticElse(self.source_span))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SingleIdentifier {
    pub value: IString,
    pub source_span: SourceSpan,
}

impl Display for SingleIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.value)
    }
}

impl GetPos for SingleIdentifier {
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

impl From<&(IString, SourceSpan)> for SingleIdentifier {
    fn from(value: &(IString, SourceSpan)) -> Self {
        Self {
            value: value.0.clone(),
            source_span: value.1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SyntacticIf(pub SourceSpan);

impl FromSourceSpan for SyntacticIf {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

impl GetPos for SyntacticIf {
    #[inline]
    fn get_source_span(&self) -> &SourceSpan {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SyntacticElse(pub SourceSpan);

impl FromSourceSpan for SyntacticElse {
    fn from_source_span(source_span: SourceSpan) -> Self {
        Self(source_span)
    }
}

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

// TODO: Validate more names
// Issue: #89

#[derive(Debug, Clone, Error, enum_assoc::Assoc)]
#[func(const fn internal_lint_id(&self) -> &'static str)]
#[func(const fn internal_primary_message(&self) -> &'static str)]
#[func(pub const fn get_secondary_message(&self) -> &'static str)]
pub enum ParseError {
    #[assoc(internal_lint_id = "unexpected_end_of_input")]
    #[assoc(
        internal_primary_message = "the lexer reached the end of input without the parser completing"
    )]
    #[assoc(get_secondary_message = "unexpected end of input")]
    #[error("the lexer reached the end of input without the parser completing")]
    UnexpectedEndOfInput {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "unexpected_token")]
    #[assoc(internal_primary_message = "")]
    #[assoc(get_secondary_message = "unexpected token")]
    #[error("unexpected token at {source_span:?}, expected {expected}")]
    UnexpectedToken {
        expected: IString,
        message: IString,
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        found: crate::lexer::Token,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "lexer_stuck")]
    #[assoc(internal_primary_message = "lexer got stuck")]
    #[assoc(get_secondary_message = "lexer got stuck after this token")]
    #[error("the lexer got stuck after the token at {source_span:?}")]
    LexerStuck {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "local_symbol_in_stage")]
    #[assoc(internal_primary_message = "cannot declare a local symbol in stage")]
    #[assoc(get_secondary_message = "modifier is not applicable in this context")]
    #[error("tried to declare a local symbol in stage at {source_span:?}")]
    LocalSymbolInStage {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "peeked_back_at_beginning")]
    #[assoc(internal_primary_message = "parser tried peeking back at the beginning of the parse")]
    #[assoc(get_secondary_message = "token caused invalid backtracking")]
    #[error("tried to peek back at the beginning of the content")]
    PeekedBackAtBeginning {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "shadowed_symbol")]
    #[assoc(internal_primary_message = "")]
    #[assoc(get_secondary_message = "symbol redefined here")]
    #[error("tried to shadow symbol {symbol}")]
    ShadowedSymbol {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        symbol: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "symbol_named_super")]
    #[assoc(internal_primary_message = "name of symbol cannot be \"super\"")]
    #[assoc(get_secondary_message = "invalid symbol name")]
    #[error("tried to name a symbol \"super\"")]
    SymbolNamedSuper {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "symbol_named_self")]
    #[assoc(internal_primary_message = "name of symbol cannot be \"self\"")]
    #[assoc(get_secondary_message = "invalid symbol name")]
    #[error("tried to name a symbol \"self\"")]
    SymbolNamedSelf {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "sprite_named_stage")]
    #[assoc(internal_primary_message = "name of sprite cannot be \"stage\"")]
    #[assoc(get_secondary_message = "sprite cannot be named \"stage\"")]
    #[error("you cannot call a sprite \"stage\", try using a canonical name")]
    SpriteNamedStage {
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "missing_dictionary_entry")]
    #[assoc(internal_primary_message = "")]
    #[assoc(get_secondary_message = "missing dictionary entry here")]
    #[error("expected key {key:?} in flat dictionary")]
    MissingDictionaryEntry {
        key: IString,
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "unknown_dictionary_entry")]
    #[assoc(internal_primary_message = "")]
    #[assoc(get_secondary_message = "unexpected dictionary entry here")]
    #[error("unexpected key {key:?} in flat dictionary")]
    UnknownDictionaryEntry {
        key: IString,
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "repeated_dictionary_entry")]
    #[assoc(internal_primary_message = "")]
    #[assoc(get_secondary_message = "repeated dictionary entry here")]
    #[error("repeated key {key:?} in flat dictionary")]
    RepeatedDictionaryEntry {
        key: IString,
        #[cfg(feature = "include_context_in_parse_errors")]
        context: IString,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "")]
    #[assoc(internal_primary_message = "")]
    #[assoc(get_secondary_message = "")]
    #[error(transparent)]
    DictionaryTypeError {
        #[from]
        source: DictionaryTypeError,
    },
    #[assoc(internal_lint_id = "")]
    #[assoc(internal_primary_message = "")]
    #[assoc(get_secondary_message = "")]
    #[error("the expression {expression:?} is not calculatable by graze, {source}")]
    InvalidConstantExpression {
        expression: Box<Expression>,
        #[source]
        source: ConstantExprEvaluationError,
    },
    #[assoc(internal_lint_id = "io_error")]
    #[assoc(internal_primary_message = "an io error occurred")]
    #[assoc(get_secondary_message = "")]
    #[error("{source}")]
    IoError {
        #[source]
        source: std::rc::Rc<std::io::Error>,
        source_span: SourceSpan,
    },
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::UnexpectedEndOfInput {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: l_source_span,
                },
                Self::UnexpectedEndOfInput {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: r_source_span,
                },
            ) => l_source_span == r_source_span,
            (
                Self::UnexpectedToken {
                    expected: l_expected,
                    message: l_message,
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    found: l_found,
                    source_span: l_source_span,
                },
                Self::UnexpectedToken {
                    expected: r_expected,
                    message: r_message,
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    found: r_found,
                    source_span: r_source_span,
                },
            ) => {
                l_expected == r_expected
                    && l_message == r_message
                    && l_found == r_found
                    && l_source_span == r_source_span
            }
            (
                Self::LexerStuck {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: l_source_span,
                },
                Self::LexerStuck {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: r_source_span,
                },
            ) => l_source_span == r_source_span,
            (
                Self::LocalSymbolInStage {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: l_source_span,
                },
                Self::LocalSymbolInStage {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: r_source_span,
                },
            ) => l_source_span == r_source_span,
            (
                Self::PeekedBackAtBeginning {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: l_source_span,
                },
                Self::PeekedBackAtBeginning {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: r_source_span,
                },
            ) => l_source_span == r_source_span,
            (
                Self::ShadowedSymbol {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    symbol: l_symbol,
                    source_span: l_source_span,
                },
                Self::ShadowedSymbol {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    symbol: r_symbol,
                    source_span: r_source_span,
                },
            ) => l_symbol == r_symbol && l_source_span == r_source_span,
            (
                Self::SymbolNamedSuper {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: l_source_span,
                },
                Self::SymbolNamedSuper {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: r_source_span,
                },
            ) => l_source_span == r_source_span,
            (
                Self::SymbolNamedSelf {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: l_source_span,
                },
                Self::SymbolNamedSelf {
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: r_source_span,
                },
            ) => l_source_span == r_source_span,
            (
                Self::MissingDictionaryEntry {
                    key: l_key,
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: l_source_span,
                },
                Self::MissingDictionaryEntry {
                    key: r_key,
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: r_source_span,
                },
            ) => l_key == r_key && l_source_span == r_source_span,
            (
                Self::UnknownDictionaryEntry {
                    key: l_key,
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: l_source_span,
                },
                Self::UnknownDictionaryEntry {
                    key: r_key,
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: r_source_span,
                },
            ) => l_key == r_key && l_source_span == r_source_span,
            (
                Self::RepeatedDictionaryEntry {
                    key: l_key,
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: l_source_span,
                },
                Self::RepeatedDictionaryEntry {
                    key: r_key,
                    #[cfg(feature = "include_context_in_parse_errors")]
                        context: _,
                    source_span: r_source_span,
                },
            ) => l_key == r_key && l_source_span == r_source_span,
            (
                Self::InvalidConstantExpression {
                    expression: l_expression,
                    source: l_source,
                },
                Self::InvalidConstantExpression {
                    expression: r_expression,
                    source: r_source,
                },
            ) => l_expression == r_expression && l_source == r_source,
            (
                Self::IoError {
                    source: l_source,
                    source_span: l_source_span,
                },
                Self::IoError {
                    source: r_source,
                    source_span: r_source_span,
                },
            ) => {
                std::rc::Rc::as_ptr(l_source) == std::rc::Rc::as_ptr(r_source)
                    && l_source_span == r_source_span
            }
            _ => false,
        }
    }
}

impl GetLintId for ParseError {
    #[inline]
    fn get_lint_id(&self) -> &'static str {
        self.internal_lint_id()
    }
}

impl ParseError {
    pub fn get_primary_message(&self) -> Cow<'static, str> {
        match self {
            Self::UnexpectedToken {
                expected,
                message: _,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                found,
                source_span: _,
            } => return Cow::Owned(format!("expected {expected}, found {found:?}")),
            Self::ShadowedSymbol {
                symbol,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span: _,
            } => return Cow::Owned(format!("name `{symbol}` is defined multiple times")),
            Self::MissingDictionaryEntry {
                key,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span: _,
            } => return Cow::Owned(format!("missing dictionary entry key: \"{key}\"")),
            Self::UnknownDictionaryEntry {
                key,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span: _,
            } => return Cow::Owned(format!("unexpected dictionary entry key: \"{key}\"")),
            Self::RepeatedDictionaryEntry {
                key,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span: _,
            } => {
                return Cow::Owned(format!(
                    "dictionary entry with key \"{key}\" defined multiple times"
                ));
            }
            Self::DictionaryTypeError { source } => {
                return Cow::Borrowed(source.get_primary_message());
            }
            Self::InvalidConstantExpression {
                expression: _,
                source,
            } => return source.get_primary_message(),
            _ => (),
        }
        Cow::Borrowed(self.internal_primary_message())
    }
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
            ParseError::SymbolNamedSelf {
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::SpriteNamedStage {
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::MissingDictionaryEntry {
                key: _,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::UnknownDictionaryEntry {
                key: _,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::RepeatedDictionaryEntry {
                key: _,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                source_span,
            } => source_span,
            ParseError::DictionaryTypeError { source } => source.get_source_span(),
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

pub trait FromWithSourceSpan<T>: Sized {
    fn from_with_source_span(value: T, source_span: SourceSpan) -> Self;

    fn from_result_with_source_span<O>(
        value: Result<O, T>,
        source_span: SourceSpan,
    ) -> Result<O, Self> {
        value.map_err(|err| Self::from_with_source_span(err, source_span))
    }
}

pub trait IntoWithSourceSpan<T>: Sized {
    fn into_with_source_span(self, source_span: SourceSpan) -> T;
}

pub trait IntoResultWithSourceSpan<T>: Sized {
    type Out: Sized;
    fn into_result_with_source_span(self, source_span: SourceSpan) -> Result<Self::Out, T>;
}

impl FromWithSourceSpan<std::io::Error> for ParseError {
    fn from_with_source_span(value: std::io::Error, source_span: SourceSpan) -> Self {
        Self::IoError {
            source: std::rc::Rc::new(value),
            source_span,
        }
    }
}

impl<A, B> IntoWithSourceSpan<A> for B
where
    A: FromWithSourceSpan<B>,
{
    fn into_with_source_span(self, source_span: SourceSpan) -> A {
        A::from_with_source_span(self, source_span)
    }
}

impl<O, A, B> IntoResultWithSourceSpan<A> for Result<O, B>
where
    A: FromWithSourceSpan<B>,
{
    type Out = O;
    fn into_result_with_source_span(self, source_span: SourceSpan) -> Result<Self::Out, A> {
        FromWithSourceSpan::from_result_with_source_span(self, source_span)
    }
}

pub trait FromSourceSpan: GetPos {
    fn from_source_span(source_span: SourceSpan) -> Self;
}

pub trait InvalidVariantFromSourceSpan {
    fn invalid_variant_from_source_span(source_span: SourceSpan) -> Self;
}

pub trait ConfigStatementFromContent {
    fn config_statement_from_content(
        config_keyword: ConfigKeyword,
        left_brace: LeftBrace,
        items: CommaSeparated<DictionaryEntry>,
        right_brace: RightBrace,
        source_span: SourceSpan,
    ) -> Self;
}

pub trait MonitorDeclarationFromContent {
    fn monitor_statement_from_content(
        monitor_keyword: MonitorKeyword,
        monitor_value: MonitorValue,
        left_brace: LeftBrace,
        items: CommaSeparated<DictionaryEntry>,
        right_brace: RightBrace,
        source_span: SourceSpan,
    ) -> Self;
}

#[derive(Debug, Clone, PartialEq, Error, Serialize, Deserialize, enum_assoc::Assoc)]
#[func(const fn internal_lint_id(&self) -> &'static str)]
#[func(pub const fn get_primary_message(&self) -> &'static str)]
#[func(pub const fn get_secondary_message(&self) -> &'static str)]
pub enum DictionaryTypeError {
    #[assoc(internal_lint_id = "list_as_primitive")]
    #[assoc(get_primary_message = "cannot interpret a list as a primitive value")]
    #[assoc(get_secondary_message = "should be a primitive value")]
    #[error("cannot interpret a list as a primitive value")]
    ListAsPrimitive { source_span: SourceSpan },
    #[assoc(internal_lint_id = "dict_as_primitive")]
    #[assoc(get_primary_message = "cannot interpret a dictionary as a primitive value")]
    #[assoc(get_secondary_message = "should be a primitive value")]
    #[error("cannot interpret a dictionary as a primitive value")]
    DictAsPrimitive { source_span: SourceSpan },
    #[assoc(internal_lint_id = "list_as_dictionary")]
    #[assoc(get_primary_message = "cannot interpret a list as a dictionary")]
    #[assoc(get_secondary_message = "should be a dictionary")]
    #[error("cannot interpret a list as a dictionary")]
    ListAsDict { source_span: SourceSpan },
    #[assoc(internal_lint_id = "primitive_as_dictionary")]
    #[assoc(get_primary_message = "cannot interpret a primitive value as a dictionary")]
    #[assoc(get_secondary_message = "should be a dictionary")]
    #[error("cannot interpret a primitive value as a dictionary")]
    PrimitiveAsDict { source_span: SourceSpan },
    #[assoc(internal_lint_id = "dict_as_list")]
    #[assoc(get_primary_message = "cannot interpret a dictionary as a list")]
    #[assoc(get_secondary_message = "should be a list")]
    #[error("cannot interpret a dictionary as a list")]
    DictAsList { source_span: SourceSpan },
    #[assoc(internal_lint_id = "primitive_as_list")]
    #[assoc(get_primary_message = "cannot interpret a primitive value as a list")]
    #[assoc(get_secondary_message = "should be a list")]
    #[error("cannot interpret a primitive value as a list")]
    PrimitiveAsList { source_span: SourceSpan },
    #[assoc(internal_lint_id = "cannot_be_dict")]
    #[assoc(get_primary_message = "used a dictionary where a list or primitive value was required")]
    #[assoc(get_secondary_message = "should be a list or primitive value")]
    #[error("used a dictionary where a list or primitive value was required")]
    CannotBeDict { source_span: SourceSpan },
    #[assoc(internal_lint_id = "invalid_dictionary_value")]
    #[assoc(get_primary_message = "used an invalid dictionary value")]
    #[assoc(get_secondary_message = "this value is invalid")]
    #[error("used an invalid dictionary value")]
    InvalidValue { source_span: SourceSpan },
}

impl GetPos for DictionaryTypeError {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            DictionaryTypeError::ListAsPrimitive { source_span }
            | DictionaryTypeError::DictAsPrimitive { source_span }
            | DictionaryTypeError::ListAsDict { source_span }
            | DictionaryTypeError::PrimitiveAsDict { source_span }
            | DictionaryTypeError::DictAsList { source_span }
            | DictionaryTypeError::PrimitiveAsList { source_span }
            | DictionaryTypeError::CannotBeDict { source_span }
            | DictionaryTypeError::InvalidValue { source_span } => source_span,
        }
    }
}

impl GetLintId for DictionaryTypeError {
    fn get_lint_id(&self) -> &'static str {
        self.internal_lint_id()
    }
}

// TODO: Add `use extension` statement
//  - [x] Decide on syntax
//  - [x] CST integration
//  - [x] Parser integration
//  - [x] Codegen integration
// Issue: #86

// TODO: Add extensions
//  - [x] Pen
//  - [ ] Music
//  - [ ] Et cetera
// Issue: #93
