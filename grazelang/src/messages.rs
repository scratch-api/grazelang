use arcstr::ArcStr as IString;
use serde::{Deserialize, Serialize};

use crate::{
    codegen::core::GrazeSb3GeneratorError,
    lexer::SourceSpan,
    parser::cst::{GetPos, ParseError},
};

pub trait GetLintId {
    fn get_lint_id(&self) -> &'static str;
}

// enum_assoc

#[derive(Debug, Clone, thiserror::Error, enum_assoc::Assoc)]
pub enum GrazeError {
    #[error("{0}")]
    Plain(IString, SourceSpan),
    #[error(transparent)]
    ParseError(#[from] ParseError),
    #[error(transparent)]
    CodegenError(#[from] GrazeSb3GeneratorError),
}

impl GetPos for GrazeError {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            GrazeError::Plain(_, source_span) => source_span,
            GrazeError::ParseError(error) => error.get_source_span(),
            GrazeError::CodegenError(error) => error.get_source_span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum GrazeWarning {
    Plain(IString, SourceSpan),
    Specific(GrazeWarningKind, IString, SourceSpan),
}

impl GetLintId for GrazeWarning {
    fn get_lint_id(&self) -> &'static str {
        match self {
            GrazeWarning::Plain(_, _) => "plain_warning",
            GrazeWarning::Specific(warning_kind, _, _) => warning_kind.get_lint_id(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, enum_assoc::Assoc)]
#[func(const fn internal_lint_id(&self) -> &'static str)]
pub enum GrazeWarningKind {
    #[assoc(internal_lint_id = "callable_as_input")]
    CallableAsInput,
    #[assoc(internal_lint_id = "block_ref_as_field")]
    BlockRefAsField,
    #[assoc(internal_lint_id = "callable_as_field")]
    CallableAsField,
    #[assoc(internal_lint_id = "empty_expression_as_field")]
    EmptyExpressionAsField,
    #[assoc(internal_lint_id = "non_field_singleton_as_field")]
    NonFieldSingletonAsField,
    #[assoc(internal_lint_id = "literal_field_value_incorrect")]
    LiteralFieldValueIncorrect,
}

impl GetLintId for GrazeWarningKind {
    #[inline]
    fn get_lint_id(&self) -> &'static str {
        self.internal_lint_id()
    }
}

#[derive(Debug, Clone)]
pub enum GrazeInfo {
    Plain(IString, SourceSpan),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum GrazeSuggestion {
    SimpleCodeChange {
        replace_source_span: SourceSpan,
        replace_text: String,
    },
}

#[derive(Debug, Clone)]
pub enum GrazeMessage {
    Error(GrazeError, Option<GrazeSuggestion>),
    Warning(GrazeWarning, Option<GrazeSuggestion>),
    Info(GrazeInfo, Option<GrazeSuggestion>),
}

impl From<ParseError> for GrazeMessage {
    fn from(value: ParseError) -> Self {
        Self::Error(value.into(), None)
    }
}
