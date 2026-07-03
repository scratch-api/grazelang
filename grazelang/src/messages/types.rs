use arcstr::ArcStr as IString;
use serde::{Deserialize, Serialize};

use crate::{
    codegen::core::GrazeSb3GeneratorError,
    eval::call::ConstantExprValue,
    lexer::SourceSpan,
    parser::cst::{Expression, GetPos, Identifier, ParseError},
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

#[derive(Debug, Clone, thiserror::Error, PartialEq, Serialize, Deserialize)]
pub enum ConstantExprEvaluationError {
    #[error(
        "the identifier {identifier:?} is not a constant expression function, is instead a constant expression value"
    )]
    NotConstFunctionButValue { identifier: Identifier },
    #[error(
        "the identifier {identifier:?} is not a constant expression function, is instead a constant expression namespace"
    )]
    NotConstFunctionButNamespace { identifier: Identifier },
    #[error("the identifier {identifier:?} is not a singleton constant expression function")]
    NotSingletonConstFunction { identifier: Identifier },
    #[error(
        "the identifier {identifier:?} is not a singleton constant expression function, is instead a constant expression value"
    )]
    NotSingletonConstFunctionButValue { identifier: Identifier },
    #[error(
        "the identifier {identifier:?} is not a singleton constant expression function, is instead a constant expression namespace"
    )]
    NotSingletonConstFunctionButNamespace { identifier: Identifier },
    #[error(
        "the identifier {identifier:?} is not a constant expression value, is instead a constant expression function"
    )]
    NotConstValueButFunction { identifier: Identifier },
    #[error(
        "the identifier {identifier:?} is not a constant expression value, is instead a constant expression namespace"
    )]
    NotConstValueButNamespace { identifier: Identifier },
    #[error("the identifier {identifier:?} does not exist as a constant expression symbol")]
    ConstIdentifierDoesNotExist { identifier: Identifier },
    #[error(
        "the const expect value {value:?} does not correspond to a constant expression symbol that can be used as a math op"
    )]
    IncorrectConstExprValueForMathOp {
        value: ConstantExprValue,
        source_span: SourceSpan,
    },
    #[error(
        "the identifier {identifier:?} contains \"super\", which is not allowed, maybe try a normalized path to the constant expression symbol"
    )]
    ConstIdentifierUsedSupper { identifier: Identifier },
    #[error(
        "the amount of parameters for this constant expression function was {unexpected:?} at {source_span:?}, expected {expected:?}"
    )]
    IncorrectParamCount {
        unexpected: usize,
        expected: usize,
        source_span: SourceSpan,
    },
    #[error(
        "tried to access a list item of identifier {identifier:?} in a constant expression, which is not possible, \
        maybe you meant to access a letter of the value of the identifier using \"@[\" instead of '['"
    )]
    ConstExprListAccess { identifier: Identifier },
    #[error(
        "tried to use {expression:?} in a constant expression when an identifier was expected in context"
    )]
    ExpectedIdentifier { expression: Box<Expression> },
}

impl GetPos for ConstantExprEvaluationError {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            Self::NotConstFunctionButValue { identifier }
            | Self::NotConstFunctionButNamespace { identifier }
            | Self::NotConstValueButFunction { identifier }
            | Self::NotConstValueButNamespace { identifier }
            | Self::NotSingletonConstFunction { identifier }
            | Self::NotSingletonConstFunctionButNamespace { identifier }
            | Self::NotSingletonConstFunctionButValue { identifier }
            | Self::ConstIdentifierDoesNotExist { identifier }
            | Self::ConstExprListAccess { identifier }
            | Self::ConstIdentifierUsedSupper { identifier } => identifier.get_source_span(),
            Self::IncorrectParamCount {
                unexpected: _,
                expected: _,
                source_span,
            }
            | Self::IncorrectConstExprValueForMathOp {
                value: _,
                source_span,
            } => source_span,
            Self::ExpectedIdentifier { expression } => expression.get_source_span(),
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
    #[assoc(internal_lint_id = "top_level_shadow_expression")]
    TopLevelShadowExpression,
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
