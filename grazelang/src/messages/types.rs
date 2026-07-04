use std::borrow::Cow;

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

#[derive(Debug, Clone, PartialEq, thiserror::Error, enum_assoc::Assoc)]
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

#[derive(Debug, Clone, thiserror::Error, PartialEq, Serialize, Deserialize, enum_assoc::Assoc)]
#[func(const fn internal_lint_id(&self) -> &'static str)]
#[func(pub const fn get_secondary_message(&self) -> &'static str)]
pub enum ConstantExprEvaluationError {
    #[assoc(internal_lint_id = "const_value_not_callable")]
    #[assoc(get_secondary_message = "not callable")]
    #[error(
        "the identifier {identifier:?} is not a constant expression function, is instead a constant expression value"
    )]
    ConstValueNotCallable { identifier: Identifier },
    #[assoc(internal_lint_id = "const_namespace_not_callable")]
    #[assoc(get_secondary_message = "not callable")]
    #[error(
        "the identifier {identifier:?} is not a constant expression function, is instead a constant expression namespace"
    )]
    ConstNamespaceNotCallable { identifier: Identifier },
    #[assoc(internal_lint_id = "const_function_not_singleton")]
    #[assoc(get_secondary_message = "must be called")]
    #[error("the identifier {identifier:?} is not a singleton constant expression function")]
    ConstFunctionNotSingleton { identifier: Identifier },
    #[assoc(internal_lint_id = "const_value_not_js_primitve")]
    #[assoc(get_secondary_message = "can only be used in a compatible const function")]
    #[error(
        "the identifier {identifier:?} is not a singleton constant expression function, is instead a constant expression value"
    )]
    ConstValueNotJsPrimitive { identifier: Identifier },
    #[assoc(internal_lint_id = "const_namespace_not_js_primitive")]
    #[assoc(get_secondary_message = "does not contain a value, only its children might")]
    #[error(
        "the identifier {identifier:?} is not a singleton constant expression function, is instead a constant expression namespace"
    )]
    ConstNamespaceNotJsPrimitive { identifier: Identifier },
    #[assoc(internal_lint_id = "const_function_not_value")]
    #[assoc(get_secondary_message = "must be called")]
    #[error(
        "the identifier {identifier:?} is not a constant expression value, is instead a constant expression function"
    )]
    ConstFunctionNotValue { identifier: Identifier },
    #[assoc(internal_lint_id = "const_namespace_not_value")]
    #[assoc(get_secondary_message = "does not contain a value, only its children might")]
    #[error(
        "the identifier {identifier:?} is not a constant expression value, is instead a constant expression namespace"
    )]
    ConstNamespaceNotValue { identifier: Identifier },
    #[assoc(internal_lint_id = "const_identifier_does_not_exist")]
    #[assoc(get_secondary_message = "identifier does not exist as a constant expression symbol")]
    #[error("the identifier {identifier:?} does not exist as a constant expression symbol")]
    ConstIdentifierDoesNotExist { identifier: Identifier },
    #[assoc(internal_lint_id = "incorrect_const_expr_value_for_math_op")]
    #[assoc(get_secondary_message = "cannot use this as a math op")]
    #[error(
        "the const expect value {value:?} does not correspond to a constant expression symbol that can be used as a math op"
    )]
    IncorrectConstExprValueForMathOp {
        value: ConstantExprValue,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "const_identifier_used_super")]
    #[assoc(get_secondary_message = "\"super\" cannot be used here")]
    #[error(
        "the identifier {identifier:?} contains \"super\", which is not allowed, maybe try a normalized path to the constant expression symbol"
    )]
    ConstIdentifierUsedSuper { identifier: Identifier },
    #[assoc(internal_lint_id = "incorrect_param_count")]
    #[assoc(get_secondary_message = "incorrect parameter count")]
    #[error(
        "the amount of parameters for this constant expression function was {unexpected:?} at {source_span:?}, expected {expected:?}"
    )]
    IncorrectParamCount {
        unexpected: usize,
        expected: usize,
        source_span: SourceSpan,
    },
    #[assoc(internal_lint_id = "const_expr_list_access")]
    #[assoc(get_secondary_message = "cannot access a list item in a constant expression")]
    #[error(
        "tried to access a list item in constant expression {expression:?}, which is not possible, \
        maybe you meant to access a letter of the value of the identifier using \"@[\" instead of '['"
    )]
    ConstExprListAccess { expression: Box<Expression> },
    #[assoc(internal_lint_id = "expected_identifier")]
    #[assoc(get_secondary_message = "expected identifier")]
    #[error(
        "tried to use {expression:?} in a constant expression when an identifier was expected in context"
    )]
    ExpectedIdentifier { expression: Box<Expression> },
}

impl GetLintId for ConstantExprEvaluationError {
    #[inline]
    fn get_lint_id(&self) -> &'static str {
        self.internal_lint_id()
    }
}

impl ConstantExprEvaluationError {
    pub fn get_primary_message(&self) -> Cow<'static, str> {
        Cow::Owned(match self {
            ConstantExprEvaluationError::ConstValueNotCallable { identifier } => {
                format!(
                    "expected const expression callable, got const expression value `{identifier}`"
                )
            }
            ConstantExprEvaluationError::ConstNamespaceNotCallable { identifier } => {
                format!(
                    "expected const expression callable, got const expression namespace `{identifier}`"
                )
            }
            ConstantExprEvaluationError::ConstFunctionNotSingleton { identifier } => {
                format!(
                    "expected const expression evaluating to a js primitive, got const expression function `{identifier}`"
                )
            }
            ConstantExprEvaluationError::ConstValueNotJsPrimitive { identifier } => {
                format!(
                    "expected const expression evaluating to a js primitive, got const expression value `{identifier}`"
                )
            }
            ConstantExprEvaluationError::ConstNamespaceNotJsPrimitive { identifier } => {
                format!(
                    "expected const expression evaluating to a js primitive, got const expression namespace `{identifier}`"
                )
            }
            ConstantExprEvaluationError::ConstFunctionNotValue { identifier } => {
                format!(
                    "expected const expression containing a special value, got const expression function `{identifier}`"
                )
            }
            ConstantExprEvaluationError::ConstNamespaceNotValue { identifier } => {
                format!(
                    "expected const expression containing a special value, got const expression namespace `{identifier}`"
                )
            }
            ConstantExprEvaluationError::ConstIdentifierDoesNotExist { identifier } => {
                format!(
                    "expected const expression containing a special value, got const expression function `{identifier}`"
                )
            }
            ConstantExprEvaluationError::IncorrectConstExprValueForMathOp {
                value,
                source_span: _,
            } => {
                format!(
                    "expected const expression containing a math op, got const expression value `{value}`"
                )
            }
            ConstantExprEvaluationError::ConstIdentifierUsedSuper { identifier } => {
                format!("cannot use \"super\" in const expression identifier `{identifier}`")
            }
            ConstantExprEvaluationError::IncorrectParamCount {
                unexpected,
                expected,
                source_span: _,
            } => {
                format!("expected {expected} parameters, got {unexpected}")
            }
            ConstantExprEvaluationError::ConstExprListAccess { expression: _ } => {
                return Cow::Borrowed("cannot access list content in constant expressions");
            }
            ConstantExprEvaluationError::ExpectedIdentifier { expression: _ } => {
                return Cow::Borrowed(
                    "expected an identifier as parameter for a constant expression function",
                );
            }
        })
    }
}

impl GetPos for ConstantExprEvaluationError {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            Self::ConstValueNotCallable { identifier }
            | Self::ConstNamespaceNotCallable { identifier }
            | Self::ConstFunctionNotValue { identifier }
            | Self::ConstNamespaceNotValue { identifier }
            | Self::ConstFunctionNotSingleton { identifier }
            | Self::ConstNamespaceNotJsPrimitive { identifier }
            | Self::ConstValueNotJsPrimitive { identifier }
            | Self::ConstIdentifierDoesNotExist { identifier }
            | Self::ConstIdentifierUsedSuper { identifier } => identifier.get_source_span(),
            Self::IncorrectParamCount {
                unexpected: _,
                expected: _,
                source_span,
            }
            | Self::IncorrectConstExprValueForMathOp {
                value: _,
                source_span,
            } => source_span,
            Self::ExpectedIdentifier { expression } | Self::ConstExprListAccess { expression } => {
                expression.get_source_span()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum GrazeMessage {
    Error(GrazeError, Option<GrazeSuggestion>),
    Warning(GrazeWarning, Option<GrazeSuggestion>),
    Info(GrazeInfo, Option<GrazeSuggestion>),
    Unsuccessful {
        error_count: usize,
        warning_count: usize,
    },
}

impl From<ParseError> for GrazeMessage {
    fn from(value: ParseError) -> Self {
        Self::Error(value.into(), None)
    }
}

impl From<GrazeSb3GeneratorError> for GrazeMessage {
    fn from(value: GrazeSb3GeneratorError) -> Self {
        Self::Error(value.into(), None)
    }
}
