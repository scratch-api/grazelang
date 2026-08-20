use std::borrow::Cow;

use arcstr::ArcStr as IString;
use serde::{Deserialize, Serialize};

use crate::{
    codegen::core::{GrazeSb3GeneratorCreationError, GrazeSb3GeneratorError},
    eval::call::ConstantExprValue,
    lexer::SourceSpan,
    parser::cst::{Expression, GetPos, Identifier, ParseError},
    zipper::WriteIntoZipError,
};

pub trait GetLintId {
    fn get_lint_id(&self) -> &'static str;
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum GrazeSourceError {
    #[error("{0}")]
    Custom(IString, SourceSpan),
    #[error(transparent)]
    ParseError(#[from] ParseError),
    #[error(transparent)]
    CodegenInitializationError(#[from] GrazeSb3GeneratorCreationError),
    #[error(transparent)]
    CodegenError(#[from] GrazeSb3GeneratorError),
    #[error(transparent)]
    ZipError(#[from] WriteIntoZipError),
    #[error(transparent)]
    CLIError(#[from] CLIError),
}

const EMPTY_SOURCE_SPAN: &SourceSpan = &(((0, 0), (0, 0)), 0);

impl GetPos for GrazeSourceError {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            GrazeSourceError::Custom(_, source_span) => source_span,
            GrazeSourceError::ParseError(error) => error.get_source_span(),
            GrazeSourceError::CodegenInitializationError(error) => error.get_source_span(),
            GrazeSourceError::CodegenError(error) => error.get_source_span(),
            GrazeSourceError::CLIError(_) | GrazeSourceError::ZipError(_) => EMPTY_SOURCE_SPAN,
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
    ConstValueNotCallable { identifier: Box<Identifier> },
    #[assoc(internal_lint_id = "const_namespace_not_callable")]
    #[assoc(get_secondary_message = "not callable")]
    #[error(
        "the identifier {identifier:?} is not a constant expression function, is instead a constant expression namespace"
    )]
    ConstNamespaceNotCallable { identifier: Box<Identifier> },
    #[assoc(internal_lint_id = "const_function_not_singleton")]
    #[assoc(get_secondary_message = "must be called")]
    #[error("the identifier {identifier:?} is not a singleton constant expression function")]
    ConstFunctionNotSingleton { identifier: Box<Identifier> },
    #[assoc(internal_lint_id = "const_value_not_js_primitve")]
    #[assoc(get_secondary_message = "can only be used in a compatible const function")]
    #[error(
        "the identifier {identifier:?} is not a singleton constant expression function, is instead a constant expression value"
    )]
    ConstValueNotJsPrimitive { identifier: Box<Identifier> },
    #[assoc(internal_lint_id = "const_namespace_not_js_primitive")]
    #[assoc(get_secondary_message = "does not contain a value, only its children might")]
    #[error(
        "the identifier {identifier:?} is not a singleton constant expression function, is instead a constant expression namespace"
    )]
    ConstNamespaceNotJsPrimitive { identifier: Box<Identifier> },
    #[assoc(internal_lint_id = "const_function_not_value")]
    #[assoc(get_secondary_message = "must be called")]
    #[error(
        "the identifier {identifier:?} is not a constant expression value, is instead a constant expression function"
    )]
    ConstFunctionNotValue { identifier: Box<Identifier> },
    #[assoc(internal_lint_id = "const_namespace_not_value")]
    #[assoc(get_secondary_message = "does not contain a value, only its children might")]
    #[error(
        "the identifier {identifier:?} is not a constant expression value, is instead a constant expression namespace"
    )]
    ConstNamespaceNotValue { identifier: Box<Identifier> },
    #[assoc(internal_lint_id = "const_identifier_does_not_exist")]
    #[assoc(get_secondary_message = "identifier does not exist as a constant expression symbol")]
    #[error("the identifier {identifier:?} does not exist as a constant expression symbol")]
    ConstIdentifierDoesNotExist { identifier: Box<Identifier> },
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
    ConstIdentifierUsedSuper { identifier: Box<Identifier> },
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
        "tried to access a list item in constant expression {expression:?}, which is not possible, maybe you meant to access a letter of the value of the identifier using \"@[\" instead of '['"
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

#[derive(Debug, Clone, PartialEq, enum_assoc::Assoc)]
pub enum GrazeSourceWarning {
    Custom {
        primary_message: IString,
        secondary_message: Option<IString>,
        source_span: SourceSpan,
    },
    Specific(GrazeWarningKind, SourceSpan),
}

impl GetLintId for GrazeSourceWarning {
    fn get_lint_id(&self) -> &'static str {
        match self {
            GrazeSourceWarning::Custom { .. } => "custom_warning",
            GrazeSourceWarning::Specific(warning_kind, _) => warning_kind.get_lint_id(),
        }
    }
}

impl GrazeSourceWarning {
    pub fn get_primary_message(&self) -> &str {
        match self {
            GrazeSourceWarning::Custom {
                primary_message, ..
            } => primary_message.as_str(),
            GrazeSourceWarning::Specific(graze_warning, _) => graze_warning.get_primary_message(),
        }
    }

    pub fn get_secondary_message(&self) -> &str {
        match self {
            GrazeSourceWarning::Custom {
                primary_message,
                secondary_message,
                ..
            } => secondary_message
                .as_ref()
                .unwrap_or(primary_message)
                .as_str(),
            GrazeSourceWarning::Specific(graze_warning, _) => graze_warning.get_secondary_message(),
        }
    }
}

impl GetPos for GrazeSourceWarning {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            GrazeSourceWarning::Custom { source_span: p, .. }
            | GrazeSourceWarning::Specific(_, p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, enum_assoc::Assoc)]
#[func(const fn internal_lint_id(&self) -> &'static str)]
#[func(pub const fn get_primary_message(&self) -> &'static str)]
#[func(pub const fn get_secondary_message(&self) -> &'static str)]
pub enum GrazeWarningKind {
    #[assoc(get_primary_message = "uncalled callable")]
    #[assoc(get_secondary_message = "should be called")]
    #[assoc(internal_lint_id = "callable_as_input")]
    CallableAsInput,
    #[assoc(get_primary_message = "reporter expression does not contain a field value")]
    #[assoc(get_secondary_message = "should be a field value")]
    #[assoc(internal_lint_id = "block_ref_as_field")]
    BlockRefAsField,
    #[assoc(get_primary_message = "uncalled callable does not contain a field value")]
    #[assoc(get_secondary_message = "should be a field value")]
    #[assoc(internal_lint_id = "callable_as_field")]
    CallableAsField,
    #[assoc(get_primary_message = "empty expression is likely not a valid field value")]
    #[assoc(get_secondary_message = "field value should be specified")]
    #[assoc(internal_lint_id = "empty_expression_as_field")]
    EmptyExpressionAsField,
    #[assoc(get_primary_message = "reporter expression does not contain a field value")]
    #[assoc(get_secondary_message = "should be a field value")]
    #[assoc(internal_lint_id = "non_field_singleton_as_field")]
    NonFieldSingletonAsField,
    #[assoc(get_primary_message = "incorrect value for field, try not specifying fields by value")]
    #[assoc(get_secondary_message = "field value should not be specified by value")]
    #[assoc(internal_lint_id = "literal_field_value_incorrect")]
    LiteralFieldValueIncorrect,
    #[assoc(get_primary_message = "incorrect value for field")]
    #[assoc(get_secondary_message = "field value must fit the context")]
    #[assoc(internal_lint_id = "field_value_incorrect")]
    FieldValueIncorrect,
    #[assoc(get_primary_message = "cannot create an isolated shadow expression")]
    #[assoc(get_secondary_message = "should not be isolated")]
    #[assoc(internal_lint_id = "top_level_shadow_expression")]
    TopLevelShadowExpression,
    #[assoc(get_primary_message = "configured a target multiple times")]
    #[assoc(get_secondary_message = "repeated target config here")]
    #[assoc(internal_lint_id = "repeated_target_config")]
    RepeatedTargetConfig,
    #[assoc(get_primary_message = "invalid value for rotation style")]
    #[assoc(get_secondary_message = "must be \"all around\", \"left-right\" or \"don't rotate\"")]
    #[assoc(internal_lint_id = "invalid_rotation_style_value")]
    InvalidRotationStyleValue,
    #[assoc(get_primary_message = "invalid value for video state")]
    #[assoc(get_secondary_message = "must be \"on\", \"off\" or \"on-flipped\"")]
    #[assoc(internal_lint_id = "invalid_video_state_value")]
    InvalidVideoStateValue,
    #[assoc(get_primary_message = "monitor value supplies inputs")]
    #[assoc(
        get_secondary_message = "corresponding block would have inputs, but only fields can be supplied in monitors"
    )]
    #[assoc(internal_lint_id = "monitor_value_has_inputs")]
    MonitorValueHasInputs,
    #[assoc(get_primary_message = "specified monitor position partially")]
    #[assoc(
        get_secondary_message = "partially specified monitor position has the same effect as unspecified monitor position"
    )]
    #[assoc(internal_lint_id = "specified_monitor_position_partially")]
    SpecifiedMonitorPositionPartially,
    #[assoc(get_primary_message = "primitive value used for list monitor")]
    #[assoc(get_secondary_message = "expected list value")]
    #[assoc(internal_lint_id = "primitive_value_for_list_monitor")]
    PrimitiveValueForListMonitor,
    #[assoc(get_primary_message = "list value used for primitive monitor")]
    #[assoc(get_secondary_message = "expected primitive value")]
    #[assoc(internal_lint_id = "list_value_for_primitive_monitor")]
    ListValueForPrimitiveMonitor,
    #[assoc(get_primary_message = "value used for a number does not look like a number")]
    #[assoc(get_secondary_message = "should be a number")]
    #[assoc(internal_lint_id = "unexpected_value_for_number")]
    UnexpectedValueForNumber,
    #[assoc(get_primary_message = "value used for a boolean does not look like a boolean")]
    #[assoc(get_secondary_message = "should be a boolean")]
    #[assoc(internal_lint_id = "unexpected_value_for_boolean")]
    UnexpectedValueForBoolean,
    #[assoc(get_primary_message = "costume number outside valid range")]
    #[assoc(
        get_secondary_message = "needs to be an integer greater than zero and less than or equal to the number of costumes"
    )]
    #[assoc(internal_lint_id = "invalid_costume_number")]
    InvalidCostumeNumber,
    #[assoc(get_primary_message = "invalid monitor mode")]
    #[assoc(get_secondary_message = "must be \"default\", \"large\", \"slider\" or \"list\"")]
    #[assoc(internal_lint_id = "invalid_monitor_mode")]
    InvalidMonitorMode,
    #[assoc(
        get_primary_message = "the stage and all sprites must have at least one costume or backdrop"
    )]
    #[assoc(get_secondary_message = "missing a costume or backdrop")]
    #[assoc(internal_lint_id = "target_without_costume")]
    TargetWithoutCostume,
    #[assoc(
        get_primary_message = "tried to assign to property of another target, can only assign to own version of it"
    )]
    #[assoc(get_secondary_message = "property belongs to another target")]
    #[assoc(internal_lint_id = "assign_property_of_other_target")]
    AssignPropertyOfOtherTarget,
    #[assoc(get_primary_message = "used conflicting canonical names")]
    #[assoc(get_secondary_message = "repeated canonical name here")]
    #[assoc(internal_lint_id = "repeated_canonical_name")]
    RepeatedCanonicalName,
    #[assoc(
        get_primary_message = "assigning a lot of items to a list might not be that efficient, maybe you meant to declare the list with an initial value instead"
    )]
    #[assoc(get_secondary_message = "list assignment has a lot of items")]
    #[assoc(internal_lint_id = "long_list_assignment")]
    LongListAssignment,
    #[assoc(get_primary_message = "used a stack block where a reporter block was expected")]
    #[assoc(get_secondary_message = "should be a reporter")]
    #[assoc(internal_lint_id = "stack_block_as_reporter")]
    StackBlockAsReporter,
    // TODO: Use `GrazeWarningKind::StackBlockAsReporter`
    // Issue: #111
}

pub const LONG_LIST_ASSIGNMENT_MININUM_LENGTH: usize = 16;

impl GetLintId for GrazeWarningKind {
    #[inline]
    fn get_lint_id(&self) -> &'static str {
        self.internal_lint_id()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GrazeSourceInfo {
    Custom {
        primary_message: IString,
        secondary_message: Option<IString>,
        source_span: SourceSpan,
    },
}

impl GetLintId for GrazeSourceInfo {
    fn get_lint_id(&self) -> &'static str {
        match self {
            GrazeSourceInfo::Custom { .. } => "custom_info",
        }
    }
}

impl GrazeSourceInfo {
    pub fn get_primary_message(&self) -> &str {
        match self {
            GrazeSourceInfo::Custom {
                primary_message, ..
            } => primary_message.as_str(),
        }
    }

    pub fn get_secondary_message(&self) -> &str {
        match self {
            GrazeSourceInfo::Custom {
                primary_message,
                secondary_message,
                ..
            } => secondary_message
                .as_ref()
                .unwrap_or(primary_message)
                .as_str(),
        }
    }
}

impl GetPos for GrazeSourceInfo {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            GrazeSourceInfo::Custom { source_span: p, .. } => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum GrazeSuggestion {
    SimpleCodeChange {
        replace_source_span: SourceSpan,
        replace_text: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum GrazeSourceMessage {
    Error(GrazeSourceError, Option<GrazeSuggestion>),
    Warning(GrazeSourceWarning, Option<GrazeSuggestion>),
    Info(GrazeSourceInfo, Option<GrazeSuggestion>),
    Unsuccessful {
        error_count: usize,
        warning_count: usize,
    },
}

#[cfg(feature = "detranspiler")]
#[derive(Debug, Clone, PartialEq)]
pub enum GrazeDetranspilerMessage {
    Error(GrazeDetranspilerError),
    Warning(GrazeDetranspilerWarning),
    Unsuccessful {
        error_count: usize,
        warning_count: usize,
    },
}

impl From<ParseError> for GrazeSourceMessage {
    fn from(value: ParseError) -> Self {
        Self::Error(value.into(), None)
    }
}

impl From<GrazeSb3GeneratorError> for GrazeSourceMessage {
    fn from(value: GrazeSb3GeneratorError) -> Self {
        Self::Error(value.into(), None)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, thiserror::Error, enum_assoc::Assoc)]
#[func(const fn internal_lint_id(&self) -> &'static str)]
#[func(pub const fn get_primary_message(&self) -> &'static str)]
pub enum CLIError {
    #[assoc(internal_lint_id = "path_does_not_exist")]
    #[assoc(get_primary_message = "path does not exist")]
    #[error("path does not exist")]
    PathDoesNotExist,
    #[assoc(internal_lint_id = "path_neither_file_nor_directory")]
    #[assoc(get_primary_message = "path is neither a file nor a directory")]
    #[error("path is neither a file nor a directory")]
    PathNeitherFileNorDirectory,
}

impl GetLintId for CLIError {
    fn get_lint_id(&self) -> &'static str {
        self.internal_lint_id()
    }
}

impl From<CLIError> for GrazeSourceMessage {
    fn from(value: CLIError) -> Self {
        Self::Error(value.into(), None)
    }
}

impl From<GrazeSb3GeneratorCreationError> for GrazeSourceMessage {
    fn from(value: GrazeSb3GeneratorCreationError) -> Self {
        Self::Error(value.into(), None)
    }
}

impl From<WriteIntoZipError> for GrazeSourceMessage {
    fn from(value: WriteIntoZipError) -> Self {
        Self::Error(value.into(), None)
    }
}

#[cfg(feature = "detranspiler")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, enum_assoc::Assoc)]
#[func(const fn internal_lint_id(&self) -> &'static str)]
pub enum GrazeDetranspilerError {
    #[assoc(internal_lint_id = "unknown_opcode")]
    UnknownOpcode { opcode: String },
    #[assoc(internal_lint_id = "unknown_variable")]
    UnknownVariable { id: String, name: String },
    #[assoc(internal_lint_id = "unknown_list")]
    UnknownList { id: String, name: String },
    #[assoc(internal_lint_id = "unknown_broadcast")]
    UnknownBroadcast { id: String, name: String },
    #[assoc(internal_lint_id = "invalid_block_reference")]
    InvalidBlockReference { block_id: String },
    #[assoc(internal_lint_id = "primitive_block_as_substack")]
    PrimitiveBlockAsSubstack {
        block_id: String,
        input_name: String,
    },
    #[assoc(internal_lint_id = "primitive_block_as_stack_block")]
    PrimitiveBlockAsStackBlock { block_id: String },
    #[assoc(internal_lint_id = "substack_in_reporter")]
    SubstackInReporter {
        block_id: String,
        input_name: String,
    },
    #[assoc(internal_lint_id = "substack_in_hat_block")]
    SubstackInHatBlock {
        block_id: String,
        input_name: String,
    },
    #[assoc(internal_lint_id = "too_many_substacks")]
    TooManySubstacks {
        block_id: String,
        input_name: String,
    },
    #[assoc(internal_lint_id = "vlb_name_incorrect")]
    VLBNameIncorrect {
        id: String,
        name: String,
        expected_name: String,
    },
}

#[cfg(feature = "detranspiler")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, enum_assoc::Assoc)]
#[func(const fn internal_lint_id(&self) -> &'static str)]
pub enum GrazeDetranspilerWarning {
    #[assoc(internal_lint_id = "unused_field")]
    UnusedField { field: String, block_id: String },
    #[assoc(internal_lint_id = "unused_input")]
    UnusedInput { input: String, block_id: String },
    #[assoc(internal_lint_id = "unexpected_empty_input")]
    UnexpectedEmptyInput { input: String, block_id: String },
    #[assoc(internal_lint_id = "missing_menu_field")]
    MissingMenuField { field: String, block_id: String },
}

#[cfg(feature = "detranspiler")]
impl From<GrazeDetranspilerError> for GrazeDetranspilerMessage {
    fn from(value: GrazeDetranspilerError) -> Self {
        Self::Error(value)
    }
}

#[cfg(feature = "detranspiler")]
impl From<GrazeDetranspilerWarning> for GrazeDetranspilerMessage {
    fn from(value: GrazeDetranspilerWarning) -> Self {
        Self::Warning(value)
    }
}
