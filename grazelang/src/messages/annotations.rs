use std::{ffi::OsStr, fmt::Write, path::PathBuf};

use annotate_snippets::{Annotation, AnnotationKind, Group, Level, Snippet};

use crate::{
    codegen::core::{GrazeSb3GeneratorCreationError, GrazeSb3GeneratorError},
    lexer::TextSpan,
    messages::types::{
        CLIError, ConstantExprEvaluationError, GetLintId, GrazeDetranspilerError,
        GrazeDetranspilerMessage, GrazeDetranspilerWarning, GrazeSourceInfo, GrazeSourceWarning,
        GrazeSourceWarningKind,
    },
    parser::cst::{GetPos, ParseError},
    utils::string_escape::normal_string_escaper,
    zipper::WriteIntoZipError,
};

use super::types::GrazeSourceMessage;

#[derive(Debug, Clone, PartialEq)]
pub struct Source {
    pub content: String,
    pub path: PathBuf,
    pub line_starts: Vec<usize>,
}

impl Source {
    pub fn as_descriptor(&self) -> SourceDescriptor<'_> {
        SourceDescriptor {
            content: &self.content,
            path: self.path.as_os_str(),
            line_starts: &self.line_starts,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceDescriptor<'a> {
    pub content: &'a str,
    pub path: &'a OsStr,
    pub line_starts: &'a [usize],
}

pub fn annotate_build<'a, I, S, P>(iter: I, mut source_getter: S, mut printer: P)
where
    I: Iterator<Item = &'a GrazeSourceMessage>,
    S: FnMut(u32) -> SourceDescriptor<'a>,
    P: for<'b> FnMut(&'b [Group<'a>], &'b GrazeSourceMessage),
{
    let mut groups = Vec::with_capacity(4);
    iter.for_each(move |value| {
        groups.clear();
        printer(value.annotate(&mut source_getter, &mut groups), value)
    });
}

pub fn annotate_unbuild<'a, I, P>(iter: I, mut printer: P)
where
    I: Iterator<Item = &'a GrazeDetranspilerMessage>,
    P: for<'b> FnMut(&'b [Group<'a>], &'b GrazeDetranspilerMessage),
{
    let mut groups = Vec::with_capacity(4);
    iter.for_each(move |value| {
        groups.clear();
        printer(value.annotate(|_| unreachable!(), &mut groups), value)
    });
}

pub fn convert_source_span(text_span: TextSpan, line_starts: &[usize]) -> std::ops::Range<usize> {
    let (a, b) = text_span;
    let a = if a.0 == 0 { 0 } else { line_starts[a.0 - 1] } + a.1;
    let b = if b.0 == 0 { 0 } else { line_starts[b.0 - 1] } + b.1;
    a..b
}

pub fn annotate_single<'a, 'b>(
    groups: &'b mut Vec<Group<'a>>,
    value: Group<'a>,
) -> &'b [Group<'a>] {
    groups.push(value);
    &*groups
}

pub trait Annotate {
    fn annotate<'a, 'b, F>(
        &'a self,
        source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>;
}

impl Annotate for GrazeSourceMessage {
    fn annotate<'a, 'b, F>(
        &'a self,
        mut source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        match self {
            GrazeSourceMessage::Error(graze_error, _graze_suggestion) => match graze_error {
                // TODO: Implement suggestions
                // Issue: #68
                super::types::GrazeSourceError::Custom(string, source_span) => {
                    let SourceDescriptor {
                        content,
                        path,
                        line_starts,
                    } = source_getter(source_span.1);
                    annotate_single(
                        groups,
                        Level::ERROR
                            .primary_title(string.as_str())
                            .id("custom_error")
                            .element(
                                Snippet::<Annotation>::source(content)
                                    .path(path.to_string_lossy())
                                    .annotation(
                                        AnnotationKind::Primary
                                            .span(convert_source_span(source_span.0, line_starts))
                                            .label(string.as_str()),
                                    ),
                            ),
                    )
                }
                super::types::GrazeSourceError::ParseError(parse_error) => {
                    parse_error.annotate(source_getter, groups)
                }
                super::types::GrazeSourceError::CodegenInitializationError(error) => {
                    error.annotate(source_getter, groups)
                }
                super::types::GrazeSourceError::CodegenError(graze_sb3_generator_error) => {
                    graze_sb3_generator_error.annotate(source_getter, groups)
                }
                super::types::GrazeSourceError::ZipError(error) => {
                    error.annotate(source_getter, groups)
                }
                super::types::GrazeSourceError::CLIError(error) => {
                    error.annotate(source_getter, groups)
                }
            },
            GrazeSourceMessage::Warning(graze_warning, _graze_suggestion) => {
                graze_warning.annotate(source_getter, groups)
            }
            GrazeSourceMessage::Info(graze_info, _graze_suggestion) => {
                graze_info.annotate(source_getter, groups)
            }
            GrazeSourceMessage::Unsuccessful {
                error_count,
                warning_count,
            } => annotate_single(
                groups,
                Group::with_title(Level::ERROR.secondary_title({
                    let error_count = *error_count;
                    let warning_count = *warning_count;
                    let mut error = String::with_capacity(100);
                    if error_count == 0 {
                        write!(error, "could not complete transpilation due to some error",)
                            .unwrap();
                    } else {
                        write!(
                            error,
                            "could not complete transpilation due to {error_count} previous error",
                        )
                        .unwrap();
                        if error_count != 1 {
                            write!(error, "s").unwrap();
                        }
                    }
                    if warning_count > 0 {
                        write!(error, "; {warning_count} warning").unwrap();
                        if warning_count != 1 {
                            write!(error, "s").unwrap();
                        }
                        write!(error, " emitted").unwrap();
                    }
                    error
                })),
            ),
        }
    }
}

impl Annotate for ParseError {
    fn annotate<'a, 'b, F>(
        &'a self,
        mut source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        let (lint_id, secondary_message, source_span) = match self {
            ParseError::InvalidConstantExpression {
                expression: _,
                source,
            } => (
                source.get_lint_id(),
                source.get_secondary_message(),
                *source.get_source_span(),
            ),
            ParseError::DictionaryTypeError { source } => (
                source.get_lint_id(),
                source.get_secondary_message(),
                *source.get_source_span(),
            ),
            _ => (
                self.get_lint_id(),
                self.get_secondary_message(),
                *self.get_source_span(),
            ),
        };
        let SourceDescriptor {
            content,
            path,
            line_starts,
        } = source_getter(source_span.1);
        let main = Level::ERROR
            .primary_title(self.get_primary_message())
            .id(lint_id)
            .element(
                Snippet::source(content)
                    .path(path.to_string_lossy())
                    .annotation(
                        AnnotationKind::Primary
                            .span(convert_source_span(source_span.0, line_starts))
                            .label(secondary_message),
                    ),
            );
        let extra = match self {
            ParseError::InvalidConstantExpression {
                source: ConstantExprEvaluationError::ConstIdentifierUsedSuper { .. },
                ..
            } => Some(Group::with_title(Level::HELP.secondary_title(
                "try using a normalized path to the constant expression symbol instead",
            ))),
            ParseError::InvalidConstantExpression {
                source: ConstantExprEvaluationError::ConstExprListAccess { .. },
                ..
            } => {
                Some(Group::with_title(Level::HELP.secondary_title("maybe you meant to access a letter of the value of the identifier using \"@[\" instead of '['")))
            }
            _ => None,
        };
        groups.push(main);
        if let Some(extra) = extra {
            groups.push(extra);
        }
        &*groups
    }
}

impl Annotate for GrazeSb3GeneratorError {
    fn annotate<'a, 'b, F>(
        &'a self,
        mut source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        if matches!(self, Self::MissingStageDeclaration) {
            return annotate_single(
                groups,
                Group::with_title(
                    Level::ERROR
                        .primary_title(self.get_primary_message())
                        .id(self.get_lint_id()),
                ),
            );
        }
        let (lint_id, secondary_message, source_span) = match self {
            GrazeSb3GeneratorError::InvalidConstantExpression {
                expression: _,
                source,
            } => (
                source.get_lint_id(),
                source.get_secondary_message(),
                *source.get_source_span(),
            ),
            GrazeSb3GeneratorError::DictionaryTypeError { error } => (
                error.get_lint_id(),
                error.get_secondary_message(),
                *error.get_source_span(),
            ),
            _ => (
                self.get_lint_id(),
                self.get_secondary_message(),
                *self.get_source_span(),
            ),
        };
        let SourceDescriptor {
            content,
            path,
            line_starts,
        } = source_getter(source_span.1);
        annotate_single(
            groups,
            Level::ERROR
                .primary_title(self.get_primary_message())
                .id(lint_id)
                .element(
                    Snippet::source(content)
                        .path(path.to_string_lossy())
                        .annotation(
                            AnnotationKind::Primary
                                .span(convert_source_span(source_span.0, line_starts))
                                .label(secondary_message),
                        ),
                ),
        )
    }
}

impl Annotate for GrazeSourceWarning {
    fn annotate<'a, 'b, F>(
        &'a self,
        mut source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        let source_span = *self.get_source_span();
        let SourceDescriptor {
            content,
            path,
            line_starts,
        } = source_getter(source_span.1);
        let main = Level::WARNING
            .primary_title(self.get_primary_message())
            .id(self.get_lint_id())
            .element(
                Snippet::<Annotation>::source(content)
                    .path(path.to_string_lossy())
                    .annotation(
                        AnnotationKind::Primary
                            .span(convert_source_span(source_span.0, line_starts))
                            .label(self.get_secondary_message()),
                    ),
            );
        let extra = if let GrazeSourceWarning::Specific(kind, _) = self {
            match kind {
                GrazeSourceWarningKind::LongListAssignment => {
                    Some(Group::with_title(Level::HELP.secondary_title(
                        "maybe you meant to declare the list with an initial value instead",
                    )))
                }
                _ => None,
            }
        } else {
            None
        };
        groups.push(main);
        if let Some(value) = extra {
            groups.push(value)
        }
        &*groups
    }
}

impl Annotate for GrazeSourceInfo {
    fn annotate<'a, 'b, F>(
        &'a self,
        mut source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        let source_span = *self.get_source_span();
        let SourceDescriptor {
            content,
            path,
            line_starts,
        } = source_getter(source_span.1);
        annotate_single(
            groups,
            Level::WARNING
                .primary_title(self.get_primary_message())
                .id(self.get_lint_id())
                .element(
                    Snippet::<Annotation>::source(content)
                        .path(path.to_string_lossy())
                        .annotation(
                            AnnotationKind::Primary
                                .span(convert_source_span(source_span.0, line_starts))
                                .label(self.get_secondary_message()),
                        ),
                ),
        )
    }
}

impl Annotate for CLIError {
    fn annotate<'a, 'b, F>(
        &'a self,
        _source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        annotate_single(
            groups,
            Group::with_title(
                Level::ERROR
                    .primary_title(self.get_primary_message())
                    .id(self.get_lint_id()),
            ),
        )
    }
}

impl Annotate for GrazeSb3GeneratorCreationError {
    fn annotate<'a, 'b, F>(
        &'a self,
        mut source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        let (lint_id, secondary_message, source_span) = (
            self.get_lint_id(),
            self.get_secondary_message(),
            *self.get_source_span(),
        );
        if let Self::ResourceDirectoryDoesNotExist { .. } = self {
            return annotate_single(
                groups,
                Group::with_title(
                    Level::ERROR
                        .primary_title(self.get_primary_message())
                        .id(self.get_lint_id()),
                ),
            );
        }
        let SourceDescriptor {
            content,
            path,
            line_starts,
        } = source_getter(source_span.1);
        annotate_single(
            groups,
            Level::ERROR
                .primary_title(self.get_primary_message())
                .id(lint_id)
                .element(
                    Snippet::source(content)
                        .path(path.to_string_lossy())
                        .annotation(
                            AnnotationKind::Primary
                                .span(convert_source_span(source_span.0, line_starts))
                                .label(secondary_message),
                        ),
                ),
        )
    }
}

impl Annotate for WriteIntoZipError {
    fn annotate<'a, 'b, F>(
        &'a self,
        _source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        annotate_single(
            groups,
            Group::with_title(
                Level::ERROR
                    .primary_title(self.get_primary_message())
                    .id(self.get_lint_id()),
            ),
        )
    }
}

impl Annotate for GrazeDetranspilerMessage {
    fn annotate<'a, 'b, F>(
        &'a self,
        source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        match self {
            GrazeDetranspilerMessage::Error(error) => error.annotate(source_getter, groups),
            GrazeDetranspilerMessage::Warning(warning) => warning.annotate(source_getter, groups),
            GrazeDetranspilerMessage::Unsuccessful {
                error_count,
                warning_count,
            } => annotate_single(
                groups,
                Group::with_title(Level::ERROR.secondary_title({
                    let error_count = *error_count;
                    let warning_count = *warning_count;
                    let mut error = String::with_capacity(100);
                    if error_count == 0 {
                        write!(error, "could not complete transpilation due to some error",)
                            .unwrap();
                    } else {
                        write!(
                            error,
                            "could not complete transpilation due to {error_count} previous error",
                        )
                        .unwrap();
                        if error_count != 1 {
                            write!(error, "s").unwrap();
                        }
                    }
                    if warning_count > 0 {
                        write!(error, "; {warning_count} warning").unwrap();
                        if warning_count != 1 {
                            write!(error, "s").unwrap();
                        }
                        write!(error, " emitted").unwrap();
                    }
                    error
                })),
            ),
        }
    }
}

impl Annotate for GrazeDetranspilerError {
    fn annotate<'a, 'b, F>(
        &'a self,
        _source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        let value = Group::with_title(
            Level::ERROR
                .primary_title(self.get_primary_message())
                .id(self.get_lint_id()),
        );
        let extra = match self {
            GrazeDetranspilerError::VLBNameIncorrect {
                id: _,
                name: _,
                expected_name,
            } => Some(Group::with_title(Level::HELP.secondary_title(format!(
                "there is, however, a variable with that id and name \"{}\"",
                normal_string_escaper(expected_name)
            )))),
            _ => None,
        };
        groups.push(value);
        if let Some(value) = extra {
            groups.push(value)
        }
        &*groups
    }
}

impl Annotate for GrazeDetranspilerWarning {
    fn annotate<'a, 'b, F>(
        &'a self,
        _source_getter: F,
        groups: &'b mut Vec<Group<'a>>,
    ) -> &'b [Group<'a>]
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        let value = Group::with_title(
            Level::WARNING
                .primary_title(self.get_primary_message())
                .id(self.get_lint_id()),
        );
        let extra = match self {
            GrazeDetranspilerWarning::UnknownExtension { extension } => {
                Some(Group::with_title(Level::HELP.secondary_title(format!(
                    "maybe try creating a markup file named \"{}.toml\" for the extension yourself",
                    normal_string_escaper(extension),
                ))))
            }
            _ => None,
        };
        groups.push(value);
        if let Some(value) = extra {
            groups.push(value)
        }
        &*groups
    }
}
