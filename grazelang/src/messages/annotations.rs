use std::{
    ffi::OsStr,
    fmt::{Write, format},
    path::PathBuf,
};

use annotate_snippets::{Annotation, AnnotationKind, Group, Level, Renderer, Snippet};

use crate::{
    codegen::core::GrazeSb3GeneratorError,
    lexer::TextSpan,
    messages::types::GetLintId,
    parser::cst::{GetPos, ParseError},
};

use super::types::GrazeMessage;

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

pub fn annotate<'a, I, F>(iter: I, renderer: &Renderer, mut source_getter: F) -> Vec<Group<'a>>
where
    I: Iterator<Item = &'a GrazeMessage>,
    F: FnMut(u32) -> SourceDescriptor<'a>,
{
    iter.map(|value| value.annotate(&mut source_getter))
        .collect()
}

pub fn convert_source_span(text_span: TextSpan, line_starts: &[usize]) -> std::ops::Range<usize> {
    let (a, b) = text_span;
    let a = if a.0 == 0 { 0 } else { line_starts[a.0 - 1] } + a.1;
    let b = if b.0 == 0 { 0 } else { line_starts[b.0 - 1] } + b.1;
    a..b
}

impl GrazeMessage {
    pub fn annotate<'a, F>(&'a self, mut source_getter: F) -> Group<'a>
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        match self {
            GrazeMessage::Error(graze_error, _graze_suggestion) => match graze_error {
                // TODO: Implement suggestions
                // Issue: #68
                super::types::GrazeError::Plain(string, source_span) => {
                    let SourceDescriptor {
                        content,
                        path,
                        line_starts,
                    } = source_getter(source_span.1);
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
                        )
                }
                super::types::GrazeError::ParseError(parse_error) => {
                    parse_error.annotate(source_getter)
                }
                super::types::GrazeError::CodegenError(graze_sb3_generator_error) => {
                    graze_sb3_generator_error.annotate(source_getter)
                }
            },
            GrazeMessage::Warning(graze_warning, graze_suggestion) => todo!(),
            GrazeMessage::Info(graze_info, graze_suggestion) => todo!(),
            GrazeMessage::Unsuccessful {
                error_count,
                warning_count,
            } => Group::with_title(Level::ERROR.secondary_title({
                let error_count = *error_count;
                let warning_count = *warning_count;
                let mut error = String::with_capacity(100);
                write!(
                    error,
                    "could not complete transpilation due to {error_count} previous error",
                )
                .unwrap();
                if error_count != 1 {
                    write!(error, "s").unwrap();
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
        }
    }
}

impl ParseError {
    pub fn annotate<'a, F>(&'a self, mut source_getter: F) -> Group<'a>
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        let (lint_id, secondary_message, source_span) =
            if let ParseError::InvalidConstantExpression {
                expression: _,
                source,
            } = self
            {
                (
                    source.get_lint_id(),
                    source.get_secondary_message(),
                    *source.get_source_span(),
                )
            } else {
                (
                    self.get_lint_id(),
                    self.get_secondary_message(),
                    *self.get_source_span(),
                )
            };
        let SourceDescriptor {
            content,
            path,
            line_starts,
        } = source_getter(source_span.1);
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
            )
    }
}

impl GrazeSb3GeneratorError {
    pub fn annotate<'a, F>(&'a self, mut _source_getter: F) -> Group<'a>
    where
        F: FnMut(u32) -> SourceDescriptor<'a>,
    {
        todo!()
    }
}
