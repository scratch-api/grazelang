use std::path::Path;

use annotate_snippets::{Annotation, AnnotationKind, Group, Level, Renderer, Snippet};

use super::types::GrazeMessage;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SourceDescriptor<'a> {
    pub content: &'a str,
    pub path: &'a str,
    pub line_starts: &'a [usize],
}

pub fn annotate<'a, I, F>(iter: I, renderer: &Renderer, mut source_getter: F) -> Vec<Group<'a>>
where
    I: Iterator<Item = &'a GrazeMessage>,
    F: FnMut(u32) -> SourceDescriptor<'a>,
{
    iter.map(|value| annotate_message(value, |value| source_getter(value)))
        .collect()
}

pub fn annotate_message<'a, F>(message: &'a GrazeMessage, mut source_getter: F) -> Group<'a>
where
    F: FnMut(u32) -> SourceDescriptor<'a>,
{
    match message {
        GrazeMessage::Error(graze_error, graze_suggestion) => match graze_error {
            super::types::GrazeError::Plain(string, source_span) => {
                let SourceDescriptor {
                    content,
                    path,
                    line_starts,
                } = source_getter(source_span.1);
                Level::ERROR.primary_title(string.as_str()).element(
                    Snippet::<Annotation>::source(content)
                        .path(path)
                        .annotation(
                            AnnotationKind::Primary
                                .span({
                                    let (a, b) = source_span.0;
                                    let a = if a.0 == 0 { 0 } else { line_starts[a.0 - 1] } + a.1;
                                    let b = if b.0 == 0 { 0 } else { line_starts[b.0 - 1] } + b.1;
                                    a..b
                                })
                                .label(string.as_str()),
                        ),
                )
            }
            super::types::GrazeError::ParseError(parse_error) => todo!(),
            super::types::GrazeError::CodegenError(graze_sb3_generator_error) => todo!(),
        },
        GrazeMessage::Warning(graze_warning, graze_suggestion) => todo!(),
        GrazeMessage::Info(graze_info, graze_suggestion) => todo!(),
    }
}
