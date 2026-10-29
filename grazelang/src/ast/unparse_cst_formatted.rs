use std::{borrow::Cow, num::NonZeroUsize};

use pretty::{Arena, DocAllocator, DocBuilder};

use super::unparse_formatted::UnparseFormattedSettings;
use crate::{
    lexer::{StyleToken, TextSpan},
    parser::cst::{
        self as cst_types, AssetDeclaration, BinOp, CanonicalIdentifier, CodeBlock,
        CustomBlockParamKind, DataDeclaration, DataDeclarationScope, DictionaryEntry,
        DictionaryValue, DoubleColonOrDot, Expression, GetPos, GrazeProgram, Identifier, ListEntry,
        Literal, MonitorValue, SingleAssetDeclaration, SingleAssetDeclarationValue,
        SingleDataDeclaration, SingleIdentifier, SpriteCodeBlock, SpriteStatement, StageCodeBlock,
        StageStatement, Statement, TopLevelStatement, UnOp, UseStatementContent, WarpSpecifier,
    },
    utils::string_escape,
};

#[derive(Debug, Clone, PartialEq)]
pub struct UnparseCSTFormattedContext<'a> {
    pub indentation: isize,
    pub width: usize,
    pub hard_lines_in_code_blocks: bool,
    pub style_tokens: &'a [StyleToken<'a>],
    pub style_token_index: usize,
    pub current_style_token_span: TextSpan,
    pub whitespace_queue: Vec<Whitespace>,
}

impl<'a> UnparseCSTFormattedContext<'a> {
    pub fn new(settings: UnparseFormattedSettings, tokens: &'a [StyleToken<'a>]) -> Self {
        Self {
            indentation: settings.indentation,
            width: settings.width,
            hard_lines_in_code_blocks: settings.hard_lines_in_code_blocks,
            style_tokens: tokens,
            style_token_index: 0,
            current_style_token_span: tokens
                .first()
                .map(|value| value.get_text_span())
                .unwrap_or(((usize::MAX, usize::MAX), (usize::MAX, usize::MAX))),
            whitespace_queue: Vec::new(),
        }
    }

    pub fn new_reused<'b>(
        &mut self,
        tokens: &'b [StyleToken<'b>],
    ) -> UnparseCSTFormattedContext<'b> {
        UnparseCSTFormattedContext {
            indentation: self.indentation,
            width: self.width,
            hard_lines_in_code_blocks: self.hard_lines_in_code_blocks,
            style_tokens: tokens,
            style_token_index: 0,
            current_style_token_span: tokens
                .first()
                .map(|value| value.get_text_span())
                .unwrap_or(((usize::MAX, usize::MAX), (usize::MAX, usize::MAX))),
            whitespace_queue: std::mem::take(&mut self.whitespace_queue),
        }
    }

    pub fn return_resources_to(&mut self, to: &mut UnparseCSTFormattedContext) {
        to.whitespace_queue = std::mem::take(&mut self.whitespace_queue);
    }

    pub fn reset(&mut self) {
        self.style_token_index = 0;
        self.current_style_token_span = self
            .style_tokens
            .get(self.style_token_index)
            .map(|value| value.get_text_span())
            .unwrap_or(((usize::MAX, usize::MAX), (usize::MAX, usize::MAX)));
        self.whitespace_queue.clear();
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Whitespace {
    TopLevelStatementEnd,
    StatementEnd,
    Space,
    Line,
    Line_,
}

fn appended_whitespace<'a, 'ctx, D>(
    whitespace: Whitespace,
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    context.whitespace_queue.push(whitespace);
    allocator.nil()
}

use appended_whitespace as ws;

use UnparseCSTFormattedContext as Context;

const ONE_NON_ZERO_USIZE: NonZeroUsize = NonZeroUsize::new(1).unwrap();

/// Invalid nodes in a cst will emit `INVALID`
pub trait UnparseCSTFormatted: Sized {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone;

    fn with_pretty_doc<'ctx, F, O>(self, f: F, context: &mut Context<'ctx>) -> O
    where
        F: for<'b> FnOnce(DocBuilder<'b, Arena<'b>, ()>) -> O,
    {
        let allocator = Arena::new();
        let value = self
            .into_pretty_doc(&allocator, context)
            .append(flush_trailing_style_tokens(&allocator, context));
        f(value)
    }

    fn unparse_formatted_into<W>(self, f: &mut W, context: &mut Context) -> std::fmt::Result
    where
        W: std::fmt::Write,
    {
        let width = context.width;
        self.with_pretty_doc(|value| value.render_fmt(width, f), context)
    }

    fn unparse_formatted_into_io<W>(self, f: &mut W, context: &mut Context) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        let width = context.width;
        self.with_pretty_doc(|value| value.render(width, f), context)
    }

    fn unparse_formatted_to_string(self, context: &mut Context) -> String {
        let mut out = String::new();
        let width = context.width;
        self.with_pretty_doc(|value| value.render_fmt(width, &mut out), context)
            .unwrap();
        out
    }
}

pub fn flush_trailing_style_tokens<'a, 'ctx, D>(
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let mut value = allocator.nil();
    let mut empty_lines_end = (usize::MAX, usize::MAX);
    let mut final_newline = false;
    while let Some(token) = context.style_tokens.get(context.style_token_index) {
        match token {
            StyleToken::EmptyLines(count, (_, end)) => {
                empty_lines_end = *end;
                if *count > ONE_NON_ZERO_USIZE {
                    value = value.append(allocator.hardline());
                    final_newline = true;
                }
            }
            StyleToken::LineComment(comment, (start, _)) => {
                if *start == empty_lines_end {
                    value = value.append(allocator.hardline());
                } else if !final_newline {
                    value = value.append(" ");
                }
                value = value
                    .append("//")
                    .append(*comment)
                    .append(allocator.hardline());
                final_newline = true;
            }
            StyleToken::MultiLineComment(comment, (start, _)) => {
                if *start == empty_lines_end {
                    value = value.append(allocator.hardline());
                } else if !final_newline {
                    value = value.append(" ");
                }
                value = value
                    .append("/*")
                    .append(allocator.intersperse(comment.lines(), allocator.hardline()))
                    .append("*/")
                    .append(allocator.hardline());
                final_newline = true;
            }
        }
        context.style_token_index += 1;
    }
    context.current_style_token_span = ((usize::MAX, usize::MAX), (usize::MAX, usize::MAX));
    context.whitespace_queue.clear();
    value
}

#[inline]
fn string_with_get_pos_into_pretty_doc<'a, 'ctx, D, G>(
    string: Cow<'a, str>,
    get_pos: &G,
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
    G: GetPos,
{
    string_into_pretty_doc(string, get_pos.get_source_span().0, allocator, context)
}

macro_rules! pretty_token {
    ($string:expr, $get_pos:expr, $allocator:expr, $context:expr$(,)?) => {
        string_with_get_pos_into_pretty_doc($string.into(), $get_pos, $allocator, $context)
    };
}

macro_rules! flush_before_group {
    ($pos:expr, $allocator:expr, $context:expr$(,)?) => {
        flush_whitespace_and_comments($pos.get_source_span().0, $allocator, $context)
    };
}

fn flush_whitespace<'a, 'ctx, D>(
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let mut value = allocator.nil();
    let use_hard_lines = context.hard_lines_in_code_blocks;
    for i in context.whitespace_queue.drain(..) {
        value = value.append(match i {
            Whitespace::TopLevelStatementEnd => allocator.hardline(),
            Whitespace::StatementEnd => {
                if use_hard_lines {
                    allocator.hardline()
                } else {
                    allocator.line()
                }
            }
            Whitespace::Space => allocator.text(" "),
            Whitespace::Line => allocator.line(),
            Whitespace::Line_ => allocator.line_(),
        });
    }
    value
}

fn flush_whitespace_and_comments_before_end_of_code_block<'a, 'ctx, D>(
    text_span: TextSpan,
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let mut value = allocator.nil();
    let mut empty_lines_end = (usize::MAX, usize::MAX);
    while context.current_style_token_span < text_span {
        if let Some(token) = context.style_tokens.get(context.style_token_index) {
            match token {
                StyleToken::EmptyLines(count, (_, end)) => {
                    empty_lines_end = *end;
                    if *count > ONE_NON_ZERO_USIZE {
                        value = value.append(allocator.hardline());
                    }
                }
                StyleToken::LineComment(comment, (start, _)) => {
                    if (start.0, start.1 - 2) == empty_lines_end {
                        value = value.append(allocator.hardline());
                    } else {
                        value = value.append(" ");
                    }
                    value = value.append("//").append(*comment);
                }
                StyleToken::MultiLineComment(comment, (start, _)) => {
                    if (start.0, start.1 - 2) == empty_lines_end {
                        value = value.append(allocator.hardline());
                    } else {
                        value = value.append(" ");
                    }
                    value = value
                        .append("/*")
                        .append(allocator.intersperse(comment.lines(), allocator.hardline()))
                        .append("*/");
                }
            }
        }
        context.style_token_index += 1;
        context.current_style_token_span = context
            .style_tokens
            .get(context.style_token_index)
            .map(|value| value.get_text_span())
            .unwrap_or(((usize::MAX, usize::MAX), (usize::MAX, usize::MAX)));
    }
    value
}

fn flush_whitespace_and_comments<'a, 'ctx, D>(
    text_span: TextSpan,
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let mut value = allocator.nil();
    let mut empty_lines_end = (usize::MAX, usize::MAX);
    let mut final_newline = false;
    let mut final_newline_after_empty_line = false;
    while context.current_style_token_span < text_span {
        if let Some(token) = context.style_tokens.get(context.style_token_index) {
            match token {
                StyleToken::EmptyLines(count, (_, end)) => {
                    empty_lines_end = *end;
                    if *count > ONE_NON_ZERO_USIZE {
                        value = value.append(allocator.hardline());
                        final_newline_after_empty_line = !final_newline;
                        final_newline = true;
                    }
                }
                StyleToken::LineComment(comment, (start, _)) => {
                    if (start.0, start.1 - 2) == empty_lines_end {
                        value = value.append(allocator.hardline());
                    } else if !final_newline {
                        value = value.append(" ");
                    }
                    value = value
                        .append("//")
                        .append(*comment)
                        .append(allocator.hardline());
                    final_newline = true;
                    final_newline_after_empty_line = false;
                }
                StyleToken::MultiLineComment(comment, (start, _)) => {
                    if (start.0, start.1 - 2) == empty_lines_end {
                        value = value.append(allocator.hardline());
                    } else if !final_newline {
                        value = value.append(" ");
                    }
                    value = value
                        .append("/*")
                        .append(allocator.intersperse(comment.lines(), allocator.hardline()))
                        .append("*/")
                        .append(allocator.hardline());
                    final_newline = true;
                    final_newline_after_empty_line = false;
                }
            }
        }
        context.style_token_index += 1;
        context.current_style_token_span = context
            .style_tokens
            .get(context.style_token_index)
            .map(|value| value.get_text_span())
            .unwrap_or(((usize::MAX, usize::MAX), (usize::MAX, usize::MAX)));
    }
    let use_hard_lines = context.hard_lines_in_code_blocks;
    if final_newline {
        if final_newline_after_empty_line {
            value = value.append(allocator.hardline());
        }
        context.whitespace_queue.clear();
    }
    for i in context.whitespace_queue.drain(..) {
        value = value.append(match i {
            Whitespace::TopLevelStatementEnd => allocator.hardline(),
            Whitespace::StatementEnd => {
                if use_hard_lines {
                    allocator.hardline()
                } else {
                    allocator.line()
                }
            }
            Whitespace::Space => allocator.text(" "),
            Whitespace::Line => allocator.line(),
            Whitespace::Line_ => allocator.line_(),
        });
    }
    value
}

#[inline]
fn string_into_pretty_doc<'a, 'ctx, D>(
    string: Cow<'a, str>,
    text_span: TextSpan,
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    flush_whitespace_and_comments(text_span, allocator, context).append(string)
}

macro_rules! pretty_token_span {
    ($string:expr, $text_span:expr, $allocator:expr, $context:expr$(,)?) => {
        string_into_pretty_doc($string.into(), $text_span, $allocator, $context)
    };
}

pub fn intersperse_csv<'a, 'ctx, D, I, V>(
    mut items: I,
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    I: Iterator<Item = (V, Option<&'a cst_types::Comma>)>,
    V: UnparseCSTFormatted + 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let Some((mut value, mut comma)) = items
        .next()
        .map(|(value, comma)| (value.into_pretty_doc(allocator, context), comma))
    else {
        return allocator.nil();
    };
    for (item, new_comma) in items {
        if let Some(comma) = comma {
            value = value
                .append(pretty_token!(",", comma, allocator, context))
                .append(ws(Whitespace::Line, allocator, context));
        }
        value = value.append(item.into_pretty_doc(allocator, context));
        comma = new_comma;
    }
    value
        .append(if let Some(comma) = comma {
            pretty_token!("", comma, allocator, context)
        } else {
            flush_whitespace(allocator, context)
        })
        .nest(context.indentation)
        .append(allocator.text(",").flat_alt(allocator.nil()))
}

pub fn intersperse_statement_end<'a, 'ctx, D, I, V>(
    mut items: I,
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    I: Iterator<Item = V>,
    V: UnparseCSTFormatted + 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let Some(mut value) = items
        .next()
        .map(|value| value.into_pretty_doc(allocator, context))
    else {
        return allocator.nil();
    };
    for item in items {
        value = value
            .append(ws(Whitespace::StatementEnd, allocator, context))
            .append(item.into_pretty_doc(allocator, context));
    }
    value
}

pub fn intersperse_top_level_statement_end<'a, 'ctx, D, I, V>(
    mut items: I,
    allocator: &'a D,
    context: &mut Context<'ctx>,
) -> DocBuilder<'a, D, ()>
where
    'ctx: 'a,
    I: Iterator<Item = V>,
    V: UnparseCSTFormatted + 'a,
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let Some(mut value) = items
        .next()
        .map(|value| value.into_pretty_doc(allocator, context))
    else {
        return allocator.nil();
    };
    for item in items {
        value = value
            .append(ws(Whitespace::TopLevelStatementEnd, allocator, context))
            .append(item.into_pretty_doc(allocator, context));
    }
    value
}

macro_rules! wrap_csv {
    ($items:expr, $start_token:expr, $end_token:expr, $start:expr, $end:expr, $line_type:ident, $allocator:expr, $context:expr) => {
        if $items.is_empty() {
            pretty_token!($start, $start_token, $allocator, $context)
                .append(pretty_token!($end, $end_token, $allocator, $context))
        } else {
            pretty_token!($start, $start_token, $allocator, $context)
                .append(
                    ws(Whitespace::$line_type, $allocator, $context)
                        .append(intersperse_csv($items.iter(), $allocator, $context))
                        .append(ws(Whitespace::$line_type, $allocator, $context))
                        .append(flush_whitespace($allocator, $context))
                        .group(),
                )
                .append(pretty_token!($end, $end_token, $allocator, $context))
        }
    };
    ($items:expr, $start_token:expr, $end_token:expr, BRACKETS, $allocator:expr, $context:expr) => {
        wrap_csv!(
            $items,
            $start_token,
            $end_token,
            "[",
            "]",
            Line_,
            $allocator,
            $context
        )
    };
    ($items:expr, $start_token:expr, $end_token:expr, PARENS, $allocator:expr, $context:expr) => {
        wrap_csv!(
            $items,
            $start_token,
            $end_token,
            "(",
            ")",
            Line_,
            $allocator,
            $context
        )
    };
    ($items:expr, $start_token:expr, $end_token:expr, BRACES, $allocator:expr, $context:expr) => {
        wrap_csv!(
            $items,
            $start_token,
            $end_token,
            "{",
            "}",
            Line,
            $allocator,
            $context
        )
    };
}

macro_rules! wrap_code_block {
    ($items:expr, $left_brace:expr, $right_brace:expr, $allocator:expr, $context:expr) => {
        if $items.is_empty() {
            pretty_token!("{", $left_brace, $allocator, $context).append(pretty_token!(
                "}",
                $right_brace,
                $allocator,
                $context,
            ))
        } else {
            pretty_token!("{", $left_brace, $allocator, $context)
                .append(
                    ws(Whitespace::StatementEnd, $allocator, $context)
                        .append(intersperse_statement_end(
                            $items.iter(),
                            $allocator,
                            $context,
                        ))
                        .append(flush_whitespace_and_comments_before_end_of_code_block(
                            $right_brace.get_source_span().0,
                            $allocator,
                            $context,
                        ))
                        .nest($context.indentation)
                        .append(ws(Whitespace::StatementEnd, $allocator, $context))
                        .append(flush_whitespace($allocator, $context))
                        .group(),
                )
                .append(pretty_token!("}", $right_brace, $allocator, $context))
        }
    };
}

impl UnparseCSTFormatted for &Literal {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        let (text, span) = match self {
            Literal::String(value, source_span) => (
                Cow::Owned(format!(
                    "\"{}\"",
                    string_escape::normal_string_escaper(value)
                )),
                source_span.0,
            ),
            Literal::DecimalInt(value, source_span)
            | Literal::DecimalFloat(value, source_span)
            | Literal::HexadecimalInt(value, source_span)
            | Literal::OctalInt(value, source_span)
            | Literal::BinaryInt(value, source_span) => {
                (Cow::Borrowed(value.as_str()), source_span.0)
            }
            Literal::Bool(value, source_span) => (
                if *value {
                    Cow::Borrowed("true")
                } else {
                    Cow::Borrowed("false")
                },
                source_span.0,
            ),
            Literal::EmptyExpression(left, right, _) => {
                return pretty_token!("(", left, allocator, context)
                    .append(pretty_token!(")", right, allocator, context));
            }
        };
        pretty_token_span!(text, span, allocator, context)
    }
}

impl UnparseCSTFormatted for &UnOp {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        let (text, span) = match self {
            UnOp::Minus(source_span) => ("-", source_span.0),
            UnOp::Not(source_span) => ("!", source_span.0),
            UnOp::Exp(source_span) => ("e^", source_span.0),
            UnOp::Pow(source_span) => ("10^", source_span.0),
        };
        pretty_token_span!(text, span, allocator, context)
    }
}

impl UnparseCSTFormatted for &BinOp {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        let (text, span) = match self {
            BinOp::Plus(source_span) => ("+", source_span.0),
            BinOp::Minus(source_span) => ("-", source_span.0),
            BinOp::Times(source_span) => ("*", source_span.0),
            BinOp::Div(source_span) => ("/", source_span.0),
            BinOp::Mod(source_span) => ("%", source_span.0),
            BinOp::Join(source_span) => ("++", source_span.0),
            BinOp::Contains(source_span) => ("contains", source_span.0),
            BinOp::And(source_span) => ("&&", source_span.0),
            BinOp::Or(source_span) => ("||", source_span.0),
            BinOp::Equals(source_span) => ("==", source_span.0),
            BinOp::NotEquals(source_span) => ("!=", source_span.0),
            BinOp::LessThan(source_span) => ("<", source_span.0),
            BinOp::GreaterThan(source_span) => (">", source_span.0),
            BinOp::LessThanOrEqual(source_span) => ("<=", source_span.0),
            BinOp::GreaterThanOrEqual(source_span) => (">=", source_span.0),
        };
        pretty_token_span!(text, span, allocator, context)
    }
}

impl UnparseCSTFormatted for &SingleIdentifier {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        pretty_token_span!(&self.value, self.source_span.0, allocator, context,)
    }
}

impl UnparseCSTFormatted for &Identifier {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        let mut value = self.root.into_pretty_doc(allocator, context);
        for (sep, ident) in self.path.iter() {
            value = value
                .append(pretty_token!("::", sep, allocator, context))
                .append(ident.into_pretty_doc(allocator, context));
        }
        for (sep, ident) in self.fields.iter() {
            value = value
                .append(pretty_token!(".", sep, allocator, context))
                .append(ident.into_pretty_doc(allocator, context));
        }
        value
    }
}

impl UnparseCSTFormatted for &CanonicalIdentifier {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        pretty_token_span!(
            format!("`{}`", string_escape::canonical_name_escaper(&self.name)),
            self.source_span.0,
            allocator,
            context,
        )
    }
}

impl UnparseCSTFormatted for &Expression {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            Expression::Literal(value) => value.into_pretty_doc(allocator, context),
            Expression::FormattedString(content, source_span) => {
                let mut value = pretty_token_span!("\"", source_span.0, allocator, context,);
                for (i, next) in content.iter().zip(
                    content
                        .iter()
                        .map(Some)
                        .chain(std::iter::once(None))
                        .skip(1),
                ) {
                    match i {
                        cst_types::FormattedStringContent::Expression(expression) => {
                            value = value
                                .append("${")
                                .append(
                                    ws(Whitespace::Line_, allocator, context)
                                        .append(expression.into_pretty_doc(allocator, context))
                                        .nest(context.indentation)
                                        .append(ws(Whitespace::Line_, allocator, context))
                                        .append(flush_whitespace(allocator, context))
                                        .group(),
                                )
                                .append(pretty_token_span!(
                                    "}",
                                    next.as_ref()
                                        .map(|value| value.get_source_span().0)
                                        .unwrap_or((source_span.0.1, source_span.0.1)),
                                    allocator,
                                    context,
                                ));
                        }
                        cst_types::FormattedStringContent::String(content, source_span) => {
                            value = value.append(pretty_token_span!(
                                string_escape::format_string_escaper(content).escape_to_string(),
                                source_span.0,
                                allocator,
                                context
                            ));
                        }
                    }
                }
                value.append("\"")
            }
            Expression::BinOp(left_operand, operator, right_operand, _) => {
                fn unparse_binop_chain<'a, 'ctx, D>(
                    expression: &'a Expression,
                    precedence: u8,
                    allocator: &'a D,
                    context: &mut Context<'ctx>,
                ) -> DocBuilder<'a, D, ()>
                where
                    'ctx: 'a,
                    D: DocAllocator<'a, ()>,
                    D::Doc: Clone,
                {
                    let Expression::BinOp(left_operand, operator, right_operand, _) = expression
                    else {
                        return expression.into_pretty_doc(allocator, context);
                    };
                    let (inner_precedence, _) = operator.get_precedence();
                    let flushed = flush_before_group!(left_operand, allocator, context);
                    let value =
                        unparse_binop_chain(left_operand, inner_precedence, allocator, context)
                            .append(ws(Whitespace::Line, allocator, context))
                            .append(operator.into_pretty_doc(allocator, context))
                            .append(ws(Whitespace::Space, allocator, context))
                            .append(unparse_binop_chain(
                                right_operand,
                                inner_precedence,
                                allocator,
                                context,
                            ));
                    flushed.append(if inner_precedence != precedence {
                        value.group()
                    } else {
                        value
                    })
                }
                let (precedence, _) = operator.get_precedence();
                flush_before_group!(left_operand, allocator, context).append(
                    unparse_binop_chain(left_operand, precedence, allocator, context)
                        .append(ws(Whitespace::Line, allocator, context))
                        .append(operator.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(unparse_binop_chain(
                            right_operand,
                            precedence,
                            allocator,
                            context,
                        ))
                        .nest(context.indentation)
                        .group(),
                )
            }
            Expression::UnOp(operator, operand, _) => operator
                .into_pretty_doc(allocator, context)
                .append(operand.into_pretty_doc(allocator, context)),
            Expression::Identifier(identifier) => identifier.into_pretty_doc(allocator, context),
            Expression::Call(function, left_parens, arguments, right_parens, _) => function
                .into_pretty_doc(allocator, context)
                .append(wrap_csv!(
                    arguments,
                    left_parens,
                    right_parens,
                    PARENS,
                    allocator,
                    context
                )),
            Expression::GetItem(list, left_bracket, item, right_bracket, _) => list
                .into_pretty_doc(allocator, context)
                .append(pretty_token!("[", left_bracket, allocator, context))
                .append(
                    ws(Whitespace::Line_, allocator, context)
                        .append(item.into_pretty_doc(allocator, context))
                        .nest(context.indentation)
                        .append(ws(Whitespace::Line_, allocator, context))
                        .append(flush_whitespace(allocator, context))
                        .group(),
                )
                .append(pretty_token!("]", right_bracket, allocator, context)),
            Expression::GetLetter(expression, left_bracket, letter, right_bracket, _) => expression
                .into_pretty_doc(allocator, context)
                .append(pretty_token!("@[", left_bracket, allocator, context))
                .append(
                    ws(Whitespace::Line_, allocator, context)
                        .append(letter.into_pretty_doc(allocator, context))
                        .nest(context.indentation)
                        .append(ws(Whitespace::Line_, allocator, context))
                        .append(flush_whitespace(allocator, context))
                        .group(),
                )
                .append(pretty_token!("]", right_bracket, allocator, context)),
            Expression::Parentheses(left_parens, expression, right_parens, _) => {
                flush_before_group!(left_parens, allocator, context).append(
                    pretty_token!("(", left_parens, allocator, context)
                        .append(
                            ws(Whitespace::Line_, allocator, context)
                                .append(expression.into_pretty_doc(allocator, context))
                                .nest(context.indentation),
                        )
                        .append(ws(Whitespace::Line_, allocator, context))
                        .append(flush_whitespace(allocator, context))
                        .group()
                        .append(pretty_token!(")", right_parens, allocator, context)),
                )
            }
        }
    }
}

impl UnparseCSTFormatted for &Statement {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            Statement::DataDeclaration(let_keyword, data_declaration, semicolon, _) => {
                pretty_token!("let", let_keyword, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(data_declaration.into_pretty_doc(allocator, context))
                    .append(pretty_token!(";", semicolon, allocator, context))
            }
            Statement::Assignment(target, assignment, value, semicolon, _) => {
                flush_before_group!(target, allocator, context).append(
                    target
                        .into_pretty_doc(allocator, context)
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(pretty_token!("=", assignment, allocator, context))
                        .append(ws(Whitespace::Line, allocator, context))
                        .append(value.into_pretty_doc(allocator, context))
                        .append(pretty_token!(";", semicolon, allocator, context))
                        .nest(context.indentation)
                        .group(),
                )
            }
            Statement::ListAssignment(
                target,
                assignment,
                left_bracket,
                value,
                right_bracket,
                semicolon,
                _,
            ) => flush_before_group!(target, allocator, context).append(
                target
                    .into_pretty_doc(allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(pretty_token!("=", assignment, allocator, context))
                    .append(ws(Whitespace::Line, allocator, context))
                    .append({
                        wrap_csv!(
                            value,
                            left_bracket,
                            right_bracket,
                            BRACKETS,
                            allocator,
                            context
                        )
                        .append(pretty_token!(";", semicolon, allocator, context))
                    })
                    .nest(context.indentation)
                    .group(),
            ),
            Statement::SetItem(
                list,
                left_bracket,
                item,
                right_bracket,
                assignment,
                value,
                semicolon,
                _,
            ) => flush_before_group!(list, allocator, context).append(
                list.into_pretty_doc(allocator, context)
                    .append(pretty_token!("[", left_bracket, allocator, context))
                    .append(
                        ws(Whitespace::Line_, allocator, context)
                            .append(item.into_pretty_doc(allocator, context))
                            .nest(context.indentation)
                            .append(ws(Whitespace::Line_, allocator, context))
                            .append(flush_whitespace(allocator, context))
                            .group(),
                    )
                    .append(pretty_token!("]", right_bracket, allocator, context))
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(pretty_token!("=", assignment, allocator, context))
                    .append(ws(Whitespace::Line, allocator, context))
                    .append(value.into_pretty_doc(allocator, context))
                    .append(pretty_token!(";", semicolon, allocator, context))
                    .nest(context.indentation)
                    .group(),
            ),
            Statement::Call(function, left_parens, arguments, right_parens, semicolon, _) => {
                function
                    .into_pretty_doc(allocator, context)
                    .append(wrap_csv!(
                        arguments,
                        left_parens,
                        right_parens,
                        PARENS,
                        allocator,
                        context
                    ))
                    .append(pretty_token!(";", semicolon, allocator, context))
            }
            Statement::Forever(control_function, code_block, _, _) => control_function
                .into_pretty_doc(allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(code_block.into_pretty_doc(allocator, context)),
            Statement::SingleInputControl(control_function, argument, code_block, _, _) => {
                control_function
                    .into_pretty_doc(allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(argument.into_pretty_doc(allocator, context))
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(code_block.into_pretty_doc(allocator, context))
            }
            Statement::MultiInputControl(
                control_function,
                left_parens,
                arguments,
                right_parens,
                code_block,
                _,
                _,
            ) => {
                let flushed = flush_before_group!(control_function, allocator, context);
                let mut value = control_function
                    .into_pretty_doc(allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(if arguments.is_empty() {
                        allocator.nil()
                    } else {
                        wrap_csv!(
                            arguments,
                            left_parens,
                            right_parens,
                            PARENS,
                            allocator,
                            context
                        )
                        .append(ws(Whitespace::Space, allocator, context))
                    })
                    .append(code_block.into_pretty_doc(allocator, context));
                if arguments.len() == 1
                    && let Some((argument, _)) = arguments.iter().next()
                {
                    value = value
                        .flat_alt(
                            control_function
                                .into_pretty_doc(allocator, context)
                                .append(ws(Whitespace::Space, allocator, context))
                                .append(argument.into_pretty_doc(allocator, context))
                                .append(ws(Whitespace::Space, allocator, context))
                                .append(code_block.into_pretty_doc(allocator, context)),
                        )
                        .group();
                }
                flushed.append(value)
            }
            Statement::IfElse(
                (syntactic_if, if_condition, if_branch),
                alternative_branches,
                else_branch,
                _,
                _,
            ) => {
                let mut value = pretty_token!("if", syntactic_if, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(if_condition.into_pretty_doc(allocator, context))
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(if_branch.into_pretty_doc(allocator, context));
                for (syntactic_else, syntactic_if, condition, code_block) in alternative_branches {
                    value = value
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(pretty_token!("else", syntactic_else, allocator, context))
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(pretty_token!("if", syntactic_if, allocator, context))
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(condition.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(code_block.into_pretty_doc(allocator, context));
                }
                if let Some((syntactic_else, code_block)) = else_branch {
                    value = value
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(pretty_token!("else", syntactic_else, allocator, context))
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(code_block.into_pretty_doc(allocator, context));
                }
                value
            }
            Statement::UseStatement(
                use_keyword,
                extension_keyword,
                use_statement_content,
                semicolon,
                _,
            ) => pretty_token!("use", use_keyword, allocator, context)
                .append(if let Some(extension_keyword) = extension_keyword {
                    pretty_token!("extension", extension_keyword, allocator, context)
                } else {
                    allocator.nil()
                })
                .append(use_statement_content.into_pretty_doc(allocator, context))
                .append(pretty_token!(";", semicolon, allocator, context)),
            Statement::EmptyStatement(semicolon) => {
                pretty_token!(";", semicolon, allocator, context)
            }
            Statement::Invalid(source_span) => {
                pretty_token_span!("INVALID;", source_span.0, allocator, context)
            }
        }
    }
}

impl UnparseCSTFormatted for &UseStatementContent {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            UseStatementContent::SingleUse {
                identifier,
                rename,
                source_span: _,
            } => {
                let value = identifier.into_pretty_doc(allocator, context);
                if let Some((as_keyword, rename)) = rename {
                    return value
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(pretty_token!("as", as_keyword, allocator, context))
                        .append(ws(Whitespace::Space, allocator, context))
                        .append(rename.into_pretty_doc(allocator, context));
                }
                value
            }
            UseStatementContent::MultiUse {
                root,
                double_colon_or_dot,
                left_brace,
                content,
                right_brace,
                source_span: _,
            } => root
                .into_pretty_doc(allocator, context)
                .append(pretty_token!(
                    match double_colon_or_dot {
                        DoubleColonOrDot::DoubleColon(_) => "::",
                        DoubleColonOrDot::Dot(_) => ".",
                    },
                    double_colon_or_dot,
                    allocator,
                    context
                ))
                .append(pretty_token!("{", left_brace, allocator, context))
                .append({
                    if content.is_empty() {
                        allocator.nil()
                    } else {
                        ws(Whitespace::Line_, allocator, context)
                            .append(intersperse_csv(content.iter(), allocator, context))
                            .append(ws(Whitespace::Line_, allocator, context))
                            .append(flush_whitespace(allocator, context))
                            .group()
                    }
                    .append(pretty_token!(
                        "}",
                        right_brace,
                        allocator,
                        context
                    ))
                }),
            UseStatementContent::Invalid(source_span) => {
                pretty_token_span!("INVALID", source_span.0, allocator, context)
            }
        }
    }
}

impl UnparseCSTFormatted for &DataDeclaration {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        let mut value = allocator.nil();
        match self {
            DataDeclaration::Mixed(scope, left_parens, declarations, right_parens, _) => {
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value = value.append(pretty_token!("(", left_parens, allocator, context));
                value = value
                    .append(if declarations.is_empty() {
                        allocator.nil()
                    } else {
                        ws(Whitespace::Line_, allocator, context)
                            .append(intersperse_csv(declarations.iter(), allocator, context))
                            .append(ws(Whitespace::Line_, allocator, context))
                            .append(flush_whitespace(allocator, context))
                            .group()
                    })
                    .append(pretty_token!(")", right_parens, allocator, context));
            }
            DataDeclaration::Vars(
                scope,
                vars_keyword,
                left_brace,
                declarations,
                right_brace,
                _,
            ) => {
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value = value
                    .append(pretty_token!("vars", vars_keyword, allocator, context))
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(pretty_token!("{", left_brace, allocator, context));
                value = value
                    .append(if declarations.is_empty() {
                        allocator.nil()
                    } else {
                        ws(Whitespace::Line_, allocator, context)
                            .append(intersperse_csv(declarations.iter(), allocator, context))
                            .append(ws(Whitespace::Line_, allocator, context))
                            .append(flush_whitespace(allocator, context))
                            .group()
                    })
                    .append(pretty_token!("}", right_brace, allocator, context));
            }
            DataDeclaration::Lists(
                scope,
                lists_keyword,
                left_brace,
                declarations,
                right_brace,
                _,
            ) => {
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value = value
                    .append(pretty_token!("lists", lists_keyword, allocator, context))
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(pretty_token!("{", left_brace, allocator, context));
                value = value
                    .append(if declarations.is_empty() {
                        allocator.nil()
                    } else {
                        ws(Whitespace::Line_, allocator, context)
                            .append(intersperse_csv(declarations.iter(), allocator, context))
                            .append(ws(Whitespace::Line_, allocator, context))
                            .append(flush_whitespace(allocator, context))
                            .group()
                    })
                    .append(pretty_token!("}", right_brace, allocator, context));
            }
            DataDeclaration::Single(single_data_declaration) => {
                value = value.append(single_data_declaration.into_pretty_doc(allocator, context));
            }
        }
        value
    }
}

impl UnparseCSTFormatted for &DataDeclarationScope {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        let (text, source_span) = match self {
            DataDeclarationScope::Global(source_span) => ("global", source_span),
            DataDeclarationScope::Local(source_span) => ("local", source_span),
            DataDeclarationScope::Cloud(source_span) => ("cloud", source_span),
            DataDeclarationScope::Unset => return allocator.nil(),
        };
        pretty_token_span!(text, source_span.0, allocator, context)
    }
}

impl UnparseCSTFormatted for &SingleDataDeclaration {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            SingleDataDeclaration::Variable(
                scope,
                var_keyword,
                canonical_identifier,
                identifier,
                assignment,
                decl_value,
                _,
            ) => {
                let mut value = allocator.nil();
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(var_keyword) = var_keyword {
                    value = value
                        .append(pretty_token!("var", var_keyword, allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value
                    .append(identifier.into_pretty_doc(allocator, context))
                    .append(
                        ws(Whitespace::Space, allocator, context)
                            .append(pretty_token!("=", assignment, allocator, context))
                            .append(ws(Whitespace::Line, allocator, context))
                            .append(decl_value.into_pretty_doc(allocator, context))
                            .nest(context.indentation)
                            .group(),
                    )
            }
            SingleDataDeclaration::EmptyVariable(
                scope,
                var_keyword,
                canonical_identifier,
                identifier,
                _,
            ) => {
                let mut value = allocator.nil();
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(var_keyword) = var_keyword {
                    value = value
                        .append(pretty_token!("var", var_keyword, allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value.append(identifier.into_pretty_doc(allocator, context))
            }
            SingleDataDeclaration::List(
                scope,
                list_keyword,
                canonical_identifier,
                identifier,
                assignment,
                left_bracket,
                decl_value,
                right_bracket,
                _,
            ) => {
                let mut value = allocator.nil();
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(list_keyword) = list_keyword {
                    value = value
                        .append(pretty_token!("list", list_keyword, allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value
                    .append(identifier.into_pretty_doc(allocator, context))
                    .append(
                        ws(Whitespace::Space, allocator, context)
                            .append(pretty_token!("=", assignment, allocator, context))
                            .append(ws(Whitespace::Line, allocator, context))
                            .append(wrap_csv!(
                                decl_value,
                                left_bracket,
                                right_bracket,
                                BRACKETS,
                                allocator,
                                context
                            ))
                            .nest(context.indentation)
                            .group(),
                    )
            }
            SingleDataDeclaration::FileList(
                scope,
                list_keyword,
                canonical_identifier,
                identifier,
                assignment,
                file_keyword,
                source,
                _,
            ) => {
                let mut value = allocator.nil();
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(list_keyword) = list_keyword {
                    value = value
                        .append(pretty_token!("list", list_keyword, allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value
                    .append(identifier.into_pretty_doc(allocator, context))
                    .append(
                        ws(Whitespace::Space, allocator, context)
                            .append(pretty_token!("=", assignment, allocator, context))
                            .append(ws(Whitespace::Line, allocator, context))
                            .append(pretty_token!("file", file_keyword, allocator, context))
                            .append(ws(Whitespace::Space, allocator, context))
                            .append(source.into_pretty_doc(allocator, context))
                            .nest(context.indentation)
                            .group(),
                    )
            }
            SingleDataDeclaration::EmptyList(
                scope,
                list_keyword,
                canonical_identifier,
                identifier,
                _,
            ) => {
                let mut value = allocator.nil();
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(list_keyword) = list_keyword {
                    value = value
                        .append(pretty_token!("list", list_keyword, allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value.append(identifier.into_pretty_doc(allocator, context))
            }
        }
    }
}

impl UnparseCSTFormatted for &ListEntry {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            ListEntry::Expression(expression) => expression.into_pretty_doc(allocator, context),
            ListEntry::Unwrap(unwrap, literal, _) => {
                pretty_token!("..", unwrap, allocator, context)
                    .append(literal.into_pretty_doc(allocator, context))
            }
        }
    }
}

macro_rules! impl_unparse_cst_for_code_block {
    ($t:ty) => {
        impl UnparseCSTFormatted for &$t {
            fn into_pretty_doc<'a, 'ctx, D>(
                self,
                allocator: &'a D,
                context: &mut Context<'ctx>,
            ) -> DocBuilder<'a, D, ()>
            where
                Self: 'a,
                'ctx: 'a,
                D: DocAllocator<'a, ()>,
                D::Doc: Clone,
            {
                wrap_code_block!(
                    self.statements,
                    &self.left_brace,
                    &self.right_brace,
                    allocator,
                    context
                )
            }
        }
    };
}

impl_unparse_cst_for_code_block!(CodeBlock);
impl_unparse_cst_for_code_block!(SpriteCodeBlock);
impl_unparse_cst_for_code_block!(StageCodeBlock);

impl UnparseCSTFormatted for &SpriteStatement {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            SpriteStatement::DataDeclaration(let_keyword, data_declaration, semicolon, _) => {
                pretty_token!("let", let_keyword, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(data_declaration.into_pretty_doc(allocator, context))
                    .append(pretty_token!(";", semicolon, allocator, context))
            }
            SpriteStatement::CostumeDeclaration(
                costume_keyword,
                asset_declaration,
                semicolon,
                _,
            ) => pretty_token!("costume", costume_keyword, allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(asset_declaration.into_pretty_doc(allocator, context))
                .append(pretty_token!(";", semicolon, allocator, context)),
            SpriteStatement::SoundDeclaration(sound_keyword, asset_declaration, semicolon, _) => {
                pretty_token!("sound", sound_keyword, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(asset_declaration.into_pretty_doc(allocator, context))
                    .append(pretty_token!(";", semicolon, allocator, context))
            }
            SpriteStatement::NoInputHatStatement(hat_function, code_block, _, _) => hat_function
                .into_pretty_doc(allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(code_block.into_pretty_doc(allocator, context)),
            SpriteStatement::SingleInputHatStatement(hat_function, argument, code_block, _, _) => {
                hat_function
                    .into_pretty_doc(allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(argument.into_pretty_doc(allocator, context))
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(code_block.into_pretty_doc(allocator, context))
            }
            SpriteStatement::MultiInputHatStatement(
                hat_function,
                left_parens,
                arguments,
                right_parens,
                code_block,
                _,
                _,
            ) => {
                let flushed = flush_before_group!(hat_function, allocator, context);
                let mut value = hat_function
                    .into_pretty_doc(allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(if arguments.is_empty() {
                        allocator.nil()
                    } else {
                        wrap_csv!(
                            arguments,
                            left_parens,
                            right_parens,
                            PARENS,
                            allocator,
                            context
                        )
                        .append(ws(Whitespace::Space, allocator, context))
                    })
                    .append(code_block.into_pretty_doc(allocator, context));
                if arguments.len() == 1
                    && let Some((argument, _)) = arguments.iter().next()
                {
                    value = value
                        .flat_alt(
                            hat_function
                                .into_pretty_doc(allocator, context)
                                .append(ws(Whitespace::Space, allocator, context))
                                .append(argument.into_pretty_doc(allocator, context))
                                .append(ws(Whitespace::Space, allocator, context))
                                .append(code_block.into_pretty_doc(allocator, context)),
                        )
                        .group();
                }
                flushed.append(value)
            }
            SpriteStatement::CustomBlockDefinition(
                is_warp,
                proc_keyword,
                canonical_identifier,
                identifier,
                left_parens,
                parameters,
                right_parens,
                code_block,
                _,
                _,
            ) => {
                let mut value = allocator.nil();
                if let Some(is_warp) = is_warp {
                    value = value
                        .append(is_warp.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value = value
                    .append(pretty_token!("proc", proc_keyword, allocator, context))
                    .append(ws(Whitespace::Space, allocator, context));
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value
                    .append(identifier.into_pretty_doc(allocator, context))
                    .append(pretty_token!("(", left_parens, allocator, context))
                    .append(if parameters.is_empty() {
                        allocator.nil()
                    } else {
                        ws(Whitespace::Line_, allocator, context)
                            .append({
                                fn convert_param<'a, 'ctx, D>(
                                    param: &'a (
                                        Option<CustomBlockParamKind>,
                                        Option<CanonicalIdentifier>,
                                        SingleIdentifier,
                                        Option<cst_types::Comma>,
                                    ),
                                    allocator: &'a D,
                                    context: &mut Context<'ctx>,
                                ) -> (DocBuilder<'a, D, ()>, Option<&'a cst_types::Comma>)
                                where
                                    'ctx: 'a,
                                    D: DocAllocator<'a, ()>,
                                    D::Doc: Clone,
                                {
                                    let mut value = allocator.nil();
                                    if let Some(param_kind) = &param.0 {
                                        value = value
                                            .append(param_kind.into_pretty_doc(allocator, context))
                                            .append(ws(Whitespace::Space, allocator, context));
                                    }
                                    if let Some(canonical_identifier) = &param.1 {
                                        value = value
                                            .append(
                                                canonical_identifier
                                                    .into_pretty_doc(allocator, context),
                                            )
                                            .append(ws(Whitespace::Space, allocator, context));
                                    }
                                    (
                                        value.append(param.2.into_pretty_doc(allocator, context)),
                                        param.3.as_ref(),
                                    )
                                }
                                let mut items = parameters.iter();
                                let Some((mut value, mut comma)) = items
                                    .next()
                                    .map(|value| convert_param(value, allocator, context))
                                else {
                                    unreachable!();
                                };
                                for item in items {
                                    if let Some(comma) = comma {
                                        value = value
                                            .append(pretty_token!(",", comma, allocator, context))
                                            .append(ws(Whitespace::Line, allocator, context));
                                    }
                                    let (item, new_comma) = convert_param(item, allocator, context);
                                    value = value.append(item);
                                    comma = new_comma;
                                }
                                value
                                    .append(if let Some(comma) = comma {
                                        pretty_token!("", comma, allocator, context)
                                    } else {
                                        flush_whitespace(allocator, context)
                                    })
                                    .nest(context.indentation)
                                    .append(allocator.text(",").flat_alt(allocator.nil()))
                            })
                            .append(allocator.line_())
                            .group()
                    })
                    .append(pretty_token!(")", right_parens, allocator, context))
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(code_block.into_pretty_doc(allocator, context))
            }
            SpriteStatement::IsolatedBlock(code_block, _, _) => {
                code_block.into_pretty_doc(allocator, context)
            }
            SpriteStatement::IsolatedExpression(left_parens, expression, right_parens, _, _) => {
                pretty_token!("(", left_parens, allocator, context)
                    .append(
                        ws(Whitespace::Line_, allocator, context)
                            .append(expression.into_pretty_doc(allocator, context))
                            .nest(context.indentation)
                            .append(ws(Whitespace::Line_, allocator, context))
                            .append(flush_whitespace(allocator, context))
                            .group(),
                    )
                    .append(pretty_token!(")", right_parens, allocator, context))
            }
            SpriteStatement::ConfigStatement(
                config_keyword,
                left_brace,
                items,
                right_brace,
                _,
                _,
            ) => pretty_token!("config", config_keyword, allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(wrap_csv!(
                    items,
                    left_brace,
                    right_brace,
                    BRACES,
                    allocator,
                    context
                )),
            SpriteStatement::MonitorDeclaration(
                monitor_keyword,
                value,
                left_brace,
                configuration,
                right_brace,
                _,
                _,
            ) => pretty_token!("monitor", monitor_keyword, allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(value.into_pretty_doc(allocator, context))
                .append(ws(Whitespace::Space, allocator, context))
                .append(wrap_csv!(
                    configuration,
                    left_brace,
                    right_brace,
                    BRACES,
                    allocator,
                    context
                )),
            SpriteStatement::UseStatement(
                use_keyword,
                extension_keyword,
                use_statement_content,
                semicolon,
                _,
            ) => {
                pretty_token!("use", use_keyword, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(if let Some(extension_keyword) = extension_keyword {
                        pretty_token!("extension", extension_keyword, allocator, context)
                            .append(ws(Whitespace::Space, allocator, context))
                    } else {
                        allocator.nil()
                    })
                    .append(use_statement_content.into_pretty_doc(allocator, context))
                    .append(pretty_token!(";", semicolon, allocator, context))
            }
            SpriteStatement::EmptyStatement(semicolon) => {
                pretty_token!(";", semicolon, allocator, context)
            }
            SpriteStatement::Invalid(source_span) => {
                pretty_token_span!("INVALID;", source_span.0, allocator, context)
            }
        }
    }
}

impl UnparseCSTFormatted for &StageStatement {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            StageStatement::DataDeclaration(let_keyword, data_declaration, semicolon, _) => {
                pretty_token!("let", let_keyword, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(data_declaration.into_pretty_doc(allocator, context))
                    .append(pretty_token!(";", semicolon, allocator, context))
            }
            StageStatement::BackdropDeclaration(
                backdrop_keyword,
                asset_declaration,
                semicolon,
                _,
            ) => pretty_token!("backdrop", backdrop_keyword, allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(asset_declaration.into_pretty_doc(allocator, context))
                .append(pretty_token!(";", semicolon, allocator, context)),
            StageStatement::SoundDeclaration(sound_keyword, asset_declaration, semicolon, _) => {
                pretty_token!("sound", sound_keyword, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(asset_declaration.into_pretty_doc(allocator, context))
                    .append(pretty_token!(";", semicolon, allocator, context))
            }
            StageStatement::NoInputHatStatement(hat_function, code_block, _, _) => hat_function
                .into_pretty_doc(allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(code_block.into_pretty_doc(allocator, context)),
            StageStatement::SingleInputHatStatement(hat_function, argument, code_block, _, _) => {
                hat_function
                    .into_pretty_doc(allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(argument.into_pretty_doc(allocator, context))
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(code_block.into_pretty_doc(allocator, context))
            }
            StageStatement::MultiInputHatStatement(
                hat_function,
                left_parens,
                arguments,
                right_parens,
                code_block,
                _,
                _,
            ) => {
                let flushed = flush_before_group!(hat_function, allocator, context);
                let mut value = hat_function
                    .into_pretty_doc(allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(if arguments.is_empty() {
                        allocator.nil()
                    } else {
                        wrap_csv!(
                            arguments,
                            left_parens,
                            right_parens,
                            PARENS,
                            allocator,
                            context
                        )
                        .append(ws(Whitespace::Space, allocator, context))
                    })
                    .append(code_block.into_pretty_doc(allocator, context));
                if arguments.len() == 1
                    && let Some((argument, _)) = arguments.iter().next()
                {
                    value = value
                        .flat_alt(
                            hat_function
                                .into_pretty_doc(allocator, context)
                                .append(ws(Whitespace::Space, allocator, context))
                                .append(argument.into_pretty_doc(allocator, context))
                                .append(ws(Whitespace::Space, allocator, context))
                                .append(code_block.into_pretty_doc(allocator, context)),
                        )
                        .group();
                }
                flushed.append(value)
            }
            StageStatement::CustomBlockDefinition(
                is_warp,
                proc_keyword,
                canonical_identifier,
                identifier,
                left_parens,
                parameters,
                right_parens,
                code_block,
                _,
                _,
            ) => {
                let mut value = allocator.nil();
                if let Some(is_warp) = is_warp {
                    value = value
                        .append(is_warp.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value = value
                    .append(pretty_token!("proc", proc_keyword, allocator, context))
                    .append(ws(Whitespace::Space, allocator, context));
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, context))
                        .append(ws(Whitespace::Space, allocator, context));
                }
                value
                    .append(identifier.into_pretty_doc(allocator, context))
                    .append(pretty_token!("(", left_parens, allocator, context))
                    .append(if parameters.is_empty() {
                        allocator.nil()
                    } else {
                        ws(Whitespace::Line_, allocator, context)
                            .append({
                                fn convert_param<'a, 'ctx, D>(
                                    param: &'a (
                                        Option<CustomBlockParamKind>,
                                        Option<CanonicalIdentifier>,
                                        SingleIdentifier,
                                        Option<cst_types::Comma>,
                                    ),
                                    allocator: &'a D,
                                    context: &mut Context<'ctx>,
                                ) -> (DocBuilder<'a, D, ()>, Option<&'a cst_types::Comma>)
                                where
                                    'ctx: 'a,
                                    D: DocAllocator<'a, ()>,
                                    D::Doc: Clone,
                                {
                                    let mut value = allocator.nil();
                                    if let Some(param_kind) = &param.0 {
                                        value = value
                                            .append(param_kind.into_pretty_doc(allocator, context))
                                            .append(ws(Whitespace::Space, allocator, context));
                                    }
                                    if let Some(canonical_identifier) = &param.1 {
                                        value = value
                                            .append(
                                                canonical_identifier
                                                    .into_pretty_doc(allocator, context),
                                            )
                                            .append(ws(Whitespace::Space, allocator, context));
                                    }
                                    (
                                        value.append(param.2.into_pretty_doc(allocator, context)),
                                        param.3.as_ref(),
                                    )
                                }
                                let mut items = parameters.iter();
                                let Some((mut value, mut comma)) = items
                                    .next()
                                    .map(|value| convert_param(value, allocator, context))
                                else {
                                    unreachable!();
                                };
                                for item in items {
                                    if let Some(comma) = comma {
                                        value = value
                                            .append(pretty_token!(",", comma, allocator, context))
                                            .append(ws(Whitespace::Line, allocator, context));
                                    }
                                    let (item, new_comma) = convert_param(item, allocator, context);
                                    value = value.append(item);
                                    comma = new_comma;
                                }
                                value
                                    .append(if let Some(comma) = comma {
                                        pretty_token!("", comma, allocator, context)
                                    } else {
                                        flush_whitespace(allocator, context)
                                    })
                                    .nest(context.indentation)
                                    .append(allocator.text(",").flat_alt(allocator.nil()))
                            })
                            .append(allocator.line_())
                            .group()
                    })
                    .append(pretty_token!(")", right_parens, allocator, context))
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(code_block.into_pretty_doc(allocator, context))
            }
            StageStatement::IsolatedBlock(code_block, _, _) => {
                code_block.into_pretty_doc(allocator, context)
            }
            StageStatement::IsolatedExpression(left_parens, expression, right_parens, _, _) => {
                pretty_token!("(", left_parens, allocator, context)
                    .append(
                        ws(Whitespace::Line_, allocator, context)
                            .append(expression.into_pretty_doc(allocator, context))
                            .nest(context.indentation)
                            .append(ws(Whitespace::Line_, allocator, context))
                            .append(flush_whitespace(allocator, context))
                            .group(),
                    )
                    .append(pretty_token!(")", right_parens, allocator, context))
            }
            StageStatement::ConfigStatement(
                config_keyword,
                left_brace,
                items,
                right_brace,
                _,
                _,
            ) => pretty_token!("config", config_keyword, allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(wrap_csv!(
                    items,
                    left_brace,
                    right_brace,
                    BRACES,
                    allocator,
                    context
                )),
            StageStatement::MonitorDeclaration(
                monitor_keyword,
                value,
                left_brace,
                configuration,
                right_brace,
                _,
                _,
            ) => pretty_token!("monitor", monitor_keyword, allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(value.into_pretty_doc(allocator, context))
                .append(ws(Whitespace::Space, allocator, context))
                .append(wrap_csv!(
                    configuration,
                    left_brace,
                    right_brace,
                    BRACES,
                    allocator,
                    context
                )),
            StageStatement::UseStatement(
                use_keyword,
                extension_keyword,
                use_statement_content,
                semicolon,
                _,
            ) => {
                pretty_token!("use", use_keyword, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(if let Some(extension_keyword) = extension_keyword {
                        pretty_token!("extension", extension_keyword, allocator, context)
                            .append(ws(Whitespace::Space, allocator, context))
                    } else {
                        allocator.nil()
                    })
                    .append(use_statement_content.into_pretty_doc(allocator, context))
                    .append(pretty_token!(";", semicolon, allocator, context))
            }
            StageStatement::EmptyStatement(semicolon) => {
                pretty_token!(";", semicolon, allocator, context)
            }
            StageStatement::Invalid(source_span) => {
                pretty_token_span!("INVALID;", source_span.0, allocator, context)
            }
        }
    }
}

impl UnparseCSTFormatted for &AssetDeclaration {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            AssetDeclaration::Multiple(left_parens, single_asset_declarations, right_parens, _) => {
                wrap_csv!(
                    single_asset_declarations,
                    left_parens,
                    right_parens,
                    PARENS,
                    allocator,
                    context
                )
            }
            AssetDeclaration::Single(single_asset_declaration) => {
                single_asset_declaration.into_pretty_doc(allocator, context)
            }
        }
    }
}

impl SingleAssetDeclarationValue {
    fn requires_whitespace_prefix(&self) -> bool {
        matches!(self, SingleAssetDeclarationValue::Dictionary(..))
    }
}

impl UnparseCSTFormatted for &SingleAssetDeclaration {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        self.0
            .as_ref()
            .map(|value| {
                value.into_pretty_doc(allocator, context).append(ws(
                    Whitespace::Space,
                    allocator,
                    context,
                ))
            })
            .unwrap_or_else(|| allocator.nil())
            .append(self.1.into_pretty_doc(allocator, context))
            .append(if self.2.requires_whitespace_prefix() {
                ws(Whitespace::Space, allocator, context)
            } else {
                allocator.nil()
            })
            .append(self.2.into_pretty_doc(allocator, context))
    }
}

impl UnparseCSTFormatted for &SingleAssetDeclarationValue {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            SingleAssetDeclarationValue::Simple(left_parens, value, right_parens, _) => {
                pretty_token!("(", left_parens, allocator, context)
                    .append(pretty_token_span!(
                        format!("\"{}\"", string_escape::normal_string_escaper(&value.0)),
                        value.1.0,
                        allocator,
                        context
                    ))
                    .append(pretty_token!(")", right_parens, allocator, context))
            }
            SingleAssetDeclarationValue::Dictionary(left_brace, items, right_brace, _) => {
                wrap_csv!(items, left_brace, right_brace, BRACES, allocator, context)
            }
        }
    }
}

impl UnparseCSTFormatted for &DictionaryEntry {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            DictionaryEntry::Valid(identifier, colon, value, _) => identifier
                .into_pretty_doc(allocator, context)
                .append(pretty_token!(":", colon, allocator, context))
                .append(ws(Whitespace::Space, allocator, context))
                .append(value.into_pretty_doc(allocator, context)),
            DictionaryEntry::Invalid(source_span) => {
                pretty_token_span!("INVALID", source_span.0, allocator, context)
            }
        }
    }
}

impl UnparseCSTFormatted for &DictionaryValue {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            DictionaryValue::Primitive(value) => value.into_pretty_doc(allocator, context),
            DictionaryValue::Dictionary(left_brace, items, right_brace, _) => {
                wrap_csv!(items, left_brace, right_brace, BRACES, allocator, context)
            }
            DictionaryValue::List(left_bracket, items, right_bracket, _) => {
                wrap_csv!(
                    items,
                    left_bracket,
                    right_bracket,
                    BRACKETS,
                    allocator,
                    context
                )
            }
            DictionaryValue::Invalid(source_span) => {
                pretty_token_span!("INVALID", source_span.0, allocator, context)
            }
        }
    }
}

impl UnparseCSTFormatted for &WarpSpecifier {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        pretty_token_span!(
            if self.is_warp { "warp" } else { "nowarp" },
            self.source_span.0,
            allocator,
            context
        )
    }
}

impl UnparseCSTFormatted for &CustomBlockParamKind {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        pretty_token_span!(
            match self.kind {
                cst_types::CustomBlockParamKindValue::Number => "num",
                cst_types::CustomBlockParamKindValue::String => "str",
                cst_types::CustomBlockParamKindValue::Boolean => "bool",
            },
            self.source_span.0,
            allocator,
            context
        )
    }
}

impl UnparseCSTFormatted for &MonitorValue {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            MonitorValue::Identifier(identifier) => identifier.into_pretty_doc(allocator, context),
            MonitorValue::Call(function, left_parens, arguments, right_parens, _) => function
                .into_pretty_doc(allocator, context)
                .append(wrap_csv!(
                    arguments,
                    left_parens,
                    right_parens,
                    PARENS,
                    allocator,
                    context
                )),
        }
    }
}

impl UnparseCSTFormatted for &TopLevelStatement {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            TopLevelStatement::Stage(stage_keyword, code_block, _, _) => {
                pretty_token!("stage", stage_keyword, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(code_block.into_pretty_doc(allocator, context))
            }
            TopLevelStatement::Sprite(
                sprite_keyword,
                canonical_identifier,
                identifier,
                code_block,
                _,
                _,
            ) => pretty_token!("sprite", sprite_keyword, allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(
                    canonical_identifier
                        .as_ref()
                        .map(|value| {
                            value.into_pretty_doc(allocator, context).append(ws(
                                Whitespace::Space,
                                allocator,
                                context,
                            ))
                        })
                        .unwrap_or_else(|| allocator.nil()),
                )
                .append(identifier.into_pretty_doc(allocator, context))
                .append(ws(Whitespace::Space, allocator, context))
                .append(code_block.into_pretty_doc(allocator, context)),
            TopLevelStatement::BroadcastDeclaration(
                broadcast_keyword,
                canonical_identifier,
                identifier,
                semicolon,
                _,
            ) => pretty_token!("broadcast", broadcast_keyword, allocator, context)
                .append(ws(Whitespace::Space, allocator, context))
                .append(
                    canonical_identifier
                        .as_ref()
                        .map(|value| value.into_pretty_doc(allocator, context).append(" "))
                        .unwrap_or_else(|| allocator.nil()),
                )
                .append(identifier.into_pretty_doc(allocator, context))
                .append(pretty_token!(";", semicolon, allocator, context)),
            TopLevelStatement::UseStatement(
                use_keyword,
                extension_keyword,
                use_statement_content,
                semicolon,
                _,
            ) => {
                pretty_token!("use", use_keyword, allocator, context)
                    .append(ws(Whitespace::Space, allocator, context))
                    .append(if let Some(extension_keyword) = extension_keyword {
                        pretty_token!("extension", extension_keyword, allocator, context)
                            .append(ws(Whitespace::Space, allocator, context))
                    } else {
                        allocator.nil()
                    })
                    .append(use_statement_content.into_pretty_doc(allocator, context))
                    .append(pretty_token!(";", semicolon, allocator, context))
            }
            TopLevelStatement::EmptyStatement(semicolon) => {
                pretty_token!(";", semicolon, allocator, context)
            }
            TopLevelStatement::Invalid(source_span) => {
                pretty_token_span!("INVALID;", source_span.0, allocator, context)
            }
        }
    }
}

impl UnparseCSTFormatted for &GrazeProgram {
    fn into_pretty_doc<'a, 'ctx, D>(
        self,
        allocator: &'a D,
        context: &mut Context<'ctx>,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        'ctx: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        intersperse_top_level_statement_end(self.0.iter(), allocator, context)
            .append(allocator.hardline())
    }
}
