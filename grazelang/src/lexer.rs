use std::sync::OnceLock;
use std::{num::NonZeroUsize, ops::Range};

use arcstr::ArcStr as IString; // Immutable string
use logos::{Lexer, Logos};
use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::utils::string_unescape::{
    unescape_canonical_name, unescape_format_string, unescape_normal_string,
};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct LexerExtras<'a> {
    pub line_starts: Vec<usize>,
    pub formatting_string_nesting: usize,
    pub style_tokens: Vec<StyleToken<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StyleToken<'a> {
    EmptyLines(NonZeroUsize, TextSpan),
    LineComment(&'a str, TextSpan),
    MultiLineComment(&'a str, TextSpan),
}

impl<'a> StyleToken<'a> {
    pub fn get_text_span(&self) -> TextSpan {
        match self {
            StyleToken::EmptyLines(_, value)
            | StyleToken::LineComment(_, value)
            | StyleToken::MultiLineComment(_, value) => *value,
        }
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Logos)]
#[logos(extras = LexerExtras<'s>)]
#[logos(skip(r"[ \t\f\n\r]+", register_newlines::<true>))]
#[logos(skip(r"//[^\n]*", register_line_comment, allow_greedy = true))]
#[logos(skip(r"/\*(?:[^*]|(?:\*[^/]))*\*/", register_multi_line_comment))]
pub enum Token {
    #[token("sprite")]
    SpriteKeyword,
    #[token("config")]
    ConfigKeyword,
    #[token("monitor")]
    MonitorKeyword,
    #[token("proc")]
    ProcKeyword,
    #[token("warp")]
    WarpKeyword,
    #[token("nowarp")]
    NowarpKeyword,
    #[token("use")]
    UseKeyword,
    #[token("as")]
    AsKeyword,
    #[token("let")]
    LetKeyword,
    #[token("cloud")]
    CloudKeyword,
    #[token("global")]
    GlobalKeyword,
    #[token("local")]
    LocalKeyword,
    #[token("var")]
    VarKeyword,
    #[token("list")]
    ListKeyword,
    #[token("{")]
    LeftBrace,
    #[token("}", handle_right_brace)]
    RightBrace(LexedRightBrace),
    #[token("(")]
    LeftParens,
    #[token(")")]
    RightParens,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("@[")]
    LetterAccessLeftBracket,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("::")]
    DoubleColon,
    #[token("=")]
    Assign,
    #[token(":")]
    Colon,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("!")]
    Not,
    #[regex(r"[eE] *\^")]
    Exp,
    #[regex(r"10 *\^")]
    Pow,
    #[token("*")]
    Times,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,
    #[token("++")]
    Join,
    #[token("==")]
    Equals,
    #[token("!=")]
    NotEquals,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessThanOrEqual,
    #[token(">=")]
    GreaterThanOrEqual,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("..")]
    Unwrap,
    #[regex(r#""(?:[^\\"$\r\n]|(?:\\.))*""#, parse_simple_string_literal)]
    SimpleString(IString),
    #[regex(r#"`(?:[^\\`\r\n]|(?:\\.))*`"#, parse_canonical_name)]
    CanonicalIdentifier(IString),
    #[regex(r#"[a-zA-Z_]\w*"#, parse_string, priority = 3)]
    Identifier(IString),
    #[regex(r#"\$\w+"#, parse_string)]
    MacroIdentifier(IString),
    #[regex(r#"[0-9](?:_?[0-9])*"#, parse_number, priority = 3)]
    DecimalInt(IString),
    #[regex(
        r#"[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?(?:[eE][+-]?[0-9](?:_?[0-9])*)?"#,
        parse_number
    )]
    #[regex(r#"\.[0-9](?:_?[0-9])*(?:[eE][+-]?[0-9](?:_?[0-9])*)?"#, parse_number)]
    #[regex(r#"[0-9](?:_?[0-9])*[eE][+-]?[0-9](?:_?[0-9])*"#, parse_number)]
    #[regex(r#"Infinity"#, parse_string)]
    #[regex(r#"NaN"#, parse_string)]
    DecimalFloat(IString),
    #[regex(r#"0[xX][0-9a-fA-F](?:_?[0-9a-fA-F])*"#, parse_number)]
    HexadecimalInt(IString),
    #[regex(r#"0[oO][0-7](?:_?[0-7])*"#, parse_number)]
    OctalInt(IString),
    #[regex(r#"0[bB][01](?:_?[01])*"#, parse_number)]
    BinaryInt(IString),
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),
    #[regex(
        r#""(?:[^\\"$\r\n]|(?:\\.)|(?:\$[^\\"{\r\n]))*\$\{"#,
        parse_left_format_string
    )]
    LeftFormattedString(IString),
}

// TODO: Add `+=` etc
// Issue: #88

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::SpriteKeyword => write!(f, "\"sprite\""),
            Token::ConfigKeyword => write!(f, "\"config\""),
            Token::MonitorKeyword => write!(f, "\"monitor\""),
            Token::ProcKeyword => write!(f, "\"proc\""),
            Token::WarpKeyword => write!(f, "\"warp\""),
            Token::NowarpKeyword => write!(f, "\"nowarp\""),
            Token::UseKeyword => write!(f, "\"use\""),
            Token::AsKeyword => write!(f, "\"as\""),
            Token::LetKeyword => write!(f, "\"let\""),
            Token::CloudKeyword => write!(f, "\"cloud\""),
            Token::GlobalKeyword => write!(f, "\"global\""),
            Token::LocalKeyword => write!(f, "\"local\""),
            Token::VarKeyword => write!(f, "\"var\""),
            Token::ListKeyword => write!(f, "\"list\""),
            Token::LeftBrace => write!(f, "'{{'"),
            Token::RightBrace(_) => write!(f, "'}}'"),
            Token::LeftParens => write!(f, "'('"),
            Token::RightParens => write!(f, "')'"),
            Token::LeftBracket => write!(f, "'['"),
            Token::RightBracket => write!(f, "']'"),
            Token::Semicolon => write!(f, "';'"),
            Token::Comma => write!(f, "','"),
            Token::Dot => write!(f, "'.'"),
            Token::Assign => write!(f, "'='"),
            Token::Colon => write!(f, "':'"),
            Token::Plus => write!(f, "'+'"),
            Token::Minus => write!(f, "'-'"),
            Token::Not => write!(f, "'!'"),
            Token::Times => write!(f, "'*'"),
            Token::Div => write!(f, "'/'"),
            Token::Mod => write!(f, "'%'"),
            Token::LessThan => write!(f, "'<'"),
            Token::GreaterThan => write!(f, "'>'"),
            Token::LetterAccessLeftBracket => write!(f, "\"@[\""),
            Token::DoubleColon => write!(f, "\"::\""),
            Token::Exp => write!(f, "\"e^\""),
            Token::Pow => write!(f, "\"10^\""),
            Token::Join => write!(f, "\"join\""),
            Token::Equals => write!(f, "\"==\""),
            Token::NotEquals => write!(f, "\"!=\""),
            Token::LessThanOrEqual => write!(f, "\"<=\""),
            Token::GreaterThanOrEqual => write!(f, "\">=\""),
            Token::And => write!(f, "\"&&\""),
            Token::Or => write!(f, "\"||\""),
            Token::Unwrap => write!(f, "\"..\""),
            Token::SimpleString(val) => write!(f, "\"{}\"", val),
            Token::CanonicalIdentifier(val) => write!(f, "\"{}\"", val),
            Token::Identifier(val) => write!(f, "\"{}\"", val),
            Token::MacroIdentifier(val) => write!(f, "\"{}\"", val),
            Token::DecimalInt(val) => write!(f, "\"{}\"", val),
            Token::DecimalFloat(val) => write!(f, "\"{}\"", val),
            Token::HexadecimalInt(val) => write!(f, "\"{}\"", val),
            Token::OctalInt(val) => write!(f, "\"{}\"", val),
            Token::BinaryInt(val) => write!(f, "\"{}\"", val),
            Token::Bool(val) => write!(f, "\"{}\"", val),
            Token::LeftFormattedString(val) => write!(f, "\"{}\"", val),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LexedRightBrace {
    Normal,
    MiddleFormattedString(IString),
    RightFormattedString(IString),
}

pub fn parse_simple_string_literal(lex: &mut Lexer<Token>) -> Option<IString> {
    let slice = lex.slice();
    unescape_normal_string(&slice[1..slice.len() - 1])
        .map(Into::into)
        .ok()
}

pub fn parse_canonical_name(lex: &mut Lexer<Token>) -> Option<IString> {
    let slice = lex.slice();
    unescape_canonical_name(&slice[1..slice.len() - 1])
        .map(Into::into)
        .ok()
}

pub fn parse_string(lex: &mut Lexer<Token>) -> IString {
    lex.slice().into()
}

pub fn parse_number(lex: &mut Lexer<Token>) -> IString {
    lex.slice()
        //.replace('_', "")
        .into()
}

pub fn parse_left_format_string(lex: &mut Lexer<Token>) -> Option<IString> {
    lex.extras.formatting_string_nesting += 1;
    let slice = lex.slice();
    unescape_format_string(&slice[1..slice.len() - 2])
        .map(Into::into)
        .ok()
}

fn middle_regex_continuation() -> &'static Regex {
    static REGEX_LOCK: OnceLock<Regex> = OnceLock::new();
    REGEX_LOCK
        .get_or_init(|| Regex::new(r#"^(?:[^\\"$\r\n]|(?:\\.)|(?:\$[^\\"{\r\n]))*\$\{"#).unwrap())
}

fn end_regex_continuation() -> &'static Regex {
    static REGEX_LOCK: OnceLock<Regex> = OnceLock::new();
    REGEX_LOCK
        .get_or_init(|| Regex::new(r#"^(?:[^\\"$\r\n]|(?:\\.)|(?:\$[^\\"{\r\n]))*""#).unwrap())
}

pub fn handle_right_brace(lex: &mut Lexer<Token>) -> Option<LexedRightBrace> {
    if lex.extras.formatting_string_nesting == 0 {
        return Some(LexedRightBrace::Normal);
    }
    let remainder = lex.remainder();
    if let Some(mat) = middle_regex_continuation().find(remainder) {
        let match_len = mat.end();
        lex.bump(match_len);
        return Some(LexedRightBrace::MiddleFormattedString(
            parse_middle_format_string(mat.as_str())?,
        ));
    }
    if let Some(mat) = end_regex_continuation().find(remainder) {
        let match_len = mat.end();
        lex.bump(match_len);
        lex.extras.formatting_string_nesting -= 1;
        return Some(LexedRightBrace::RightFormattedString(
            parse_right_format_string(mat.as_str())?,
        ));
    }
    None
}

pub fn parse_middle_format_string(slice: &str) -> Option<IString> {
    unescape_format_string(&slice[..slice.len() - 2])
        .map(Into::into)
        .ok()
}

pub fn parse_right_format_string(slice: &str) -> Option<IString> {
    unescape_format_string(&slice[..slice.len() - 1])
        .map(Into::into)
        .ok()
}

pub fn register_newlines<const W: bool>(lex: &mut Lexer<Token>) {
    let mut extras = Vec::new();
    let slice: &str = lex.slice();
    let span = lex.span();
    let start_offset = span.start;
    let mut prev_is_return = false;
    for (i, b) in slice.bytes().enumerate() {
        if match b {
            b'\r' => {
                prev_is_return = true;
                true
            }
            b'\n' if !prev_is_return => {
                prev_is_return = false;
                true
            }
            _ => {
                prev_is_return = false;
                false
            }
        } {
            extras.push(start_offset + i + 1);
        }
    }
    if W && let Some(lines) = NonZeroUsize::new(extras.len()) {
        let text_span = (
            (
                lex.extras.line_starts.len(),
                span.start - lex.extras.line_starts.last().copied().unwrap_or_default(),
            ),
            (
                lex.extras.line_starts.len() + extras.len(),
                span.end
                    - extras
                        .last()
                        .or_else(|| lex.extras.line_starts.last())
                        .copied()
                        .unwrap_or_default(),
            ),
        );
        lex.extras
            .style_tokens
            .push(StyleToken::EmptyLines(lines, text_span));
    }
    lex.extras.line_starts.extend(extras);
}

pub fn register_multi_line_comment(lex: &mut Lexer<Token>) {
    let (line_starts_len_before, final_line_start_before) = (
        lex.extras.line_starts.len(),
        lex.extras.line_starts.last().copied().unwrap_or_default(),
    );
    register_newlines::<false>(lex);
    let slice = lex.slice();
    let span = lex.span();
    let text_span = (
        (
            line_starts_len_before,
            span.start + 2 - final_line_start_before,
        ),
        (
            lex.extras.line_starts.len(),
            span.end - 2 - lex.extras.line_starts.last().copied().unwrap_or_default(),
        ),
    );
    lex.extras.style_tokens.push(StyleToken::MultiLineComment(
        &slice[2..slice.len() - 2],
        text_span,
    ));
}

pub fn register_line_comment(lex: &mut Lexer<Token>) {
    let span = lex.span();
    let (line_starts_len, final_line_start) = (
        lex.extras.line_starts.len(),
        lex.extras.line_starts.last().copied().unwrap_or_default(),
    );
    let text_span = (
        (line_starts_len, span.start + 2 - final_line_start),
        (line_starts_len, span.end - final_line_start),
    );
    lex.extras
        .style_tokens
        .push(StyleToken::LineComment(&lex.slice()[2..], text_span));
}

pub type TextSpan = ((usize, usize), (usize, usize));
pub type SourceFileId = u32;
pub type SourceSpan = (TextSpan, SourceFileId);

pub fn get_position(lex: &Lexer<Token>, character_index: usize) -> (usize, usize) {
    let last_newline_index = *match lex.extras.line_starts.last() {
        Some(value) => value,
        None => return (0, character_index),
    };
    if last_newline_index > character_index {
        let line = lex
            .extras
            .line_starts
            .partition_point(|&x| x <= character_index);
        let line_start = if line == 0 {
            0
        } else {
            lex.extras.line_starts[line - 1]
        };
        return (line, character_index - line_start);
    }
    (
        lex.extras.line_starts.len(),
        character_index - last_newline_index,
    )
}

pub fn get_source_span(lex: &Lexer<Token>, character_range: Range<usize>) -> TextSpan {
    (
        get_position(lex, character_range.start),
        get_position(lex, character_range.end),
    )
}

#[inline]
pub fn create_lexer<'a>(input: &'a str) -> Lexer<'a, Token> {
    Token::lexer(input)
}
