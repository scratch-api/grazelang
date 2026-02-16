use std::ops::Range;
use std::sync::OnceLock;

use arcstr::ArcStr as IString; // Immutable string
use arcstr::literal as literal_istring;
use logos::{Lexer, Logos};
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::from_str as json_from_str;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Logos)]
#[logos(extras = (Vec<usize>, usize))]
#[logos(skip r"[ \t\f]+")]
#[logos(skip(r"\n|\r\n?", register_newline))]
#[logos(skip r"//.*")]
#[logos(skip r"/\*(?:[^*]|(?:\*[^/]))*\*/")]
pub enum Token {
    #[token("sprite")]
    SpriteKeyword,
    #[token("stage")]
    StageKeyword,
    #[token("proc")]
    ProcKeyword,
    #[token("warp")]
    WarpKeyword,
    #[token("nowarp")]
    NowarpKeyword,
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
    #[token("vars")]
    VarsKeyword,
    #[token("list")]
    ListKeyword,
    #[token("lists")]
    ListsKeyword,
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
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("::")]
    ScopeResolution,
    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("!")]
    Not,
    #[regex(r"[eE]\s*\^")]
    Exp,
    #[regex(r"10\s*\^")]
    Pow,
    #[token("*")]
    Times,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,
    #[token("++")]
    #[token("join")]
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
    #[regex(r#""(?:[^\\"$]|(?:\\.))*""#, parse_simple_string_literal)]
    SimpleString(IString),
    #[regex(r#"`(?:[^\\`]|(?:\\.))*`"#, parse_canonical_name)]
    CanonicalName(IString),
    #[regex(r#"\w+"#, parse_string)]
    Identifier(IString),
    #[regex(r#"\$\w+"#, parse_string)]
    MacroIdentifier(IString),
    #[regex(r#"[+-]?[0-9](?:_?[0-9])*"#, parse_number, priority = 3)]
    DecimalInt(IString),
    #[regex(
        r#"[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?(?:[eE][+-]?[0-9](?:_?[0-9])*)?"#,
        parse_number
    )]
    #[regex(r#"\.[0-9](?:_?[0-9])*(?:[eE][+-]?[0-9](?:_?[0-9])*)?"#, parse_number)]
    #[regex(r#"[0-9](?:_?[0-9])*[eE][+-]?[0-9](?:_?[0-9])*"#, parse_number)]
    #[regex(r#"[+-]?Infinity"#, parse_string)]
    #[token("NaN", |_| literal_istring!("NaN"))]
    DecimalFloat(IString),
    #[regex(r#"0x[0-9a-fA-F](?:_?[0-9a-fA-F])*"#, parse_number)]
    HexadecimalInt(IString),
    #[regex(r#"0o[0-7](?:_?[0-7])*"#, parse_number)]
    OctalInt(IString),
    #[regex(r#"0b[01](?:_?[01])*"#, parse_number)]
    BinaryInt(IString),
    #[regex(r#""(?:[^"$]|(?:\\.))*\$\{"#, parse_left_format_string)]
    LeftFormattedString(IString),
    // #[regex(r#"\}(?:[^"$]|(?:\\.))*\$\{"#, parse_middle_format_string)]
    // MiddleFormattedString(IString),
    // #[regex(r#"\}(?:[^"$]|(?:\\.))*""#, parse_right_format_string)]
    // RightFormattedString(IString),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LexedRightBrace {
    Normal,
    MiddleFormattedString(IString),
    RightFormattedString(IString),
}

pub fn parse_simple_string_literal(lex: &mut Lexer<Token>) -> Option<IString> {
    json_from_str::<'_, IString>(lex.slice()).ok()
}

pub fn parse_canonical_name(lex: &mut Lexer<Token>) -> Option<IString> {
    let slice = lex.slice();
    let inner = &slice[1..slice.len() - 1];
    json_from_str::<'_, IString>(
        format!("\"{}\"", inner.replace('"', "\\\"").replace("\\`", "`")).as_str(),
    )
    .ok()
}

pub fn parse_string(lex: &mut Lexer<Token>) -> IString {
    lex.slice().into()
}

pub fn parse_number(lex: &mut Lexer<Token>) -> IString {
    lex.slice().replace('_', "").into()
}

pub fn parse_left_format_string(lex: &mut Lexer<Token>) -> Option<IString> {
    lex.extras.1 += 1;
    let slice = lex.slice();
    let json = slice[..slice.len() - 2].to_string() + "\"";
    json_from_str::<'_, IString>(&json).ok()
}

fn middle_regex_continuation() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r#"^(?:[^"$]|(?:\\.))*\$\{"#).unwrap())
}

fn end_regex_continuation() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r#"^(?:[^"$]|(?:\\.))*""#).unwrap())
}

pub fn handle_right_brace(lex: &mut Lexer<Token>) -> Option<LexedRightBrace> {
    if lex.extras.1 <= 0 {
        return Some(LexedRightBrace::Normal);
    }
    let remainder = lex.remainder();
    if let Some(mat) = middle_regex_continuation().find(remainder) {
        let match_len = mat.end();
        lex.bump(match_len);
        return Some(LexedRightBrace::MiddleFormattedString(parse_middle_format_string( mat.as_str())?));
    }
    if let Some(mat) = end_regex_continuation().find(remainder) {
        let match_len = mat.end();
        lex.bump(match_len);
        lex.extras.1 -= 1;
        return Some(LexedRightBrace::RightFormattedString(parse_right_format_string(mat.as_str())?));
    }
    None
}

pub fn parse_middle_format_string(slice: &str) -> Option<IString> {
    let json = String::from("\"") + &slice[..slice.len() - 2] + "\"";
    json_from_str::<'_, IString>(&json).ok()
}

pub fn parse_right_format_string(slice: &str) -> Option<IString> {
    let json = String::from("\"") + &slice[..];
    json_from_str::<'_, IString>(&json).ok()
}

pub fn register_newline(lex: &mut Lexer<Token>) {
    lex.extras.0.push(lex.span().end);
}

pub type PosRange = ((usize, usize), (usize, usize));

pub fn get_position(lex: &Lexer<Token>, character_index: usize) -> (usize, usize) {
    let last_newline_index = *match lex.extras.0.last() {
        Some(value) => value,
        None => return (0, character_index),
    };
    if last_newline_index > character_index {
        todo!()
    }
    (lex.extras.0.len() - 1, character_index - last_newline_index)
}

pub fn get_pos_range(lex: &Lexer<Token>, character_range: Range<usize>) -> PosRange {
    (
        get_position(lex, character_range.start),
        get_position(lex, character_range.end),
    )
}
