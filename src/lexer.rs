use logos::{Lexer, Logos};
use serde::{Serialize, Deserialize};
use arcstr::ArcStr as IString; // Immutable string
use arcstr::literal as literal_istring;
use serde_json::from_str as json_from_str;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Logos)]
#[logos(skip r"\s+")]
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
    #[token("}")]
    RightBrace,
    #[token("(")]
    LeftParens,
    #[token(")")]
    RightParens,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,
    #[token("==")]
    Equals,
    #[token("!=")]
    NotEquals,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[regex(r#""(?:[^"$]|(?:\\.))*""#, parse_simple_string_literal)]
    SimpleString(IString),
    #[regex(r#"`(?:[^`]|(?:\\.))*`"#, parse_canonical_name)]
    CanonicalName(IString),
    #[regex(r#"\w+"#, parse_string)]
    Identifier(IString),
    #[regex(r#"[+-]?[0-9](?:_?[0-9])*"#, parse_number, priority=3)]
    DecimalInt(IString),
    #[regex(r#"[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?(?:[eE][+-]?[0-9](?:_?[0-9])*)?"#, parse_number)]
    #[regex(r#"\.[0-9](?:_?[0-9])*(?:[eE][+-]?[0-9](?:_?[0-9])*)?"#, parse_number)]
    #[regex(r#"[0-9](?:_?[0-9])*[eE][+-]?[0-9](?:_?[0-9])*"#, parse_number)]
    #[regex(r#"[+-]?Infinity"#, parse_string)]
    #[regex("NaN", |_| literal_istring!("NaN"))]
    DecimalFloat(IString),
    #[regex(r#"0x[0-9a-fA-F](?:_?[0-9a-fA-F])*"#, parse_number)]
    HexadecimalInt(IString),
    #[regex(r#"0o[0-7](?:_?[0-7])*"#, parse_number)]
    OctalInt(IString),
    #[regex(r#"0b[01](?:_?[01])*"#, parse_number)]
    BinaryInt(IString),
    #[regex(r#""(?:[^"$]|(?:\\.))*$\{"#, parse_left_format_string)]
    LeftFormattedString(IString),
    #[regex(r#"\}(?:[^"$]|(?:\\.))*$\{"#, parse_middle_format_string)]
    MiddleFormattedString(IString),
    #[regex(r#"\}(?:[^"$]|(?:\\.))*""#, parse_right_format_string)]
    RightFormattedString(IString),
}

pub fn parse_simple_string_literal(lex: &mut Lexer<Token>)
    -> Option<IString> {
    json_from_str::<'_, IString>(lex.slice()).ok()
}

pub fn parse_canonical_name(lex: &mut Lexer<Token>)
    -> Option<IString> {
    let slice = lex.slice();
    let inner = &slice[1..slice.len() - 1];
    json_from_str::<'_, IString>(
        format!(
            "\"{}\"",
            inner
                .replace('"', "\\\"")
                .replace("\\`", "`")
        ).as_str()
    ).ok()
}

pub fn parse_string(lex: &mut Lexer<Token>) -> IString {
    lex.slice().into()
}

pub fn parse_number(lex: &mut Lexer<Token>) -> IString {
    lex.slice().replace('_', "").into()
}

pub fn parse_left_format_string(lex: &mut Lexer<Token>)
    -> Option<IString> {
    let slice = lex.slice();
    let json = slice[..slice.len() - 2].to_string() + "\"";
    return json_from_str::<'_, IString>(&json).ok()
}

pub fn parse_middle_format_string(lex: &mut Lexer<Token>)
    -> Option<IString> {
    let slice = lex.slice();
    let json = String::from("\"") + &slice[1..slice.len() - 2] + "\"";
    return json_from_str::<'_, IString>(&json).ok()
}

pub fn parse_right_format_string(lex: &mut Lexer<Token>)
    -> Option<IString> {
    let slice = lex.slice();
    let json = String::from("\"") + &slice[1..];
    return json_from_str::<'_, IString>(&json).ok()
}
