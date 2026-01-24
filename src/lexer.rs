use logos::{Lexer, Logos};
use serde::{Serialize, Deserialize};
use arcstr::ArcStr as IString; // Immutable string
use serde_json::{from_str};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Logos)]
#[logos(skip r"\s+")]
#[logos(skip r"//.*")]
#[logos(skip r"/\*(?:[^*]|(?:\*[^/]))*\*/")]
pub enum Token {
    #[token("sprite")]
    SpriteKeyword,
    #[token("stage")]
    StageKeyword,
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
    #[regex(r#""(?:[^"]|(?:\\.))*""#, parse_simple_string)]
    SimpleString(IString),
    #[regex(r#"`(?:[^`]|(?:\\.))*`"#, parse_canonical_name)]
    CanonicalName(IString),
    #[regex(r#"\w+"#, parse_string)]
    Identifier(IString)
}

pub fn parse_simple_string(lex: &mut Lexer<Token>)
    -> Option<IString> {
    from_str::<'_, IString>(lex.slice()).ok()
}

pub fn parse_canonical_name(lex: &mut Lexer<Token>)
    -> Option<IString> {
    let slice = lex.slice();
    let inner = &slice[1..slice.len() - 1];
    from_str::<'_, IString>(
        format!(
            "\"{}\"",
            inner
                .replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace("\\`", "\"")
        ).as_str()
    ).ok()
}

pub fn parse_string(lex: &mut Lexer<Token>) -> IString {
    lex.slice().into()
} 
