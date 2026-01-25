use std::iter::Peekable;

use logos::Lexer;
use crate::{lexer::Token, names::Namespace};
use super::ast::{ParseError, BinOp, Expression};

macro_rules! expect_token {
    ($peekable:expr, $variant:path) => {
        match $peekable.peek() {
            Some(Ok($variant(data))) => {
                let data = data.clone();
                $peekable.next();
                Ok(Some(data))
            },
            Some(Err(_)) => Err(()),
            _ => Ok(None)
        }
    };
    ($peekable:expr, $variant:path, |$data:ident| $body:expr) => {
        match $peekable.peek() {
            Some(Ok($variant(data))) => {
                let $data = data.clone();
                $peekable.next();
                Ok(Some($body))
            },
            Some(Err(_)) => Err(()),
            _ => Ok(None)
        }
    };
}

macro_rules! peek_token {
    ($peekable:expr) => {
        match $peekable.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck),
            None => return Err(ParseError::UnexpectedEndOfInput)
        }
    };
    (optional $peekable:expr) => {
        match $peekable.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck),
            None => return Ok(None)
        }
    };
}

macro_rules! next_token {
    ($peekable:expr) => {
        match $peekable.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck),
            None => return Err(ParseError::UnexpectedEndOfInput)
        }
    };
    (optional $peekable:expr) => {
        match $peekable.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck),
            None => return Err(ParseError::UnexpectedEndOfInput)
        }
    };
}

type ParseIn<'a, 'b, 'c> = &'a mut Peekable<&'b mut Lexer<'c, Token>>;
type ParseOut<T> = Result<T, ParseError>;

pub fn enter(lex: &mut Lexer<Token>) {
    let mut namespace = Namespace::default();
    parse_lexed_entrypoint(&mut lex.peekable(), &mut namespace);
}

pub fn parse_lexed_entrypoint(peekable: ParseIn, namespace: &mut Namespace) {
    
    todo!()
}

pub fn parse_expression_without_binops(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<Expression> {
    let token = next_token!(peekable);
    use super::ast::Literal as LLiteral;
    use Expression::Literal as ELiteral;
    match token {
        Token::SimpleString(string) => Ok(ELiteral(LLiteral::String(string.clone()))),
        Token::DecimalInt(string) => Ok(ELiteral(LLiteral::DecimalInt(string.clone()))),
        Token::DecimalFloat(string) => Ok(ELiteral(LLiteral::DecimalFloat(string.clone()))),
        Token::HexadecimalInt(string) => Ok(ELiteral(LLiteral::HexadecimalInt(string.clone()))),
        Token::OctalInt(string) => Ok(ELiteral(LLiteral::OctalInt(string.clone()))),
        Token::BinaryInt(string) => Ok(ELiteral(LLiteral::BinaryInt(string.clone()))),
        _ => Err(ParseError::UnexpectedToken)
    }
}

pub fn parse_binary_operation(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<Option<BinOp>> {
    let token = peek_token!(optional peekable);
    use super::ast::BinOp;
    Ok(Some(match token {
        Token::Plus => BinOp::Plus,
        Token::Minus => BinOp::Minus,
        Token::Times => BinOp::Times,
        Token::Div => BinOp::Div,
        Token::Mod => BinOp::Mod,
        Token::And => BinOp::And,
        Token::Or => BinOp::Or,
        Token::Equals => BinOp::Equals,
        Token::NotEquals => BinOp::NotEquals,
        Token::LessThan => BinOp::LessThan,
        Token::GreaterThan => BinOp::GreaterThan,
        Token::LessThanOrEqual => BinOp::LessThanOrEqual,
        Token::GreaterThanOrEqual => BinOp::GreaterThanOrEqual,
        _ => return Ok(None)
    }))
}

pub fn parse_expression(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<Expression> {
    let mut expressions = vec![parse_expression_without_binops(peekable, namespace)?];
    let mut binops = Vec::<BinOp>::default();

    loop {
        binops.push(match parse_binary_operation(peekable, namespace)? {
            Some(value) => value,
            None => break
        });
        expressions.push(parse_expression_without_binops(peekable, namespace)?);
    }
    todo!()
}

