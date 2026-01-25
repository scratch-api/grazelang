use std::iter::Peekable;

use logos::Lexer;
use crate::{lexer::Token, parser::ast::Expression};
use super::ast::ParseError;

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

type ParseIn = &mut Peekable<&mut Lexer<Token>>;
type ParseOut<T> = Result<T, ParseError>;

pub fn enter(lex: &mut Lexer<Token>) {
    parse_lexed_entrypoint(&mut lex.peekable());
}

pub fn parse_lexed_entrypoint(peekable: ParseIn) {
    
}

pub fn parse_expression(peekable: ParseIn) -> ParseOut<Expression> {
    return match peekable.peek() {
        Some(Token::SimpleString(string)) => Ok(Expression::Literal(super::ast::Literal::String(string.clone()))),
        Some(Token::DecimalInt(string)) => Ok(Expression::Literal(super::ast::Literal::DecimalInt(string.clone()))),
        Some(Token::DecimalFloat(string)) => Ok(Expression::Literal(super::ast::Literal::DecimalFloat(string.clone()))),
        Some(Token::HexadecimalInt(string)) => Ok(Expression::Literal(super::ast::Literal::HexadecimalInt(string.clone()))),
        Some(Token::OctalInt(string)) => Ok(Expression::Literal(super::ast::Literal::OctalInt(string.clone()))),
        Some(Token::BinaryInt(string)) => Ok(Expression::Literal(super::ast::Literal::BinaryInt(string.clone()))),
        None => Err(ParseError::UnexpectedEndOfInput)
    }
}

