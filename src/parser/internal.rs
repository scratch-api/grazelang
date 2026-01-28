use super::ast::{BinOp, Expression, ParseError};
use crate::{lexer::Token, names::Namespace, parser::ast::Identifier};
use arcstr::ArcStr as IString;
use logos::Lexer;
use std::{collections::{VecDeque, vec_deque}, iter::Peekable, sync::Arc};

macro_rules! expect_token {
    ($peekable:expr, $variant:path) => {
        match $peekable.peek() {
            Some(Ok($variant(data))) => {
                let data = data.clone();
                $peekable.next();
                Ok(Some(data))
            }
            Some(Err(_)) => Err(ParseError::LexerStuck),
            _ => Ok(None),
        }
    };
    ($peekable:expr, $variant:path, |$data:ident| $body:expr) => {
        match $peekable.peek() {
            Some(Ok($variant(data))) => {
                let $data = data.clone();
                $peekable.next();
                Ok(Some($body))
            }
            Some(Err(_)) => Err(ParseError::LexerStuck),
            _ => Ok(None),
        }
    };
}

macro_rules! peek_token {
    ($peekable:expr) => {
        match $peekable.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck),
            None => return Err(ParseError::UnexpectedEndOfInput),
        }
    };
    (optional $peekable:expr) => {
        match $peekable.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck),
            None => return Ok(None),
        }
    };
}

macro_rules! next_token {
    ($peekable:expr) => {
        match $peekable.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck),
            None => return Err(ParseError::UnexpectedEndOfInput),
        }
    };
    (optional $peekable:expr) => {
        match $peekable.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck),
            None => return Err(ParseError::UnexpectedEndOfInput),
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

pub fn parse_single_identifier(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<IString> {
    if let Token::Identifier(value) = next_token!(peekable) {
        return Ok(value)
    }
    Err(ParseError::UnexpectedToken)
}

pub fn parse_full_identifier(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<Identifier> {
    let mut names = vec![if let Token::Identifier(value) = next_token!(peekable) {
        value
    } else {
        return Err(ParseError::UnexpectedToken);
    }];
    let mut scope: Option<Vec<IString>> = None;
    loop {
        match peek_token!(peekable) {
            Token::ScopeResolution => {
                if let Some(_) = scope {
                    return Err(ParseError::UnexpectedToken);
                }
            }
            Token::Dot => {
                if let None = scope {
                    scope = Some(names);
                    names = Vec::default();
                }
            }
            _ => break,
        }
        peekable.next();
        if let Token::Identifier(value) = next_token!(peekable) {
            names.push(value);
        } else {
            return Err(ParseError::UnexpectedToken);
        }
    }
    if names.len() > 1
        && scope == None
    {
        scope = Some(names);
        names = Vec::default();
    }
    Ok(Identifier {
        scope: scope.unwrap_or_default(),
        names,
    })
}

pub mod expression {
    use std::collections::{self, LinkedList, VecDeque};

    use crate::parser::ast::Associativity;

    use super::*;
    pub fn parse_expression_without_binops(
        peekable: ParseIn,
        namespace: &mut Namespace,
    ) -> ParseOut<Expression> {
        let token = next_token!(peekable);
        use super::super::ast::Literal as LLiteral;
        use Expression::Literal as ELiteral;
        match token {
            Token::SimpleString(string) => Ok(ELiteral(LLiteral::String(string.clone()))),
            Token::DecimalInt(string) => Ok(ELiteral(LLiteral::DecimalInt(string.clone()))),
            Token::DecimalFloat(string) => Ok(ELiteral(LLiteral::DecimalFloat(string.clone()))),
            Token::HexadecimalInt(string) => Ok(ELiteral(LLiteral::HexadecimalInt(string.clone()))),
            Token::OctalInt(string) => Ok(ELiteral(LLiteral::OctalInt(string.clone()))),
            Token::BinaryInt(string) => Ok(ELiteral(LLiteral::BinaryInt(string.clone()))),
            Token::LeftBrace => {
                let expr = parse_expression(peekable, namespace)?;
                if let Token::RightBrace = next_token!(peekable) {
                    Ok(expr)
                } else {
                    Err(ParseError::UnexpectedToken)
                }
            }
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    pub fn parse_binary_operation(
        peekable: ParseIn,
        namespace: &mut Namespace,
    ) -> ParseOut<Option<BinOp>> {
        let token = peek_token!(optional peekable);
        use super::super::ast::BinOp;
        let result = Ok(Some(match token {
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
            _ => return Ok(None),
        }));
        peekable.next();
        result
    }

    /**
     `expressions` should have length `n` and `binops` should have length `n - 1`.
    Panics if this is not the case.
     */
    pub fn order_operations(mut expressions: VecDeque<Expression>, mut binops: VecDeque<BinOp>) -> Expression {
        let mut output_stack = Vec::<Expression>::default();
        let mut operator_stack = Vec::<BinOp>::default();
        for _ in 0..binops.len() {
            output_stack.push(expressions.pop_front().unwrap());
            let current_op = binops.pop_front().unwrap();
            let (current_prec, current_associativity) = current_op.get_precedence();
            loop {
                let (prev_prec, _) = match operator_stack.last() {
                    Some(value) => value,
                    None => break
                }.get_precedence();
                if prev_prec < current_prec || (prev_prec == current_prec && current_associativity != Associativity::Left) {
                    break;
                }
                let b = Box::new(output_stack.pop().unwrap());
                let a = Box::new(output_stack.pop().unwrap());
                let op = operator_stack.pop().unwrap();
                output_stack.push(Expression::BinOp(a, op, b));
            }
            operator_stack.push(current_op);
        }
        output_stack.push(expressions.pop_front().unwrap());
        while !operator_stack.is_empty() {
            let b = Box::new(output_stack.pop().unwrap());
            let a = Box::new(output_stack.pop().unwrap());
            let op = operator_stack.pop().unwrap();
            output_stack.push(Expression::BinOp(a, op, b));
        }
        output_stack.pop().unwrap()
        
    }
}

pub fn parse_expression(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<Expression> {
    use expression::*;
    let mut expressions = VecDeque::from([parse_expression_without_binops(peekable, namespace)?]);
    let mut binops = VecDeque::<BinOp>::default();

    loop {
        binops.push_back(match parse_binary_operation(peekable, namespace)? {
            Some(value) => value,
            None => break,
        });
        expressions.push_back(parse_expression_without_binops(peekable, namespace)?);
    }
    
    Ok(order_operations(expressions, binops))
}
