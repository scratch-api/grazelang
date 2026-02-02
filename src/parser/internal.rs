use super::ast::{BinOp, Expression, ParseError};
use crate::{
    lexer::{
        PosRange, Token, get_pos_range as internal_get_pos_range,
        get_position as internal_get_position,
    },
    names::Namespace,
    parser::ast::Identifier,
};
use arcstr::ArcStr as IString;
use logos::Lexer;
use std::{collections::VecDeque, iter::Peekable};

macro_rules! expect_token {
    ($peekable:expr, $variant:path) => {
        match $peekable.0.peek() {
            Some(Ok($variant(data))) => {
                let data = data.clone();
                skip_token!($peekable);
                Ok(Some(data))
            }
            Some(Err(_)) => Err(ParseError::LexerStuck(get_token_position!($peekable))),
            _ => Ok(None),
        }
    };
    ($peekable:expr, $variant:path, |$data:ident| $body:expr) => {
        match $peekable.0.peek() {
            Some(Ok($variant(data))) => {
                let $data = data.clone();
                skip_token!($peekable);
                Ok(Some($body))
            }
            Some(Err(_)) => Err(ParseError::LexerStuck(get_token_position!($peekable))),
            _ => Ok(None),
        }
    };
}

macro_rules! peek_token {
    ($peekable:expr) => {
        match $peekable.0.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return { Err(ParseError::LexerStuck(get_token_position!($peekable))) },
            None => return Err(ParseError::UnexpectedEndOfInput),
        }
    };
    (optional $peekable:expr) => {
        match $peekable.0.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($peekable))),
            None => return Ok(None),
        }
    };
}

macro_rules! next_token {
    ($peekable:expr) => {{
        $peekable.1.next();
        match $peekable.0.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($peekable))),
            None => return Err(ParseError::UnexpectedEndOfInput),
        }
    }};
    (optional $peekable:expr) => {{
        $peekable.1.next();
        match $peekable.0.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($peekable))),
            None => return Ok(None),
        }
    }};
}

macro_rules! skip_token {
    ($peekable:expr) => {{
        $peekable.1.next();
        if let Some(Err(_)) = $peekable.0.next() {
            return Err(ParseError::LexerStuck(get_token_position!($peekable)))
        }
    }};
}

macro_rules! get_token_position {
    ($peekable:expr) => {
        internal_get_pos_range($peekable.1, $peekable.1.span())
    };
}

macro_rules! get_token_start {
    ($peekable:expr) => {
        internal_get_position($peekable.1, $peekable.1.span().start)
    };
}

macro_rules! get_token_end {
    ($peekable:expr) => {
        internal_get_position($peekable.1, $peekable.1.span().end)
    };
}

#[macro_export]
macro_rules! make_parse_in {
    ($lexer:expr) => {
        &mut (&mut (&mut $lexer.clone()).peekable(), $lexer)
    };
}

type ParseIn<'a, 'b, 'c, 'd, 'e, 'f> = &'a mut (
    &'b mut Peekable<&'c mut Lexer<'d, Token>>,
    &'e mut Lexer<'f, Token>,
);
type ParseOut<T> = Result<T, ParseError>;

pub fn enter(lex: &mut Lexer<Token>) {
    let mut namespace = Namespace::default();
    parse_lexed_entrypoint(make_parse_in!(lex), &mut namespace);
}

pub fn parse_lexed_entrypoint(peekable: ParseIn, namespace: &mut Namespace) {
    todo!()
}

pub fn parse_single_identifier(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<IString> {
    if let Token::Identifier(value) = next_token!(peekable) {
        return Ok(value);
    }
    Err(ParseError::UnexpectedToken(get_token_position!(peekable)))
}

pub fn parse_full_identifier(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, PosRange)> =
        vec![if let Token::Identifier(value) = next_token!(peekable) {
            (value, get_token_position!(peekable))
        } else {
            return Err(ParseError::UnexpectedToken(get_token_position!(peekable)));
        }];
    let start_pos = get_token_start!(peekable);
    let mut scope: Option<Vec<(IString, PosRange)>> = None;
    loop {
        match peek_token!(peekable) {
            Token::ScopeResolution => {
                if let Some(_) = scope {
                    skip_token!(peekable);
                    return Err(ParseError::UnexpectedToken(get_token_position!(peekable)));
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
        skip_token!(peekable);
        if let Token::Identifier(value) = next_token!(peekable) {
            names.push((value, get_token_position!(peekable)));
        } else {
            return Err(ParseError::UnexpectedToken(get_token_position!(peekable)));
        }
    }
    if names.len() > 1 && scope == None {
        scope = Some(names);
        names = Vec::default();
    }
    Ok(Identifier {
        scope: scope.unwrap_or_default(),
        names,
        pos_range: (start_pos, get_token_end!(peekable)),
    })
}

pub fn parse_full_identifier_starting_with(
    peekable: ParseIn,
    namespace: &mut Namespace,
    value: IString,
) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, PosRange)> = vec![(value, get_token_position!(peekable))];
    let start_pos = get_token_start!(peekable);
    let mut scope: Option<Vec<(IString, PosRange)>> = None;
    loop {
        match peek_token!(peekable) {
            Token::ScopeResolution => {
                if let Some(_) = scope {
                    return Err(ParseError::UnexpectedToken(get_token_position!(peekable)));
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
        skip_token!(peekable);
        if let Token::Identifier(value) = next_token!(peekable) {
            names.push((value, get_token_position!(peekable)));
        } else {
            return Err(ParseError::UnexpectedToken(get_token_position!(peekable)));
        }
    }
    if names.len() > 1 && scope == None {
        scope = Some(names);
        names = Vec::default();
    }
    Ok(Identifier {
        scope: scope.unwrap_or_default(),
        names,
        pos_range: (start_pos, get_token_end!(peekable)),
    })
}

pub mod expression {
    use std::collections::VecDeque;

    use crate::parser::ast::{Associativity, FormattedStringContent, GetPos};

    use super::*;
    pub fn parse_expression_without_binops(
        peekable: ParseIn,
        namespace: &mut Namespace,
    ) -> ParseOut<Expression> {
        let token = next_token!(peekable);
        let token_position = get_token_position!(peekable);
        use super::super::ast::Literal as LLiteral;
        use super::super::ast::UnOp;
        use Expression::Literal as ELiteral;
        match token {
            Token::SimpleString(string) => {
                Ok(ELiteral(LLiteral::String(string.clone(), token_position)))
            }
            Token::DecimalInt(string) => Ok(ELiteral(LLiteral::DecimalInt(
                string.clone(),
                token_position,
            ))),
            Token::DecimalFloat(string) => Ok(ELiteral(LLiteral::DecimalFloat(
                string.clone(),
                token_position,
            ))),
            Token::HexadecimalInt(string) => Ok(ELiteral(LLiteral::HexadecimalInt(
                string.clone(),
                token_position,
            ))),
            Token::OctalInt(string) => {
                Ok(ELiteral(LLiteral::OctalInt(string.clone(), token_position)))
            }
            Token::BinaryInt(string) => Ok(ELiteral(LLiteral::BinaryInt(
                string.clone(),
                token_position,
            ))),
            Token::Minus => Ok(Expression::UnOp(
                UnOp::Minus(token_position),
                Box::new(parse_expression_without_binops(peekable, namespace)?),
                (token_position.0, get_token_end!(peekable)),
            )),
            Token::Not => Ok(Expression::UnOp(
                UnOp::Not(token_position),
                Box::new(parse_expression_without_binops(peekable, namespace)?),
                (token_position.0, get_token_end!(peekable)),
            )),
            Token::Exp => Ok(Expression::UnOp(
                UnOp::Exp(token_position),
                Box::new(parse_expression_without_binops(peekable, namespace)?),
                (token_position.0, get_token_end!(peekable)),
            )),
            Token::Pow => Ok(Expression::UnOp(
                UnOp::Pow(token_position),
                Box::new(parse_expression_without_binops(peekable, namespace)?),
                (token_position.0, get_token_end!(peekable)),
            )),
            Token::Identifier(value) => {
                let identifier = parse_full_identifier_starting_with(peekable, namespace, value)?;
                match peek_token!(peekable) {
                    Token::LeftParens => {
                        Ok(Expression::Call(identifier, parse_expression_list(peekable, namespace)?, (token_position.0, get_token_end!(peekable))))
                    }
                    Token::LeftBracket => {
                        Ok(Expression::GetItem(identifier, Box::new(parse_expression(peekable, namespace)?), (token_position.0, get_token_end!(peekable))))
                    }
                    _ => Ok(Expression::Identifier(identifier)),
                }
            }
            Token::LeftParens => {
                let expr = parse_expression(peekable, namespace)?;
                let end_pos = if let Token::RightParens = next_token!(peekable) {
                    get_token_end!(peekable)
                } else {
                    return Err(ParseError::UnexpectedToken(get_token_position!(peekable)));
                };
                Ok(Expression::Parentheses(
                    Box::new(expr),
                    (token_position.0, end_pos),
                ))
            }
            Token::LeftFormattedString(string) => {
                let mut expressions = Vec::<FormattedStringContent>::new();
                if !string.is_empty() {
                    expressions.push(FormattedStringContent::String(string, token_position));
                }
                loop {
                    expressions.push(FormattedStringContent::Expression(parse_expression(
                        peekable, namespace,
                    )?));
                    match next_token!(peekable) {
                        Token::MiddleFormattedString(string) => {
                            if !string.is_empty() {
                                expressions.push(FormattedStringContent::String(
                                    string,
                                    get_token_position!(peekable),
                                ));
                            }
                        }
                        Token::RightFormattedString(string) => {
                            if !string.is_empty() {
                                expressions.push(FormattedStringContent::String(
                                    string,
                                    get_token_position!(peekable),
                                ));
                            }
                            break;
                        }
                        _ => return Err(ParseError::UnexpectedToken(get_token_position!(peekable))),
                    }
                }
                Ok(Expression::FormattedString(
                    expressions,
                    (token_position.0, get_token_end!(peekable)),
                ))
            }
            _ => Err(ParseError::UnexpectedToken(get_token_position!(peekable))),
        }
    }

    pub fn parse_binary_operation(
        peekable: ParseIn,
        namespace: &mut Namespace,
    ) -> ParseOut<Option<BinOp>> {
        let token = peek_token!(optional peekable);
        use super::super::ast::BinOp;
        let result = match token {
            Token::Plus => BinOp::Plus,
            Token::Minus => BinOp::Minus,
            Token::Times => BinOp::Times,
            Token::Div => BinOp::Div,
            Token::Mod => BinOp::Mod,
            Token::Join => BinOp::Join,
            Token::And => BinOp::And,
            Token::Or => BinOp::Or,
            Token::Equals => BinOp::Equals,
            Token::NotEquals => BinOp::NotEquals,
            Token::LessThan => BinOp::LessThan,
            Token::GreaterThan => BinOp::GreaterThan,
            Token::LessThanOrEqual => BinOp::LessThanOrEqual,
            Token::GreaterThanOrEqual => BinOp::GreaterThanOrEqual,
            _ => return Ok(None),
        };
        skip_token!(peekable);
        Ok(Some(result(get_token_position!(peekable))))
    }

    /**
     `expressions` should have length `n` and `binops` should have length `n - 1`.
    Panics if this is not the case.
     */
    pub fn order_operations(
        mut expressions: VecDeque<Expression>,
        mut binops: VecDeque<BinOp>,
    ) -> Expression {
        let mut output_stack = Vec::<Expression>::default();
        let mut operator_stack = Vec::<BinOp>::default();
        for _ in 0..binops.len() {
            output_stack.push(expressions.pop_front().unwrap());
            let current_op = binops.pop_front().unwrap();
            let (current_prec, current_associativity) = current_op.get_precedence();
            loop {
                let (prev_prec, _) = match operator_stack.last() {
                    Some(value) => value,
                    None => break,
                }
                .get_precedence();
                if prev_prec < current_prec
                    || (prev_prec == current_prec && current_associativity != Associativity::Left)
                {
                    break;
                }
                let b = Box::new(output_stack.pop().unwrap());
                let b_pos = b.as_ref().get_position().1;
                let a = Box::new(output_stack.pop().unwrap());
                let a_pos = a.as_ref().get_position().0;
                let op = operator_stack.pop().unwrap();
                output_stack.push(Expression::BinOp(a, op, b, (a_pos, b_pos)));
            }
            operator_stack.push(current_op);
        }
        output_stack.push(expressions.pop_front().unwrap());
        while !operator_stack.is_empty() {
            let b = Box::new(output_stack.pop().unwrap());
            let b_pos = b.as_ref().get_position().1;
            let a = Box::new(output_stack.pop().unwrap());
            let a_pos = a.as_ref().get_position().0;
            let op = operator_stack.pop().unwrap();
            output_stack.push(Expression::BinOp(a, op, b, (a_pos, b_pos)));
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

pub fn parse_expression_list(
    peekable: ParseIn,
    namespace: &mut Namespace,
) -> ParseOut<Vec<Expression>> {
    if next_token!(peekable) != Token::LeftParens {
        return Err(ParseError::UnexpectedToken(get_token_position!(peekable)));
    }
    let mut expressions = Vec::<Expression>::default();
    loop {
        expressions.push(parse_expression(peekable, namespace)?);
        match next_token!(peekable) {
            Token::Comma => (),
            Token::RightParens => break,
            _ => return Err(ParseError::UnexpectedToken(get_token_position!(peekable))),
        }
    }
    Ok(expressions)
}
