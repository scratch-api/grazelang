use grazelang::names::Namespace;
use grazelang::{parser, lexer};
use logos::Logos;
use std::collections::VecDeque;
use parser::internal;
use parser::ast::{Expression, Literal, BinOp};

fn make_literal_int(int: i32) -> Expression {
    Expression::Literal(Literal::DecimalInt(format!("{}", int).into()))
}

fn main() {
    let mut lexer = lexer::Token::lexer("3 + 2 + 3 + 7 / 2 - 1 * 2 % 2");
    dbg!(internal::parse_expression(&mut (&mut lexer).peekable(), &mut Namespace::new()).unwrap());
    // println!("Hello, world!");
}
