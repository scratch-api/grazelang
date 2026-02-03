use grazelang::names::Namespace;
use grazelang::{lexer, make_parse_in, parser};
use logos::Logos;
use parser::internal;

fn main() {
    let mut lexer = lexer::Token::lexer(r#"(the)"#);
    dbg!(internal::parse_expression(make_parse_in!(&mut lexer), &mut Namespace::new()).unwrap());
    // println!("Hello, world!");
}
