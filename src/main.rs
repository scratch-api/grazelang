use grazelang::names::Namespace;
use grazelang::parser::parse_context::ParseContext;
use grazelang::{lexer, make_parse_in, parser};
use logos::Logos;
use parser::internal;

fn test_case(case: &str) {
    let mut lexer = lexer::Token::lexer(case);
    match internal::parse_expression(make_parse_in!(&mut lexer), &mut ParseContext::new()) {
        Ok(value) => {
            dbg!(value);
        },
        Err(err) => {
            dbg!(err);
        },
    }
}

fn main() {
    test_case("case");
    test_case("case.case");
    test_case("case::case");
}
