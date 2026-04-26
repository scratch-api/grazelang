use grazelang::parser::parse_context::ParseContext;
use grazelang::visitor::GrazeVisitor;
use grazelang::{codegen, lexer, make_parse_in, parser};

fn main() {
    let mut lexer = lexer::create_lexer(include_str!("./test.graze"));
    let mut context = ParseContext::new();

    let parsed = parser::parse_graze_program(make_parse_in!(&mut lexer), &mut context).unwrap();

    let mut context = codegen::core::GrazeSb3GeneratorContext::new(context).unwrap();
    let visitor = codegen::core::GrazeSb3Generator;

    visitor.visit_graze_program(&parsed, &mut context).unwrap();

    println!("{}", serde_json::to_string(&context.sb3).unwrap());
}
