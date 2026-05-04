use grazelang::parser::parse_context::ParseContext;
use grazelang::visitor::GrazeVisitor;
use grazelang::{
    codegen, lexer,
    parser::{self, core::PeekableLexer},
};

fn main() {
    let lexer = lexer::create_lexer(include_str!("./test.graze"));
    let mut context = ParseContext::new();

    let parsed = parser::parse_graze_program(&mut PeekableLexer::new(lexer), &mut context).unwrap();

    let mut context = codegen::core::GrazeSb3GeneratorContext::new(context).unwrap();
    let visitor = codegen::core::GrazeSb3Generator;

    visitor.visit_graze_program(&parsed, &mut context).unwrap();

    dbg!(&context.asset_files);

    println!("{}", serde_json::to_string(&context.sb3).unwrap());
}
