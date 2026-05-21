use grazelang::parser::context::{GrazeMessage, GrazeMessageSetting, ParseContext};
use grazelang::settings::GrazeSettings;
use grazelang::visitor::GrazeVisitor;
use grazelang::{
    codegen, lexer,
    parser::{self, core::PeekableLexer},
};

fn main() {
    let lexer = lexer::create_lexer(include_str!("./test.graze"));
    let mut context = ParseContext::new(
        Default::default(),
        Default::default(),
    );

    let parsed = parser::parse_graze_program(&mut PeekableLexer::new(lexer), &mut context).unwrap();

    for message in &context.messages {
        dbg!(message);
    }

    if !context.successful {
        dbg!(parsed);
        panic!("Parsing unsuccessful.");
    }

    let mut context = codegen::core::GrazeSb3GeneratorContext::new(context).unwrap();
    let visitor = codegen::core::GrazeSb3Generator;

    visitor.visit_graze_program(&parsed, &mut context).unwrap();

    dbg!(&context.asset_files);

    println!("{}", serde_json::to_string(&context.sb3).unwrap());
}
