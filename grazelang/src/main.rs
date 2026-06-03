use std::path::Path;

use clap::Parser;
use grazelang::cli::input::Cli;
use grazelang::parser::context::ParseContext;
use grazelang::visitor::GrazeVisitor;
use grazelang::zipper;
use grazelang::{
    codegen, lexer,
    parser::{self, core::PeekableLexer},
};

fn main() {

    let cli = Cli::parse();
    // let lexer = lexer::create_lexer(include_str!("./test.graze"));
    // let mut context = ParseContext::new(Default::default(), Default::default());

    // let parsed = parser::parse_graze_program(&mut PeekableLexer::new(lexer), &mut context).unwrap();

    // if !context.successful {
    //     for message in &context.messages {
    //         dbg!(message);
    //     }
    //     dbg!(parsed);
    //     panic!("Parsing unsuccessful.");
    // }

    // let mut context = codegen::core::GrazeSb3GeneratorContext::new(context).unwrap();
    // let visitor = codegen::core::GrazeSb3Generator;

    // visitor.visit_graze_program(&parsed, &mut context).unwrap();

    // for message in &context.messages {
    //     dbg!(message);
    // }

    // dbg!(&context.asset_files);

    // println!("{}", serde_json::to_string(&context.sb3).unwrap());

    // zipper::write_to_zip_path(Path::new("./test.sb3"), &context).unwrap();
}
