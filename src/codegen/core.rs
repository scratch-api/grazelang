use std::collections::HashMap;

use rand::rand_core::block;
use serde::{Deserialize, Serialize};

use crate::{
    codegen::{
        ids::IdCounter,
        project_json::{Sb3Block, Sb3BlockMutation, Sb3FieldValue, Sb3InputValue, Sb3Root},
    },
    parser::parse_context::{IdString, ParseContext},
    visitor::{GrazeVisitor, default_visit_expression_binary_operation, default_visit_expression_call, default_visit_expression_formatted_string, default_visit_expression_get_item, default_visit_expression_unary_operation},
};

type GrazeSb3GeneratorError = ();

pub struct GrazeSb3Generator;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GrazeSb3GeneratorContext {
    pub sb3: Sb3Root,
    pub parse_context: ParseContext,
    pub block_counter: IdCounter,
    pub current_block_id: IdString,
    pub current_parent: Option<String>,
}

impl GrazeSb3GeneratorContext {
    fn new(parse_context: ParseContext) -> Self {
        let mut block_counter = IdCounter::new();
        let next_block_id = block_counter.get_new_id();
        Self {
            sb3: Sb3Root::default(),
            parse_context,
            block_counter,
            current_block_id: next_block_id,
            current_parent: None,
        }
    }

    fn new_next_id(&mut self) -> Option<String> {
        self.current_block_id = self.block_counter.get_new_id();
        Some(self.get_current_id())
    }

    fn get_current_id(&mut self) -> String {
        self.current_block_id.as_str().into()
    }
}

pub fn make_block(
    context: &mut GrazeSb3GeneratorContext,
    opcode: String,
    is_last: bool,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
    shadow: bool,
    mutation: Option<Sb3BlockMutation>,
) -> Sb3Block {
    Sb3Block {
        opcode,
        next: if is_last { None } else { context.new_next_id() },
        parent: context.current_parent.clone(),
        inputs,
        fields,
        shadow,
        top_level: false,
        mutation,
        x: None,
        y: None,
    }
}

pub fn make_top_level_block(
    context: &mut GrazeSb3GeneratorContext,
    opcode: String,
    is_last: bool,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
    shadow: bool,
    mutation: Option<Sb3BlockMutation>,
    x: f64,
    y: f64,
) -> Sb3Block {
    Sb3Block {
        opcode,
        next: if is_last { None } else { context.new_next_id() },
        parent: context.current_parent.clone(),
        inputs,
        fields,
        shadow,
        top_level: true,
        mutation,
        x: Some(x),
        y: Some(y),
    }
}

macro_rules! wrap_in_parent {
    ($context:expr, $action:expr) => {{
        let block_id = $context.get_current_id();
        let old_parent = $context.current_parent.replace(block_id);
        let out = $action;
        $context.current_parent = old_parent;
        out
    }};
}

impl GrazeVisitor<GrazeSb3GeneratorContext, GrazeSb3GeneratorError> for GrazeSb3Generator {
    fn visit_expression_binary_operation(
        &self,
        value: (
            &Box<crate::parser::ast::Expression>,
            &crate::parser::ast::BinOp,
            &Box<crate::parser::ast::Expression>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_parent!(context, {
            default_visit_expression_binary_operation(self, value, context)
        })
    }

    fn visit_expression_call(
            &self,
            value: (
                &crate::parser::ast::Identifier,
                &crate::parser::ast::LeftParens,
                &Vec<(crate::parser::ast::Expression, Option<crate::parser::ast::Comma>)>,
                &crate::parser::ast::RightParens,
                &crate::lexer::PosRange,
            ),
            context: &mut GrazeSb3GeneratorContext,
        ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_parent!(context, {
            default_visit_expression_call(self, value, context)
        })
    }

    fn visit_expression_formatted_string(
            &self,
            value: (&Vec<crate::parser::ast::FormattedStringContent>, &crate::lexer::PosRange),
            context: &mut GrazeSb3GeneratorContext,
        ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_parent!(context, {
            default_visit_expression_formatted_string(self, value, context)
        })
    }

    fn visit_expression_get_item(
            &self,
            value: (
                &crate::parser::ast::Identifier,
                &crate::parser::ast::LeftBracket,
                &Box<crate::parser::ast::Expression>,
                &crate::parser::ast::RightBracket,
                &crate::lexer::PosRange,
            ),
            context: &mut GrazeSb3GeneratorContext,
        ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_parent!(context, {
            default_visit_expression_get_item(self, value, context)
        })
    }

    fn visit_expression_unary_operation(
            &self,
            value: (&crate::parser::ast::UnOp, &Box<crate::parser::ast::Expression>, &crate::lexer::PosRange),
            context: &mut GrazeSb3GeneratorContext,
        ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_parent!(context, {
            default_visit_expression_unary_operation(self, value, context)
        })
    }
}
