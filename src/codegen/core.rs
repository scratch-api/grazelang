use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    iter,
    rc::{Rc, Weak},
};

use arcstr::{ArcStr as IString, literal};
use rand::SeedableRng;
use rand_xoshiro::Xoshiro256StarStar;

use super::{
    ids::IdCounter,
    project_json::{
        Sb3Block, Sb3BlockMutation, Sb3FieldValue, Sb3InputValue, Sb3PrimitiveBlock, Sb3Root,
    },
};

use crate::{
    parser::{
        ast::Identifier,
        parse_context::{
            ActualSymbol, IdString, KnownBlock, ParseContext, Target, TargetSymbolDescriptor,
        },
    },
    visitor::{
        GrazeVisitor, default_visit_expression_binary_operation, default_visit_expression_call,
        default_visit_expression_formatted_string, default_visit_expression_get_item,
        default_visit_expression_unary_operation,
    },
};

type GrazeSb3GeneratorError = ();

pub struct GrazeSb3Generator;

#[derive(Debug, Clone)]
pub struct GrazeSb3GeneratorContext {
    pub sb3: Sb3Root,
    pub targets: Vec<Target>,
    pub root_symbol: Rc<RefCell<ActualSymbol>>,
    pub block_counter: IdCounter,
    pub arg_stack: Vec<KnownBlock>,
    pub current_block_id: IdString,
    pub current_parent: Option<String>,
}

/// Placeholder function, will be replaced/moved/implemented later
pub fn get_std_lib_namespace_count() -> usize {
    todo!()
}

/// Placeholder function, will be replaced/moved/implemented later
pub fn get_std_lib_namespaces() -> Vec<(IString, ActualSymbol)> {
    todo!()
}

impl GrazeSb3GeneratorContext {
    fn new(mut parse_context: ParseContext) -> Self {
        let mut rng = Xoshiro256StarStar::from_seed(parse_context.random_seed);
        let targets: Vec<Target> = parse_context.parsed_targets.into();
        let mut root_symbol = Rc::new(RefCell::new(ActualSymbol::Namespace(
            HashMap::with_capacity(get_std_lib_namespace_count() + 4),
            Weak::new(),
        )));
        // ^ extensions + sprites + broadcasts + global variables + global lists
        let mut targets_symbol = Rc::new(RefCell::new(ActualSymbol::Namespace(
            HashMap::with_capacity(targets.len()),
            Weak::new(),
        )));
        for target in &targets {
            ActualSymbol::insert_child(
                &targets_symbol,
                target.get_namespace_name(),
                Rc::new(RefCell::new(ActualSymbol::KnownBlock(
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal(super::project_json::Sb3Primitive::String(
                            target.get_field_name(),
                        )),
                    },
                    Some(
                        target
                            .borrow_symbols()
                            .iter()
                            .map(|(key, value)| {
                                (
                                    key.clone(),
                                    Rc::new(RefCell::new(value.derive_actual_symbol(&mut rng))),
                                )
                            })
                            .collect(),
                    ),
                    Weak::new(),
                ))),
            );
        }
        ActualSymbol::insert_child(&root_symbol, literal!("sprites"), targets_symbol);
        ActualSymbol::insert_child(
            &root_symbol,
            literal!("broadcasts"),
            Rc::new(RefCell::new(ActualSymbol::Namespace(
                parse_context
                    .broadcasts
                    .iter()
                    .map(|(key, value)| {
                        (
                            key.clone(),
                            Rc::new(RefCell::new(value.derive_actual_symbol(&mut rng))),
                        )
                    })
                    .collect(),
                Weak::new(),
            ))),
        );
        let mut variables_symbol = Rc::new(RefCell::new(ActualSymbol::new_namespace()));
        let mut lists_symbol = Rc::new(RefCell::new(ActualSymbol::new_namespace()));
        for (name, symbol) in parse_context.global_symbols.drain() {
            match &symbol {
                TargetSymbolDescriptor::Var(_) => {
                    ActualSymbol::insert_child(
                        &variables_symbol,
                        name,
                        Rc::new(RefCell::new(symbol.derive_actual_symbol(&mut rng))),
                    );
                }
                TargetSymbolDescriptor::List(_) => {
                    ActualSymbol::insert_child(
                        &lists_symbol,
                        name,
                        Rc::new(RefCell::new(symbol.derive_actual_symbol(&mut rng))),
                    );
                }
                _ => (), // Handled just to be sure although it shouldn't happen
            }
        }
        ActualSymbol::insert_child(
            &root_symbol,
            literal!("variables"),
            variables_symbol,
        );

        ActualSymbol::insert_child(
            &root_symbol,
            literal!("lists"),
            lists_symbol,
        );
        for (name, symbol) in get_std_lib_namespaces() {
            ActualSymbol::insert_child(&root_symbol, name, Rc::new(RefCell::new(symbol)));
        }
        let mut block_counter = IdCounter::new();
        let next_block_id = block_counter.get_new_id();
        Self {
            sb3: Sb3Root::default(),
            targets,
            root_symbol,
            block_counter,
            arg_stack: Vec::new(),
            current_block_id: next_block_id,
            current_parent: None,
        }
    }

    fn new_block(&mut self) {
        self.current_block_id = self.block_counter.get_new_id();
    }

    fn get_current_block_id(&mut self) -> IdString {
        self.current_block_id.clone()
    }

    fn push_block_ref(&mut self, block_arg: KnownBlock) {
        self.arg_stack.push(block_arg);
    }

    /// Get the newest child's block id and
    fn pop_block_ref(&mut self) -> Option<KnownBlock> {
        self.arg_stack.pop()
    }

    pub fn resolve_symbol(&self, identifier: &Identifier) -> Option<KnownBlock> {
        let mut queue = identifier
            .scope
            .iter()
            .chain(identifier.names.iter())
            .map(|value| &value.0)
            .collect::<VecDeque<_>>();

        let mut current = &self.root_symbol;
        while let Some(value) = queue.pop_front() {}
        todo!()
    }

    // TODO:
    // pub fn resolve_symbol(&self, identifier: &Identifier) -> Result<KnownBlock, ()> {
    //     let mut queue = identifier
    //         .scope
    //         .iter()
    //         .chain(identifier.names.iter())
    //         .map(|value| &value.0)
    //         .collect::<VecDeque<_>>();
    //     let mut current = &self.root_symbol;
    //     while let Some(value) = queue.pop_front() {
    //         if let ActualSymbol::Sprites = current {
    //             current = self.parsed_targets.get(value).ok_or(())?;
    //         } else {
    //             todo!()
    //         }
    //     }
    //     todo!()
    // }
}

pub fn make_block(
    context: &GrazeSb3GeneratorContext,
    opcode: String,
    next: Option<String>,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
    shadow: bool,
    mutation: Option<Sb3BlockMutation>,
) -> Sb3Block {
    Sb3Block {
        opcode,
        next,
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
    context: &GrazeSb3GeneratorContext,
    opcode: String,
    next: Option<String>,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
    shadow: bool,
    mutation: Option<Sb3BlockMutation>,
    pos: (f64, f64),
) -> Sb3Block {
    Sb3Block {
        opcode,
        next,
        parent: context.current_parent.clone(),
        inputs,
        fields,
        shadow,
        top_level: true,
        mutation,
        x: Some(pos.0),
        y: Some(pos.1),
    }
}

impl KnownBlock {
    #[inline]
    fn from_id(id: IdString) -> Self {
        Self::BlockRef { id }
    }
}

impl From<IdString> for KnownBlock {
    #[inline]
    fn from(value: IdString) -> Self {
        Self::from_id(value)
    }
}

impl From<Sb3FieldValue> for KnownBlock {
    #[inline]
    fn from(value: Sb3FieldValue) -> Self {
        Self::FieldValue { value }
    }
}

impl From<Sb3PrimitiveBlock> for KnownBlock {
    #[inline]
    fn from(value: Sb3PrimitiveBlock) -> Self {
        Self::PrimitiveBlock { value }
    }
}

macro_rules! wrap_in_parent {
    ($context:expr, $identifier:pat => $action:expr) => {{
        let block_id = $context.get_current_block_id();
        let $identifier = block_id.clone();
        let old_parent = $context.current_parent.replace(block_id.to_string());
        $context.new_block();
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
        wrap_in_parent!(context, this_id => {
            context.push_block_ref(this_id.clone().into());
            default_visit_expression_binary_operation(self, value, context)?;
            let (op_b_id, op_a_id) = (context.pop_block_ref().unwrap(), context.pop_block_ref().unwrap());
        });
        Ok(())
    }

    fn visit_expression_call(
        &self,
        value: (
            &crate::parser::ast::Identifier,
            &crate::parser::ast::LeftParens,
            &Vec<(
                crate::parser::ast::Expression,
                Option<crate::parser::ast::Comma>,
            )>,
            &crate::parser::ast::RightParens,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_parent!(context, this_id => {
            context.push_block_ref(this_id.clone().into());
            default_visit_expression_call(self, value, context)?;
            let args = iter::repeat_with(|| context.pop_block_ref().unwrap())
                .take(value.2.len())
                .collect::<Vec<_>>();
        });
        Ok(())
    }

    fn visit_expression_formatted_string(
        &self,
        value: (
            &Vec<crate::parser::ast::FormattedStringContent>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_parent!(context, this_id => {
            context.push_block_ref(this_id.clone().into());
            default_visit_expression_formatted_string(self, value, context)?;
        });
        Ok(())
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
        wrap_in_parent!(context, this_id => {
            context.push_block_ref(this_id.clone().into());
            default_visit_expression_get_item(self, value, context)?;
        });
        Ok(())
    }

    fn visit_expression_unary_operation(
        &self,
        value: (
            &crate::parser::ast::UnOp,
            &Box<crate::parser::ast::Expression>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_parent!(context, this_id => {
            context.push_block_ref(this_id.clone().into());
            default_visit_expression_unary_operation(self, value, context)?;
        });
        Ok(())
    }

    fn visit_expression_identifier(
        &self,
        value: &crate::parser::ast::Identifier,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        // context.push_block_ref();
        todo!()
    }
}
