#![allow(clippy::result_large_err)]
use std::{
    cell::RefCell,
    collections::HashMap,
    iter::{self, zip},
    rc::{Rc, Weak},
};

use arcstr::{ArcStr as IString, literal};
use grazelang_library::{
    CallBlockParam, CallBlockParamKind, CallableKnownBlockSignature,
    SimpleCallableKnownBlockSignature, project_json::Sb3Primitive,
};
use rand::SeedableRng;
use rand_xoshiro::Xoshiro256StarStar;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::{
    ids::IdCounter,
    project_json::{
        Sb3Block, Sb3BlockMutation, Sb3FieldValue, Sb3InputValue, Sb3PrimitiveBlock, Sb3Root,
    },
};

use crate::{
    codegen::project_json::{IsShadow, Sb3InputRepr, Sb3Target, TargetAttachment},
    library,
    names::Namespace,
    parser::{
        ast::{
            BinOpDescriptor, Expression, FormattedStringContent, Identifier, ListEntry,
            UnOpDescriptor,
        },
        parse_context::{
            IdString, KnownBlock, ParseContext, ResolveKnownBlock, Symbol, Target,
            TargetSymbolDescriptor,
        },
    },
    visitor::{
        GrazeVisitor, default_visit_expression_binary_operation, default_visit_expression_call,
        default_visit_expression_formatted_string, default_visit_expression_get_item,
        default_visit_expression_get_letter, default_visit_expression_identifier,
        default_visit_expression_literal, default_visit_expression_unary_operation,
        default_visit_formatted_string_content, default_visit_isolated_block,
        default_visit_isolated_expression, default_visit_statement_assignment,
        default_visit_statement_call, default_visit_statement_forever,
        default_visit_statement_list_assignment, default_visit_statement_multi_input_control,
        default_visit_statement_set_item, default_visit_statement_single_input_control,
        default_visit_top_level_statement_sprite, default_visit_top_level_statement_stage,
    },
};

#[derive(Debug, Clone, Error)]
pub enum GrazeSb3GeneratorError {
    #[error("the identifier {identifier:?} was not found")]
    UnknownIdentifier { identifier: Identifier },
    #[error("the identifier {identifier:?} is not a block")]
    IdentifierIsNotABlock { identifier: Identifier },
    #[error("in this context, no menu input was expected, found {input_menu_value:?}")]
    UnexpectedInputMenu { input_menu_value: Sb3FieldValue },
    #[error("the amount of parameters for this block was {unexpected:?}, expected {expected:?}")]
    IncorrectParamCount { unexpected: usize, expected: usize },
    #[error("tried to get a list item for a non list, {identifier:?}")]
    ListAccessForNonLists { identifier: Identifier },
    #[error("cannot initialize stage multiple times")]
    RepeatedStageInitialization,
    #[error("tried to call the identifier {identifier:?} as a c block when it was not possible")]
    BlockIsNotCBlock { identifier: Identifier },
    #[error("tried to pass normal parameter {param:?} as a block stack")]
    PassedNormalParamAsBlockStack { param: Param },
}

pub struct GrazeSb3Generator;

#[derive(Debug, Clone)]
pub struct GrazeSb3GeneratorContext {
    pub sb3: Sb3Root,
    pub targets: Vec<Target>,
    pub root_symbol: Rc<RefCell<Symbol>>,
    pub block_counter: IdCounter,
    pub arg_stack: Vec<Param>,
    pub current_block_id: IdString,
    pub current_parent: Option<String>,
    pub current_sb3_target: Option<Sb3Target>,
    /// Is None while and after being initialized
    pub uninitialized_stage: Option<Sb3Target>,
    pub current_previous_block: Option<IdString>,
    pub formatted_string_context: FormattedStringContext,
    pub target_attachments: HashMap<IString, Vec<TargetAttachment>>,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct FormattedStringContext {
    pub ids: Vec<Option<String>>,
    pub current_idx: usize,
}

impl FormattedStringContext {
    fn new() -> Self {
        Self::default()
    }
}

impl GrazeSb3GeneratorContext {
    pub fn new(parse_context: ParseContext) -> Result<Self, std::io::Error> {
        let mut this = Self::without_standard_namespaces(parse_context)?;
        this.add_namespace_refs(library::get_standard_library_namespaces());
        Ok(this)
    }

    pub fn without_standard_namespaces(
        mut parse_context: ParseContext,
    ) -> Result<Self, std::io::Error> {
        let mut rng = Xoshiro256StarStar::from_seed(parse_context.random_seed);
        let targets: Vec<Target> = parse_context.parsed_targets.into();
        let root_symbol = Rc::new_cyclic(|me| {
            RefCell::new(Symbol::Namespace(
                HashMap::with_capacity(library::get_standard_library_namespace_count() + 4),
                // ^ extensions + sprites + broadcasts + global variables + global lists
                me.clone(),
            ))
        });

        let targets_symbol = Rc::new(RefCell::new(Symbol::Namespace(
            HashMap::with_capacity(targets.len()),
            Weak::new(),
        )));
        let mut target_attachments = HashMap::with_capacity(targets.len());
        for target in &targets {
            let mut namespace = Namespace::new();
            Symbol::insert_child(
                &targets_symbol,
                target.get_namespace_name(),
                Rc::new(RefCell::new(Symbol::KnownBlock(
                    Box::new(KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal(super::project_json::Sb3Primitive::String(
                            target.get_field_name(),
                        )),
                    }),
                    {
                        let symbol_count = target.borrow_symbols().len();
                        let (symbols, assets) = target
                            .borrow_symbols()
                            .iter()
                            .map(|(key, value)| {
                                value
                                    .derive_symbol_and_attachment(&mut rng, &mut namespace)
                                    .map(|(symbol, asset)| {
                                        ((key.clone(), Rc::new(RefCell::new(symbol))), asset)
                                    })
                            })
                            .try_fold::<_, _, Result<_, std::io::Error>>(
                                (HashMap::with_capacity(symbol_count), Vec::new()),
                                |(mut symbols, mut assets), item| {
                                    let ((key, symbol), asset) = item?;
                                    symbols.insert(key, symbol);
                                    if let Some(asset) = asset {
                                        assets.push(asset);
                                    }
                                    Ok((symbols, assets))
                                },
                            )?;
                        target_attachments.insert(target.get_namespace_name(), assets);
                        symbols
                    },
                    Weak::new(),
                ))),
            );
        }
        Symbol::insert_child(&root_symbol, literal!("sprites"), targets_symbol);
        Symbol::insert_child(
            &root_symbol,
            literal!("broadcasts"),
            Rc::new(RefCell::new(Symbol::Namespace(
                parse_context
                    .broadcasts
                    .iter()
                    .map(|(key, value)| {
                        (
                            key.clone(),
                            Rc::new(RefCell::new(value.derive_symbol(&mut rng))),
                        )
                    })
                    .collect(),
                Weak::new(),
            ))),
        );
        let variables_symbol = Rc::new(RefCell::new(Symbol::new_namespace()));
        let lists_symbol = Rc::new(RefCell::new(Symbol::new_namespace()));
        let mut global_namespace = Namespace::new();
        {
            let stage_target_attachments = target_attachments.get_mut("stage").unwrap();
            for (name, symbol) in parse_context.global_symbols.drain() {
                match &symbol {
                    TargetSymbolDescriptor::Var(_) => {
                        let (symbol, attachment) = symbol
                            .derive_symbol_and_attachment(&mut rng, &mut global_namespace)
                            .unwrap();
                        if let Some(attachment) = attachment {
                            stage_target_attachments.push(attachment);
                        }
                        Symbol::insert_child(
                            &variables_symbol,
                            name,
                            Rc::new(RefCell::new(symbol)),
                        );
                    }
                    TargetSymbolDescriptor::List(_) => {
                        let (symbol, attachment) = symbol
                            .derive_symbol_and_attachment(&mut rng, &mut global_namespace)
                            .unwrap();
                        if let Some(attachment) = attachment {
                            stage_target_attachments.push(attachment);
                        }
                        Symbol::insert_child(&lists_symbol, name, Rc::new(RefCell::new(symbol)));
                    }
                    _ => (), // Handled just to be sure although it shouldn't happen
                }
            }
        }
        Symbol::insert_child(&root_symbol, literal!("vars"), variables_symbol);

        Symbol::insert_child(&root_symbol, literal!("lists"), lists_symbol);
        let mut block_counter = IdCounter::new();
        let next_block_id = block_counter.get_new_id();
        Ok(Self {
            sb3: Sb3Root::default(),
            targets,
            root_symbol,
            block_counter,
            arg_stack: Vec::new(),
            current_block_id: next_block_id,
            current_parent: None,
            current_sb3_target: None,
            uninitialized_stage: Some(Sb3Target::new_stage()),
            current_previous_block: None,
            formatted_string_context: FormattedStringContext::new(),
            target_attachments,
        })
    }

    pub fn add_namespaces<I>(&mut self, namespaces: I)
    where
        I: Iterator<Item = (IString, Symbol)>,
    {
        self.add_namespace_refs(namespaces.map(|(key, value)| (key, Rc::new(RefCell::new(value)))));
    }

    pub fn add_namespace_refs<I>(&mut self, namespaces: I)
    where
        I: Iterator<Item = (IString, Rc<RefCell<Symbol>>)>,
    {
        for (name, symbol) in namespaces {
            Symbol::insert_child(&self.root_symbol, name, symbol);
        }
    }

    fn new_block(&mut self) {
        self.current_block_id = self.block_counter.get_new_id();
    }

    fn get_current_block_id(&mut self) -> IdString {
        self.current_block_id.clone()
    }

    fn push_param(&mut self, block_arg: Param) {
        self.arg_stack.push(block_arg);
    }

    fn pop_param(&mut self) -> Option<Param> {
        self.arg_stack.pop()
    }

    pub fn resolve_identifier(&self, identifier: &Identifier) -> Option<Rc<RefCell<Symbol>>> {
        identifier
            .scope
            .iter()
            .chain(identifier.names.iter())
            .try_fold(self.root_symbol.clone(), |current, (next, _)| {
                if next == &literal!("super") {
                    current.borrow().get_parent().upgrade()
                } else {
                    current.borrow().get_child(next)
                }
            })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Param {
    Owned(KnownBlock),
    LazyIdentifier(Identifier),
    BlockStack(String),
}

pub fn make_block(
    parent: Option<String>,
    opcode: String,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
    shadow: bool,
    mutation: Option<Sb3BlockMutation>,
) -> Sb3Block {
    let top_level = parent.is_none();
    Sb3Block {
        opcode,
        next: None,
        parent,
        inputs,
        fields,
        shadow,
        top_level,
        mutation,
        x: None,
        y: None,
    }
}

pub fn make_top_level_block(
    opcode: String,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
    shadow: bool,
    mutation: Option<Sb3BlockMutation>,
    pos: (f64, f64),
) -> Sb3Block {
    Sb3Block {
        opcode,
        next: None,
        parent: None,
        inputs,
        fields,
        shadow,
        top_level: true,
        mutation,
        x: Some(pos.0),
        y: Some(pos.1),
    }
}

pub fn wrap_in_reporter<F, O>(context: &mut GrazeSb3GeneratorContext, action: F) -> O
where
    O: Sized,
    F: FnOnce(&mut GrazeSb3GeneratorContext, Option<String>, IString) -> O,
{
    let block_id = context.get_current_block_id();
    let old_parent = context.current_parent.replace(block_id.to_string());
    context.new_block();
    let out = action(context, old_parent.clone(), block_id);
    context.current_parent = old_parent;
    out
}

pub fn wrap_in_statement<F, O>(context: &mut GrazeSb3GeneratorContext, action: F) -> O
where
    O: Sized,
    F: FnOnce(&mut GrazeSb3GeneratorContext, Option<String>, IString) -> O,
{
    let block_id = context.get_current_block_id();
    let old_parent = context.current_parent.replace(block_id.to_string());
    context.new_block();
    if let Some(previous) = context.current_previous_block.take() {
        context
            .current_sb3_target
            .as_mut()
            .unwrap()
            .blocks
            .get_mut(previous.as_str())
            .unwrap()
            .next = Some(block_id.to_string());
    } else if old_parent.is_some() {
        // First statement in block stack is used in STACK argument of parent or similar
        context.push_param(Param::BlockStack(block_id.to_string()));
    }
    let out = action(context, old_parent.clone(), block_id.clone());
    context.current_parent = Some(block_id.to_string());
    context.current_previous_block = Some(block_id);
    out
}

macro_rules! with_known_block {
    ($context:expr, $param:expr, $known_block:ident => $action:expr) => {
        match $param {
            Param::Owned(ref $known_block) => $action,
            Param::LazyIdentifier(value) => {
                let rc = $context.resolve_identifier(&value).ok_or_else(|| {
                    GrazeSb3GeneratorError::UnknownIdentifier {
                        identifier: value.clone(),
                    }
                })?;
                let actual_identifier = rc.borrow();
                let $known_block = actual_identifier.get_block().ok_or_else(|| {
                    GrazeSb3GeneratorError::IdentifierIsNotABlock {
                        identifier: value.clone(),
                    }
                })?;
                $action
            }
            Param::BlockStack(_) => {
                todo!() // TODO: warn user about incorrect usage
            }
        }
    };
}

macro_rules! get_actual_identifier {
    ($context:expr, $identifier:expr) => {{
        let identifier = $identifier;
        let actual_identifier = $context.resolve_identifier(identifier).ok_or_else(|| {
            GrazeSb3GeneratorError::UnknownIdentifier {
                identifier: identifier.clone(),
            }
        });
        actual_identifier
    }};
}

macro_rules! get_known_block {
    ($actual_identifier_ref:expr, $identifier:expr) => {{
        $actual_identifier_ref.get_block().ok_or_else(|| {
            GrazeSb3GeneratorError::IdentifierIsNotABlock {
                identifier: $identifier.clone(),
            }
        })
    }};
}

pub fn introduce_input_menu(
    opcode: &IString,
    field_name: &IString,
    value: Sb3FieldValue,
    context: &mut GrazeSb3GeneratorContext,
) -> IdString {
    wrap_in_reporter(context, |context, parent, this_id| {
        add_block(
            context,
            &this_id,
            make_block(
                parent,
                opcode.to_string(),
                HashMap::new(),
                HashMap::from([(field_name.to_string(), value)]),
                true,
                None,
            ),
        );
        this_id
    })
}

pub fn add_param_to_params(
    context: &mut GrazeSb3GeneratorContext,
    param: &CallBlockParam,
    value: &KnownBlock,
    inputs: &mut HashMap<String, Sb3InputValue>,
    fields: &mut HashMap<String, Sb3FieldValue>,
) -> Result<(), GrazeSb3GeneratorError> {
    let param_name = param.name.to_string();
    match &param.kind {
        CallBlockParamKind::Input { default } => {
            inputs.insert(
                param_name,
                (
                    known_block_to_input_repr_no_menu(value, context)?,
                    default.as_ref(),
                )
                    .into(),
            );
        }
        CallBlockParamKind::Field { .. } => {
            fields.insert(param_name, value.resolve_for_field(context));
        }
        CallBlockParamKind::MenuInput {
            opcode,
            field_name,
            default,
        } => {
            let (input_repr, is_menu) = match value.resolve_for_input(context) {
                grazelang_library::KnownBlockInput::PrimitiveInput(sb3_primitive_block) => {
                    (Sb3InputRepr::PrimitiveBlock(sb3_primitive_block), false)
                }
                grazelang_library::KnownBlockInput::BlockRef(id) => {
                    (Sb3InputRepr::Reference(id.to_string()), false)
                }
                grazelang_library::KnownBlockInput::SimpleBlock(opcode, params) => (
                    Sb3InputRepr::Reference(
                        introduce_input_simple_block(opcode, params.iter(), context)?.to_string(),
                    ),
                    false,
                ),
                grazelang_library::KnownBlockInput::Menu(input_menu_value) => (
                    Sb3InputRepr::Reference(
                        introduce_input_menu(opcode, field_name, input_menu_value, context)
                            .to_string(),
                    ),
                    true,
                ),
            };
            inputs.insert(
                param_name,
                if is_menu {
                    Sb3InputValue::Shadow(input_repr)
                } else {
                    Sb3InputValue::ObscuredShadow {
                        value: input_repr,
                        shadow: Sb3InputRepr::Reference(
                            introduce_input_menu(opcode, field_name, default.clone(), context)
                                .to_string(),
                        ),
                    }
                },
            );
        }
        CallBlockParamKind::BlockStack => {
            todo!() // TODO: warn user about incorrect usage
        }
    }
    Ok(())
}

pub fn add_params<'a, I>(
    context: &mut GrazeSb3GeneratorContext,
    params: I,
    inputs: &mut HashMap<String, Sb3InputValue>,
    fields: &mut HashMap<String, Sb3FieldValue>,
) -> Result<(), GrazeSb3GeneratorError>
where
    I: Iterator<Item = &'a (CallBlockParam, KnownBlock)>,
{
    for (param, value) in params {
        add_param_to_params(context, param, value, inputs, fields)?;
    }
    Ok(())
}

pub fn introduce_input_simple_block<'a, I>(
    opcode: &IString,
    params: I,
    context: &mut GrazeSb3GeneratorContext,
) -> Result<IdString, GrazeSb3GeneratorError>
where
    I: Iterator<Item = &'a (CallBlockParam, KnownBlock)>,
{
    wrap_in_reporter(context, |context, parent, this_id| {
        let mut fields = HashMap::new();
        let mut inputs = HashMap::new();
        add_params(context, params, &mut inputs, &mut fields)?;
        add_block(
            context,
            &this_id,
            make_block(parent, opcode.to_string(), inputs, fields, false, None),
        );
        Ok(this_id)
    })
}

pub fn known_block_to_input_repr_no_menu(
    known_block: &KnownBlock,
    context: &mut GrazeSb3GeneratorContext,
) -> Result<(Sb3InputRepr, IsShadow), GrazeSb3GeneratorError> {
    let known_block_input = known_block.resolve_for_input(context);
    match known_block_input {
        grazelang_library::KnownBlockInput::PrimitiveInput(sb3_primitive_block) => {
            let is_shadow = sb3_primitive_block.is_shadow();
            Ok((Sb3InputRepr::PrimitiveBlock(sb3_primitive_block), is_shadow))
        }
        grazelang_library::KnownBlockInput::BlockRef(id) => {
            Ok((Sb3InputRepr::Reference(id.to_string()), IsShadow::No))
        }
        grazelang_library::KnownBlockInput::SimpleBlock(opcode, params) => Ok((
            Sb3InputRepr::Reference(
                introduce_input_simple_block(opcode, params.iter(), context)?.to_string(),
            ),
            IsShadow::No,
        )),
        grazelang_library::KnownBlockInput::Menu(input_menu_value) => {
            Err(GrazeSb3GeneratorError::UnexpectedInputMenu { input_menu_value })
        }
    }
}

pub fn param_to_input_repr_no_menu(
    param: Param,
    context: &mut GrazeSb3GeneratorContext,
) -> Result<(Sb3InputRepr, IsShadow), GrazeSb3GeneratorError> {
    with_known_block!(context, param, value => {
        known_block_to_input_repr_no_menu(value, context)
    })
}

pub fn add_block(context: &mut GrazeSb3GeneratorContext, id: &IdString, block: Sb3Block) {
    context
        .current_sb3_target
        .as_mut()
        .unwrap() // the visitor should always guarantee there is a target when blocks are added
        .blocks
        .insert(id.to_string(), block);
}

pub fn create_control_block<I>(
    context: &mut GrazeSb3GeneratorContext,
    identifier: &Identifier,
    args: I,
    arg_count: usize,
    substack: Param,
    parent: Option<String>,
    this_id: IString,
) -> Result<(), GrazeSb3GeneratorError>
where
    I: Iterator<Item = Param>,
{
    let actual_identifier = get_actual_identifier!(context, identifier)?;
    let actual_identifier_ref = actual_identifier.borrow();
    let known_block = get_known_block!(actual_identifier_ref, identifier)?;
    let CallableKnownBlockSignature(opcode, params, known_params) =
        known_block.resolve_for_call_block(context);
    let mut fields = HashMap::new();
    let mut inputs = HashMap::new();
    add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
    if params.len() - 1 != arg_count {
        return Err(GrazeSb3GeneratorError::IncorrectParamCount {
            unexpected: arg_count,
            expected: params.len() - 1,
        });
    }
    let substack_input_name = if let CallBlockParam {
        kind: CallBlockParamKind::BlockStack,
        name,
    } = params.last().unwrap()
    {
        name
    } else {
        return Err(GrazeSb3GeneratorError::BlockIsNotCBlock {
            identifier: identifier.clone(),
        });
    };
    let substack = if let Param::BlockStack(block_ref) = substack {
        block_ref
    } else {
        return Err(GrazeSb3GeneratorError::PassedNormalParamAsBlockStack { param: substack });
    };
    for (param, value) in zip(params.iter(), args) {
        with_known_block!(context, value, value => {
            add_param_to_params(context, param, value, &mut inputs, &mut fields)?;
        });
    }
    inputs.insert(
        substack_input_name.to_string(),
        Sb3InputValue::NoShadow(Sb3InputRepr::Reference(substack)),
    );
    add_block(
        context,
        &this_id,
        make_block(parent, opcode.to_string(), inputs, fields, false, None),
    );
    Ok(())
}

impl GrazeVisitor<GrazeSb3GeneratorContext, GrazeSb3GeneratorError> for GrazeSb3Generator {
    // Expressions:

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
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_binary_operation(self, value, context)?;
            let (op_b_param, op_a_param) =
                (context.pop_param().unwrap(), context.pop_param().unwrap());
            let BinOpDescriptor {
                opcode,
                operand_a_input_name,
                operand_b_input_name,
                operand_a_default,
                operand_b_default,
                is_negated,
            } = value.1.get_descriptor();
            let inputs = HashMap::from([
                (
                    operand_a_input_name,
                    (
                        param_to_input_repr_no_menu(op_a_param, context)?,
                        operand_a_default,
                    )
                        .into(),
                ),
                (
                    operand_b_input_name,
                    (
                        param_to_input_repr_no_menu(op_b_param, context)?,
                        operand_b_default,
                    )
                        .into(),
                ),
            ]);
            if is_negated {
                let inner_reporter_id = wrap_in_reporter(context, |context, parent, this_id| {
                    add_block(
                        context,
                        &this_id,
                        make_block(
                            parent,
                            opcode.to_string(),
                            inputs,
                            HashMap::new(),
                            false,
                            None,
                        ),
                    );
                    Ok(this_id)
                })?;
                add_block(
                    context,
                    &this_id,
                    make_block(
                        parent,
                        "operator_not".to_string(),
                        HashMap::from([(
                            "OPERAND".to_string(),
                            Sb3InputValue::NoShadow(Sb3InputRepr::Reference(
                                inner_reporter_id.to_string(),
                            )),
                        )]),
                        HashMap::new(),
                        false,
                        None,
                    ),
                );
                context.push_param(Param::Owned(this_id.into()));
                return Ok(());
            }

            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
                    HashMap::new(),
                    false,
                    None,
                ),
            );
            context.push_param(Param::Owned(this_id.into()));
            Ok(())
        })
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
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_call(self, value, context)?;
            let reversed_args = iter::repeat_with(|| context.pop_param().unwrap())
                .take(value.2.len())
                .collect::<Vec<_>>();
            let actual_identifier = get_actual_identifier!(context, value.0)?;
            let actual_identifier_ref = actual_identifier.borrow();
            let known_block = get_known_block!(actual_identifier_ref, value.0)?;
            let CallableKnownBlockSignature(opcode, params, known_params) =
                known_block.resolve_for_call_block(context);
            let mut fields = HashMap::new();
            let mut inputs = HashMap::new();
            add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
            if params.len() != reversed_args.len() {
                return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                    unexpected: reversed_args.len(),
                    expected: params.len(),
                });
            }
            for (param, value) in zip(params.iter(), reversed_args.into_iter().rev()) {
                with_known_block!(context, value, value => {
                    add_param_to_params(context, param, value, &mut inputs, &mut fields)?;
                });
            }
            add_block(
                context,
                &this_id,
                make_block(parent, opcode.to_string(), inputs, fields, false, None),
            );
            context.push_param(Param::Owned(this_id.into()));
            Ok(())
        })
    }

    fn visit_expression_formatted_string(
        &self,
        value: (
            &Vec<crate::parser::ast::FormattedStringContent>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        enum FormattedStringTree {
            Expression,
            String(String),
            EmptyString,
            Joined(
                Box<FormattedStringTree>,
                Box<FormattedStringTree>,
                Option<String>,
                IdString,
            ),
        }
        fn join_recursively(
            context: &mut GrazeSb3GeneratorContext,
            value: &[FormattedStringContent],
        ) -> FormattedStringTree {
            match value.len() {
                0 => FormattedStringTree::EmptyString,
                1 => match &value[0] {
                    FormattedStringContent::Expression(_) => FormattedStringTree::Expression,
                    FormattedStringContent::String(arc_str, _) => {
                        FormattedStringTree::String(arc_str.to_string())
                    }
                },
                _ => {
                    let mid = value.len() / 2;
                    wrap_in_reporter(context, |context, parent, this_id| {
                        let parent = context
                            .formatted_string_context
                            .ids
                            .pop()
                            .flatten()
                            .or(parent);
                        context
                            .formatted_string_context
                            .ids
                            .push(Some(this_id.to_string()));
                        let left = Box::new(join_recursively(context, &value[..mid]));
                        context
                            .formatted_string_context
                            .ids
                            .push(Some(this_id.to_string()));
                        let right = Box::new(join_recursively(context, &value[mid..]));
                        FormattedStringTree::Joined(left, right, parent, this_id)
                    })
                }
            }
        }
        fn make_join(
            parent: Option<String>,
            left: Sb3InputValue,
            right: Sb3InputValue,
        ) -> Sb3Block {
            make_block(
                parent,
                "operator_join".to_string(),
                HashMap::from([
                    ("STRING1".to_string(), left),
                    ("STRING2".to_string(), right),
                ]),
                HashMap::new(),
                false,
                None,
            )
        }
        fn convert_into_block_tree(
            context: &mut GrazeSb3GeneratorContext,
            value: FormattedStringTree,
        ) -> Result<Param, GrazeSb3GeneratorError> {
            match value {
                FormattedStringTree::Expression => Ok(context.pop_param().unwrap()),
                FormattedStringTree::String(string) => {
                    Ok(Param::Owned(KnownBlock::PrimitiveBlock {
                        value: string.into(),
                    }))
                }
                FormattedStringTree::EmptyString => Ok(Param::Owned(KnownBlock::PrimitiveBlock {
                    value: "".into(),
                })),
                FormattedStringTree::Joined(left, right, parent, this_id) => {
                    // right comes first because a stack is LIFO
                    let right = convert_into_block_tree(context, *right)?;
                    let left = convert_into_block_tree(context, *left)?;
                    let left = param_to_input_repr_no_menu(left, context)?;
                    let right = param_to_input_repr_no_menu(right, context)?;
                    #[cfg(feature = "use_shadows_for_formatted_strings")]
                    let left = (
                        left,
                        Some(Sb3PrimitiveBlock::String(
                            #[cfg(feature = "use_actual_defaults_for_formatted_strings")]
                            "apple ".into(),
                            #[cfg(not(feature = "use_actual_defaults_for_formatted_strings"))]
                            "".into(),
                        )),
                    );
                    #[cfg(feature = "use_shadows_for_formatted_strings")]
                    let right = (
                        right,
                        Some(Sb3PrimitiveBlock::String(
                            #[cfg(feature = "use_actual_defaults_for_formatted_strings")]
                            "banana".into(),
                            #[cfg(not(feature = "use_actual_defaults_for_formatted_strings"))]
                            "".into(),
                        )),
                    );
                    add_block(
                        context,
                        &this_id,
                        make_join(parent, left.into(), right.into()),
                    );
                    Ok(Param::Owned(KnownBlock::BlockRef { id: this_id }))
                }
            }
        }
        let section_count = value.0.len();
        context.formatted_string_context = FormattedStringContext {
            ids: if section_count == 1 {
                vec![context.current_parent.clone()]
            } else {
                Vec::with_capacity(section_count)
            },
            current_idx: 0,
        };
        let formatted_string = join_recursively(context, value.0);
        let formatted_string = match formatted_string {
            FormattedStringTree::Expression => {
                default_visit_expression_formatted_string(self, value, context)?;
                return Ok(());
            }
            FormattedStringTree::String(string) => {
                default_visit_expression_formatted_string(self, value, context)?;
                context.push_param(Param::Owned(KnownBlock::PrimitiveBlock {
                    value: string.into(),
                }));
                return Ok(());
            }
            FormattedStringTree::EmptyString => {
                default_visit_expression_formatted_string(self, value, context)?;
                context.push_param(Param::Owned(KnownBlock::PrimitiveBlock {
                    value: "".into(),
                }));
                return Ok(());
            }
            FormattedStringTree::Joined(left, right, parent, this_id) => {
                FormattedStringTree::Joined(left, right, parent, this_id)
            }
        };
        default_visit_expression_formatted_string(self, value, context)?;
        let param = convert_into_block_tree(context, formatted_string)?;
        context.push_param(param);
        Ok(())
    }

    fn visit_formatted_string_content(
        &self,
        value: &FormattedStringContent,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_parent = context.formatted_string_context.ids
            [context.formatted_string_context.current_idx]
            .clone();
        context.formatted_string_context.current_idx += 1;
        default_visit_formatted_string_content(self, value, context)
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
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_get_item(self, value, context)?;
            let index = context.pop_param().unwrap();
            let index_input_value = (
                param_to_input_repr_no_menu(index, context)?,
                Some(Sb3PrimitiveBlock::Integer(Sb3Primitive::Int128(1))),
            );
            let actual_identifier = get_actual_identifier!(context, value.0)?;
            let actual_identifier_ref = actual_identifier.borrow();
            let known_block = get_known_block!(actual_identifier_ref, value.0)?;
            let (canonical_name, id) = match known_block {
                KnownBlock::List { canonical_name, id } => (canonical_name, id),
                _ => {
                    return Err(GrazeSb3GeneratorError::ListAccessForNonLists {
                        identifier: value.0.clone(),
                    });
                }
            };
            let list_field_value = Sb3FieldValue::WithId {
                value: (canonical_name as &str).into(),
                id: id.to_string(),
            };
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "data_itemoflist".to_string(),
                    HashMap::from([("INDEX".to_string(), index_input_value.into())]),
                    HashMap::from([("LIST".to_string(), list_field_value)]),
                    false,
                    None,
                ),
            );
            context.push_param(Param::Owned(this_id.into()));
            Ok(())
        })
    }

    fn visit_expression_get_letter(
        &self,
        value: (
            &Box<Expression>,
            &crate::parser::ast::LetterAccessLeftBracket,
            &Box<Expression>,
            &crate::parser::ast::RightBracket,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_get_letter(self, value, context)?;
            let string = context.pop_param().unwrap();
            let string_input_value: (_, Option<Sb3PrimitiveBlock>) = (
                param_to_input_repr_no_menu(string, context)?,
                Some("apple".into()),
            );
            let index = context.pop_param().unwrap();
            let index_input_value = (
                param_to_input_repr_no_menu(index, context)?,
                Some(Sb3PrimitiveBlock::PositiveInteger(
                    super::project_json::Sb3Primitive::Int128(1),
                )),
            );
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "operator_letter_of".to_string(),
                    HashMap::from([
                        ("STRING".to_string(), string_input_value.into()),
                        ("LETTER".to_string(), index_input_value.into()),
                    ]),
                    HashMap::new(),
                    false,
                    None,
                ),
            );
            context.push_param(Param::Owned(this_id.into()));
            Ok(())
        })
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
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_unary_operation(self, value, context)?;
            let operand = context.pop_param().unwrap();
            let UnOpDescriptor {
                opcode,
                operand_input_name,
                field_values,
                default,
            } = value.0.get_descriptor();
            let operand_input_value = (param_to_input_repr_no_menu(operand, context)?, default);
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    opcode.to_string(),
                    HashMap::from([(operand_input_name, operand_input_value.into())]),
                    field_values,
                    false,
                    None,
                ),
            );
            context.push_param(Param::Owned(this_id.into()));
            Ok(())
        })
    }

    fn visit_expression_identifier(
        &self,
        value: &crate::parser::ast::Identifier,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        default_visit_expression_identifier(self, value, context)?;
        context.push_param(Param::LazyIdentifier(value.clone()));
        Ok(())
    }

    fn visit_expression_literal(
        &self,
        value: &crate::parser::ast::Literal,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        default_visit_expression_literal(self, value, context)?;
        context.push_param(Param::Owned(KnownBlock::PrimitiveBlock {
            value: value.into(),
        }));
        Ok(())
    }

    // Statements:

    fn visit_statement_assignment(
        &self,
        value: (
            &Identifier,
            &crate::parser::ast::NormalAssignmentOperator,
            &Expression,
            &crate::parser::ast::Semicolon,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_assignment(self, value, context)?;
            let actual_identifier = get_actual_identifier!(context, value.0)?;
            let actual_identifier_ref = actual_identifier.borrow();
            let known_block = get_known_block!(actual_identifier_ref, value.0)?;
            let SimpleCallableKnownBlockSignature(opcode, param, known_params) =
                known_block.resolve_for_assignment(context);
            let assignment_value = context.pop_param().unwrap();
            let mut fields = HashMap::new();
            let mut inputs = HashMap::new();
            add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
            with_known_block!(context, assignment_value, known_block => {
                add_param_to_params(context, param, known_block, &mut inputs, &mut fields)?;
            });
            add_block(
                context,
                &this_id,
                make_block(parent, opcode.to_string(), inputs, fields, false, None),
            );
            Ok(())
        })
    }

    fn visit_statement_multi_input_control(
        &self,
        value: (
            &Identifier,
            &crate::parser::ast::LeftParens,
            &Vec<(Expression, Option<crate::parser::ast::Comma>)>,
            &crate::parser::ast::RightParens,
            &crate::parser::ast::CodeBlock,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_multi_input_control(self, value, context)?;
            let substack = context.pop_param().unwrap();
            let reversed_args = iter::repeat_with(|| context.pop_param().unwrap())
                .take(value.2.len())
                .collect::<Vec<_>>();
            let arg_count = reversed_args.len();
            create_control_block(
                context,
                value.0,
                reversed_args.into_iter().rev(),
                arg_count,
                substack,
                parent,
                this_id,
            )
        })
    }

    fn visit_statement_single_input_control(
        &self,
        value: (
            &Identifier,
            &Expression,
            &crate::parser::ast::CodeBlock,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_single_input_control(self, value, context)?;
            let substack = context.pop_param().unwrap();
            let arg = context.pop_param().unwrap();
            create_control_block(
                context,
                value.0,
                iter::once(arg),
                1,
                substack,
                parent,
                this_id,
            )
        })
    }

    fn visit_statement_call(
        &self,
        value: (
            &Identifier,
            &crate::parser::ast::LeftParens,
            &Vec<(Expression, Option<crate::parser::ast::Comma>)>,
            &crate::parser::ast::RightParens,
            &crate::parser::ast::Semicolon,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_call(self, value, context)?;
            let reversed_args = iter::repeat_with(|| context.pop_param().unwrap())
                .take(value.2.len())
                .collect::<Vec<_>>();
            let actual_identifier = get_actual_identifier!(context, value.0)?;
            let actual_identifier_ref = actual_identifier.borrow();
            let known_block = get_known_block!(actual_identifier_ref, value.0)?;
            let CallableKnownBlockSignature(opcode, params, known_params) =
                known_block.resolve_for_call_block(context);
            let mut fields = HashMap::new();
            let mut inputs = HashMap::new();
            add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
            if params.len() != reversed_args.len() {
                return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                    unexpected: reversed_args.len(),
                    expected: params.len(),
                });
            }
            for (param, value) in zip(params.iter(), reversed_args.into_iter().rev()) {
                with_known_block!(context, value, value => {
                    add_param_to_params(context, param, value, &mut inputs, &mut fields)?;
                });
            }
            add_block(
                context,
                &this_id,
                make_block(parent, opcode.to_string(), inputs, fields, false, None),
            );
            Ok(())
        })
    }

    fn visit_statement_forever(
        &self,
        value: (
            &Identifier,
            &crate::parser::ast::CodeBlock,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_forever(self, value, context)?;
            let substack = context.pop_param().unwrap();
            create_control_block(
                context,
                value.0,
                iter::empty(),
                0,
                substack,
                parent,
                this_id,
            )
        })
    }

    fn visit_statement_set_item(
        &self,
        value: (
            &Identifier,
            &crate::parser::ast::LeftBracket,
            &Expression,
            &crate::parser::ast::RightBracket,
            &crate::parser::ast::NormalAssignmentOperator,
            &Expression,
            &crate::parser::ast::Semicolon,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_set_item(self, value, context)?;
            let item_value = context.pop_param().unwrap();
            let item_value_input_value = (
                param_to_input_repr_no_menu(item_value, context)?,
                Some(Sb3PrimitiveBlock::String("thing".into())),
            );
            let index = context.pop_param().unwrap();
            let index_input_value = (
                param_to_input_repr_no_menu(index, context)?,
                Some(Sb3PrimitiveBlock::Integer(Sb3Primitive::Int128(1))),
            );
            let actual_identifier = get_actual_identifier!(context, value.0)?;
            let actual_identifier_ref = actual_identifier.borrow();
            let known_block = get_known_block!(actual_identifier_ref, value.0)?;
            let (canonical_name, id) = match known_block {
                KnownBlock::List { canonical_name, id } => (canonical_name, id),
                _ => {
                    return Err(GrazeSb3GeneratorError::ListAccessForNonLists {
                        identifier: value.0.clone(),
                    });
                }
            };
            let list_field_value = Sb3FieldValue::WithId {
                value: (canonical_name as &str).into(),
                id: id.to_string(),
            };
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "data_replaceitemoflist".to_string(),
                    HashMap::from([
                        ("INDEX".to_string(), index_input_value.into()),
                        ("ITEM".to_string(), item_value_input_value.into()),
                    ]),
                    HashMap::from([("LIST".to_string(), list_field_value)]),
                    false,
                    None,
                ),
            );
            Ok(())
        })
    }

    fn visit_statement_list_assignment(
        &self,
        value: (
            &Identifier,
            &crate::parser::ast::NormalAssignmentOperator,
            &crate::parser::ast::LeftBracket,
            &Vec<(
                crate::parser::ast::ListEntry,
                Option<crate::parser::ast::Comma>,
            )>,
            &crate::parser::ast::RightBracket,
            &crate::parser::ast::Semicolon,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        let actual_identifier = get_actual_identifier!(context, value.0)?;
        let actual_identifier_ref = actual_identifier.borrow();
        let known_block = get_known_block!(actual_identifier_ref, value.0)?;
        let (canonical_name, id) = match known_block {
            KnownBlock::List { canonical_name, id } => (canonical_name, id),
            _ => {
                return Err(GrazeSb3GeneratorError::ListAccessForNonLists {
                    identifier: value.0.clone(),
                });
            }
        };
        let list_field_value = Sb3FieldValue::WithId {
            value: (canonical_name as &str).into(),
            id: id.to_string(),
        };
        wrap_in_statement(context, |context, parent, this_id| {
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "data_deletealloflist".to_string(),
                    HashMap::new(),
                    HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                    false,
                    None,
                ),
            );
        });
        for item in value.3 {
            match &item.0 {
                ListEntry::Expression(expression) => {
                    wrap_in_statement(context, |context, parent, this_id| {
                        self.visit_expression(expression, context)?;
                        let param = context.pop_param().unwrap();
                        let value = param_to_input_repr_no_menu(param, context)?;
                        #[cfg(feature = "use_shadows_for_formatted_strings")]
                        let value = (
                            value,
                            Some(Sb3PrimitiveBlock::String(
                                #[cfg(feature = "use_actual_defaults_for_formatted_strings")]
                                "thing".into(),
                                #[cfg(not(feature = "use_actual_defaults_for_formatted_strings"))]
                                "".into(),
                            )),
                        );
                        add_block(
                            context,
                            &this_id,
                            make_block(
                                parent,
                                "data_addtolist".to_string(),
                                HashMap::from([("ITEM".to_string(), value.into())]),
                                HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                                false,
                                None,
                            ),
                        );
                        Ok(())
                    })?;
                }
                ListEntry::Unwrap(literal, _) => {
                    for c in literal.get_string_value().as_str().chars() {
                        wrap_in_statement(context, |context, parent, this_id| {
                            add_block(
                                context,
                                &this_id,
                                make_block(
                                    parent,
                                    "data_addtolist".to_string(),
                                    HashMap::from([(
                                        "ITEM".to_string(),
                                        Sb3InputValue::Shadow(Sb3InputRepr::PrimitiveBlock(
                                            Sb3PrimitiveBlock::String(c.to_string().into()),
                                        )),
                                    )]),
                                    HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                                    false,
                                    None,
                                ),
                            );
                            Ok(())
                        })?;
                    }
                }
            }
        }
        Ok(())
    }

    // Target statements:

    fn visit_isolated_block(
        &self,
        value: (
            &crate::parser::ast::CodeBlock,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        default_visit_isolated_block(self, value, context)
    }

    fn visit_isolated_expression(
        &self,
        value: (
            &crate::parser::ast::LeftParens,
            &Expression,
            &crate::parser::ast::RightParens,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        default_visit_isolated_expression(self, value, context)
    }

    // Assets:

    fn visit_top_level_statement_stage(
        &self,
        value: (
            &crate::parser::ast::StageKeyword,
            &crate::parser::ast::StageCodeBlock,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        let mut stage = context
            .uninitialized_stage
            .take()
            .ok_or(GrazeSb3GeneratorError::RepeatedStageInitialization)?;
        let target_name = literal!("stage");
        let assets = context.target_attachments.remove(&target_name).unwrap();
        for asset in assets {
            match asset {
                TargetAttachment::Costume(costume) => stage.costumes.push(costume),
                TargetAttachment::Sound(sound) => stage.sounds.push(sound),
                TargetAttachment::Var(name, sb3_variable_declaration) => {
                    stage.variables.insert(name, sb3_variable_declaration);
                }
                TargetAttachment::List(name, sb3_list_declaration) => {
                    stage.lists.insert(name, sb3_list_declaration);
                }
            }
        }
        context.current_sb3_target = Some(stage);
        default_visit_top_level_statement_stage(self, value, context)?;
        context
            .sb3
            .targets
            .push(context.current_sb3_target.take().unwrap());
        Ok(())
    }

    fn visit_top_level_statement_sprite(
        &self,
        value: (
            &crate::parser::ast::SpriteKeyword,
            &Option<crate::parser::ast::CanonicalIdentifier>,
            &Identifier,
            &crate::parser::ast::SpriteCodeBlock,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        let target_name = value
            .1
            .as_ref()
            .map(|value| value.name.clone())
            .unwrap_or_else(|| value.2.names.last().unwrap().0.clone());
        // TODO: explicitly error when multiple sprites have the same name
        let assets = context.target_attachments.remove(&target_name).unwrap();
        let mut new_sprite = Sb3Target::new_sprite(target_name.to_string());
        for asset in assets {
            match asset {
                TargetAttachment::Costume(costume) => new_sprite.costumes.push(costume),
                TargetAttachment::Sound(sound) => new_sprite.sounds.push(sound),
                TargetAttachment::Var(name, sb3_variable_declaration) => {
                    new_sprite.variables.insert(name, sb3_variable_declaration);
                }
                TargetAttachment::List(name, sb3_list_declaration) => {
                    new_sprite.lists.insert(name, sb3_list_declaration);
                }
            }
        }
        new_sprite.layer_order = context.sb3.targets.len()
            + if context.uninitialized_stage.is_some() {
                1
            } else {
                0
            };
        context.current_sb3_target = Some(new_sprite);
        default_visit_top_level_statement_sprite(self, value, context)?;
        context
            .sb3
            .targets
            .push(context.current_sb3_target.take().unwrap());
        Ok(())
    }
}
