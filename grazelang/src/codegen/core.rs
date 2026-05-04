#![allow(clippy::result_large_err)]
use std::{
    collections::HashMap,
    iter::{self, zip},
    rc::Rc,
};

use arcstr::{ArcStr as IString, literal};
use grazelang_library::{
    BindInfo, CallBlockParam, CallBlockParamKind, CallableKnownBlockSignature, HasShadow,
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
    library::{self, create_sprite_dependent_symbols, create_stage_dependent_symbols},
    names::Namespace,
    parser::{
        ast::{
            BinOpDescriptor, CustomBlockParamKind, CustomBlockParamKindValue, DataDeclarationScope,
            Expression, FormattedStringContent, Identifier, ListEntry, Literal, UnOpDescriptor,
        },
        parse_context::{
            IdString, KnownBlock, ParseContext, ResolveKnownBlock, Symbol, SymbolId, SymbolTable,
            Target, TargetSymbolDescriptor,
        },
    },
    visitor::{
        GrazeVisitor, default_visit_code_block, default_visit_custom_block_definition,
        default_visit_expression_binary_operation, default_visit_expression_call,
        default_visit_expression_formatted_string, default_visit_expression_get_item,
        default_visit_expression_get_letter, default_visit_expression_identifier,
        default_visit_expression_literal, default_visit_expression_unary_operation,
        default_visit_formatted_string_content, default_visit_isolated_block,
        default_visit_isolated_expression, default_visit_multi_input_hat_statement,
        default_visit_no_input_hat_statement, default_visit_single_input_hat_statement,
        default_visit_statement_assignment, default_visit_statement_call,
        default_visit_statement_forever, default_visit_statement_multi_input_control,
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
    // Root symbol is always 0.
    pub symbol_table: SymbolTable,
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
    pub asset_files: HashMap<String, IString>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AssetFile {
    pub file_name: String,
    pub file_path: IString,
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

pub fn add_bind_info(symbol: &mut Symbol, parent_target: &IString) {
    if let Some(known_block) = &mut symbol.known_block {
        match Rc::get_mut(known_block).unwrap() {
            KnownBlock::Variable {
                bind_info,
                canonical_name,
                id,
                ..
            } => {
                bind_info.replace(BindInfo {
                    parent_target: parent_target.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field { default: None },
                                name: literal!("PROPERTY"),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::WithId {
                                    value: Sb3Primitive::String(canonical_name.to_string()),
                                    id: id.to_string(),
                                },
                            },
                        ),
                        {
                            let name = literal!("OBJECT");
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: name.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name,
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(parent_target.as_str().into()),
                                },
                            )
                        },
                    ],
                });
            }
            KnownBlock::List { .. }
            | KnownBlock::FieldValue { .. }
            | KnownBlock::BlockRef { .. }
            | KnownBlock::PrimitiveBlock { .. }
            | KnownBlock::Callable(..)
            | KnownBlock::PartialCallable(..)
            | KnownBlock::SingletonReporter { .. }
            | KnownBlock::CustomBlock { .. }
            | KnownBlock::Empty => (),
        }
    }
}

pub const STAGE_ISTRING: &IString = &literal!("stage");
pub const SPRITES_ISTRING: &IString = &literal!("sprites");
pub const MY_BLOCKS_ISTRING: &IString = &literal!("my_blocks");

impl GrazeSb3GeneratorContext {
    pub fn new(parse_context: ParseContext) -> Result<Self, std::io::Error> {
        let mut this = Self::without_standard_namespaces(parse_context)?;
        library::add_standard_library_namespaces(&mut this.symbol_table, Default::default());
        Self::alias_standard_namespaces(&mut this.symbol_table, Default::default());
        Ok(this)
    }

    fn alias_standard_namespaces(symbol_table: &mut SymbolTable, root_symbol: SymbolId) {
        let namespaces = &symbol_table[root_symbol].namespace;
        let mut flattened = HashMap::<IString, Option<SymbolId>>::new();
        for &namespace in namespaces.values() {
            for (key, &value) in &symbol_table[namespace].namespace {
                if let Some(mut_value) = flattened.get_mut(key) {
                    mut_value.take();
                } else {
                    flattened.insert(key.clone(), Some(value));
                }
            }
        }
        for (key, value) in flattened {
            if let Some(value) = value {
                symbol_table.insert_alias(root_symbol, key, value);
            }
        }
    }

    pub fn without_standard_namespaces(
        mut parse_context: ParseContext,
    ) -> Result<Self, std::io::Error> {
        let mut rng = Xoshiro256StarStar::from_seed(parse_context.random_seed);
        let targets: Vec<Target> = parse_context.parsed_targets.into();
        let standard_library_namespace_count = library::get_standard_library_namespace_count();
        let mut symbol_table =
            SymbolTable::with_capacity(1 + standard_library_namespace_count + 4 + targets.len());
        let root_symbol = symbol_table.new_symbol(Symbol {
            known_block: None,
            namespace: HashMap::with_capacity(standard_library_namespace_count + 4),
            parent: Default::default(),
        });

        let targets_symbol = symbol_table.new_child_symbol(
            root_symbol,
            SPRITES_ISTRING.clone(),
            None,
            targets.len(),
        );
        let mut asset_files = HashMap::new();
        let mut target_attachments = HashMap::with_capacity(targets.len());
        for target in &targets {
            let mut namespace = Namespace::new();

            let is_stage = matches!(target, Target::Stage { .. });
            let symbol_count = target.borrow_symbols().len()
                // Accounts for the symbols that every target has e.g. volume
                + if is_stage {
                    3
                } else {
                    7
                };
            let target_symbol = symbol_table.new_child_symbol(
                targets_symbol,
                target.get_namespace_name().clone(),
                Some(Rc::new(KnownBlock::FieldValue {
                    value: Sb3FieldValue::Normal(super::project_json::Sb3Primitive::String(
                        target.get_field_name(),
                    )),
                })),
                symbol_count,
            );
            let (mut symbols, assets) = target
                .borrow_symbols()
                .iter()
                .map(|(key, value)| {
                    value.derive_related_data(&mut rng, &mut namespace).map(
                        |(mut symbol, attachment, asset_file)| {
                            add_bind_info(&mut symbol, target.get_namespace_name());
                            if let Some(AssetFile {
                                file_name,
                                file_path,
                            }) = asset_file
                            {
                                asset_files.insert(file_name, file_path);
                            }
                            ((key.clone(), symbol), attachment)
                        },
                    )
                })
                .try_fold::<_, _, Result<_, std::io::Error>>(
                    (HashMap::with_capacity(symbol_count), Vec::new()),
                    |(mut symbols, mut target_attachments), item| {
                        let ((key, symbol), asset) = item?;
                        symbols.insert(key, symbol);
                        if let Some(asset) = asset {
                            target_attachments.push(asset);
                        }
                        Ok((symbols, target_attachments))
                    },
                )?;
            symbols.extend(if is_stage {
                create_stage_dependent_symbols(target.get_namespace_name())
            } else {
                create_sprite_dependent_symbols(target.get_namespace_name())
            });
            target_attachments.insert(target.get_namespace_name().clone(), assets);
            for (key, symbol) in symbols {
                let symbol = symbol_table.new_symbol(symbol);
                symbol_table.insert_child(target_symbol, key, symbol);
            }
        }
        let stage_symbol = symbol_table
            .get_child(targets_symbol, STAGE_ISTRING)
            .unwrap();

        let broadcasts_symbol = symbol_table.new_child_symbol(
            root_symbol,
            literal!("broadcasts"),
            None,
            parse_context.broadcasts.len(),
        );
        for (key, value) in &parse_context.broadcasts {
            let symbol = symbol_table.new_symbol(value.derive_symbol(&mut rng));
            symbol_table.insert_child(broadcasts_symbol, key.clone(), symbol);
        }
        let variables_symbol =
            symbol_table.new_child_symbol(root_symbol, literal!("vars"), None, 0);
        let lists_symbol = symbol_table.new_child_symbol(root_symbol, literal!("lists"), None, 0);
        let mut global_namespace = Namespace::new();
        {
            let stage_target_attachments = target_attachments.get_mut("stage").unwrap();
            for (name, symbol) in parse_context.global_symbols.drain() {
                match &symbol {
                    TargetSymbolDescriptor::Var(_) => {
                        let (symbol, attachment, asset_file) = symbol
                            .derive_related_data(&mut rng, &mut global_namespace)
                            .unwrap();
                        if let Some(AssetFile {
                            file_name,
                            file_path,
                        }) = asset_file
                        {
                            asset_files.insert(file_name, file_path);
                        }
                        let mut symbol_for_stage: Symbol = Symbol {
                            known_block: symbol
                                .known_block
                                .as_ref()
                                .map(|value| Rc::new(value.as_ref().clone())),
                            namespace: symbol.namespace.clone(),
                            parent: symbol.parent,
                        };
                        add_bind_info(&mut symbol_for_stage, &literal!("_stage_"));
                        if let Some(attachment) = attachment {
                            stage_target_attachments.push(attachment);
                        }
                        let symbol_for_stage = symbol_table.new_symbol(symbol_for_stage);
                        let symbol = symbol_table.new_symbol(symbol);
                        symbol_table.insert_child(stage_symbol, name.clone(), symbol_for_stage);
                        symbol_table.insert_child(variables_symbol, name, symbol);
                    }
                    TargetSymbolDescriptor::List(_) => {
                        let (symbol, attachment, asset_file) = symbol
                            .derive_related_data(&mut rng, &mut global_namespace)
                            .unwrap();
                        if let Some(AssetFile {
                            file_name,
                            file_path,
                        }) = asset_file
                        {
                            asset_files.insert(file_name, file_path);
                        }
                        if let Some(attachment) = attachment {
                            stage_target_attachments.push(attachment);
                        }
                        let symbol = symbol_table.new_symbol(symbol);
                        symbol_table.insert_child(lists_symbol, name, symbol);
                    }
                    _ => (), // Handled just to be sure although it shouldn't happen
                }
            }
        }
        let mut block_counter = IdCounter::new();
        let next_block_id = block_counter.get_new_id();
        Ok(Self {
            sb3: Sb3Root::default(),
            targets,
            symbol_table,
            block_counter,
            arg_stack: Vec::new(),
            current_block_id: next_block_id,
            current_parent: None,
            current_sb3_target: None,
            uninitialized_stage: Some(Sb3Target::new_stage()),
            current_previous_block: None,
            formatted_string_context: FormattedStringContext::new(),
            target_attachments,
            asset_files,
        })
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

    pub fn resolve_path<'a, I>(&self, mut iterator: I) -> Option<SymbolId>
    where
        I: Iterator<Item = &'a IString>,
    {
        iterator.try_fold(SymbolId::default(), |current, next| {
            if next.as_str() == "super" {
                Some(self.symbol_table[current].parent)
            } else {
                self.symbol_table.get_child(current, next)
            }
        })
    }

    pub fn resolve_identifier(&self, identifier: &Identifier) -> Option<SymbolId> {
        self.resolve_path(
            identifier
                .scope
                .iter()
                .chain(identifier.names.iter())
                .map(|(next, _)| next),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Param {
    Owned(KnownBlock),
    LazyIdentifier(Identifier),
    BlockStack(Option<String>),
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
        let value = Param::BlockStack(Some(block_id.to_string()));
        context.push_param(value);
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
                let symbol_id = $context.resolve_identifier(&value).ok_or_else(|| {
                    GrazeSb3GeneratorError::UnknownIdentifier {
                        identifier: value.clone(),
                    }
                })?;
                let symbol = &$context.symbol_table[symbol_id];
                let known_block_rc = symbol.known_block.clone();
                let $known_block = known_block_rc.as_ref().ok_or_else(|| {
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

pub fn get_symbol_id(
    context: &mut GrazeSb3GeneratorContext,
    identifier: &Identifier,
) -> Result<SymbolId, GrazeSb3GeneratorError> {
    context.resolve_identifier(identifier).ok_or_else(|| {
        GrazeSb3GeneratorError::UnknownIdentifier {
            identifier: identifier.clone(),
        }
    })
}

pub fn get_known_block<'a>(
    symbol: &'a Symbol,
    identifier: &Identifier,
) -> Result<&'a Rc<KnownBlock>, GrazeSb3GeneratorError> {
    symbol
        .known_block
        .as_ref()
        .ok_or_else(|| GrazeSb3GeneratorError::IdentifierIsNotABlock {
            identifier: identifier.clone(),
        })
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

pub fn create_input_value<D>(
    input_repr: Option<(Sb3InputRepr, IsShadow)>,
    default: Option<D>,
) -> Option<Sb3InputValue>
where
    ((Sb3InputRepr, IsShadow), Option<D>): Into<Sb3InputValue>,
    D: Into<Sb3InputValue>,
{
    if let Some(input_repr) = input_repr {
        Some((input_repr, default).into())
    } else {
        default.map(Into::into)
    }
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
            if let Some(input_value) = create_input_value(
                known_block_to_input_repr_no_menu(value, context)?,
                default.as_ref(),
            ) {
                inputs.insert(param_name, input_value);
            }
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
                grazelang_library::KnownBlockInput::Empty => (
                    Sb3InputRepr::Reference(
                        introduce_input_menu(opcode, field_name, default.clone(), context)
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
) -> Result<Option<(Sb3InputRepr, IsShadow)>, GrazeSb3GeneratorError> {
    let known_block_input = known_block.resolve_for_input(context);
    match known_block_input {
        grazelang_library::KnownBlockInput::PrimitiveInput(sb3_primitive_block) => {
            let is_shadow = sb3_primitive_block.is_shadow();
            Ok(Some((
                Sb3InputRepr::PrimitiveBlock(sb3_primitive_block),
                is_shadow,
            )))
        }
        grazelang_library::KnownBlockInput::BlockRef(id) => Ok(Some((
            Sb3InputRepr::Reference(id.to_string()),
            IsShadow::No,
        ))),
        grazelang_library::KnownBlockInput::SimpleBlock(opcode, params) => Ok(Some((
            Sb3InputRepr::Reference(
                introduce_input_simple_block(opcode, params.iter(), context)?.to_string(),
            ),
            IsShadow::No,
        ))),
        grazelang_library::KnownBlockInput::Menu(input_menu_value) => {
            Err(GrazeSb3GeneratorError::UnexpectedInputMenu { input_menu_value })
        }
        grazelang_library::KnownBlockInput::Empty => Ok(None),
    }
}

pub fn param_to_input_repr_no_menu(
    param: Param,
    context: &mut GrazeSb3GeneratorContext,
) -> Result<Option<(Sb3InputRepr, IsShadow)>, GrazeSb3GeneratorError> {
    with_known_block!(context, param, value => {
        known_block_to_input_repr_no_menu(value, context)
    })
}

pub fn make_proc_call_mutation(
    mutation: (&IString, &[(IString, HasShadow)], &bool),
) -> Sb3BlockMutation {
    Sb3BlockMutation::ProceduresCall {
        procedure_code: mutation.0.to_string(),
        argument_ids: mutation.1.iter().map(|value| value.0.to_string()).collect(),
        warp: *mutation.2,
    }
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
    let symbol_id = get_symbol_id(context, identifier)?;
    let symbol = &context.symbol_table[symbol_id];
    let known_block = get_known_block(symbol, identifier)?.clone();
    let CallableKnownBlockSignature(opcode, params, known_params, mutation) =
        known_block.resolve_for_call_block(context);
    let mut fields = HashMap::new();
    let mut inputs = HashMap::new();
    add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
    if params.len() != arg_count + 1 {
        return Err(GrazeSb3GeneratorError::IncorrectParamCount {
            unexpected: arg_count + 1,
            expected: params.len(),
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
    if let Some(substack) = substack {
        inputs.insert(
            substack_input_name.to_string(),
            Sb3InputValue::NoShadow(Sb3InputRepr::Reference(substack)),
        );
    }
    add_block(
        context,
        &this_id,
        make_block(
            parent,
            opcode.to_string(),
            inputs,
            fields,
            false,
            mutation.map(make_proc_call_mutation),
        ),
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
            let mut inputs = HashMap::with_capacity(2);
            if let Some(input_value) = create_input_value(
                param_to_input_repr_no_menu(op_a_param, context)?,
                operand_a_default.as_ref(),
            ) {
                inputs.insert(operand_a_input_name, input_value);
            }
            if let Some(input_value) = create_input_value(
                param_to_input_repr_no_menu(op_b_param, context)?,
                operand_b_default.as_ref(),
            ) {
                inputs.insert(operand_b_input_name, input_value);
            }
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
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
            let CallableKnownBlockSignature(opcode, params, known_params, mutation) =
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
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
                    fields,
                    false,
                    mutation.map(make_proc_call_mutation),
                ),
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
            left: Option<Sb3InputValue>,
            right: Option<Sb3InputValue>,
        ) -> Sb3Block {
            let mut inputs = HashMap::with_capacity(2);
            if let Some(left) = left {
                inputs.insert("STRING1".to_string(), left);
            }
            if let Some(right) = right {
                inputs.insert("STRING2".to_string(), right);
            }
            make_block(
                parent,
                "operator_join".to_string(),
                inputs,
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
                    let left = create_input_value(
                        left,
                        #[cfg(feature = "use_shadows_for_formatted_strings")]
                        Some(Sb3PrimitiveBlock::String(
                            #[cfg(feature = "use_actual_defaults_for_formatted_strings")]
                            "apple ".into(),
                            #[cfg(not(feature = "use_actual_defaults_for_formatted_strings"))]
                            "".into(),
                        )),
                        #[cfg(not(feature = "use_shadows_for_formatted_strings"))]
                        None::<Sb3PrimitiveBlock>,
                    );
                    let right = create_input_value(
                        right,
                        #[cfg(feature = "use_shadows_for_formatted_strings")]
                        Some(Sb3PrimitiveBlock::String(
                            #[cfg(feature = "use_actual_defaults_for_formatted_strings")]
                            "banana".into(),
                            #[cfg(not(feature = "use_actual_defaults_for_formatted_strings"))]
                            "".into(),
                        )),
                        #[cfg(not(feature = "use_shadows_for_formatted_strings"))]
                        None::<Sb3PrimitiveBlock>,
                    );
                    add_block(context, &this_id, make_join(parent, left, right));
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

            let inputs = if let Some(index_input_value) = create_input_value(
                param_to_input_repr_no_menu(index, context)?,
                Some(Sb3PrimitiveBlock::Integer(Sb3Primitive::Int128(1))),
            ) {
                HashMap::from([("INDEX".to_string(), index_input_value)])
            } else {
                HashMap::new()
            };
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
            let (canonical_name, id) = match known_block.as_ref() {
                KnownBlock::List { canonical_name, id } => (canonical_name, id),
                _ => {
                    return Err(GrazeSb3GeneratorError::ListAccessForNonLists {
                        identifier: value.0.clone(),
                    });
                }
            };
            let list_field_value = Sb3FieldValue::WithId {
                value: canonical_name.as_str().into(),
                id: id.to_string(),
            };
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "data_itemoflist".to_string(),
                    inputs,
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
            let string_input_value = create_input_value::<Sb3PrimitiveBlock>(
                param_to_input_repr_no_menu(string, context)?,
                Some("apple".into()),
            );
            let index = context.pop_param().unwrap();
            let index_input_value = create_input_value(
                param_to_input_repr_no_menu(index, context)?,
                Some(Sb3PrimitiveBlock::PositiveInteger(
                    super::project_json::Sb3Primitive::Int128(1),
                )),
            );
            let mut inputs = HashMap::with_capacity(2);
            if let Some(string_input_value) = string_input_value {
                inputs.insert("STRING".to_string(), string_input_value);
            }
            if let Some(index_input_value) = index_input_value {
                inputs.insert("LETTER".to_string(), index_input_value);
            }
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "operator_letter_of".to_string(),
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
                extra_inputs,
                field_values,
                default,
            } = value.0.get_descriptor();
            let operand_input_value =
                create_input_value(param_to_input_repr_no_menu(operand, context)?, default);
            let mut inputs = HashMap::with_capacity(extra_inputs.len() + 1);
            if let Some(operand_input_value) = operand_input_value {
                inputs.insert(operand_input_name, operand_input_value);
            }
            for (key, value) in extra_inputs {
                inputs.insert(
                    key,
                    Sb3InputValue::Shadow(Sb3InputRepr::PrimitiveBlock(value)),
                );
            }
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
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
        context.push_param(match value {
            Literal::EmptyExpression(..) => Param::Owned(KnownBlock::Empty),
            _ => Param::Owned(KnownBlock::PrimitiveBlock {
                value: value.into(),
            }),
        });
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
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
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
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
            let CallableKnownBlockSignature(opcode, params, known_params, mutation) =
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
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
                    fields,
                    false,
                    mutation.map(make_proc_call_mutation),
                ),
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
            let item_value_input_value = create_input_value::<Sb3PrimitiveBlock>(
                param_to_input_repr_no_menu(item_value, context)?,
                Some("thing".into()),
            );
            let index = context.pop_param().unwrap();
            let index_input_value = create_input_value(
                param_to_input_repr_no_menu(index, context)?,
                Some(Sb3PrimitiveBlock::Integer(Sb3Primitive::Int128(1))),
            );
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
            let (canonical_name, id) = match known_block.as_ref() {
                KnownBlock::List { canonical_name, id } => (canonical_name, id),
                _ => {
                    return Err(GrazeSb3GeneratorError::ListAccessForNonLists {
                        identifier: value.0.clone(),
                    });
                }
            };
            let list_field_value = Sb3FieldValue::WithId {
                value: canonical_name.as_str().into(),
                id: id.to_string(),
            };
            let mut inputs = HashMap::with_capacity(2);
            if let Some(index_input_value) = index_input_value {
                inputs.insert("INDEX".to_string(), index_input_value);
            }
            if let Some(item_value_input_value) = item_value_input_value {
                inputs.insert("ITEM".to_string(), item_value_input_value);
            }
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "data_replaceitemoflist".to_string(),
                    inputs,
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
        let symbol_id = get_symbol_id(context, value.0)?;
        let symbol = &context.symbol_table[symbol_id];
        let known_block = get_known_block(symbol, value.0)?.clone();
        let (canonical_name, id) = match known_block.as_ref() {
            KnownBlock::List { canonical_name, id } => (canonical_name, id),
            _ => {
                return Err(GrazeSb3GeneratorError::ListAccessForNonLists {
                    identifier: value.0.clone(),
                });
            }
        };
        let list_field_value = Sb3FieldValue::WithId {
            value: canonical_name.as_str().into(),
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
                        let value = create_input_value(
                            value,
                            #[cfg(feature = "use_shadows_for_list_assignment")]
                            Some(Sb3PrimitiveBlock::String(
                                #[cfg(feature = "use_actual_defaults_for_list_assignment")]
                                "thing".into(),
                                #[cfg(not(feature = "use_actual_defaults_for_list_assignment"))]
                                "".into(),
                            )),
                            #[cfg(not(feature = "use_shadows_for_list_assignment"))]
                            None::<Sb3PrimitiveBlock>,
                        );
                        let inputs = if let Some(value) = value {
                            HashMap::from([("ITEM".to_string(), value)])
                        } else {
                            HashMap::new()
                        };
                        add_block(
                            context,
                            &this_id,
                            make_block(
                                parent,
                                "data_addtolist".to_string(),
                                inputs,
                                HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                                false,
                                None,
                            ),
                        );
                        Ok(())
                    })?;
                }
                ListEntry::Unwrap(literal, _) => {
                    for c in literal.get_string_value().chars() {
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

    fn visit_statement_data_declaration(
        &self,
        value: (
            &crate::parser::ast::LetKeyword,
            &crate::parser::ast::DataDeclaration,
            &crate::parser::ast::Semicolon,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        let stage_var_symbol = [literal!("vars")];
        let stage_list_symbol = [literal!("lists")];
        let (this_target_var_symbol, this_target_list_symbol) = {
            let current_target = context.current_sb3_target.as_ref().unwrap();
            if current_target.is_stage {
                (
                    (stage_var_symbol[0].clone(), None),
                    (stage_list_symbol[0].clone(), None),
                )
            } else {
                let value: (_, Option<IString>) = (
                    SPRITES_ISTRING.clone(),
                    Some(current_target.name.as_str().into()),
                );
                (value.clone(), value)
            }
        };
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        pub enum SingleAssignment {
            List([Option<IString>; 3], Vec<Sb3Primitive>),
            Var([Option<IString>; 3], Sb3Primitive),
        }
        let assignments: Vec<SingleAssignment> = match value.1 {
            crate::parser::ast::DataDeclaration::Mixed(parent_scope, _, items, _, _)
            | crate::parser::ast::DataDeclaration::Vars(parent_scope, _, _, items, _, _)
            | crate::parser::ast::DataDeclaration::Lists(parent_scope, _, _, items, _, _) => items
                .iter()
                .map(|(value, _)| match value {
                    crate::parser::ast::SingleDataDeclaration::Variable(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                        expression,
                        _,
                    ) => {
                        let var = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            (parent_scope, my_scope),
                            (
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_),
                                DataDeclarationScope::Unset
                            ) | (
                                _,
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                            )
                        ) {
                            [Some(stage_var_symbol[0].clone()), Some(var), None]
                        } else {
                            let (a, b) = this_target_var_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(var)],
                                None => [Some(a), Some(var), None],
                            }
                        };
                        SingleAssignment::Var(path, expression.calculate_value())
                    }
                    crate::parser::ast::SingleDataDeclaration::EmptyVariable(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                    ) => {
                        let var = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            (parent_scope, my_scope),
                            (
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_),
                                DataDeclarationScope::Unset
                            ) | (
                                _,
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                            )
                        ) {
                            [Some(stage_var_symbol[0].clone()), Some(var), None]
                        } else {
                            let (a, b) = this_target_var_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(var)],
                                None => [Some(a), Some(var), None],
                            }
                        };
                        SingleAssignment::Var(path, "".into())
                    }
                    crate::parser::ast::SingleDataDeclaration::List(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                        _,
                        items,
                        _,
                        _,
                    ) => {
                        let list = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            (parent_scope, my_scope),
                            (
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_),
                                DataDeclarationScope::Unset
                            ) | (
                                _,
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                            )
                        ) {
                            [Some(stage_list_symbol[0].clone()), Some(list), None]
                        } else {
                            let (a, b) = this_target_list_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(list)],
                                None => [Some(a), Some(list), None],
                            }
                        };
                        SingleAssignment::List(path, {
                            let mut values = Vec::with_capacity(items.len());
                            for (value, _) in items {
                                match value {
                                    ListEntry::Expression(expression) => {
                                        values.push(expression.calculate_value());
                                    }
                                    ListEntry::Unwrap(literal, _) => {
                                        for c in literal.get_string_value().chars() {
                                            values.push(c.to_string().into());
                                        }
                                    }
                                }
                            }
                            values
                        })
                    }
                    crate::parser::ast::SingleDataDeclaration::EmptyList(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                    ) => {
                        let list = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            (parent_scope, my_scope),
                            (
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_),
                                DataDeclarationScope::Unset
                            ) | (
                                _,
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                            )
                        ) {
                            [Some(stage_list_symbol[0].clone()), Some(list), None]
                        } else {
                            let (a, b) = this_target_list_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(list)],
                                None => [Some(a), Some(list), None],
                            }
                        };
                        SingleAssignment::List(path, Vec::new())
                    }
                })
                .collect(),
            crate::parser::ast::DataDeclaration::Single(single_data_declaration) => {
                let single_data_declaration = single_data_declaration.as_ref();
                let single_assignment = match single_data_declaration {
                    crate::parser::ast::SingleDataDeclaration::Variable(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                        expression,
                        _,
                    ) => {
                        let var = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            my_scope,
                            DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                        ) {
                            [Some(stage_var_symbol[0].clone()), Some(var), None]
                        } else {
                            let (a, b) = this_target_var_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(var)],
                                None => [Some(a), Some(var), None],
                            }
                        };
                        SingleAssignment::Var(path, expression.calculate_value())
                    }
                    crate::parser::ast::SingleDataDeclaration::EmptyVariable(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                    ) => {
                        let var = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            my_scope,
                            DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                        ) {
                            [Some(stage_var_symbol[0].clone()), Some(var), None]
                        } else {
                            let (a, b) = this_target_var_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(var)],
                                None => [Some(a), Some(var), None],
                            }
                        };
                        SingleAssignment::Var(path, "".into())
                    }
                    crate::parser::ast::SingleDataDeclaration::List(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                        _,
                        items,
                        _,
                        _,
                    ) => {
                        let list = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            my_scope,
                            DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                        ) {
                            [Some(stage_list_symbol[0].clone()), Some(list), None]
                        } else {
                            let (a, b) = this_target_list_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(list)],
                                None => [Some(a), Some(list), None],
                            }
                        };
                        SingleAssignment::List(path, {
                            let mut values = Vec::with_capacity(items.len());
                            for (value, _) in items {
                                match value {
                                    ListEntry::Expression(expression) => {
                                        values.push(expression.calculate_value());
                                    }
                                    ListEntry::Unwrap(literal, _) => {
                                        for c in literal.get_string_value().chars() {
                                            values.push(c.to_string().into());
                                        }
                                    }
                                }
                            }
                            values
                        })
                    }
                    crate::parser::ast::SingleDataDeclaration::EmptyList(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                    ) => {
                        let list = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            my_scope,
                            DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                        ) {
                            [Some(stage_list_symbol[0].clone()), Some(list), None]
                        } else {
                            let (a, b) = this_target_list_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(list)],
                                None => [Some(a), Some(list), None],
                            }
                        };
                        SingleAssignment::List(path, Vec::new())
                    }
                };
                vec![single_assignment]
            }
        };
        for assignment in assignments {
            match assignment {
                SingleAssignment::List(identifier, sb3_primitives) => {
                    let symbol_id = context
                        .resolve_path(identifier.iter().map_while(|value| value.as_ref()))
                        .unwrap();
                    let symbol = &context.symbol_table[symbol_id];
                    let known_block = symbol.known_block.as_ref().unwrap().clone();
                    let (canonical_name, id) = match known_block.as_ref() {
                        KnownBlock::List { canonical_name, id } => (canonical_name, id),
                        _ => unreachable!(),
                    };
                    let list_field_value = Sb3FieldValue::WithId {
                        value: canonical_name.as_str().into(),
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
                    for item in sb3_primitives {
                        wrap_in_statement(context, |context, parent, this_id| {
                            let value = Sb3InputValue::Shadow(Sb3InputRepr::PrimitiveBlock(
                                Sb3PrimitiveBlock::String(item),
                            ));
                            add_block(
                                context,
                                &this_id,
                                make_block(
                                    parent,
                                    "data_addtolist".to_string(),
                                    HashMap::from([("ITEM".to_string(), value)]),
                                    HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                                    false,
                                    None,
                                ),
                            );
                            Ok(())
                        })?;
                    }
                }
                SingleAssignment::Var(identifier, sb3_primitive) => {
                    wrap_in_statement(context, |context, parent, this_id| {
                        let symbol_id = context
                            .resolve_path(identifier.iter().map_while(|value| value.as_ref()))
                            .unwrap();
                        let symbol = &context.symbol_table[symbol_id];
                        let known_block = symbol.known_block.as_ref().unwrap().clone();
                        let SimpleCallableKnownBlockSignature(opcode, param, known_params) =
                            known_block.resolve_for_assignment(context);
                        let mut fields = HashMap::new();
                        let mut inputs = HashMap::new();
                        add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
                        add_param_to_params(
                            context,
                            param,
                            &KnownBlock::PrimitiveBlock {
                                value: Sb3PrimitiveBlock::String(sb3_primitive),
                            },
                            &mut inputs,
                            &mut fields,
                        )?;
                        add_block(
                            context,
                            &this_id,
                            make_block(parent, opcode.to_string(), inputs, fields, false, None),
                        );
                        Ok(())
                    })?;
                }
            }
        }
        Ok(())
    }

    fn visit_statement_if_else(
        &self,
        value: (
            &(
                crate::parser::ast::SyntacticIf,
                Expression,
                crate::parser::ast::CodeBlock,
            ),
            &Vec<(
                crate::parser::ast::SyntacticElse,
                crate::parser::ast::SyntacticIf,
                Expression,
                crate::parser::ast::CodeBlock,
            )>,
            &Option<(
                crate::parser::ast::SyntacticElse,
                crate::parser::ast::CodeBlock,
            )>,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        pub fn make_if_else_recursively(
            visitor: &GrazeSb3Generator,
            context: &mut GrazeSb3GeneratorContext,
            else_ifs: &[(
                crate::parser::ast::SyntacticElse,
                crate::parser::ast::SyntacticIf,
                Expression,
                crate::parser::ast::CodeBlock,
            )],
            else_value: &Option<(
                crate::parser::ast::SyntacticElse,
                crate::parser::ast::CodeBlock,
            )>,
        ) -> Result<(), GrazeSb3GeneratorError> {
            wrap_in_statement(context, |context, parent, this_id| {
                let use_if_else = else_ifs.len() > 1 || else_value.is_some();
                let opcode = if use_if_else {
                    "control_if_else"
                } else {
                    "control_if"
                };
                let mut inputs = HashMap::with_capacity(if use_if_else { 3 } else { 2 });
                visitor.visit_expression(&else_ifs[0].2, context)?;
                let condition_value = context.pop_param().unwrap();
                if let Some(condition) = create_input_value(
                    param_to_input_repr_no_menu(condition_value, context)?,
                    None::<Sb3PrimitiveBlock>,
                ) {
                    inputs.insert("CONDITION".to_string(), condition);
                }
                visitor.visit_code_block(&else_ifs[0].3, context)?;
                let first_branch = context.pop_param().unwrap();
                let first_branch = if let Param::BlockStack(block_ref) = first_branch {
                    block_ref
                } else {
                    return Err(GrazeSb3GeneratorError::PassedNormalParamAsBlockStack {
                        param: first_branch,
                    });
                };
                if let Some(first_branch) = first_branch {
                    inputs.insert(
                        "SUBSTACK".to_string(),
                        Sb3InputValue::NoShadow(Sb3InputRepr::Reference(first_branch)),
                    );
                }
                match (else_ifs.len() > 1, else_value) {
                    (true, _) => {
                        context.current_previous_block = None;
                        make_if_else_recursively(visitor, context, &else_ifs[1..], else_value)?;
                        if let Some(Param::BlockStack(Some(inner_id))) = context.pop_param() {
                            inputs.insert(
                                "SUBSTACK2".to_string(),
                                Sb3InputValue::NoShadow(Sb3InputRepr::Reference(inner_id)),
                            );
                        };
                    }
                    (false, Some(else_value)) => {
                        context.current_previous_block = None;
                        visitor.visit_code_block(&else_value.1, context)?;
                        let else_branch = context.pop_param().unwrap();
                        let else_branch = if let Param::BlockStack(block_ref) = else_branch {
                            block_ref
                        } else {
                            return Err(GrazeSb3GeneratorError::PassedNormalParamAsBlockStack {
                                param: else_branch,
                            });
                        };
                        if let Some(else_branch) = else_branch {
                            inputs.insert(
                                "SUBSTACK2".to_string(),
                                Sb3InputValue::NoShadow(Sb3InputRepr::Reference(else_branch)),
                            );
                        }
                    }
                    (false, None) => (),
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
                Ok(())
            })
        }
        wrap_in_statement(context, |context, parent, this_id| {
            let use_if_else = !value.1.is_empty() || value.2.is_some();
            let opcode = if use_if_else {
                "control_if_else"
            } else {
                "control_if"
            };
            let mut inputs = HashMap::with_capacity(if use_if_else { 3 } else { 2 });
            self.visit_expression(&value.0.1, context)?;
            let condition_value = context.pop_param().unwrap();
            if let Some(condition) = create_input_value(
                param_to_input_repr_no_menu(condition_value, context)?,
                None::<Sb3PrimitiveBlock>,
            ) {
                inputs.insert("CONDITION".to_string(), condition);
            }
            self.visit_code_block(&value.0.2, context)?;
            let first_branch = context.pop_param().unwrap();
            let first_branch = if let Param::BlockStack(block_ref) = first_branch {
                block_ref
            } else {
                return Err(GrazeSb3GeneratorError::PassedNormalParamAsBlockStack {
                    param: first_branch,
                });
            };
            if let Some(first_branch) = first_branch {
                inputs.insert(
                    "SUBSTACK".to_string(),
                    Sb3InputValue::NoShadow(Sb3InputRepr::Reference(first_branch)),
                );
            }
            if use_if_else {
                context.current_previous_block = None;
                make_if_else_recursively(self, context, value.1, value.2)?;
                if let Some(Param::BlockStack(Some(inner_id))) = context.pop_param() {
                    inputs.insert(
                        "SUBSTACK2".to_string(),
                        Sb3InputValue::NoShadow(Sb3InputRepr::Reference(inner_id.to_string())),
                    );
                };
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
            Ok(())
        })
    }

    // Helpers:

    fn visit_code_block(
        &self,
        value: &crate::parser::ast::CodeBlock,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        default_visit_code_block(self, value, context)?;
        if value.statements.is_empty() {
            context.push_param(Param::BlockStack(None));
        }
        Ok(())
    }

    // Target statements:

    fn visit_no_input_hat_statement(
        &self,
        value: (
            &Identifier,
            &crate::parser::ast::CodeBlock,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        let (parent, this_id) = wrap_in_statement(context, |_, parent, this_id| (parent, this_id));
        let symbol_id = get_symbol_id(context, value.0)?;
        let symbol = &context.symbol_table[symbol_id];
        let known_block = get_known_block(symbol, value.0)?.clone();
        let CallableKnownBlockSignature(opcode, params, known_params, mutation) =
            known_block.resolve_for_call_block(context);
        add_block(
            context,
            &this_id,
            make_block(
                parent.clone(),
                opcode.to_string(),
                HashMap::new(), // both of these will be replaced when the argument is available
                HashMap::new(),
                false,
                mutation.map(make_proc_call_mutation),
            ),
        );
        default_visit_no_input_hat_statement(self, value, context)?;
        let mut fields = HashMap::new();
        let mut inputs = HashMap::new();
        add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
        if !params.is_empty() {
            return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                unexpected: 0,
                expected: params.len(),
            });
        }
        let block = context
            .current_sb3_target
            .as_mut()
            .unwrap() // the visitor should always guarantee there is a target when blocks are added
            .blocks
            .get_mut(this_id.as_str())
            .unwrap();
        block.fields = fields;
        block.inputs = inputs;
        Ok(())
    }

    fn visit_single_input_hat_statement(
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
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        let (parent, this_id) = wrap_in_statement(context, |_, parent, this_id| (parent, this_id));
        let symbol_id = get_symbol_id(context, value.0)?;
        let symbol = &context.symbol_table[symbol_id];
        let known_block = get_known_block(symbol, value.0)?.clone();
        let CallableKnownBlockSignature(opcode, params, known_params, mutation) =
            known_block.resolve_for_call_block(context);
        add_block(
            context,
            &this_id,
            make_block(
                parent.clone(),
                opcode.to_string(),
                HashMap::new(), // both of these will be replaced when the argument is available
                HashMap::new(),
                false,
                mutation.map(make_proc_call_mutation),
            ),
        );
        default_visit_single_input_hat_statement(self, value, context)?;
        let arg = context.pop_param().unwrap();
        let mut fields = HashMap::new();
        let mut inputs = HashMap::new();
        add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
        let skip_param = if params.len() != 1 {
            if params.is_empty() && value.1.is_empty() {
                true
            } else {
                return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                    unexpected: 1,
                    expected: params.len(),
                });
            }
        } else {
            false
        };
        if !skip_param {
            let param = params.first().unwrap();
            let prev_parent = if let Some(parent) = parent {
                context.current_parent.replace(parent)
            } else {
                None
            };
            with_known_block!(context, arg, value => {
                add_param_to_params(context, param, value, &mut inputs, &mut fields)?;
            });
            context.current_parent = prev_parent;
        }
        let block = context
            .current_sb3_target
            .as_mut()
            .unwrap() // the visitor should always guarantee there is a target when blocks are added
            .blocks
            .get_mut(this_id.as_str())
            .unwrap();
        block.fields = fields;
        block.inputs = inputs;
        Ok(())
    }

    fn visit_multi_input_hat_statement(
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
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        let (parent, this_id) = wrap_in_statement(context, |_, parent, this_id| (parent, this_id));
        let symbol_id = get_symbol_id(context, value.0)?;
        let symbol = &context.symbol_table[symbol_id];
        let known_block = get_known_block(symbol, value.0)?.clone();
        let CallableKnownBlockSignature(opcode, params, known_params, mutation) =
            known_block.resolve_for_call_block(context);
        add_block(
            context,
            &this_id,
            make_block(
                parent.clone(),
                opcode.to_string(),
                HashMap::new(), // both of these will be replaced when the argument is available
                HashMap::new(),
                false,
                mutation.map(make_proc_call_mutation),
            ),
        );
        default_visit_multi_input_hat_statement(self, value, context)?;
        let reversed_args = iter::repeat_with(|| context.pop_param().unwrap())
            .take(value.2.len())
            .collect::<Vec<_>>();
        let mut fields = HashMap::new();
        let mut inputs = HashMap::new();
        add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
        if params.len() != reversed_args.len() {
            return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                unexpected: reversed_args.len(),
                expected: params.len(),
            });
        }
        let prev_parent = if let Some(parent) = parent {
            context.current_parent.replace(parent)
        } else {
            None
        };
        for (param, value) in zip(params.iter(), reversed_args.into_iter().rev()) {
            with_known_block!(context, value, value => {
                add_param_to_params(context, param, value, &mut inputs, &mut fields)?;
            });
        }
        context.current_parent = prev_parent;
        let block = context
            .current_sb3_target
            .as_mut()
            .unwrap() // the visitor should always guarantee there is a target when blocks are added
            .blocks
            .get_mut(this_id.as_str())
            .unwrap();
        block.fields = fields;
        block.inputs = inputs;
        Ok(())
    }

    fn visit_custom_block_definition(
        &self,
        value: (
            &Option<crate::parser::ast::WarpSpecifier>,
            &crate::parser::ast::ProcKeyword,
            &Option<crate::parser::ast::CanonicalIdentifier>,
            &Identifier,
            &crate::parser::ast::LeftParens,
            &Vec<(
                Option<crate::parser::ast::CustomBlockParamKind>,
                Option<crate::parser::ast::CanonicalIdentifier>,
                Identifier,
                Option<crate::parser::ast::Comma>,
            )>,
            &crate::parser::ast::RightParens,
            &crate::parser::ast::CodeBlock,
            &Option<crate::parser::ast::Semicolon>,
            &crate::lexer::PosRange,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        let current_target = context.current_sb3_target.as_ref().unwrap();

        let proc_name = value.3.to_single().unwrap().0.clone();
        let proc_symbol_id = context
            .resolve_path(
                if current_target.is_stage {
                    [SPRITES_ISTRING.clone(), STAGE_ISTRING.clone(), proc_name]
                } else {
                    [
                        SPRITES_ISTRING.clone(),
                        current_target.name.as_str().into(),
                        proc_name,
                    ]
                }
                .iter(),
            )
            .unwrap();
        let proc_symbol = &context.symbol_table[proc_symbol_id];
        let proc_known_block = proc_symbol.known_block.as_ref().unwrap().clone();
        let (proccode, params, is_warp) = if let KnownBlock::CustomBlock {
            proccode,
            call_params: _,
            params,
            is_warp,
        } = proc_known_block.as_ref()
        {
            (proccode, params, is_warp)
        } else {
            unreachable!()
        };
        wrap_in_statement(context, |context, parent, this_id| {
            let prototype_id = wrap_in_statement(context, |context, parent, this_id| {
                add_block(
                    context,
                    &this_id,
                    make_block(
                        parent.clone(),
                        "procedures_prototype".to_string(),
                        HashMap::new(),
                        HashMap::new(),
                        false,
                        Some(Sb3BlockMutation::ProceduresPrototype {
                            procedure_code: proccode.to_string(),
                            argument_ids: params.iter().map(|value| value.0.to_string()).collect(),
                            warp: *is_warp,
                            argument_names: value
                                .5
                                .iter()
                                .map(|value| {
                                    value
                                        .1
                                        .as_ref()
                                        .map(|value| value.name.to_string())
                                        .unwrap_or_else(|| {
                                            value.2.to_single().unwrap().0.to_string()
                                        })
                                })
                                .collect(),
                            argument_defaults: params
                                .iter()
                                .map(|value| {
                                    serde_json::Value::String(if value.1 == HasShadow::No {
                                        "false".to_string()
                                    } else {
                                        "".to_string()
                                    })
                                })
                                .collect(),
                        }),
                    ),
                );
                Ok(this_id)
            })?;
            add_block(
                context,
                &this_id,
                make_block(
                    parent.clone(),
                    "procedures_definition".to_string(),
                    HashMap::from([(
                        "custom_block".to_string(),
                        Sb3InputValue::NoShadow(Sb3InputRepr::Reference(prototype_id.to_string())),
                    )]),
                    HashMap::new(),
                    false,
                    None,
                ),
            );
            Ok(())
        })?;
        let arguments = value
            .5
            .iter()
            .map(|value| {
                let name = value
                    .1
                    .as_ref()
                    .map(|value| value.name.to_string())
                    .unwrap_or_else(|| value.2.to_single().unwrap().0.to_string());
                (
                    name.as_str().into(),
                    Symbol {
                        known_block: Some(Rc::new(KnownBlock::SingletonReporter {
                            opcode: if matches!(
                                value.0,
                                Some(CustomBlockParamKind {
                                    kind: CustomBlockParamKindValue::Boolean,
                                    pos_range: _,
                                })
                            ) {
                                literal!("argument_reporter_boolean")
                            } else {
                                literal!("argument_reporter_string_number")
                            },
                            params: vec![(
                                CallBlockParam {
                                    kind: CallBlockParamKind::Field { default: None },
                                    name: literal!("VALUE"),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(Sb3Primitive::String(name)),
                                },
                            )],
                            field: None,
                            assign: None,
                            bind_info: None,
                        })),
                        namespace: HashMap::new(),
                        parent: Default::default(),
                    },
                )
            })
            .collect::<HashMap<_, _>>();
        const ARGUMENTS_ISTRING: &IString = &literal!("args");
        let arguments_symbol_id = context.symbol_table.new_child_symbol(
            Default::default(),
            ARGUMENTS_ISTRING.clone(),
            None,
            arguments.len(),
        );
        for (key, arg) in arguments {
            let arg = context.symbol_table.new_symbol(arg);
            context
                .symbol_table
                .insert_child(arguments_symbol_id, key, arg);
        }
        default_visit_custom_block_definition(self, value, context)?;
        context.symbol_table[Default::default()]
            .namespace
            .remove(ARGUMENTS_ISTRING);
        Ok(())
    }

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
        context.arg_stack.clear();
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
        context.arg_stack.clear();
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
        let target_name = STAGE_ISTRING.clone();
        let assets = context.target_attachments.remove(&target_name).unwrap();
        let my_blocks_symbol_id = context
            .symbol_table
            .get_child(Default::default(), MY_BLOCKS_ISTRING)
            .unwrap();
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
                TargetAttachment::CustomBlock(name, custom_block) => {
                    context.symbol_table.new_child_symbol(
                        my_blocks_symbol_id,
                        name,
                        Some(Rc::new(custom_block)),
                        0,
                    );
                }
            }
        }
        context.current_sb3_target = Some(stage);
        default_visit_top_level_statement_stage(self, value, context)?;
        context
            .sb3
            .targets
            .push(context.current_sb3_target.take().unwrap());
        context.symbol_table[my_blocks_symbol_id].namespace.clear();
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
        let my_blocks_symbol_id = context
            .symbol_table
            .get_child(Default::default(), MY_BLOCKS_ISTRING)
            .unwrap();
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
                TargetAttachment::CustomBlock(name, custom_block) => {
                    context.symbol_table.new_child_symbol(
                        my_blocks_symbol_id,
                        name,
                        Some(Rc::new(custom_block)),
                        0,
                    );
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
        context.symbol_table[my_blocks_symbol_id].namespace.clear();
        Ok(())
    }
}
