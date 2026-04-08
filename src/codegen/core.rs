use std::{
    cell::RefCell,
    collections::HashMap,
    iter::{self, zip},
    rc::{Rc, Weak},
};

use arcstr::{ArcStr as IString, literal};
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
    codegen::project_json::{IsShadow, Sb3InputRepr, Sb3Target},
    names::Namespace,
    parser::{
        ast::{BinOpDescriptor, Expression, FormattedStringContent, Identifier, UnOpDescriptor},
        parse_context::{
            ActualIdentifier, CallBlockParam, CallableKnownBlockSignature, IdString, KnownBlock,
            KnownBlockInput, ParseContext, SimpleCallableKnownBlockSignature, Target,
            TargetSymbolDescriptor,
        },
    },
    visitor::{
        GrazeVisitor, default_visit_expression_binary_operation, default_visit_expression_call, default_visit_expression_formatted_string, default_visit_expression_get_item, default_visit_expression_get_letter, default_visit_expression_unary_operation, default_visit_formatted_string_content
    },
};

#[derive(Debug, Clone, Error)]
pub enum GrazeSb3GeneratorError {
    #[error("the identifier {identifier:?} was not found")]
    UnknownIdentifier { identifier: Identifier },
    #[error("the identifier {identifier:?} is not a block")]
    IdentifierIsNotABlock { identifier: ActualIdentifier },
    #[error("in this context, no menu input was expected, found {input_menu_value:?}")]
    UnexpectedInputMenu { input_menu_value: Sb3FieldValue },
    #[error("the amount of parameters for this block was {unexpected:?}, expected {expected:?}")]
    IncorrectParamCount { unexpected: usize, expected: usize },
}

pub struct GrazeSb3Generator;

#[derive(Debug, Clone)]
pub struct GrazeSb3GeneratorContext {
    pub sb3: Sb3Root,
    pub targets: Vec<Target>,
    pub root_symbol: Rc<RefCell<ActualIdentifier>>,
    pub block_counter: IdCounter,
    pub arg_stack: Vec<Param>,
    pub current_block_id: IdString,
    pub current_parent: Option<String>,
    pub current_sb3_target: Sb3Target,
    /// Is None while and after being initialized
    pub uninitialized_stage: Option<Sb3Target>,
    pub previous_block_stack: Vec<Option<IdString>>,
    pub formatted_string_context: FormattedStringContext,
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

/// Placeholder function, will be replaced/moved/implemented later
pub fn get_std_lib_namespace_count() -> usize {
    todo!()
}

/// Placeholder function, will be replaced/moved/implemented later
pub fn get_std_lib_namespaces() -> Vec<(IString, ActualIdentifier)> {
    todo!()
}

impl GrazeSb3GeneratorContext {
    fn new(mut parse_context: ParseContext) -> Self {
        let mut rng = Xoshiro256StarStar::from_seed(parse_context.random_seed);
        let targets: Vec<Target> = parse_context.parsed_targets.into();
        let root_symbol = Rc::new_cyclic(|me| {
            RefCell::new(ActualIdentifier::Namespace(
                HashMap::with_capacity(get_std_lib_namespace_count() + 4),
                // ^ extensions + sprites + broadcasts + global variables + global lists
                me.clone(),
            ))
        });

        let targets_symbol = Rc::new(RefCell::new(ActualIdentifier::Namespace(
            HashMap::with_capacity(targets.len()),
            Weak::new(),
        )));
        for target in &targets {
            let mut namespace = Namespace::new();
            ActualIdentifier::insert_child(
                &targets_symbol,
                target.get_namespace_name(),
                Rc::new(RefCell::new(ActualIdentifier::KnownBlock(
                    Box::new(KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal(super::project_json::Sb3Primitive::String(
                            target.get_field_name(),
                        )),
                    }),
                    Some(
                        target
                            .borrow_symbols()
                            .iter()
                            .map(|(key, value)| {
                                (
                                    key.clone(),
                                    Rc::new(RefCell::new(
                                        value.derive_actual_symbol(&mut rng, &mut namespace),
                                    )),
                                )
                            })
                            .collect(),
                    ),
                    Weak::new(),
                ))),
            );
        }
        ActualIdentifier::insert_child(&root_symbol, literal!("sprites"), targets_symbol);
        ActualIdentifier::insert_child(
            &root_symbol,
            literal!("broadcasts"),
            Rc::new(RefCell::new(ActualIdentifier::Namespace(
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
        let variables_symbol = Rc::new(RefCell::new(ActualIdentifier::new_namespace()));
        let lists_symbol = Rc::new(RefCell::new(ActualIdentifier::new_namespace()));
        let mut global_namespace = Namespace::new();
        for (name, symbol) in parse_context.global_symbols.drain() {
            match &symbol {
                TargetSymbolDescriptor::Var(_) => {
                    ActualIdentifier::insert_child(
                        &variables_symbol,
                        name,
                        Rc::new(RefCell::new(
                            symbol.derive_actual_symbol(&mut rng, &mut global_namespace),
                        )),
                    );
                }
                TargetSymbolDescriptor::List(_) => {
                    ActualIdentifier::insert_child(
                        &lists_symbol,
                        name,
                        Rc::new(RefCell::new(
                            symbol.derive_actual_symbol(&mut rng, &mut global_namespace),
                        )),
                    );
                }
                _ => (), // Handled just to be sure although it shouldn't happen
            }
        }
        ActualIdentifier::insert_child(&root_symbol, literal!("variables"), variables_symbol);

        ActualIdentifier::insert_child(&root_symbol, literal!("lists"), lists_symbol);
        for (name, symbol) in get_std_lib_namespaces() {
            ActualIdentifier::insert_child(&root_symbol, name, Rc::new(RefCell::new(symbol)));
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
            current_sb3_target: Sb3Target::new_stage(),
            uninitialized_stage: None, // TODO: add stage here
            previous_block_stack: Vec::new(),
            formatted_string_context: FormattedStringContext::new(),
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

    pub fn resolve_identifier(
        &self,
        identifier: &Identifier,
    ) -> Option<Rc<RefCell<ActualIdentifier>>> {
        identifier
            .scope
            .iter()
            .chain(identifier.names.iter())
            .try_fold(self.root_symbol.clone(), |current, (next, _)| {
                if next == &literal!("super") {
                    // TODO: Reason about whether there should be a "root" option.
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
}

pub fn make_block(
    parent: Option<String>,
    opcode: String,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
    shadow: bool,
    mutation: Option<Sb3BlockMutation>,
) -> Sb3Block {
    Sb3Block {
        opcode,
        next: None,
        parent,
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
    parent: Option<String>,
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
        parent,
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
    ($context:expr, ($parent:ident: $identifier:ident) => $action:expr) => {{
        let block_id = $context.get_current_block_id();
        let $identifier = block_id.clone();
        let old_parent = $context.current_parent.replace(block_id.to_string());
        let $parent = old_parent.clone();
        $context.new_block();
        let out = $action;
        $context.current_parent = old_parent;
        out
    }};
    ($context:expr, (_: $identifier:ident) => $action:expr) => {{
        let block_id = $context.get_current_block_id();
        let $identifier = block_id.clone();
        let old_parent = $context.current_parent.replace(block_id.to_string());
        $context.new_block();
        let out = $action;
        $context.current_parent = old_parent;
        out
    }};
}

macro_rules! wrap_in_statement_parent {
    ($context:expr, $identifier:ident => $action:expr) => {{
        let block_id = $context.get_current_block_id();
        let $identifier = block_id.clone();
        let old_parent = $context.current_parent.replace(block_id.to_string());
        $context.new_block();
        $context.previous_block_stack.push(empty_id);
        let out = $action;
        $context.current_parent = old_parent;
        out
    }};
}

macro_rules! with_known_block {
    ($param:expr, $context:expr, $known_block:ident => $action:expr) => {
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
                        identifier: actual_identifier.clone(),
                    }
                })?;
                $action
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
        })?;
        actual_identifier
    }};
}

macro_rules! get_known_block {
    ($actual_identifier_ref:expr) => {{
        $actual_identifier_ref.get_block().ok_or_else(|| {
            GrazeSb3GeneratorError::IdentifierIsNotABlock {
                identifier: $actual_identifier_ref.clone(),
            }
        })?
    }};
}

pub fn introduce_input_menu(
    opcode: &IString,
    field_name: &IString,
    value: Sb3FieldValue,
    context: &mut GrazeSb3GeneratorContext,
) -> IdString {
    wrap_in_parent!(context, (parent: this_id) => {
        add_reporter_block(
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
        crate::parser::parse_context::CallBlockParamKind::Input { default } => {
            inputs.insert(
                param_name,
                (
                    known_block_to_input_repr_no_menu(value, context)?,
                    default.as_ref(),
                )
                    .into(),
            );
        }
        crate::parser::parse_context::CallBlockParamKind::Field => {
            fields.insert(param_name, value.resolve_for_field(context));
        }
        crate::parser::parse_context::CallBlockParamKind::MenuInput {
            opcode,
            field_name,
            default,
        } => {
            let (input_repr, is_menu) = match value.resolve_for_input(context) {
                crate::parser::parse_context::KnownBlockInput::PrimitiveInput(
                    sb3_primitive_block,
                ) => (Sb3InputRepr::PrimitiveBlock(sb3_primitive_block), false),
                crate::parser::parse_context::KnownBlockInput::BlockRef(id) => {
                    (Sb3InputRepr::Reference(id.to_string()), false)
                }
                crate::parser::parse_context::KnownBlockInput::SimpleBlock(opcode, params) => (
                    Sb3InputRepr::Reference(
                        introduce_input_simple_block(opcode, params, context)?.to_string(),
                    ),
                    false,
                ),
                crate::parser::parse_context::KnownBlockInput::Menu(input_menu_value) => (
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
    }
    Ok(())
}

pub fn introduce_input_simple_block(
    opcode: &IString,
    params: &Vec<(CallBlockParam, KnownBlock)>,
    context: &mut GrazeSb3GeneratorContext,
) -> Result<IdString, GrazeSb3GeneratorError> {
    wrap_in_parent!(context, (parent: this_id) => {
        let mut fields = HashMap::new();
        let mut inputs = HashMap::new();
        for (param, value) in params {
            add_param_to_params(context, param, value, &mut inputs, &mut fields)?;
        }
        add_reporter_block(
            context,
            &this_id,
            make_block(
                parent,
                opcode.to_string(),
                inputs,
                fields,
                false,
                None,
            ),
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
        crate::parser::parse_context::KnownBlockInput::PrimitiveInput(sb3_primitive_block) => Ok((
            Sb3InputRepr::PrimitiveBlock(sb3_primitive_block),
            IsShadow::Yes,
        )),
        crate::parser::parse_context::KnownBlockInput::BlockRef(id) => {
            Ok((Sb3InputRepr::Reference(id.to_string()), IsShadow::No))
        }
        crate::parser::parse_context::KnownBlockInput::SimpleBlock(opcode, params) => Ok((
            Sb3InputRepr::Reference(
                introduce_input_simple_block(opcode, params, context)?.to_string(),
            ),
            IsShadow::No,
        )),
        crate::parser::parse_context::KnownBlockInput::Menu(input_menu_value) => {
            Err(GrazeSb3GeneratorError::UnexpectedInputMenu { input_menu_value })
        }
    }
}

pub fn param_to_input_repr_no_menu(
    param: Param,
    context: &mut GrazeSb3GeneratorContext,
) -> Result<(Sb3InputRepr, IsShadow), GrazeSb3GeneratorError> {
    with_known_block!(param, context, value => {
        known_block_to_input_repr_no_menu(value, context)
    })
}

impl From<(Sb3InputRepr, IsShadow)> for Sb3InputValue {
    fn from(value: (Sb3InputRepr, IsShadow)) -> Self {
        let (input_repr, is_shadow) = value;
        if is_shadow == IsShadow::Yes {
            Self::Shadow(input_repr)
        } else {
            Self::NoShadow(input_repr)
        }
    }
}

impl From<((Sb3InputRepr, IsShadow), Option<Sb3PrimitiveBlock>)> for Sb3InputValue {
    fn from(value: ((Sb3InputRepr, IsShadow), Option<Sb3PrimitiveBlock>)) -> Self {
        let ((input_repr, is_shadow), shadow) = value;
        if is_shadow == IsShadow::Yes {
            return Self::Shadow(input_repr);
        }
        if let Some(shadow) = shadow {
            Self::ObscuredShadow {
                value: input_repr,
                shadow: Sb3InputRepr::PrimitiveBlock(shadow),
            }
        } else {
            Self::NoShadow(input_repr)
        }
    }
}

impl From<((Sb3InputRepr, IsShadow), Option<&Sb3PrimitiveBlock>)> for Sb3InputValue {
    fn from(value: ((Sb3InputRepr, IsShadow), Option<&Sb3PrimitiveBlock>)) -> Self {
        let ((input_repr, is_shadow), shadow) = value;
        if is_shadow == IsShadow::Yes {
            return Self::Shadow(input_repr);
        }
        if let Some(shadow) = shadow {
            Self::ObscuredShadow {
                value: input_repr,
                shadow: Sb3InputRepr::PrimitiveBlock(shadow.clone()),
            }
        } else {
            Self::NoShadow(input_repr)
        }
    }
}

pub fn add_reporter_block(context: &mut GrazeSb3GeneratorContext, id: &IdString, block: Sb3Block) {
    context
        .current_sb3_target
        .blocks
        .insert(id.to_string(), block);
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
        wrap_in_parent!(context, (parent: this_id) => {
            default_visit_expression_binary_operation(self, value, context)?;
            let (op_b_param, op_a_param) = (context.pop_param().unwrap(), context.pop_param().unwrap());
            let BinOpDescriptor {
                opcode,
                operand_a_input_name,
                operand_b_input_name,
            } = value.1.get_descriptor();
            let inputs = HashMap::from([
                (operand_a_input_name, param_to_input_repr_no_menu(op_a_param, context)?.into()),
                (operand_b_input_name, param_to_input_repr_no_menu(op_b_param, context)?.into()),
            ]);
            add_reporter_block(
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
            context.push_param(Param::Owned(this_id.clone().into()));
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
        wrap_in_parent!(context, (parent: this_id) => {
            default_visit_expression_call(self, value, context)?;
            let reversed_args = iter::repeat_with(|| context.pop_param().unwrap())
                .take(value.2.len())
                .collect::<Vec<_>>();
            let actual_identifier = get_actual_identifier!(context, value.0);
            let actual_identifier_ref = actual_identifier.borrow();
            let known_block = get_known_block!(actual_identifier_ref);
            let CallableKnownBlockSignature(opcode, params, known_params) = known_block.resolve_for_call_block(context);
            let mut fields = HashMap::new();
            let mut inputs = HashMap::new();
            for (param, value) in known_params.iter() {
                add_param_to_params(context, param, value, &mut inputs, &mut fields)?;
            }
            if params.len() != reversed_args.len() {
                return Err(GrazeSb3GeneratorError::IncorrectParamCount { unexpected: reversed_args.len(), expected: params.len() })
            }
            for (param, value) in zip(params.iter(), reversed_args.into_iter().rev()) {
                with_known_block!(value, context, value => {
                    add_param_to_params(context, param, value, &mut inputs, &mut fields)?;
                });
            }
            add_reporter_block(
                context,
                &this_id,
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
                    fields,
                    false,
                    None,
                ),
            );
            context.push_param(Param::Owned(this_id.clone().into()));
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
                    wrap_in_parent!(context, (parent: this_id) => {
                        let parent = context.formatted_string_context.ids.pop().flatten().or(parent);
                        context.formatted_string_context.ids.push(Some(this_id.to_string()));
                        let left = Box::new(join_recursively(context, &value[..mid]));
                        context.formatted_string_context.ids.push(Some(this_id.to_string()));
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
                        Some(&Sb3PrimitiveBlock::String(
                            #[cfg(feature = "use_actual_defaults_for_formatted_strings")]
                            "apple ".into(),
                            #[cfg(not(feature = "use_actual_defaults_for_formatted_strings"))]
                            "".into(),
                        )),
                    );
                    #[cfg(feature = "use_shadows_for_formatted_strings")]
                    let right = (
                        right,
                        Some(&Sb3PrimitiveBlock::String(
                            #[cfg(feature = "use_actual_defaults_for_formatted_strings")]
                            "banana".into(),
                            #[cfg(not(feature = "use_actual_defaults_for_formatted_strings"))]
                            "".into(),
                        )),
                    );
                    add_reporter_block(
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
        wrap_in_parent!(context, (parent: this_id) => {
            todo!();
            default_visit_expression_get_item(self, value, context)?;
        });
        Ok(())
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
        wrap_in_parent!(context, (parent: this_id) => {
            todo!();
            default_visit_expression_get_letter(self, value, context)?;
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
        wrap_in_parent!(context, (parent: this_id) => {
            default_visit_expression_unary_operation(self, value, context)?;
            let operand = context.pop_param().unwrap();
            let UnOpDescriptor { opcode, operand_input_name, field_values, default } = value.0.get_descriptor();
            let operand_input_value = (param_to_input_repr_no_menu(operand, context)?, default);
            add_reporter_block(
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
            context.push_param(Param::Owned(this_id.clone().into()));
        });
        Ok(())
    }

    fn visit_expression_identifier(
        &self,
        value: &crate::parser::ast::Identifier,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.push_param(Param::LazyIdentifier(value.clone()));
        Ok(())
    }
}
