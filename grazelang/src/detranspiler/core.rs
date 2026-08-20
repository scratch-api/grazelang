use std::collections::HashMap;

use arcstr::ArcStr as IString;
use grazelang_types::project_json;
use serde::{Deserialize, Serialize, de::DeserializeOwned};

use crate::{
    ast::types as ast_types,
    detranspiler::get_info::{
        Argument, ArgumentKind, FieldValueInfo, get_block_kind_info, get_field_value_info,
    },
    library::{BlockShape, get_block_shape},
    messages::types::{GrazeDetranspilerError, GrazeDetranspilerMessage, GrazeDetranspilerWarning},
    names::{DetranspilerAssetNamespace, DetranspilerTargetNamespace},
    settings::{GrazeDetranspilerSettings, GrazeMessageSetting},
};

#[derive(Debug, Clone, PartialEq)]
pub struct DetranspilerContext {
    pub stage_target_idx: usize,
    pub targets: Vec<DetranspilerTarget>,
    pub asset_namespace: DetranspilerAssetNamespace,
    pub assets: HashMap<AssetPath, OutAssetPath>,
    pub messages: Vec<GrazeDetranspilerMessage>,
    pub settings: GrazeDetranspilerSettings,
    pub broadcasts: HashMap<DataId, DetranspilerBroadcast>,
}

pub(super) type DetranspilerResult<T> = Result<T, GrazeDetranspilerError>;

type OutAssetPath = String;
type AssetPath = String;
type AssetId = String;
type DataId = String;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerTarget {
    pub is_stage: bool,
    pub costumes: HashMap<AssetId, DetranspilerAsset<DetranspilerCostumeUncommonData>>,
    pub sounds: HashMap<AssetId, DetranspilerAsset<DetranspilerSoundUncommonData>>,
    pub data: HashMap<DataId, DetranspilerVarOrList>,
    pub scripts: Vec<DetranspilerTargetBlockStack>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DetranspilerTargetBlockStack {
    HatBlock {
        hat_function: ast_types::Identifier,
        arguments: Vec<ast_types::Expression>,
        stack: ast_types::CodeBlock,
    },
    IsolatedStack {
        stack: ast_types::CodeBlock,
    },
    IsolatedExpression {
        expression: ast_types::Expression,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorrowedDetranspilerVLB<'a> {
    Broadcast(&'a DetranspilerBroadcast),
    VarOrList(&'a DetranspilerVarOrList),
}

impl<'a> BorrowedDetranspilerVLB<'a> {
    pub fn get_canonical_name(self) -> Option<&'a IString> {
        match self {
            BorrowedDetranspilerVLB::Broadcast(broadcast) => broadcast.canonical_name.as_ref(),
            BorrowedDetranspilerVLB::VarOrList(var_or_list) => var_or_list.canonical_name.as_ref(),
        }
    }

    pub fn get_name(self) -> &'a IString {
        match self {
            BorrowedDetranspilerVLB::Broadcast(broadcast) => &broadcast.name,
            BorrowedDetranspilerVLB::VarOrList(var_or_list) => &var_or_list.name,
        }
    }

    pub fn get_original_name(self) -> &'a IString {
        match self {
            BorrowedDetranspilerVLB::Broadcast(broadcast) => {
                broadcast.canonical_name.as_ref().unwrap_or(&broadcast.name)
            }
            BorrowedDetranspilerVLB::VarOrList(var_or_list) => var_or_list
                .canonical_name
                .as_ref()
                .unwrap_or(&var_or_list.name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerBroadcast {
    pub canonical_name: Option<IString>,
    pub name: IString,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerVarOrList {
    pub canonical_name: Option<IString>,
    pub name: IString,
    pub kind: DetranspilerVarOrListKind,
}

impl DetranspilerVarOrList {
    #[inline]
    pub fn get_original_name(&self) -> &IString {
        self.canonical_name.as_ref().unwrap_or(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DetranspilerVarOrListKind {
    Variable {
        value: project_json::Sb3PrimitiveOrBool,
    },
    List {
        value: Vec<project_json::Sb3PrimitiveOrBool>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(bound(deserialize = "D: DeserializeOwned"))]
pub struct DetranspilerAsset<D>
where
    D: std::fmt::Debug + Clone + PartialEq + Serialize + DeserializeOwned,
{
    pub canonical_name: Option<IString>,
    pub name: IString,
    pub file_extension: String,
    pub uncommon_data: D,
    pub asset_name: IString,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerCostumeUncommonData {
    rotation_center_x: f64,
    rotation_center_y: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerSoundUncommonData;

#[inline]
pub fn emit_message<M>(
    context: &mut DetranspilerContext,
    message: M,
    message_type: GrazeMessageSetting,
) where
    M: FnOnce() -> GrazeDetranspilerMessage,
{
    if context.settings.message_setting >= message_type {
        context.messages.push(message());
    }
}

#[inline]
pub fn emit_message_eager(
    context: &mut DetranspilerContext,
    message: GrazeDetranspilerMessage,
    message_type: GrazeMessageSetting,
) {
    if context.settings.message_setting >= message_type {
        context.messages.push(message);
    }
}

macro_rules! try_or_emit_message {
    ($value:expr, $context:expr, $default_value:expr) => {
        unwrap_or_emit_message!($value, $context, return $default_value)
    };
}

macro_rules! unwrap_or_emit_message {
    ($value:expr, $context:expr, $default_value:expr) => {
        match $value {
            Ok(value) => value,
            Err(err) => {
                emit_error!(err, $context);
                $default_value
            }
        }
    };
}

macro_rules! emit_error_inline {
    ($err:expr, $context:expr) => {{
        let err = $err;
        if matches!(
            $context.settings.message_setting,
            GrazeMessageSetting::ExitOnError | GrazeMessageSetting::ExitOnErrorUnlogged
        ) {
            return Err(err);
        }
        if $context.settings.message_setting >= GrazeMessageSetting::Errors {
            $context.messages.push(err.into());
        }
    }};
}

macro_rules! emit_error {
    ($err:expr, $context:expr) => {{
        let err = $err;
        let context = &mut *$context;
        if matches!(
            context.settings.message_setting,
            GrazeMessageSetting::ExitOnError | GrazeMessageSetting::ExitOnErrorUnlogged
        ) {
            return Err(err);
        }
        emit_message_eager(context, err.into(), GrazeMessageSetting::Errors);
    }};
}

pub fn name_matches<S>(check_name: &str, canonical_name: Option<&S>, name: &S) -> bool
where
    S: AsRef<str>,
{
    canonical_name.unwrap_or(name).as_ref() == check_name
}

/// Result is bubbled
pub fn lookup_vlb<'a>(
    name: &str,
    id: &str,
    target_idx: usize,
    context: &'a mut DetranspilerContext,
) -> DetranspilerResult<Option<BorrowedDetranspilerVLB<'a>>> {
    let result = context
        .targets
        .get(context.stage_target_idx)
        .and_then(|value: &DetranspilerTarget| value.data.get(id))
        .or_else(|| {
            context
                .targets
                .get(target_idx)
                .and_then(|value| value.data.get(id))
        })
        .map(BorrowedDetranspilerVLB::VarOrList)
        .or_else(|| {
            context
                .broadcasts
                .get(id)
                .map(BorrowedDetranspilerVLB::Broadcast)
        });
    if let Some(value) = &result
        && let expected_name = value.get_original_name().as_str()
        && expected_name != name
    {
        emit_error_inline!(
            GrazeDetranspilerError::VLBNameIncorrect {
                id: id.to_string(),
                name: name.to_string(),
                expected_name: expected_name.to_string()
            },
            context
        )
    }
    Ok(result)
}

/// Result is bubbled
pub fn lookup_var_or_list<'a>(
    name: &str,
    id: &str,
    target_idx: usize,
    context: &'a mut DetranspilerContext,
) -> DetranspilerResult<Option<&'a DetranspilerVarOrList>> {
    let result = context
        .targets
        .get(context.stage_target_idx)
        .and_then(|value: &DetranspilerTarget| value.data.get(id))
        .or_else(|| {
            context
                .targets
                .get(target_idx)
                .and_then(|value| value.data.get(id))
        });
    if let Some(value) = &result
        && let expected_name = value.get_original_name().as_str()
        && expected_name != name
    {
        emit_error_inline!(
            GrazeDetranspilerError::VLBNameIncorrect {
                id: id.to_string(),
                name: name.to_string(),
                expected_name: expected_name.to_string()
            },
            context
        )
    }
    Ok(result)
}

pub fn create_block_identifier(block_name: &IString) -> ast_types::Identifier {
    ast_types::Identifier {
        path: vec![ast_types::SingleIdentifier {
            value: block_name.clone(),
        }],
    }
}

pub fn get_literal_from_sb3_primitive(value: &project_json::Sb3Primitive) -> ast_types::Literal {
    match value {
        project_json::Sb3Primitive::String(value) => ast_types::Literal::String(value.into()),
        project_json::Sb3Primitive::Int128(value) => {
            ast_types::Literal::DecimalInt(arcstr::format!("{value}"))
        }
        project_json::Sb3Primitive::Int(value) => {
            ast_types::Literal::DecimalInt(arcstr::format!("{value}"))
        }
        project_json::Sb3Primitive::Float(value) => {
            ast_types::Literal::DecimalFloat(arcstr::format!("{value}"))
        }
    }
}

pub fn convert_project(project: &project_json::Sb3Root, settings: GrazeDetranspilerSettings) {
    macro_rules! emit_error_top_level {
        ($context:expr, $err:expr, $unlogged_failure:ident, $exit_after_target_conversion:ident) => {{
            let context = &mut *$context;
            emit_message_eager(context, $err.into(), GrazeMessageSetting::Errors);
            match context.settings.message_setting {
                GrazeMessageSetting::ExitOnError => {
                    context.messages.push($err.into());
                    $exit_after_target_conversion = true;
                    break;
                }
                GrazeMessageSetting::ExitOnErrorUnlogged => {
                    $unlogged_failure = true;
                    $exit_after_target_conversion = true;
                    break;
                }
                _ => (),
            }
        }};
    }
    macro_rules! unwrap_bubbled_result_or {
        ($context_ident:pat => $result:expr, $context:expr, $fallback:expr, $unlogged_failure:ident$(, $exit_after_target_conversion:ident)?) => {{
            let context = &mut *$context;
            match {
                let $context_ident = &mut *context;
                $result
            } {
                Ok(value) => value,
                Err(err) => {
                    if context.settings.message_setting == GrazeMessageSetting::ExitOnError {
                        context.messages.push(err.into());
                    } else {
                        $unlogged_failure = true;
                    }
                    $($exit_after_target_conversion = true;)?
                    $fallback;
                }
            }
        }};
    }
    let mut context = DetranspilerContext {
        stage_target_idx: 0,
        targets: Vec::new(),
        asset_namespace: DetranspilerAssetNamespace::new(),
        assets: HashMap::new(),
        messages: Vec::new(),
        settings,
        broadcasts: HashMap::new(),
    };
    // TODO: Use broadcast data in detranspiler
    // Issue: #113
    let mut has_stage = false;
    let mut exit_after_target_conversion = false;
    let mut unlogged_failure = false;
    for target in &project.targets {
        if target.is_stage {
            if has_stage {
                emit_error_top_level!(
                    &mut context,
                    GrazeDetranspilerError::MultipleStages,
                    unlogged_failure,
                    exit_after_target_conversion
                );
            }
            context.stage_target_idx = context.targets.len();
            has_stage = true;
        }
        let target = unwrap_bubbled_result_or!(
            context => convert_target(target, context),
            &mut context,
            break,
            unlogged_failure,
            exit_after_target_conversion
        );
        context.targets.push(target);
    }
    if !exit_after_target_conversion {
        for (idx, target) in project.targets.iter().enumerate() {
            unwrap_bubbled_result_or!(
                context => fill_target(target, context, idx),
                &mut context,
                break,
                unlogged_failure
            );
        }
    }
    todo!()
}

// A function is unbubbled iff it tries (`?`) any unbubbled result or returns a Err at any point without checking if
// ExitOnError or ExitOnErrorUnlogged is on. A function is bubbled iff it is not unbubbled.
// A Result is unbubbled iff it results from an unbubbled function or is an Err that is created without checking if
// ExitOnError or ExitOnErrorUnlogged is on. A Result is bubbled iff it is not unbubbled.

// Bubbling is done to allow the detranspiler to catch as many errors or warnings at once as possible

/// Result is bubbled
pub fn convert_target(
    target: &project_json::Sb3Target,
    context: &mut DetranspilerContext,
) -> DetranspilerResult<DetranspilerTarget> {
    fn get_asset_name_and_register_asset(
        context: &mut DetranspilerContext,
        name: &str,
        asset_id: &str,
        data_format: &str,
        md5ext: &str,
    ) -> IString {
        let asset_name = context.asset_namespace.get_symbol(name, asset_id);
        let out_asset_path = format!("{asset_name}.{}", data_format);
        if !context.assets.contains_key(md5ext) {
            context.assets.insert(md5ext.to_string(), out_asset_path);
        }
        asset_name
    }
    fn get_canonical_name_and_name(
        namespace: &mut DetranspilerTargetNamespace,
        canonical_name: IString,
    ) -> (Option<IString>, IString) {
        let name = namespace.introduce_new_name(canonical_name.clone());
        (
            (name.as_str() != canonical_name.as_str()).then_some(canonical_name),
            name,
        )
    }
    let mut namespace = DetranspilerTargetNamespace::new();
    let costumes = target
        .costumes
        .iter()
        .map(|value| {
            (value.asset_id.clone(), {
                let (canonical_name, name) =
                    get_canonical_name_and_name(&mut namespace, value.name.as_str().into());
                DetranspilerAsset {
                    canonical_name,
                    name,
                    file_extension: value.data_format.clone(),
                    uncommon_data: DetranspilerCostumeUncommonData {
                        rotation_center_x: value.rotation_center_x,
                        rotation_center_y: value.rotation_center_y,
                    },
                    asset_name: get_asset_name_and_register_asset(
                        context,
                        &value.name,
                        &value.asset_id,
                        &value.data_format,
                        &value.md5ext,
                    ),
                }
            })
        })
        .collect();
    let sounds = target
        .sounds
        .iter()
        .map(|value| {
            (value.asset_id.clone(), {
                let (canonical_name, name) =
                    get_canonical_name_and_name(&mut namespace, value.name.as_str().into());
                DetranspilerAsset {
                    canonical_name,
                    name,
                    file_extension: value.data_format.clone(),
                    uncommon_data: DetranspilerSoundUncommonData,
                    asset_name: get_asset_name_and_register_asset(
                        context,
                        &value.name,
                        &value.asset_id,
                        &value.data_format,
                        &value.md5ext,
                    ),
                }
            })
        })
        .collect();
    let mut data = HashMap::with_capacity(target.variables.len() + target.lists.len());
    for (id, variable) in &target.variables {
        let (canonical_name, name) =
            get_canonical_name_and_name(&mut namespace, variable.name.as_str().into());
        data.insert(
            id.to_string(),
            DetranspilerVarOrList {
                canonical_name,
                name,
                kind: DetranspilerVarOrListKind::Variable {
                    value: variable.value.clone(),
                },
            },
        );
    }
    // TODO: Use monitor data in detranspiler
    // Issue: #112
    Ok(DetranspilerTarget {
        is_stage: target.is_stage,
        costumes,
        sounds,
        data,
        scripts: Vec::new(),
    })
}

/// Result is bubbled
pub fn fill_target(
    target: &project_json::Sb3Target,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<()> {
    for (block_id, block) in &target.blocks {
        let project_json::Sb3Block::Normal(normal_block) = block else {
            continue;
        };
        if !normal_block.top_level {
            continue;
        }
        let block_stack = match get_block_shape(&normal_block.opcode) {
            BlockShape::Hat => {
                let (hat_function, arguments) = unwrap_or_emit_message!(
                    convert_hat_block(normal_block, block_id, &target.blocks, context, target_idx),
                    context,
                    {
                        let block_stack = if let Some(next_block_id) = &normal_block.next
                            && let Some(next_block) = unwrap_or_emit_message!(
                                resolve_block_ref(next_block_id, &target.blocks).map(Some),
                                context,
                                None
                            ) {
                            convert_block_stack(
                                next_block,
                                next_block_id,
                                &target.blocks,
                                context,
                                target_idx,
                            )?
                        } else {
                            ast_types::CodeBlock {
                                statements: Vec::new(),
                            }
                        };
                        context.targets.get_mut(target_idx).unwrap().scripts.push(
                            DetranspilerTargetBlockStack::IsolatedStack { stack: block_stack },
                        );
                        continue;
                    }
                );
                DetranspilerTargetBlockStack::HatBlock {
                    hat_function,
                    arguments,
                    stack: {
                        if let Some(next_block_id) = &normal_block.next
                            && let Some(next_block) = unwrap_or_emit_message!(
                                resolve_block_ref(next_block_id, &target.blocks).map(Some),
                                context,
                                None
                            )
                        {
                            convert_block_stack(
                                next_block,
                                next_block_id,
                                &target.blocks,
                                context,
                                target_idx,
                            )?
                        } else {
                            ast_types::CodeBlock {
                                statements: Vec::new(),
                            }
                        }
                    },
                }
            }
            BlockShape::Stack => DetranspilerTargetBlockStack::IsolatedStack {
                stack: convert_block_stack(block, block_id, &target.blocks, context, target_idx)?,
            },
            BlockShape::Reporter => DetranspilerTargetBlockStack::IsolatedExpression {
                expression: convert_reporter_block(
                    block,
                    block_id,
                    &target.blocks,
                    context,
                    target_idx,
                )?,
            },
        };
        context
            .targets
            .get_mut(target_idx)
            .unwrap()
            .scripts
            .push(block_stack);
    }
    Ok(())
}

/// Result is unbubbled
pub fn resolve_block_ref<'a>(
    block_id: &str,
    blocks: &'a HashMap<String, project_json::Sb3Block>,
) -> DetranspilerResult<&'a project_json::Sb3Block> {
    let Some(next_block) = blocks.get(block_id) else {
        return Err(GrazeDetranspilerError::InvalidBlockReference {
            block_id: block_id.to_string(),
        });
    };
    Ok(next_block)
}

/// Result is bubbled
pub fn convert_reporter_block(
    block: &project_json::Sb3Block,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<ast_types::Expression> {
    Ok(try_or_emit_message!(
        match block {
            project_json::Sb3Block::Normal(sb3_normal_block) => {
                convert_normal_reporter_block(
                    sb3_normal_block,
                    block_id,
                    blocks,
                    context,
                    target_idx,
                )
            }
            project_json::Sb3Block::Primitive(sb3_primitive_block) => {
                convert_primitive_reporter_block(sb3_primitive_block, context, target_idx)
            }
        },
        context,
        Ok(ast_types::Expression::Literal(
            ast_types::Literal::EmptyExpression,
        ))
    ))
}

/// Result is unbubbled
macro_rules! convert_block {
    ($block:expr, $block_id:expr, $blocks:expr, $context:expr, $target_idx:expr$(, $arg_context_pat:pat => $stack_err:expr)?) => {{
        let block = &*$block;
        let block_id = &*$block_id;
        let blocks = &*$blocks;
        let context = &mut *$context;
        let target_idx = $target_idx;
        let block_kind_info = get_block_kind_info(block)?;
        let mut tracked_args = 0_usize;
        let mut parameters = Vec::new();
        let mut stack_params = Vec::new();
        for Argument {
            name: argument_name,
            kind,
            ignore,
        } in &block_kind_info.arguments
        {
            if if kind.is_field() {
                block.fields.contains_key(argument_name.as_str())
            } else {
                block.inputs.contains_key(argument_name.as_str())
            } {
                tracked_args += 1;
            }
            if *ignore {
                continue;
            }
            match kind {
                ArgumentKind::Field => {
                    let Some(field_value) = block.fields.get(argument_name.as_str()) else {
                        parameters.push(ast_types::Expression::Literal(
                            ast_types::Literal::EmptyExpression,
                        ));
                        continue;
                    };
                    parameters.push(unwrap_or_emit_message!(
                        convert_field_value_info(
                            get_field_value_info(field_value, &block.opcode),
                            field_value,
                            target_idx,
                            context,
                        ),
                        context,
                        {
                            parameters.push(ast_types::Expression::Literal(
                                ast_types::Literal::EmptyExpression,
                            ));
                            continue;
                        }
                    ));
                }
                ArgumentKind::Input => {
                    let Some(input) = block.inputs.get(argument_name.as_str()) else {
                        parameters.push(ast_types::Expression::Literal(
                            ast_types::Literal::EmptyExpression,
                        ));
                        // ArgumentKind::Input is only for possibly empty inputs
                        continue;
                    };
                    let (project_json::Sb3InputValue::Shadow(input_repr)
                    | project_json::Sb3InputValue::NoShadow(input_repr)
                    | project_json::Sb3InputValue::ObscuredShadow {
                        value: input_repr,
                        shadow: _,
                    }) = input;
                    parameters.push(match input_repr {
                        project_json::Sb3InputRepr::Reference(block_id) => convert_reporter_block(
                            unwrap_or_emit_message!(
                                blocks.get(block_id).ok_or_else(|| {
                                    GrazeDetranspilerError::InvalidBlockReference {
                                        block_id: block_id.clone(),
                                    }
                                }),
                                context,
                                {
                                    parameters.push(ast_types::Expression::Literal(
                                        ast_types::Literal::EmptyExpression,
                                    ));
                                    continue;
                                }
                            ),
                            block_id,
                            blocks,
                            context,
                            target_idx,
                        )?,
                        project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                            convert_primitive_reporter_block(block, context, target_idx),
                            context,
                            ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                        ),
                    });
                }
                ArgumentKind::StackInput => {
                    let Some(input) = block.inputs.get(argument_name.as_str()) else {
                        stack_params.push(ast_types::CodeBlock {
                            statements: Vec::new(),
                        });
                        continue;
                    };
                    let (project_json::Sb3InputValue::Shadow(input_repr)
                    | project_json::Sb3InputValue::NoShadow(input_repr)
                    | project_json::Sb3InputValue::ObscuredShadow {
                        value: input_repr,
                        shadow: _,
                    }) = input;
                    stack_params.push(match input_repr {
                        project_json::Sb3InputRepr::Reference(block_id) => convert_block_stack(
                            unwrap_or_emit_message!(
                                blocks.get(block_id).ok_or_else(|| {
                                    GrazeDetranspilerError::InvalidBlockReference {
                                        block_id: block_id.clone(),
                                    }
                                }),
                                context,
                                {
                                    stack_params.push(ast_types::CodeBlock {
                                        statements: Vec::new(),
                                    });
                                    continue;
                                }
                            ),
                            block_id,
                            blocks,
                            context,
                            target_idx,
                        )?,
                        project_json::Sb3InputRepr::PrimitiveBlock(_) => {
                            emit_error!(
                                GrazeDetranspilerError::PrimitiveBlockAsSubstack {
                                    block_id: block_id.to_string(),
                                    input_name: argument_name.to_string()
                                },
                                context
                            );
                            ast_types::CodeBlock {
                                statements: Vec::new(),
                            }
                        }
                    });
                    $(
                        let $arg_context_pat = (&mut *context, argument_name);
                        $stack_err;
                    )?
                }
                ArgumentKind::MenuInput {
                    menu_opcode,
                    menu_field,
                } => {
                    let Some(input) = block.inputs.get(argument_name.as_str()) else {
                        parameters.push(ast_types::Expression::Literal(
                            ast_types::Literal::EmptyExpression,
                        ));
                        emit_message(
                            context,
                            || {
                                GrazeDetranspilerWarning::UnexpectedEmptyInput {
                                    input: argument_name.to_string(),
                                    block_id: block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                        continue;
                    };
                    let (project_json::Sb3InputValue::Shadow(input_repr)
                    | project_json::Sb3InputValue::NoShadow(input_repr)
                    | project_json::Sb3InputValue::ObscuredShadow {
                        value: input_repr,
                        shadow: _,
                    }) = input;
                    parameters.push(match input_repr {
                        project_json::Sb3InputRepr::Reference(block_id) => {
                            let inner_block = unwrap_or_emit_message!(
                                blocks.get(block_id).ok_or_else(|| {
                                    GrazeDetranspilerError::InvalidBlockReference {
                                        block_id: block_id.clone(),
                                    }
                                }),
                                context,
                                {
                                    parameters.push(ast_types::Expression::Literal(
                                        ast_types::Literal::EmptyExpression,
                                    ));
                                    continue;
                                }
                            );
                            if let project_json::Sb3Block::Normal(inner_block) = inner_block
                                && inner_block.opcode.as_str() == menu_opcode.as_str()
                            {
                                for key in inner_block.fields.keys() {
                                    if key.as_str() == menu_field.as_str() {
                                        continue;
                                    }
                                    let arg = key.clone();
                                    emit_message(
                                        context,
                                        || {
                                            GrazeDetranspilerWarning::UnusedField {
                                                field: arg,
                                                block_id: block_id.to_string(),
                                            }
                                            .into()
                                        },
                                        GrazeMessageSetting::Warnings,
                                    );
                                }
                                for key in inner_block.inputs.keys() {
                                    let arg = key.clone();
                                    emit_message(
                                        context,
                                        || {
                                            GrazeDetranspilerWarning::UnusedInput {
                                                input: arg,
                                                block_id: block_id.to_string(),
                                            }
                                            .into()
                                        },
                                        GrazeMessageSetting::Warnings,
                                    );
                                }
                                if let Some(field_value) = inner_block.fields.get(menu_field.as_str()) {
                                    unwrap_or_emit_message!(
                                        convert_field_value_info(
                                            get_field_value_info(field_value, menu_opcode),
                                            field_value,
                                            target_idx,
                                            context,
                                        ),
                                        context,
                                        {
                                            parameters.push(ast_types::Expression::Literal(
                                                ast_types::Literal::EmptyExpression,
                                            ));
                                            continue;
                                        }
                                    )
                                } else {
                                    parameters.push(ast_types::Expression::Literal(
                                        ast_types::Literal::EmptyExpression,
                                    ));
                                    emit_message(
                                        context,
                                        || {
                                            GrazeDetranspilerWarning::MissingMenuField {
                                                field: menu_field.to_string(),
                                                block_id: block_id.clone(),
                                            }
                                            .into()
                                        },
                                        GrazeMessageSetting::Warnings,
                                    );
                                    continue;
                                }
                            } else {
                                convert_reporter_block(inner_block, block_id, blocks, context, target_idx)?
                            }
                        }
                        project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                            convert_primitive_reporter_block(block, context, target_idx),
                            context,
                            ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                        ),
                    });
                }
            }
        }
        if tracked_args != block.fields.len() + block.inputs.len() {
            type IsField = bool;
            let mut tracked_args =
                HashMap::<String, IsField>::with_capacity(block.fields.len() + block.inputs.len());
            for key in block.fields.keys() {
                tracked_args.insert(key.clone(), true);
            }
            for key in block.inputs.keys() {
                tracked_args.insert(key.clone(), false);
            }
            for Argument {
                name,
                kind: _,
                ignore: _,
            } in &block_kind_info.arguments
            {
                tracked_args.remove(name.as_str());
            }
            for (arg, is_field) in tracked_args {
                emit_message(
                    context,
                    || {
                        if is_field {
                            GrazeDetranspilerWarning::UnusedField {
                                field: arg,
                                block_id: block_id.to_string(),
                            }
                        } else {
                            GrazeDetranspilerWarning::UnusedInput {
                                input: arg,
                                block_id: block_id.to_string(),
                            }
                        }
                        .into()
                    },
                    GrazeMessageSetting::Warnings,
                );
            }
        }
        (block_kind_info, parameters, stack_params)
    }};
}

/// Result is unbubbled
pub fn convert_normal_reporter_block(
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<ast_types::Expression> {
    // TODO: Implement binary and unary operators in detranspiler
    // Issue: #108
    let (block_kind_info, parameters, _) = convert_block!(
        block,
        block_id,
        blocks,
        context,
        target_idx,
        (context, input_name) => emit_error!(
            GrazeDetranspilerError::SubstackInReporter {
                block_id: block_id.to_string(),
                input_name: input_name.to_string(),
            },
            context
        )
    );
    let function = create_block_identifier(&block_kind_info.block_name);
    if block_kind_info.is_singleton && parameters.is_empty() {
        return Ok(ast_types::Expression::Identifier(function));
    }
    Ok(ast_types::Expression::Call {
        function,
        arguments: parameters,
    })
}

/// Result is unbubbled
pub fn convert_field_value_info(
    field_value_info: Option<FieldValueInfo>,
    field_value: &project_json::Sb3FieldValue,
    target_idx: usize,
    context: &mut DetranspilerContext,
) -> DetranspilerResult<ast_types::Expression> {
    Ok(match field_value_info {
        Some(value) => ast_types::Expression::Identifier(ast_types::Identifier {
            path: vec![ast_types::SingleIdentifier {
                value: value.field_value_name,
            }],
        }),
        None => match field_value {
            project_json::Sb3FieldValue::Normal(value) => {
                ast_types::Expression::Literal(get_literal_from_sb3_primitive(value))
            }
            project_json::Sb3FieldValue::WithId { value, id } => {
                ast_types::Expression::Identifier(ast_types::Identifier {
                    path: vec![ast_types::SingleIdentifier {
                        value: match lookup_vlb(&value.as_cow_str(), id, target_idx, context)?
                            .ok_or_else(|| GrazeDetranspilerError::UnknownVariable {
                                id: id.clone(),
                                name: value.to_string(),
                            })? {
                            BorrowedDetranspilerVLB::Broadcast(broadcast) => broadcast.name.clone(),
                            BorrowedDetranspilerVLB::VarOrList(var_or_list) => {
                                var_or_list.name.clone()
                            }
                        },
                    }],
                })
            }
        },
    })
}

/// Result is unbubbled
pub fn convert_primitive_reporter_block(
    block: &project_json::Sb3PrimitiveBlock,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<ast_types::Expression> {
    match block {
        project_json::Sb3PrimitiveBlock::Number(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::PositiveNumber(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::PositiveInteger(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::Integer(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::Angle(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::Color(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::String(sb3_primitive) => Ok(
            ast_types::Expression::Literal(get_literal_from_sb3_primitive(sb3_primitive)),
        ),
        project_json::Sb3PrimitiveBlock::Broadcast { name, id } => {
            let broadcast = context
                .broadcasts
                .get(id)
                .filter(|value| name_matches(name, value.canonical_name.as_ref(), &value.name))
                .ok_or_else(|| GrazeDetranspilerError::UnknownBroadcast {
                    id: id.clone(),
                    name: name.clone(),
                })?;
            Ok(ast_types::Expression::Identifier(ast_types::Identifier {
                path: vec![ast_types::SingleIdentifier {
                    value: broadcast.name.clone(),
                }],
            }))
        }
        project_json::Sb3PrimitiveBlock::Variable {
            name,
            id,
            x: _,
            y: _,
        } => {
            let variable = lookup_var_or_list(name, id, target_idx, context)?.ok_or_else(|| {
                GrazeDetranspilerError::UnknownVariable {
                    id: id.clone(),
                    name: name.clone(),
                }
            })?;
            Ok(ast_types::Expression::Identifier(ast_types::Identifier {
                path: vec![ast_types::SingleIdentifier {
                    value: variable.name.clone(),
                }],
            }))
        }
        project_json::Sb3PrimitiveBlock::List {
            name,
            id,
            x: _,
            y: _,
        } => {
            let list = lookup_var_or_list(name, id, target_idx, context)?.ok_or_else(|| {
                GrazeDetranspilerError::UnknownVariable {
                    id: id.clone(),
                    name: name.clone(),
                }
            })?;
            Ok(ast_types::Expression::Identifier(ast_types::Identifier {
                path: vec![ast_types::SingleIdentifier {
                    value: list.name.clone(),
                }],
            }))
        }
    }
}

/// Result is unbubbled
pub fn convert_hat_block(
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<(ast_types::Identifier, Vec<ast_types::Expression>)> {
    let (block_kind_info, parameters, _) = convert_block!(
        block,
        block_id,
        blocks,
        context,
        target_idx,
        (context, input_name) => emit_error!(
            GrazeDetranspilerError::SubstackInHatBlock {
                block_id: block_id.to_string(),
                input_name: input_name.to_string(),
            },
            context
        )
    );
    let function = create_block_identifier(&block_kind_info.block_name);
    Ok((function, parameters))
}

// IString allows for more efficient cycle detection
type NextBlockId = IString;

/// Result is bubbled
pub fn convert_block_stack(
    block: &project_json::Sb3Block,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<ast_types::CodeBlock> {
    // TODO: Implement cycle detection in block conversions
    // Issue: #109
    let mut statements = Vec::new();
    let mut current_block = block;
    let mut current_block_id = IString::from(block_id);
    loop {
        let (statement, next_block_id) = unwrap_or_emit_message!(
            convert_stack_block(
                current_block,
                &current_block_id,
                blocks,
                context,
                target_idx
            ),
            context,
            break
        );
        statements.push(statement);
        let Some(next_block_id) = next_block_id else {
            break;
        };
        let next_block =
            unwrap_or_emit_message!(resolve_block_ref(&next_block_id, blocks), context, break);
        current_block = next_block;
        current_block_id = next_block_id;
    }
    Ok(ast_types::CodeBlock { statements })
}

/// Result is unbubbled
pub fn convert_stack_block(
    block: &project_json::Sb3Block,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<(ast_types::Statement, Option<NextBlockId>)> {
    // TODO: Implement if else block and if else chains in detranspiler
    // Issue: #107

    // TODO: Implement list assignment in detranspiler
    // Issue: #106
    let project_json::Sb3Block::Normal(block) = block else {
        return Err(GrazeDetranspilerError::PrimitiveBlockAsStackBlock {
            block_id: block_id.to_string(),
        });
    };
    let mut has_substack = false;
    let (block_kind_info, parameters, stack_params) = convert_block!(
        block,
        block_id,
        blocks,
        context,
        target_idx,
        (context, input_name) => {
            if has_substack {
                emit_error!(
                    GrazeDetranspilerError::SubstackInReporter {
                        block_id: block_id.to_string(),
                        input_name: input_name.to_string()
                    },
                    context
                )
            }
            has_substack = true;
        }
    );
    let function = create_block_identifier(&block_kind_info.block_name);
    let next_block = block.next.as_deref().map(Into::into);
    if let Some(substack) = stack_params.into_iter().next() {
        return Ok((
            ast_types::Statement::Control {
                control_function: function,
                arguments: parameters,
                code_block: substack,
            },
            next_block,
        ));
    }
    Ok((
        ast_types::Statement::Call {
            function,
            arguments: parameters,
        },
        next_block,
    ))
}
