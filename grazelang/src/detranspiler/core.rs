use std::collections::HashMap;

use arcstr::ArcStr as IString;
use grazelang_types::project_json;
use serde::{Deserialize, Serialize, de::DeserializeOwned};

use crate::{
    ast::core as ast_types,
    detranspiler::get_info::{Argument, ArgumentKind, get_block_kind_info, get_field_value_info},
    messages::types::{GrazeDetranspilerError, GrazeDetranspilerMessage, GrazeDetranspilerWarning},
    names::{BidirectionalNamespace, IStringNamespace},
    settings::{GrazeDetranspilerSettings, GrazeMessageSetting},
};

#[derive(Debug, Clone, PartialEq)]
pub struct DetranspilerContext {
    pub targets: Vec<DetranspilerTarget>,
    pub asset_namespace: BidirectionalNamespace,
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
    pub costumes: HashMap<AssetId, DetranspilerAsset<DetranspilerCostumeUncommonData>>,
    pub sounds: HashMap<AssetId, DetranspilerAsset<DetranspilerSoundUncommonData>>,
    pub data: HashMap<DataId, DetranspilerVarOrList>,
    pub is_stage: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DetranspilerVLB {
    Broadcast(DetranspilerBroadcast),
    VarOrList(DetranspilerVarOrList),
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

pub fn emit_message(
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
        emit_message(context, err.into(), GrazeMessageSetting::Errors);
    }};
}

pub fn name_matches<S>(check_name: &str, canonical_name: Option<&S>, name: &S) -> bool
where
    S: AsRef<str>,
{
    canonical_name.unwrap_or(name).as_ref() == check_name
}

pub fn lookup_vlb(
    name: &str,
    id: &str,
    target: &DetranspilerTarget,
    context: &DetranspilerContext,
) -> Option<DetranspilerVLB> {
    todo!()
}

pub fn block_check_name_collision(
    name: &str,
    target: &DetranspilerTarget,
    context: &DetranspilerContext,
) -> bool {
    todo!()
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
        use std::fmt::Write;
        let asset_name = context.asset_namespace.get_symbol(name, asset_id);
        let mut out_asset_path = String::with_capacity(asset_name.len() + data_format.len() + 1);
        write!(&mut out_asset_path, "{asset_name}.{}", data_format).unwrap();
        if !context.assets.contains_key(md5ext) {
            context.assets.insert(md5ext.to_string(), out_asset_path);
        }
        asset_name
    }
    fn get_canonical_name_and_name(
        namespace: &mut IStringNamespace,
        canonical_name: IString,
    ) -> (Option<IString>, IString) {
        let name = namespace.introduce_new_name(canonical_name.clone());
        (
            (name.as_str() != canonical_name.as_str()).then_some(canonical_name),
            name,
        )
    }
    let mut namespace = IStringNamespace::new();
    // TODO: Fill namespace with default namespaces
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
    Ok(DetranspilerTarget {
        costumes,
        sounds,
        data,
        is_stage: target.is_stage,
    })
}

pub fn convert_reporter_block(
    block: &project_json::Sb3Block,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target: &DetranspilerTarget,
) -> DetranspilerResult<ast_types::Expression> {
    Ok(try_or_emit_message!(
        match block {
            project_json::Sb3Block::Normal(sb3_normal_block) => {
                convert_normal_reporter_block(sb3_normal_block, block_id, blocks, context, target)
            }
            project_json::Sb3Block::Primitive(sb3_primitive_block) => {
                convert_primitive_reporter_block(sb3_primitive_block, blocks, context, target)
            }
        },
        context,
        Ok(ast_types::Expression::Literal(
            ast_types::Literal::EmptyExpression
        ))
    ))
}

pub fn convert_normal_reporter_block(
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target: &DetranspilerTarget,
) -> DetranspilerResult<ast_types::Expression> {
    let block_kind_info = get_block_kind_info(block)?;
    let mut tracked_args = 0_usize;
    let mut parameters = Vec::new();
    for Argument { name, kind, ignore } in &block_kind_info.arguments {
        if if kind.is_field() {
            block.fields.contains_key(name.as_str())
        } else {
            block.inputs.contains_key(name.as_str())
        } {
            tracked_args += 1;
        }
        if *ignore {
            continue;
        }
        match kind {
            ArgumentKind::Field => {
                let Some(field_value) = block.fields.get(name.as_str()) else {
                    parameters.push(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                    continue;
                };
                match get_field_value_info(field_value, &block.opcode) {
                    Some(value) => {
                        parameters.push(ast_types::Expression::Identifier(ast_types::Identifier {
                            path: vec![ast_types::SingleIdentifier {
                                value: value.field_value_name,
                            }],
                        }));
                    }
                    None => {
                        parameters.push(match field_value {
                            project_json::Sb3FieldValue::Normal(value) => {
                                ast_types::Expression::Literal(get_literal_from_sb3_primitive(
                                    value,
                                ))
                            }
                            project_json::Sb3FieldValue::WithId { value, id } => {
                                ast_types::Expression::Identifier(ast_types::Identifier {
                                    path: vec![ast_types::SingleIdentifier {
                                        value: match lookup_vlb(
                                            &value.as_cow_str(),
                                            id,
                                            target,
                                            context,
                                        )
                                        .ok_or_else(|| GrazeDetranspilerError::UnknownVariable {
                                            id: id.clone(),
                                            name: value.to_string(),
                                        })? {
                                            DetranspilerVLB::Broadcast(broadcast) => {
                                                broadcast.name.clone()
                                            }
                                            DetranspilerVLB::VarOrList(var_or_list) => {
                                                var_or_list.name.clone()
                                            }
                                        },
                                    }],
                                })
                            }
                        });
                    }
                }
            }
            ArgumentKind::Input => {
                let Some(input) = block.inputs.get(name.as_str()) else {
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
                        blocks.get(block_id).ok_or_else(|| {
                            GrazeDetranspilerError::InvalidBlockReference {
                                block_id: block_id.clone(),
                            }
                        })?,
                        block_id,
                        blocks,
                        context,
                        target,
                    )?,
                    project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                        convert_primitive_reporter_block(block, blocks, context, target),
                        context,
                        ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                    ),
                });
            }
            ArgumentKind::StackInput => todo!(),
            ArgumentKind::MenuInput {
                menu_opcode,
                menu_field,
            } => todo!(),
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
                .into(),
                GrazeMessageSetting::Warnings,
            );
        }
    }
    todo!()
}

pub fn convert_primitive_reporter_block(
    block: &project_json::Sb3PrimitiveBlock,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target: &DetranspilerTarget,
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
            // TODO: Fallback to globals
            let variable = target
                .data
                .get(id)
                .filter(|value| name_matches(name, value.canonical_name.as_ref(), &value.name))
                .ok_or_else(|| GrazeDetranspilerError::UnknownVariable {
                    id: id.clone(),
                    name: name.clone(),
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
            let list = target
                .data
                .get(id)
                .filter(|value| name_matches(name, value.canonical_name.as_ref(), &value.name))
                .ok_or_else(|| GrazeDetranspilerError::UnknownList {
                    id: id.clone(),
                    name: name.clone(),
                })?;
            Ok(ast_types::Expression::Identifier(ast_types::Identifier {
                path: vec![ast_types::SingleIdentifier {
                    value: list.name.clone(),
                }],
            }))
        }
    }
}

// IString allows for more efficient cycle detection
type NextBlockId = IString;

pub fn convert_stack_block(
    block: &project_json::Sb3Block,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target: &DetranspilerTarget,
) -> DetranspilerResult<(ast_types::Statement, NextBlockId)> {
    todo!()
}
