use std::collections::HashMap;

use grazelang_library::{
    AliasSegment, CallBlockParam, CallBlockParamKind, KnownBlock, LibraryItem, LibraryItemValue,
    SimpleCallableKnownBlockSignature,
    project_json::{Sb3FieldValue, Sb3Primitive, Sb3PrimitiveBlock},
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolboxCategory {
    pub category: String,
    pub id: String,
    pub blocks: Vec<BlockEntry>,
    pub alt_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BlockEntry {
    #[serde(rename = "type")]
    pub opcode: String,
    pub args: Vec<BlockArg>,
    pub alt_name: Option<String>,
    pub assign: Option<AssignmentDescriptor>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AssignmentDescriptor {
    pub opcode: String,
    pub value: BlockArg,
    pub known_params: Vec<KnownParam>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct KnownParam {
    pub param: BlockArg,
    pub value: Sb3FieldValue,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum BlockArg {
    #[serde(rename_all = "camelCase")]
    Field {
        name: String,
        field_type: String,
        value: Option<Sb3Primitive>,
        options: Option<Vec<MenuOption>>,
    },
    #[serde(rename_all = "camelCase")]
    Input {
        name: String,
        menu_field_name: Option<String>,
        input_type: InputType,
        check: Option<Sb3Primitive>,
        shadow: Option<ShadowData>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MenuOption {
    pub label: String,
    pub value: String,
    pub alt_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ShadowData {
    #[serde(rename = "type")]
    pub shadow_type: String,
    pub default_value: Option<String>,
    pub options: Option<Vec<MenuOption>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InputType {
    Value,
    Statement,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AssociatedLibraryItem {
    name: String,
    field_value: Sb3FieldValue,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProcessedBlockEntry {
    pub name: String,
    pub opcode: String,
    pub library_item: LibraryItem,
    pub associated_items: Vec<AssociatedLibraryItem>,
}

pub fn primitive_opcode_to_sb3_primitive_block(
    opcode: &str,
    value: Sb3Primitive,
) -> Option<Sb3PrimitiveBlock> {
    match opcode {
        "math_number" => Some(Sb3PrimitiveBlock::Number(value)),
        "math_positive_number" => Some(Sb3PrimitiveBlock::PositiveNumber(value)),
        "math_whole_number" => Some(Sb3PrimitiveBlock::PositiveInteger(value)),
        "math_integer" => Some(Sb3PrimitiveBlock::Integer(value)),
        "math_angle" => Some(Sb3PrimitiveBlock::Angle(value)),
        "colour_picker" => Some(Sb3PrimitiveBlock::Color(value)),
        "text" => Some(Sb3PrimitiveBlock::String(value)),
        // "event_broadcast_menu" => Some(Sb3PrimitiveBlock::Broadcast(value)), // TODO: implement these
        // "data_variable" => Some(Sb3PrimitiveBlock::Variable(value)),
        // "data_listcontents" => Some(Sb3PrimitiveBlock::List(value)),
        _ => None,
    }
}

impl BlockEntry {
    fn process(self) -> ProcessedBlockEntry {
        pub fn convert_block_arg_into_call_block_param(
            arg: BlockArg,
            associated_items: Option<&mut Vec<AssociatedLibraryItem>>,
        ) -> CallBlockParam {
            match arg {
                BlockArg::Field {
                    name,
                    field_type: _, // TODO: Should this be used somehow?
                    value,
                    options,
                } => {
                    if let Some(associated_items) = associated_items {
                        options.iter().flatten().for_each(|value| {
                            associated_items.push(AssociatedLibraryItem {
                                name: value.alt_name.as_ref().unwrap_or(&value.label).clone(),
                                field_value: Sb3FieldValue::Normal(value.value.clone().into()),
                            })
                        })
                    }
                    CallBlockParam {
                        kind: CallBlockParamKind::Field {
                            default: value.map(Sb3FieldValue::Normal),
                        },
                        name: name.into(),
                    }
                }
                BlockArg::Input {
                    name,
                    menu_field_name,
                    input_type,
                    check: _,
                    shadow,
                } => CallBlockParam {
                    kind: match shadow {
                        Some(ShadowData {
                            shadow_type,
                            default_value,
                            options,
                        }) => {
                            if let Some(associated_items) = associated_items {
                                options.iter().flatten().for_each(|value| {
                                    associated_items.push(AssociatedLibraryItem {
                                        name: value
                                            .alt_name
                                            .as_ref()
                                            .unwrap_or(&value.label)
                                            .clone(),
                                        field_value: Sb3FieldValue::Normal(
                                            value.value.clone().into(),
                                        ),
                                    })
                                })
                            }
                            if let Some(primitive_block) = primitive_opcode_to_sb3_primitive_block(
                                &shadow_type,
                                Sb3Primitive::String(default_value.clone().unwrap_or_default()),
                            ) {
                                CallBlockParamKind::Input {
                                    default: Some(primitive_block),
                                }
                            } else {
                                CallBlockParamKind::MenuInput {
                                    opcode: shadow_type.into(),
                                    field_name: menu_field_name
                                        .map(|value| value.as_str().into())
                                        .unwrap_or_else(|| name.as_str().into()),
                                    default: Sb3FieldValue::Normal(
                                        default_value.unwrap_or_default().into(),
                                    ),
                                }
                            }
                        }
                        None => match input_type {
                            InputType::Value => CallBlockParamKind::Input { default: None },
                            InputType::Statement => CallBlockParamKind::BlockStack,
                        },
                    },
                    name: name.into(),
                },
            }
        }
        let BlockEntry {
            opcode,
            args,
            alt_name,
            assign,
        } = self;
        if args.is_empty() {
            return ProcessedBlockEntry {
                name: alt_name.unwrap_or_else(|| opcode.split_once('_').unwrap().1.to_string()),
                opcode: opcode.clone(),
                library_item: LibraryItem {
                    namespace: HashMap::new(),
                    value: Some(LibraryItemValue::KnownBlock(Box::new(
                        KnownBlock::SingletonReporter {
                            opcode: opcode.into(),
                            params: Vec::new(),
                            field: None,
                            assign: assign.map(
                                |AssignmentDescriptor {
                                     opcode,
                                     value,
                                     known_params,
                                 }| {
                                    SimpleCallableKnownBlockSignature(
                                        opcode.into(),
                                        convert_block_arg_into_call_block_param(value, None),
                                        known_params
                                            .into_iter()
                                            .map(|KnownParam { param, value }| {
                                                (
                                                    convert_block_arg_into_call_block_param(
                                                        param, None,
                                                    ),
                                                    KnownBlock::FieldValue { value },
                                                )
                                            })
                                            .collect(),
                                    )
                                },
                            ),
                        },
                    ))),
                },
                associated_items: Vec::new(),
            };
        }
        let mut associated_items = Vec::<AssociatedLibraryItem>::new();
        let known_block_args = args
            .into_iter()
            .map(|arg| convert_block_arg_into_call_block_param(arg, Some(&mut associated_items)))
            .collect::<Vec<_>>();
        ProcessedBlockEntry {
            name: alt_name.unwrap_or_else(|| opcode.split_once('_').unwrap().1.to_string()),
            library_item: LibraryItem {
                namespace: HashMap::new(),
                value: Some(LibraryItemValue::KnownBlock(Box::new(
                    KnownBlock::Callable(opcode.as_str().into(), known_block_args),
                ))),
            },
            opcode,
            associated_items,
        }
    }
}

pub fn process_toolbox_category(
    value: ToolboxCategory,
) -> (String, LibraryItem, Vec<(String, LibraryItem)>) {
    let ToolboxCategory {
        category: _,
        id,
        blocks,
        alt_name,
    } = value;
    let mut associated_items = HashMap::<String, (Sb3FieldValue, Vec<String>)>::new();
    let mut namespace = HashMap::<String, LibraryItem>::with_capacity(blocks.len());
    for block in blocks {
        let ProcessedBlockEntry {
            name,
            opcode,
            library_item: item,
            associated_items: new_associated_items,
        } = block.process();
        for AssociatedLibraryItem {
            name: associated_item_name,
            field_value: associated_item_field_value,
        } in new_associated_items
        {
            if let Some(current) = associated_items.get_mut(&associated_item_name) {
                assert_eq!(&current.0, &associated_item_field_value);
                current.1.push(opcode.clone());
            } else {
                associated_items.insert(
                    associated_item_name,
                    (associated_item_field_value, vec![opcode.clone()]),
                );
            }
        }
        namespace.insert(name, item);
    }
    let mut processed_associated_items =
        Vec::<(String, LibraryItem)>::with_capacity(associated_items.len());
    for (name, (field_value, _opcodes)) in associated_items {
        // TODO: use opcodes
        if let Some(current) = namespace.get_mut(&name) {
            if let Some(LibraryItemValue::KnownBlock(known_block)) = &mut current.value
                && let KnownBlock::SingletonReporter {
                    opcode: _,
                    params: _,
                    field,
                    assign: _,
                } = known_block.as_mut()
            {
                field.replace(field_value.clone());
            } else {
                todo!() // TODO: warn about overlap
            }
        } else {
            namespace.insert(
                name.clone(),
                LibraryItem {
                    namespace: HashMap::new(),
                    value: Some(LibraryItemValue::Alias(vec![
                        AliasSegment::Super,
                        AliasSegment::Child("menus".to_string()),
                        AliasSegment::Child(name.clone()),
                    ])),
                },
            );
        }
        processed_associated_items.push((
            name,
            LibraryItem {
                namespace: HashMap::new(),
                value: Some(LibraryItemValue::KnownBlock(Box::new(
                    KnownBlock::FieldValue { value: field_value },
                ))),
            },
        ));
    }
    (
        alt_name.unwrap_or(id),
        LibraryItem {
            namespace,
            value: None,
        },
        processed_associated_items,
    )
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LibraryCache {
    pub hash: String,
    pub value: String,
}
