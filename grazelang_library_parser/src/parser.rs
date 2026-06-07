use std::collections::{HashMap, HashSet};

use arcstr::ArcStr as IString;
use grazelang_library::{
    CallBlockParam, CallBlockParamKind, KnownBlock, LibraryItem, LibraryItemValue, NO_CATEGORY_ID,
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
    #[serde(default)]
    pub is_simpleton: bool, // TODO: implement this
    pub assign: Option<AssignmentDescriptor>, // TODO: implement these
                                              // Issue: #40
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
        option_category: Option<IString>,
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
    pub option_category: Option<IString>,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InputType {
    Value,
    Statement,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AssociatedLibraryItem {
    pub name: String,
    pub field_value: Sb3Primitive,
    pub category: Option<IString>,
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
        // Issue: #37
        // "data_variable" => Some(Sb3PrimitiveBlock::Variable(value)),
        // "data_listcontents" => Some(Sb3PrimitiveBlock::List(value)),
        _ => None,
    }
}

pub fn get_menu_category_id(
    menu_category_ids: &mut HashMap<IString, u32>,
    category: Option<&IString>,
) -> u32 {
    if let Some(category) = category {
        if let Some(&id) = menu_category_ids.get(category) {
            id
        } else {
            let id = menu_category_ids.len() as u32;
            menu_category_ids.insert(category.clone(), id);
            id
        }
    } else {
        NO_CATEGORY_ID
    }
}

impl BlockEntry {
    fn process(self, menu_category_ids: &mut HashMap<IString, u32>) -> ProcessedBlockEntry {
        pub fn convert_block_arg_into_call_block_param(
            arg: BlockArg,
            associated_items: Option<&mut Vec<AssociatedLibraryItem>>,
            menu_category_ids: &mut HashMap<IString, u32>,
        ) -> CallBlockParam {
            match arg {
                BlockArg::Field {
                    name,
                    field_type: _,
                    value,
                    options,
                    option_category,
                } => {
                    if let Some(associated_items) = associated_items {
                        options.iter().flatten().for_each(|value| {
                            associated_items.push(AssociatedLibraryItem {
                                name: value.alt_name.as_ref().unwrap_or(&value.label).clone(),
                                field_value: value.value.clone().into(),
                                category: option_category.clone(),
                            })
                        })
                    }
                    CallBlockParam {
                        kind: CallBlockParamKind::Field {
                            default: value.map(Sb3FieldValue::Normal),
                            category: get_menu_category_id(
                                menu_category_ids,
                                option_category.as_ref(),
                            ),
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
                            option_category,
                        }) => {
                            if let Some(associated_items) = associated_items {
                                options.iter().flatten().for_each(|value| {
                                    associated_items.push(AssociatedLibraryItem {
                                        name: value
                                            .alt_name
                                            .as_ref()
                                            .unwrap_or(&value.label)
                                            .clone(),
                                        field_value: value.value.clone().into(),

                                        category: option_category.clone(),
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
                                    category: get_menu_category_id(
                                        menu_category_ids,
                                        option_category.as_ref(),
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
                                        convert_block_arg_into_call_block_param(
                                            value,
                                            None,
                                            menu_category_ids,
                                        ),
                                        known_params
                                            .into_iter()
                                            .map(|KnownParam { param, value }| {
                                                let category = match &param {
                                                    BlockArg::Field {
                                                        name: _,
                                                        field_type: _,
                                                        value: _,
                                                        options: _,
                                                        option_category,
                                                    } => get_menu_category_id(
                                                        menu_category_ids,
                                                        option_category.as_ref(),
                                                    ),
                                                    BlockArg::Input {
                                                        name: _,
                                                        menu_field_name: _,
                                                        input_type: _,
                                                        check: _,
                                                        shadow: _,
                                                    } => NO_CATEGORY_ID,
                                                };
                                                (
                                                    convert_block_arg_into_call_block_param(
                                                        param,
                                                        None,
                                                        menu_category_ids,
                                                    ),
                                                    KnownBlock::FieldValue {
                                                        value,
                                                        categories: HashSet::from([category]),
                                                    },
                                                )
                                            })
                                            .collect(),
                                    )
                                },
                            ),
                            bind_info: None,
                        },
                    ))),
                },
                associated_items: Vec::new(),
            };
        }
        let mut associated_items = Vec::new();
        let known_block_args = args
            .into_iter()
            .map(|arg| {
                convert_block_arg_into_call_block_param(
                    arg,
                    Some(&mut associated_items),
                    menu_category_ids,
                )
            })
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

pub fn merge_associated_item(item_a: &mut LibraryItem, item_b: LibraryItem) {
    let LibraryItem {
        namespace: _,
        value: value_a,
    } = item_a;
    let LibraryItem {
        namespace: _,
        value: value_b,
    } = item_b;
    let categories_b = if let Some(LibraryItemValue::KnownBlock(known_block)) = value_b
        && let KnownBlock::FieldValue {
            value: _,
            categories,
        } = *known_block
    {
        categories
    } else {
        return;
    };
    if let Some(LibraryItemValue::KnownBlock(known_block)) = value_a
        && let KnownBlock::FieldValue {
            value: _,
            categories,
        } = known_block.as_mut()
    {
        categories.extend(categories_b);
    }
}

pub fn process_toolbox_category(
    value: ToolboxCategory,
    category_entries: &mut HashMap<u32, HashSet<String>>,
    menu_category_ids: &mut HashMap<IString, u32>,
) -> (String, LibraryItem, Vec<(String, LibraryItem)>) {
    let ToolboxCategory {
        category: _,
        id,
        blocks,
        alt_name,
    } = value;
    let mut associated_items = HashMap::<String, (Sb3Primitive, HashSet<u32>)>::new();
    let mut namespace = HashMap::<String, LibraryItem>::with_capacity(blocks.len());
    for block in blocks {
        let ProcessedBlockEntry {
            name,
            opcode: _,
            library_item: item,
            associated_items: new_associated_items,
        } = block.process(menu_category_ids);
        for AssociatedLibraryItem {
            name: associated_item_name,
            field_value: associated_item_field_value,
            category,
        } in new_associated_items
        {
            let category_id = get_menu_category_id(menu_category_ids, category.as_ref());
            category_entries
                .entry(category_id)
                .or_default()
                .insert(associated_item_field_value.to_string());
            if let Some(current) = associated_items.get_mut(&associated_item_name) {
                assert_eq!(&current.0, &associated_item_field_value);
                current.1.insert(category_id);
            } else {
                associated_items.insert(
                    associated_item_name,
                    (associated_item_field_value, HashSet::from([category_id])),
                );
            }
        }
        namespace.insert(name, item);
    }
    let mut processed_associated_items =
        Vec::<(String, LibraryItem)>::with_capacity(associated_items.len());
    for (name, (field_value, categories)) in associated_items {
        // if let Some(current) = namespace.get_mut(&name) {
        //     if let Some(LibraryItemValue::KnownBlock(known_block)) = &mut current.value
        //         && let KnownBlock::SingletonReporter {
        //             opcode: _,
        //             params: _,
        //             field,
        //             assign: _,
        //             bind_info: _,
        //         } = known_block.as_mut()
        //     {
        //         field.replace(field_value.clone());
        //     } else {
        //     }
        // } else {
        //     // namespace.insert(
        //     //     name.clone(),
        //     //     LibraryItem {
        //     //         namespace: HashMap::new(),
        //     //         value: Some(LibraryItemValue::Alias(vec![
        //     //             AliasSegment::Super,
        //     //             AliasSegment::Child("menus".to_string()),
        //     //             AliasSegment::Child(name.clone()),
        //     //         ])),
        //     //     },
        //     // );
        // }
        processed_associated_items.push((
            name,
            LibraryItem {
                namespace: HashMap::new(),
                value: Some(LibraryItemValue::KnownBlock(Box::new(
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal(field_value),
                        categories,
                    },
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
