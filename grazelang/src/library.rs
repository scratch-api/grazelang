use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::LazyLock,
};

use arcstr::{ArcStr as IString, literal};
use grazelang_types::{
    AliasSegment, BACKDROPS_CATEGORY_ID, BindInfo, COSTUMES_CATEGORY_ID, CallBlockParam,
    CallBlockParamKind, ConstantExprLibraryItem, KnownBlock, LibraryItem, LibraryItemValue,
    NO_CATEGORY_ID, SimpleCallableKnownBlockSignature,
    project_json::{Sb3FieldValue, Sb3PrimitiveBlock},
};
use grazelang_library_parser::{generate_constant_expr_library, generate_library};

use crate::{
    codegen::core::GrazeSb3GeneratorContext,
    parser::context::{Symbol, SymbolId, SymbolTable},
};

pub fn get_generated_library() -> (HashMap<String, LibraryItem>, HashMap<u32, HashSet<IString>>) {
    generate_library!("schemas/toolbox_schema.json")
}

pub static CONSTANT_EXPR_LIBRARY: LazyLock<ConstantExprLibraryItem> = LazyLock::new(|| {
    let mut library = generate_constant_expr_library!("schemas/toolbox_schema.json");
    let mut flattened = HashMap::<String, Option<ConstantExprLibraryItem>>::new();
    for namespace in library.namespace.values() {
        for (key, value) in &namespace.namespace {
            if let Some(mut_value) = flattened.get_mut(key) {
                mut_value.take();
            } else {
                flattened.insert(key.clone(), Some(value.clone()));
            }
        }
    }
    for (key, value) in flattened {
        if let Some(value) = value {
            library.namespace.insert(key, value);
        }
    }
    library
});

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConstExpLookupError {
    NotFound,
    UsedSuper,
}

pub fn const_expr_lookup<'a, I>(
    mut path: I,
) -> Result<&'static ConstantExprLibraryItem, ConstExpLookupError>
where
    I: Iterator<Item = &'a str>,
{
    path.try_fold::<&ConstantExprLibraryItem, _, _>(&CONSTANT_EXPR_LIBRARY, |current, value| {
        if value == "super" {
            return Err(ConstExpLookupError::UsedSuper);
        }
        current
            .namespace
            .get(value)
            .ok_or(ConstExpLookupError::NotFound)
    })
}

pub fn convert_generated_library(
    library: HashMap<String, LibraryItem>,
    symbol_table: &mut SymbolTable,
    root_symbol: SymbolId,
) {
    #[derive(Debug, Clone, PartialEq)]
    pub enum ConvertedSymbol {
        Symbol(SymbolId),
        Alias(Vec<AliasSegment>),
    }
    pub fn recursively_convert(
        namespace: LibraryItem,
        symbol_table: &mut SymbolTable,
        aliases: &mut Vec<(SymbolId, IString, Vec<AliasSegment>)>,
    ) -> ConvertedSymbol {
        let my_symbol = match namespace.value {
            Some(LibraryItemValue::Alias(alias)) => ConvertedSymbol::Alias(alias),
            Some(LibraryItemValue::KnownBlock(known_block)) => {
                ConvertedSymbol::Symbol(symbol_table.new_symbol(Symbol {
                    known_block: Some(Rc::new(*known_block)),
                    namespace: HashMap::new(),
                    parent: Default::default(),
                }))
            }
            None => ConvertedSymbol::Symbol(symbol_table.new_symbol(Symbol {
                known_block: None,
                namespace: HashMap::new(),
                parent: Default::default(),
            })),
        };
        if let ConvertedSymbol::Symbol(my_symbol) = &my_symbol {
            for (child_name, child) in namespace.namespace {
                let child = recursively_convert(child, symbol_table, aliases);
                match child {
                    ConvertedSymbol::Symbol(child) => {
                        symbol_table.insert_child(*my_symbol, child_name.into(), child);
                    }
                    ConvertedSymbol::Alias(alias_segments) => {
                        aliases.push((*my_symbol, child_name.into(), alias_segments));
                    }
                }
            }
        }
        my_symbol
    }
    let mut aliases = Vec::new();
    library.into_iter().for_each(|(name, namespace)| {
        if let ConvertedSymbol::Symbol(symbol) =
            recursively_convert(namespace, symbol_table, &mut aliases)
        {
            symbol_table.insert_child(root_symbol, name.as_str().into(), symbol);
        }
    });
    for (parent_symbol, alias_name, segments) in aliases {
        let mut current = parent_symbol;
        for segment in segments {
            current = match segment {
                AliasSegment::Super => symbol_table[current].parent,
                AliasSegment::Child(child) => {
                    symbol_table.get_child(current, child.as_str()).unwrap()
                }
            }
        }
        symbol_table.insert_alias(parent_symbol, alias_name, current);
    }
}

/// Output is not guaranteed to be correct
pub fn get_standard_library_namespace_count() -> usize {
    11
}

pub fn add_standard_library_namespaces(
    context: &mut GrazeSb3GeneratorContext,
    root_symbol: SymbolId,
) {
    let (raw_library, category_entries) = get_generated_library();
    convert_generated_library(raw_library, &mut context.symbol_table, root_symbol);
    for (key, values) in category_entries {
        match context.field_category_entries.entry(key) {
            std::collections::hash_map::Entry::Vacant(v) => {
                v.insert(values.clone());
            }
            std::collections::hash_map::Entry::Occupied(mut o) => {
                o.get_mut().extend(values.iter().cloned());
            }
        }
        for value in values {
            match context.field_entry_categories.entry(value) {
                std::collections::hash_map::Entry::Vacant(v) => {
                    v.insert(HashSet::from([key]));
                }
                std::collections::hash_map::Entry::Occupied(mut o) => {
                    o.get_mut().insert(key);
                }
            }
        }
    }
}

/// Creates symbols like `sprites.<sprite_name>.x` that are to be accessed as `sensing_of` blocks for a sprite
pub fn create_sprite_dependent_symbols(target_name: &IString) -> Vec<(IString, Symbol)> {
    const OBJECT_ISTRING: &IString = &literal!("OBJECT");
    const PROPERTY_ISTRING: &IString = &literal!("PROPERTY");
    #[inline]
    fn symbol_of(name: IString, known_block: KnownBlock) -> (IString, Symbol) {
        (
            name,
            Symbol {
                known_block: Some(Rc::new(known_block)),
                namespace: HashMap::new(),
                parent: Default::default(),
            },
        )
    }
    vec![
        symbol_of(
            literal!("x_position"),
            KnownBlock::SingletonReporter {
                opcode: literal!("motion_xposition"),
                params: Vec::new(),
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("motion_setx"),
                    CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some(Sb3PrimitiveBlock::Number("0".into())),
                        },
                        name: literal!("X"),
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("x position".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
        symbol_of(
            literal!("y_position"),
            KnownBlock::SingletonReporter {
                opcode: literal!("motion_yposition"),
                params: Vec::new(),
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("motion_sety"),
                    CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some(Sb3PrimitiveBlock::Number("0".into())),
                        },
                        name: literal!("Y"),
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("y position".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
        symbol_of(
            literal!("direction"),
            KnownBlock::SingletonReporter {
                opcode: literal!("motion_direction"),
                params: Vec::new(),
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("motion_pointindirection"),
                    CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some(Sb3PrimitiveBlock::Angle("90".into())),
                        },
                        name: literal!("DIRECTION"),
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("direction".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
        symbol_of(
            literal!("costume_number"),
            KnownBlock::SingletonReporter {
                opcode: literal!("looks_costumenumbername"),
                params: vec![(
                    CallBlockParam {
                        kind: CallBlockParamKind::Field {
                            default: None,
                            category: NO_CATEGORY_ID,
                        },
                        name: literal!("NUMBER_NAME"),
                    },
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal("number".into()),
                        categories: HashSet::from([NO_CATEGORY_ID]),
                    },
                )],
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("looks_switchcostumeto"),
                    {
                        let name = literal!("COSTUME");
                        CallBlockParam {
                            kind: CallBlockParamKind::MenuInput {
                                opcode: literal!("looks_costume"),
                                field_name: name.clone(),
                                default: Sb3FieldValue::Normal("".into()),
                                category: COSTUMES_CATEGORY_ID,
                            },
                            name,
                        }
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("costume #".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
        symbol_of(
            literal!("costume_name"),
            KnownBlock::SingletonReporter {
                opcode: literal!("looks_costumenumbername"),
                params: vec![(
                    CallBlockParam {
                        kind: CallBlockParamKind::Field {
                            default: None,
                            category: NO_CATEGORY_ID,
                        },
                        name: literal!("NUMBER_NAME"),
                    },
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal("name".into()),
                        categories: HashSet::from([NO_CATEGORY_ID]),
                    },
                )],
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("looks_switchcostumeto"),
                    {
                        let name = literal!("COSTUME");
                        CallBlockParam {
                            kind: CallBlockParamKind::MenuInput {
                                opcode: literal!("looks_costume"),
                                field_name: name.clone(),
                                default: Sb3FieldValue::Normal("".into()),
                                category: COSTUMES_CATEGORY_ID,
                            },
                            name,
                        }
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("costume name".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
        symbol_of(
            literal!("size"),
            KnownBlock::SingletonReporter {
                opcode: literal!("looks_size"),
                params: Vec::new(),
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("looks_setsizeto"),
                    CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some(Sb3PrimitiveBlock::Number("100".into())),
                        },
                        name: literal!("SIZE"),
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("size".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
        symbol_of(
            literal!("volume"),
            KnownBlock::SingletonReporter {
                opcode: literal!("sound_volume"),
                params: Vec::new(),
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("sound_setvolumeto"),
                    CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some(Sb3PrimitiveBlock::Number("100".into())),
                        },
                        name: literal!("VOLUME"),
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("volume".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
    ]
}

/// Creates symbols like `sprites.<sprite_name>.x` that are to be accessed as `sensing_of` blocks for the stage
pub fn create_stage_dependent_symbols(target_name: &IString) -> Vec<(IString, Symbol)> {
    const OBJECT_ISTRING: &IString = &literal!("OBJECT");
    const PROPERTY_ISTRING: &IString = &literal!("PROPERTY");
    #[inline]
    fn symbol_of(name: IString, known_block: KnownBlock) -> (IString, Symbol) {
        (
            name,
            Symbol {
                known_block: Some(Rc::new(known_block)),
                namespace: HashMap::new(),
                parent: Default::default(),
            },
        )
    }
    vec![
        symbol_of(
            literal!("backdrop_number"),
            KnownBlock::SingletonReporter {
                opcode: literal!("looks_backdropnumbername"),
                params: vec![(
                    CallBlockParam {
                        kind: CallBlockParamKind::Field {
                            default: None,
                            category: NO_CATEGORY_ID,
                        },
                        name: literal!("NUMBER_NAME"),
                    },
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal("number".into()),
                        categories: HashSet::from([NO_CATEGORY_ID]),
                    },
                )],
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("looks_switchbackdropto"),
                    {
                        let name = literal!("BACKDROP");
                        CallBlockParam {
                            kind: CallBlockParamKind::MenuInput {
                                opcode: literal!("looks_backdrops"),
                                field_name: name.clone(),
                                default: Sb3FieldValue::Normal("".into()),
                                category: BACKDROPS_CATEGORY_ID,
                            },
                            name,
                        }
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("backdrop #".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
        symbol_of(
            literal!("backdrop_name"),
            KnownBlock::SingletonReporter {
                opcode: literal!("looks_backdropnumbername"),
                params: vec![(
                    CallBlockParam {
                        kind: CallBlockParamKind::Field {
                            default: None,
                            category: NO_CATEGORY_ID,
                        },
                        name: literal!("NUMBER_NAME"),
                    },
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal("name".into()),
                        categories: HashSet::from([NO_CATEGORY_ID]),
                    },
                )],
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("looks_switchbackdropto"),
                    {
                        let name = literal!("BACKDROP");
                        CallBlockParam {
                            kind: CallBlockParamKind::MenuInput {
                                opcode: literal!("looks_backdrops"),
                                field_name: name.clone(),
                                default: Sb3FieldValue::Normal("".into()),
                                category: BACKDROPS_CATEGORY_ID,
                            },
                            name,
                        }
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("backdrop name".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
        symbol_of(
            literal!("volume"),
            KnownBlock::SingletonReporter {
                opcode: literal!("sound_volume"),
                params: Vec::new(),
                field: None,
                assign: Some(SimpleCallableKnownBlockSignature(
                    literal!("sound_setvolumeto"),
                    CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some(Sb3PrimitiveBlock::Number("100".into())),
                        },
                        name: literal!("VOLUME"),
                    },
                    Vec::new(),
                )),
                bind_info: Some(BindInfo {
                    parent_target: target_name.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("volume".into()),
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
    ]
}
