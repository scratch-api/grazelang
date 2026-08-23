use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::LazyLock,
};

use arcstr::{ArcStr as IString, literal};
use grazelang_library_parser::{
    generate_constant_expr_library, generate_dynamic_generate_library, generate_library,
};
use grazelang_types::{
    AliasSegment, BACKDROPS_CATEGORY_ID, BindInfo, COSTUMES_CATEGORY_ID, CallBlockParam,
    CallBlockParamKind, ConstantExprLibraryItem, KnownBlock, LibraryItem, LibraryItemValue,
    NO_CATEGORY_ID, SimpleCallableKnownBlockSignature,
    project_json::{Sb3FieldValue, Sb3PrimitiveBlock},
};
use serde::{Deserialize, Serialize};

use crate::{
    codegen::core::GrazeSb3GeneratorContext,
    parser::context::{Symbol, SymbolId, SymbolTable},
};

pub fn get_generated_library() -> (HashMap<String, LibraryItem>, HashMap<u32, HashSet<IString>>) {
    let (a, b, _): (_, _, [&'static str; _]) =
        generate_library!("src/library_markups/standard_toolbox.toml");
    (a, b)
}

pub type ExtensionData = (
    HashMap<String, LibraryItem>,
    HashMap<u32, HashSet<IString>>,
    Vec<&'static str>,
);

pub fn get_extension(extension: &str) -> Option<ExtensionData> {
    match extension {
        "pen" => {
            let (a, b, c): (_, _, [&'static str; _]) =
                generate_library!("src/library_markups/extensions/pen.toml");
            Some((a, b, Vec::from(c)))
        }
        "music" => {
            let (a, b, c): (_, _, [&'static str; _]) =
                generate_library!("src/library_markups/extensions/music.toml");
            Some((a, b, Vec::from(c)))
        }
        _ => None,
    }
}

pub static CONSTANT_EXPR_LIBRARY: LazyLock<ConstantExprLibraryItem> = LazyLock::new(|| {
    let mut library = generate_constant_expr_library!("src/library_markups/standard_toolbox.toml");
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

generate_dynamic_generate_library!(pub);

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
                    sprite_name: None,
                }))
            }
            None => ConvertedSymbol::Symbol(symbol_table.new_symbol(Symbol {
                known_block: None,
                namespace: HashMap::new(),
                parent: Default::default(),
                sprite_name: None,
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
    pub fn convert_category(
        namespace: LibraryItem,
        symbol_table: &mut SymbolTable,
        aliases: &mut Vec<(SymbolId, IString, Vec<AliasSegment>)>,
        current_symbol: Option<SymbolId>,
    ) -> Option<ConvertedSymbol> {
        if let Some(my_symbol) = current_symbol {
            for (child_name, child) in namespace.namespace {
                let child = recursively_convert(child, symbol_table, aliases);
                match child {
                    ConvertedSymbol::Symbol(child) => {
                        symbol_table.insert_child(my_symbol, child_name.into(), child);
                    }
                    ConvertedSymbol::Alias(alias_segments) => {
                        aliases.push((my_symbol, child_name.into(), alias_segments));
                    }
                }
            }
            return None;
        }
        Some(recursively_convert(namespace, symbol_table, aliases))
    }
    let mut aliases = Vec::new();
    library.into_iter().for_each(|(name, namespace)| {
        if let Some(ConvertedSymbol::Symbol(symbol)) = convert_category(
            namespace,
            symbol_table,
            &mut aliases,
            symbol_table.get_child(root_symbol, name.as_str()),
        ) {
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
    15
}

pub fn populate_field_category_entries(
    context: &mut GrazeSb3GeneratorContext,
    category_entries: HashMap<u32, HashSet<IString>>,
) {
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

pub fn add_builtin_extension(
    context: &mut GrazeSb3GeneratorContext,
    root_symbol: SymbolId,
    extension: &str,
) -> Option<()> {
    let (raw_library, category_entries, extensions) = get_extension(extension)?;
    convert_generated_library(raw_library, &mut context.symbol_table, root_symbol);
    populate_field_category_entries(context, category_entries);
    context
        .sb3
        .extensions
        .extend(extensions.iter().map(|value| value.to_string()));
    Some(())
}

pub fn add_external_extension(
    context: &mut GrazeSb3GeneratorContext,
    root_symbol: SymbolId,
    extension: &std::path::Path,
    use_cache: bool,
    create_cache: bool,
) -> Option<()> {
    let (raw_library, category_entries, extensions) =
        dynamic_generate_library(extension, use_cache, create_cache)?;
    let category_entries = category_entries
        .iter()
        .map(|value| (*value.0, value.1.iter().map(Into::into).collect()))
        .collect();
    convert_generated_library(raw_library, &mut context.symbol_table, root_symbol);
    populate_field_category_entries(context, category_entries);
    context
        .sb3
        .extensions
        .extend(extensions.iter().map(|value| value.to_string()));
    Some(())
}

pub fn add_standard_library_namespaces(
    context: &mut GrazeSb3GeneratorContext,
    root_symbol: SymbolId,
) {
    let (raw_library, category_entries) = get_generated_library();
    convert_generated_library(raw_library, &mut context.symbol_table, root_symbol);
    populate_field_category_entries(context, category_entries);
}

#[inline]
fn sensing_bind_info(target_name: &IString, property: &str) -> BindInfo {
    BindInfo {
        parent_target: target_name.clone(),
        property_of_params: vec![property_param(property), object_param(target_name)],
    }
}

fn property_param(property: &str) -> (CallBlockParam, KnownBlock) {
    (
        CallBlockParam {
            kind: CallBlockParamKind::Field {
                default: None,
                category: NO_CATEGORY_ID,
            },
            name: PROPERTY_ISTRING.clone(),
        },
        KnownBlock::FieldValue {
            value: Sb3FieldValue::Normal(property.into()),
            categories: HashSet::from([NO_CATEGORY_ID]),
        },
    )
}

fn object_param(target_name: &IString) -> (CallBlockParam, KnownBlock) {
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
}

pub const OBJECT_ISTRING: &IString = &literal!("OBJECT");
pub const PROPERTY_ISTRING: &IString = &literal!("PROPERTY");

/// Creates symbols like `sprites.<sprite_name>.x` that are to be accessed as `sensing_of` blocks for a sprite
pub fn create_sprite_dependent_symbols(target_name: &IString) -> Vec<(IString, Symbol)> {
    #[inline]
    fn symbol_of(
        name: IString,
        known_block: KnownBlock,
        target_name: &IString,
    ) -> (IString, Symbol) {
        (
            name,
            Symbol {
                known_block: Some(Rc::new(known_block)),
                namespace: HashMap::new(),
                parent: Default::default(),
                sprite_name: Some(target_name.clone()),
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
                bind_info: Some(sensing_bind_info(target_name, "x position")),
            },
            target_name,
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
                bind_info: Some(sensing_bind_info(target_name, "y position")),
            },
            target_name,
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
                bind_info: Some(sensing_bind_info(target_name, "direction")),
            },
            target_name,
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
                bind_info: Some(sensing_bind_info(target_name, "costume #")),
            },
            target_name,
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
                bind_info: Some(sensing_bind_info(target_name, "costume name")),
            },
            target_name,
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
                bind_info: Some(sensing_bind_info(target_name, "size")),
            },
            target_name,
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
                bind_info: Some(sensing_bind_info(target_name, "volume")),
            },
            target_name,
        ),
    ]
}

/// Creates symbols like `sprites.<sprite_name>.x` that are to be accessed as `sensing_of` blocks for the stage
pub fn create_stage_dependent_symbols(target_name: &IString) -> Vec<(IString, Symbol)> {
    #[inline]
    fn symbol_of(name: IString, known_block: KnownBlock) -> (IString, Symbol) {
        (
            name,
            Symbol {
                known_block: Some(Rc::new(known_block)),
                namespace: HashMap::new(),
                parent: Default::default(),
                sprite_name: None,
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
                bind_info: Some(sensing_bind_info(target_name, "backdrop #")),
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
                bind_info: Some(sensing_bind_info(target_name, "backdrop name")),
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
                bind_info: Some(sensing_bind_info(target_name, "volume")),
            },
        ),
    ]
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BlockShape {
    Hat,
    Stack,
    Reporter,
}

pub fn get_block_shape(opcode: &str) -> BlockShape {
    match opcode {
        "event_whenflagclicked"
        | "event_whenkeypressed"
        | "event_whenthisspriteclicked"
        | "event_whenbackdropswitchesto"
        | "event_whengreaterthan"
        | "event_whenbroadcastreceived"
        | "control_start_as_clone" => BlockShape::Hat,
        "motion_xposition"
        | "motion_yposition"
        | "motion_direction"
        | "looks_costumenumbername"
        | "looks_backdropnumbername"
        | "looks_size"
        | "sound_volume"
        | "sensing_touchingobject"
        | "sensing_touchingcolor"
        | "sensing_coloristouchingcolor"
        | "sensing_distanceto"
        | "sensing_answer"
        | "sensing_keypressed"
        | "sensing_mousedown"
        | "sensing_mousex"
        | "sensing_mousey"
        | "sensing_loudness"
        | "sensing_timer"
        | "sensing_of"
        | "sensing_current"
        | "sensing_dayssince2000"
        | "sensing_online"
        | "sensing_username"
        | "operator_add"
        | "operator_subtract"
        | "operator_multiply"
        | "operator_divide"
        | "operator_random"
        | "operator_gt"
        | "operator_lt"
        | "operator_equals"
        | "operator_and"
        | "operator_or"
        | "operator_not"
        | "operator_join"
        | "operator_letter_of"
        | "operator_length"
        | "operator_contains"
        | "operator_mod"
        | "operator_round"
        | "operator_mathop"
        | "data_variable"
        | "data_listcontents"
        | "music_getTempo" => BlockShape::Reporter,
        _ => BlockShape::Stack,
    }
}
