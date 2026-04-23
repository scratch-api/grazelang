use std::{
    cell::RefCell,
    collections::HashMap,
    ops::DerefMut,
    rc::{Rc, Weak},
};

use arcstr::{ArcStr as IString, literal};
use grazelang_library::{
    AliasSegment, BindInfo, CallBlockParam, CallBlockParamKind, KnownBlock, LibraryItem,
    LibraryItemValue, SimpleCallableKnownBlockSignature,
    project_json::{Sb3FieldValue, Sb3PrimitiveBlock},
};
use grazelang_library_parser::generate_library;

use crate::parser::parse_context::Symbol;

pub fn get_generated_library() -> HashMap<String, LibraryItem> {
    generate_library!("schemas/toolbox_schema.json")
}

pub fn convert_generated_library(
    library: HashMap<String, LibraryItem>,
) -> impl Iterator<Item = (IString, Rc<RefCell<Symbol>>)> {
    pub fn recursively_convert(
        namespace: LibraryItem,
        aliases: &mut Vec<(Rc<RefCell<Symbol>>, Vec<AliasSegment>)>,
    ) -> Rc<RefCell<Symbol>> {
        let (my_symbol, alias_content) = match namespace.value {
            Some(LibraryItemValue::Alias(alias)) => (
                Rc::new(RefCell::new(Symbol::Alias(Weak::new(), Weak::new()))),
                Some(alias),
            ),
            Some(LibraryItemValue::KnownBlock(known_block)) => (
                Rc::new(RefCell::new(Symbol::KnownBlock(
                    known_block,
                    HashMap::new(),
                    Weak::new(),
                ))),
                None,
            ),
            None => (Rc::new(RefCell::new(Symbol::new_namespace())), None),
        };
        if let Some(alias_content) = alias_content {
            aliases.push((my_symbol.clone(), alias_content));
        }
        for (child_name, child) in namespace.namespace {
            let child = recursively_convert(child, aliases);
            Symbol::insert_child(&my_symbol, child_name.into(), child);
        }
        my_symbol
    }
    let root = Rc::new(RefCell::new(Symbol::new_namespace()));
    let mut aliases = Vec::new();
    library.into_iter().for_each(|(name, namespace)| {
        Symbol::insert_child(
            &root,
            name.as_str().into(),
            recursively_convert(namespace, &mut aliases),
        );
    });
    for (alias_symbol, segments) in aliases {
        let mut current = alias_symbol.borrow().get_parent().upgrade().unwrap();
        for segment in segments {
            current = match segment {
                AliasSegment::Super => current.borrow().get_parent().upgrade().unwrap(),
                AliasSegment::Child(child) => current.borrow().get_child(&child.into()).unwrap(),
            }
        }
        if let Symbol::Alias(target, _) = alias_symbol.borrow_mut().deref_mut() {
            *target = Rc::downgrade(&current);
        }
    }
    let root = Rc::try_unwrap(root).unwrap().into_inner();
    match root {
        Symbol::Namespace(namespace, _) => namespace.into_iter(),
        _ => unreachable!(),
    }
}

/// Output is not guaranteed to be correct
pub fn get_standard_library_namespace_count() -> usize {
    10
}

pub fn get_standard_library_namespaces() -> impl Iterator<Item = (IString, Rc<RefCell<Symbol>>)> {
    convert_generated_library(get_generated_library())
}

/// Creates symbols like `sprites.<sprite_name>.x` that are to be accessed as `sensing_of` blocks for a sprite
pub fn create_sprite_dependent_symbols(
    target_name: &IString,
) -> Vec<(IString, Rc<RefCell<Symbol>>)> {
    const OBJECT_ISTRING: &IString = &literal!("OBJECT");
    const PROPERTY_ISTRING: &IString= &literal!("PROPERTY");
    #[inline]
    fn symbol_of(name: IString, known_block: KnownBlock) -> (IString, Rc<RefCell<Symbol>>) {
        (
            name,
            Rc::new(RefCell::new(Symbol::KnownBlock(
                Box::new(known_block),
                HashMap::new(),
                Weak::new(),
            ))),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("x position".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("y position".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("direction".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
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
                        kind: CallBlockParamKind::Field { default: None },
                        name: literal!("NUMBER_NAME"),
                    },
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal("number".into()),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("costume #".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
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
                        kind: CallBlockParamKind::Field { default: None },
                        name: literal!("NUMBER_NAME"),
                    },
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal("name".into()),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("costume name".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("size".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("volume".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
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
pub fn create_stage_dependent_symbols(
    target_name: &IString,
) -> Vec<(IString, Rc<RefCell<Symbol>>)> {
    const OBJECT_ISTRING: &IString = &literal!("OBJECT");
    const PROPERTY_ISTRING: &IString= &literal!("PROPERTY");
    #[inline]
    fn symbol_of(name: IString, known_block: KnownBlock) -> (IString, Rc<RefCell<Symbol>>) {
        (
            name,
            Rc::new(RefCell::new(Symbol::KnownBlock(
                Box::new(known_block),
                HashMap::new(),
                Weak::new(),
            ))),
        )
    }
    vec![
        symbol_of(
            literal!("backdrop_number"),
            KnownBlock::SingletonReporter {
                opcode: literal!("looks_backdropnumbername"),
                params: vec![(
                    CallBlockParam {
                        kind: CallBlockParamKind::Field { default: None },
                        name: literal!("NUMBER_NAME"),
                    },
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal("number".into()),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("backdrop #".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
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
                        kind: CallBlockParamKind::Field { default: None },
                        name: literal!("NUMBER_NAME"),
                    },
                    KnownBlock::FieldValue {
                        value: Sb3FieldValue::Normal("name".into()),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("backdrop name".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
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
                                kind: CallBlockParamKind::Field { default: None },
                                name: PROPERTY_ISTRING.clone(),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::Normal("volume".into()),
                            },
                        ),
                        {
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: OBJECT_ISTRING.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                    },
                                    name: OBJECT_ISTRING.clone(),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(target_name.as_str().into()),
                                },
                            )
                        },
                    ],
                }),
            },
        ),
    ]
}
