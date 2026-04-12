pub mod project_json;

use std::collections::HashMap;

use arcstr::{ArcStr as IString, literal};
use quote::ToTokens;
use serde::{Deserialize, Serialize};

use crate::project_json::{Sb3FieldValue, Sb3PrimitiveBlock};

/// Resolved symbols or primitive expressions like strings for usage in inputs or fields
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum KnownBlock {
    Variable {
        canonical_name: String,
        id: IString,
        assign: SimpleCallableKnownBlockSignature,
    },
    List {
        canonical_name: String,
        id: IString,
    },
    /// Cannot be used for variables in the context of an input
    FieldValue {
        value: project_json::Sb3FieldValue,
    },
    BlockRef {
        id: IString,
    },
    /// Intended for primitive expressions like strings but can be used for variables etc aswell
    PrimitiveBlock {
        value: project_json::Sb3PrimitiveBlock,
    },
    Callable(IString, Vec<CallBlockParam>),
    PartialCallable(
        IString,
        Vec<(CallBlockParam, KnownBlock)>,
        Vec<CallBlockParam>,
    ),
    /// Reporter block without any inputs or fields e.g. direction
    SingletonReporter {
        opcode: IString,
        params: Vec<(CallBlockParam, KnownBlock)>,
        field: Option<project_json::Sb3FieldValue>,
        assign: Option<SimpleCallableKnownBlockSignature>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum KnownBlockInput<'a> {
    PrimitiveInput(project_json::Sb3PrimitiveBlock),
    BlockRef(IString),
    SimpleBlock(&'a IString, &'a Vec<(CallBlockParam, KnownBlock)>),
    Menu(project_json::Sb3FieldValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallableKnownBlockSignature<'a>(
    pub &'a IString,
    pub &'a Vec<CallBlockParam>,
    pub &'a Vec<(CallBlockParam, KnownBlock)>,
);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SimpleCallableKnownBlockSignature(
    pub IString,
    pub CallBlockParam,
    pub Vec<(CallBlockParam, KnownBlock)>,
);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CallBlockParamKind {
    Input {
        default: Option<project_json::Sb3PrimitiveBlock>,
    },
    Field {
        default: Option<project_json::Sb3FieldValue>,
    },
    MenuInput {
        opcode: IString,
        field_name: IString,
        default: project_json::Sb3FieldValue,
    },
    BlockStack,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CallBlockParam {
    pub kind: CallBlockParamKind,
    pub name: IString,
}

impl KnownBlock {
    pub fn new_variable(canonical_name: String, id: IString) -> Self {
        let assign = SimpleCallableKnownBlockSignature(
            literal!("data_setvariableto"),
            CallBlockParam {
                kind: CallBlockParamKind::Input {
                    default: Some("0".into()),
                },
                name: literal!("VALUE"),
            },
            vec![(
                CallBlockParam {
                    kind: CallBlockParamKind::Field { default: None },
                    name: literal!("VARIABLE"),
                },
                KnownBlock::FieldValue {
                    value: project_json::Sb3FieldValue::WithId {
                        value: (&canonical_name as &str).into(),
                        id: id.to_string(),
                    },
                },
            )],
        );
        Self::Variable {
            canonical_name,
            id,
            assign,
        }
    }
}

impl From<IString> for KnownBlock {
    #[inline]
    fn from(value: IString) -> Self {
        Self::BlockRef { id: value }
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AliasSegment {
    Super,
    Child(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LibraryItemValue {
    KnownBlock(Box<KnownBlock>),
    Alias(Vec<AliasSegment>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LibraryItem {
    pub namespace: HashMap<String, LibraryItem>,
    pub value: Option<LibraryItemValue>,
}
