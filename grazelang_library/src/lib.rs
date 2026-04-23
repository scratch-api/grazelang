pub mod project_json;

use proc_macro2::TokenStream;
use std::collections::HashMap;

use arcstr::{ArcStr as IString, literal};
use quote::{ToTokens, TokenStreamExt, quote};
use serde::{Deserialize, Serialize};

use crate::project_json::{Sb3FieldValue, Sb3PrimitiveBlock};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BindInfo {
    pub parent_target: IString,
    pub property_of_params: Vec<(CallBlockParam, KnownBlock)>,
}

impl ToTokens for BindInfo {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let BindInfo {
            parent_target,
            property_of_params,
        } = self;
        let parent_target = parent_target.as_str();
        let (property_of_params_keys, property_of_params_values): (Vec<_>, Vec<_>) =
            property_of_params
                .iter()
                .map(|(key, value)| (key, value))
                .unzip();
        tokens.append_all(quote! {
            ::grazelang_library::BindInfo {
                parent_target: ::arcstr::literal!(#parent_target),
                property_of_params: vec![#( (#property_of_params_keys, #property_of_params_values ) ),*]
            }
        });
    }
}

/// Resolved symbols or primitive expressions like strings for usage in inputs or fields
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum KnownBlock {
    Variable {
        canonical_name: String,
        id: IString,
        assign: SimpleCallableKnownBlockSignature,
        bind_info: Option<BindInfo>,
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
        bind_info: Option<BindInfo>,
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

impl ToTokens for SimpleCallableKnownBlockSignature {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let SimpleCallableKnownBlockSignature(opcode, param, known_params) = self;
        let opcode = opcode.as_str();
        let (keys, values): (Vec<_>, Vec<_>) = known_params.iter().map(|(a, b)| (a, b)).unzip();

        tokens.append_all(quote! {
            ::grazelang_library::SimpleCallableKnownBlockSignature(
                ::arcstr::literal!(#opcode),
                #param,
                ::std::vec![#( (#keys, #values) ),*]
            )
        });
    }
}

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

impl ToTokens for CallBlockParamKind {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let prefix = quote!(::grazelang_library::CallBlockParamKind);
        match self {
            CallBlockParamKind::Input { default } => {
                let default = quote_option(default.as_ref());
                tokens.append_all(quote!(#prefix::Input { default: #default }));
            }
            CallBlockParamKind::Field { default } => {
                let default = quote_option(default.as_ref());
                tokens.append_all(quote!(#prefix::Field { default: #default }));
            }
            CallBlockParamKind::MenuInput {
                opcode,
                field_name,
                default,
            } => {
                let opcode = opcode.as_str();
                let field_name = field_name.as_str();
                tokens.append_all(quote! {
                    #prefix::MenuInput {
                        opcode: ::arcstr::literal!(#opcode),
                        field_name: ::arcstr::literal!(#field_name),
                        default: #default,
                    }
                });
            }
            CallBlockParamKind::BlockStack => tokens.append_all(quote!(#prefix::BlockStack)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CallBlockParam {
    pub kind: CallBlockParamKind,
    pub name: IString,
}

impl ToTokens for CallBlockParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let CallBlockParam { kind, name } = self;
        let name = name.as_str();

        tokens.append_all(quote! {
            ::grazelang_library::CallBlockParam {
                kind: #kind,
                name: ::arcstr::literal!(#name)
            }
        });
    }
}

impl KnownBlock {
    pub fn new_variable(canonical_name: String, id: IString, bind_info: Option<BindInfo>) -> Self {
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
            bind_info,
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

impl ToTokens for KnownBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let prefix = quote!(::grazelang_library::KnownBlock);
        match self {
            KnownBlock::Variable {
                canonical_name,
                id,
                assign,
                bind_info,
            } => {
                let id = id.as_str();
                let bind_info = quote_option(bind_info.as_ref());
                tokens.append_all(quote! {
                    #prefix::Variable {
                        canonical_name: #canonical_name.to_string(),
                        id: ::arcstr::literal!(#id),
                        assign: #assign,
                        bind_info: #bind_info,
                    }
                });
            }
            KnownBlock::List {
                canonical_name,
                id,
            } => {
                let id = id.as_str();
                tokens.append_all(quote! {
                    #prefix::List {
                        canonical_name: #canonical_name.to_string(),
                        id: ::arcstr::literal!(#id),
                    }
                });
            }
            KnownBlock::FieldValue { value } => {
                tokens.append_all(quote!(#prefix::FieldValue { value: #value }))
            }
            KnownBlock::BlockRef { id } => {
                let id = id.as_str();
                tokens.append_all(quote!(#prefix::BlockRef { id: ::arcstr::literal!(#id) }))
            }
            KnownBlock::PrimitiveBlock { value } => {
                tokens.append_all(quote!(#prefix::PrimitiveBlock { value: #value }))
            }
            KnownBlock::Callable(opcode, params) => {
                let opcode = opcode.as_str();
                tokens.append_all(quote!(#prefix::Callable(::arcstr::literal!(#opcode), ::std::vec![#( #params ),*])));
            }
            KnownBlock::PartialCallable(opcode, known, params) => {
                let opcode = opcode.as_str();
                let (keys, values): (Vec<_>, Vec<_>) = known.iter().map(|(k, v)| (k, v)).unzip();
                tokens.append_all(quote! {
                    #prefix::PartialCallable(
                        ::arcstr::literal!(#opcode),
                        ::std::vec![#( (#keys, #values) ),*],
                        ::std::vec![#( #params ),*]
                    )
                });
            }
            KnownBlock::SingletonReporter {
                opcode,
                params,
                field,
                assign,
                bind_info,
            } => {
                let opcode = opcode.as_str();
                let (keys, values): (Vec<_>, Vec<_>) = params.iter().map(|(k, v)| (k, v)).unzip();
                let field = quote_option(field.as_ref());
                let assign = quote_option(assign.as_ref());
                let bind_info = quote_option(bind_info.as_ref());
                tokens.append_all(quote! {
                    #prefix::SingletonReporter {
                        opcode: ::arcstr::literal!(#opcode),
                        params: ::std::vec![#( (#keys, #values) ),*],
                        field: #field,
                        assign: #assign,
                        bind_info: #bind_info,
                    }
                });
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AliasSegment {
    Super,
    Child(String),
}

impl ToTokens for AliasSegment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            AliasSegment::Super => {
                tokens.append_all(quote!(::grazelang_library::AliasSegment::Super))
            }
            AliasSegment::Child(s) => {
                tokens.append_all(quote!(::grazelang_library::AliasSegment::Child(#s.to_string())))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LibraryItemValue {
    KnownBlock(Box<KnownBlock>),
    Alias(Vec<AliasSegment>),
}

impl ToTokens for LibraryItemValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            LibraryItemValue::KnownBlock(known_block) => {
                let known_block = known_block.as_ref();
                tokens.append_all(quote! {
                    ::grazelang_library::LibraryItemValue::KnownBlock(::std::boxed::Box::new(#known_block))
                })
            }
            LibraryItemValue::Alias(alias_segments) => {
                let alias_segments = alias_segments.iter();
                tokens.append_all(quote! {
                    ::grazelang_library::LibraryItemValue::Alias(vec![#( #alias_segments ),*])
                })
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LibraryItem {
    pub namespace: HashMap<String, LibraryItem>,
    pub value: Option<LibraryItemValue>,
}

pub fn quote_option<T>(value: Option<&T>) -> TokenStream
where
    T: ToTokens,
{
    match value {
        Some(v) => quote!(::std::option::Option::Some(#v)),
        None => quote!(::std::option::Option::None),
    }
}

impl ToTokens for LibraryItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let LibraryItem { namespace, value } = self;
        let keys = namespace.keys();
        let values = namespace.values();
        let value = quote_option(value.as_ref());

        tokens.append_all(quote! {
            ::grazelang_library::LibraryItem {
                namespace: ::std::collections::HashMap::from([#( (#keys.to_string(), #values) ),*]),
                value: #value,
            }
        });
    }
}
