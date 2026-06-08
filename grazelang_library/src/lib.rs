pub mod project_json;

use proc_macro2::TokenStream;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc, sync::Arc,
};

use arcstr::{ArcStr as IString, literal};
use quote::{ToTokens, TokenStreamExt, quote};
use serde::{Deserialize, Serialize};

use crate::project_json::Sb3PrimitiveBlock;

pub const NO_CATEGORY_ID: u32 = 0;
pub const VARIABLES_CATEGORY_ID: u32 = 1;
pub const LISTS_CATEGORY_ID: u32 = 2;
pub const BROADCASTS_CATEGORY_ID: u32 = 3;
pub const COSTUMES_CATEGORY_ID: u32 = 4;
pub const BACKDROPS_CATEGORY_ID: u32 = 5;
pub const BACKDROP_TARGETS_CATEGORY_ID: u32 = 6;
pub const SOUNDS_CATEGORY_ID: u32 = 7;
pub const DESTINATIONS_CATEGORY_ID: u32 = 8; // sprites or mouse pointer or random position
pub const DIRECTIONS_CATEGORY_ID: u32 = 9; // sprites or mouse pointer or random direction
pub const CLONABLES_CATEGORY_ID: u32 = 10; // sprites or myself
pub const COLLIDERS_CATEGORY_ID: u32 = 11; // sprites or mouse pointer or edge
pub const LOCATIONS_CATEGORY_ID: u32 = 12; // sprites or mouse pointer
pub const PROPERTIES_CATEGORY_ID: u32 = 13;
pub const OBJECTS_CATEGORY_ID: u32 = 14; // sprites or the stage
pub const PEN_PROPERTIES_CATEGORY_ID: u32 = 15;

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
        categories: HashSet<u32>,
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
    CustomBlock {
        proccode: IString,
        call_params: Vec<CallBlockParam>,
        params: Vec<(IString, HasShadow)>,
        is_warp: bool,
    },
    BoundMethod {
        #[serde(with = "serde_arc_as_value")]
        signature: Arc<MethodSignature>,
        #[serde(with = "serde_rc_slice_as_value")]
        bound_params: Rc<[(CallBlockParam, KnownBlock)]>,
    },
    Empty,
}

mod serde_rc_slice_as_value {
    use std::rc::Rc;

    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn serialize<T, S>(value: &Rc<[T]>, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: Serialize,
        S: Serializer,
    {
        value.as_ref().serialize(serializer)
    }

    pub fn deserialize<'de, T, D>(deserializer: D) -> Result<Rc<[T]>, D::Error>
    where
        T: Deserialize<'de> + Clone,
        D: Deserializer<'de>,
    {
        Ok(Rc::from(Vec::<T>::deserialize(deserializer)?))
    }
}

mod serde_arc_as_value {
    use std::sync::Arc;

    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn serialize<T, S>(value: &Arc<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: Serialize,
        S: Serializer,
    {
        value.as_ref().serialize(serializer)
    }

    pub fn deserialize<'de, T, D>(deserializer: D) -> Result<Arc<T>, D::Error>
    where
        T: Deserialize<'de>,
        D: Deserializer<'de>,
    {
        Ok(Arc::new(T::deserialize(deserializer)?))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum KnownBlockInput<'a> {
    PrimitiveInput(project_json::Sb3PrimitiveBlock),
    BlockRef(IString),
    SimpleBlock(&'a IString, &'a [(CallBlockParam, KnownBlock)]),
    Menu(project_json::Sb3FieldValue, &'a HashSet<u32>),
    Empty,
}

type CallableKnownBlockSignatureMutation<'a> =
    Option<(&'a IString, &'a [(IString, HasShadow)], &'a bool)>;

#[derive(Debug, Clone, PartialEq)]
pub struct CallableKnownBlockSignature<'a>(
    pub &'a IString,
    pub &'a [CallBlockParam],
    pub &'a [(CallBlockParam, KnownBlock)],
    pub CallableKnownBlockSignatureMutation<'a>,
);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SimpleCallableKnownBlockSignature(
    pub IString,
    pub CallBlockParam,
    pub Vec<(CallBlockParam, KnownBlock)>,
);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MethodSignature {
    pub opcode: IString,
    pub unbound_params: Vec<CallBlockParam>,
}

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
        category: u32,
    },
    MenuInput {
        opcode: IString,
        field_name: IString,
        default: project_json::Sb3FieldValue,
        category: u32,
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
            CallBlockParamKind::Field { default, category } => {
                let default = quote_option(default.as_ref());
                tokens
                    .append_all(quote!(#prefix::Field { default: #default, category: #category }));
            }
            CallBlockParamKind::MenuInput {
                opcode,
                field_name,
                default,
                category,
            } => {
                let opcode = opcode.as_str();
                let field_name = field_name.as_str();
                tokens.append_all(quote! {
                    #prefix::MenuInput {
                        opcode: ::arcstr::literal!(#opcode),
                        field_name: ::arcstr::literal!(#field_name),
                        default: #default,
                        category: #category,
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum HasShadow {
    Yes,
    No,
}

impl ToTokens for HasShadow {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            HasShadow::Yes => {
                tokens.append_all(quote! {
                    ::grazelang_library::HasShadow::Yes
                });
            }
            HasShadow::No => {
                tokens.append_all(quote! {
                    ::grazelang_library::HasShadow::No
                });
            }
        }
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
                    kind: CallBlockParamKind::Field {
                        default: None,
                        category: VARIABLES_CATEGORY_ID,
                    },
                    name: literal!("VARIABLE"),
                },
                KnownBlock::FieldValue {
                    value: project_json::Sb3FieldValue::WithId {
                        value: (&canonical_name as &str).into(),
                        id: id.to_string(),
                    },
                    categories: HashSet::from([VARIABLES_CATEGORY_ID]),
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
            KnownBlock::List { canonical_name, id } => {
                let id = id.as_str();
                tokens.append_all(quote! {
                    #prefix::List {
                        canonical_name: #canonical_name.to_string(),
                        id: ::arcstr::literal!(#id),
                    }
                });
            }
            KnownBlock::FieldValue { value, categories } => {
                let categories = categories.iter();
                tokens.append_all(quote!(#prefix::FieldValue {
                    value: #value,
                    categories: ::std::collections::HashSet::from([#( #categories ),*])
                }))
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
            KnownBlock::CustomBlock {
                proccode,
                call_params,
                params,
                is_warp,
            } => {
                let proccode = proccode.as_str();
                let (keys, values): (Vec<_>, Vec<_>) =
                    params.iter().map(|(k, v)| (k.as_str(), v)).unzip();
                tokens.append_all(quote! {
                    #prefix::CustomBlock {
                        proccode: ::arcstr::literal!(#proccode),
                        call_params: ::std::vec![#( #call_params ),*],
                        params: ::std::vec![#( (::arcstr::literal!(#keys), #values) ),*],
                        is_warp: #is_warp,
                    }
                });
            }
            KnownBlock::BoundMethod { signature, bound_params } => {
                let opcode = signature.opcode.as_str();
                let unbound_params = &signature.unbound_params;
                let (keys, values): (Vec<_>, Vec<_>) = 
                    bound_params.iter().map(|(k, v)| (k, v)).unzip();
                tokens.append_all(quote! {
                    #prefix::BoundMethod {
                        signature: ::std::sync::Arc::new(::grazelang_library::MethodSignature {
                            opcode: ::arcstr::literal!(#opcode),
                            unbound_params: ::std::vec![#( #unbound_params ),*],
                        }),
                        bound_params: ::std::rc::Rc::<[_]>::from([#( (#keys, #values) ),*]),
                    }
                });
            }
            KnownBlock::Empty => {
                tokens.append_all(quote! {
                    #prefix::Empty
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
