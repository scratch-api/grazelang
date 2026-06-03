use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
    io::Read,
    ops::{Index, IndexMut},
    rc::Rc,
    sync::LazyLock,
};

use arcstr::{ArcStr as IString, literal};
pub use grazelang_library::KnownBlock;
use grazelang_library::{
    BACKDROP_TARGETS_CATEGORY_ID, BACKDROPS_CATEGORY_ID, BROADCASTS_CATEGORY_ID,
    COSTUMES_CATEGORY_ID, CallBlockParam, CallableKnownBlockSignature, HasShadow, KnownBlockInput,
    LISTS_CATEGORY_ID, PROPERTIES_CATEGORY_ID, SOUNDS_CATEGORY_ID,
    SimpleCallableKnownBlockSignature, VARIABLES_CATEGORY_ID,
    project_json::{
        Sb3ListDeclaration, Sb3Primitive, Sb3PrimitiveBlock, Sb3VariableDeclaration,
        TargetAttachment,
    },
};
use rand::{Rng, SeedableRng};
use rand_xoshiro::Xoshiro256StarStar;
use serde::{Deserialize, Serialize};

use crate::{
    codegen::{
        self,
        core::{AssetFile, emit_message},
        ids::generate_random_id_as_string,
    },
    lexer::PosRange,
    messages::{GrazeMessage, GrazeWarning, GrazeWarningKind},
    names::Namespace,
    parser::cst::CustomBlockParamKindValue,
    settings::GrazeSettings,
};

pub type IdString = IString;

macro_rules! static_ref_of_const {
    ($expr:expr, $type_:ty) => {{
        const VALUE: &$type_ = &$expr;
        VALUE
    }};
}

macro_rules! static_ref_hashset {
    ($name:ident, [$($x:expr),* $(,)?]) => {
        pub static $name: LazyLock<HashSet<u32>> = LazyLock::new(|| {
            HashSet::from([$($x),*])
        });
    };
}

static_ref_hashset!(NO_CATEGORIES, []);
static_ref_hashset!(
    VARIABLE_CATEGORIES,
    [VARIABLES_CATEGORY_ID, PROPERTIES_CATEGORY_ID]
);
static_ref_hashset!(LIST_CATEGORIES, [LISTS_CATEGORY_ID]);
static_ref_hashset!(BROADCAST_CATEGORIES, [BROADCASTS_CATEGORY_ID]);
static_ref_hashset!(
    ANY_CATEGORIES,
    [
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
        25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63
    ]
);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Primitive {
    String(IString),
    Number(IString),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub value: Sb3Primitive,
    pub is_cloud: bool,
    /// vars declared as a normal statement are assigned to every time the statement is reached and their initial value is empty
    pub value_is_initial_value: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub value: Vec<Sb3Primitive>,
    /// lists declared as a normal statement are assigned to every time the statement is reached and their initial value is empty
    pub value_is_initial_value: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CustomBlockParamDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub kind: CustomBlockParamKindValue,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CustomBlockDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub args: Vec<CustomBlockParamDescriptor>,
    pub is_warp: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CostumeDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub source: IString,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BackdropDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub source: IString,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SoundDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub source: IString,
}

pub trait ResolveKnownBlock {
    fn resolve_for_input<'a>(
        &'a self,
        pos_range: PosRange,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> KnownBlockInput<'a>;

    fn resolve_for_field<'a>(
        &'a self,
        pos_range: PosRange,
        context: &'a mut codegen::core::GrazeSb3GeneratorContext,
    ) -> (codegen::project_json::Sb3FieldValue, &'a HashSet<u32>);

    fn resolve_for_call_block<'a>(
        &'a self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> Option<CallableKnownBlockSignature<'a>>;

    fn resolve_for_assignment<'a>(
        &'a self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> &'a SimpleCallableKnownBlockSignature;
}

impl ResolveKnownBlock for KnownBlock {
    fn resolve_for_input<'a>(
        &'a self,
        pos_range: PosRange,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> KnownBlockInput<'a> {
        use codegen::project_json::Sb3PrimitiveBlock;
        match self {
            KnownBlock::Variable {
                canonical_name,
                id,
                assign: _,
                bind_info,
            } => {
                if let Some(bind_info) = bind_info
                    && let Some(target) = context.current_sb3_target.as_ref()
                    && bind_info.parent_target.as_str() != target.name.as_str()
                {
                    return KnownBlockInput::SimpleBlock(
                        static_ref_of_const!(literal!("sensing_of"), IString),
                        &bind_info.property_of_params,
                    );
                }
                KnownBlockInput::PrimitiveInput(Sb3PrimitiveBlock::Variable {
                    name: canonical_name.to_string(),
                    id: id.to_string(),
                    x: None,
                    y: None,
                })
            }
            KnownBlock::List { canonical_name, id } => {
                KnownBlockInput::PrimitiveInput(Sb3PrimitiveBlock::List {
                    name: canonical_name.to_string(),
                    id: id.to_string(),
                    x: None,
                    y: None,
                })
            }
            KnownBlock::FieldValue { value, categories } => {
                KnownBlockInput::Menu(value.clone(), categories)
            }
            KnownBlock::BlockRef { id } => KnownBlockInput::BlockRef(id.clone()),
            KnownBlock::PrimitiveBlock { value } => KnownBlockInput::PrimitiveInput(value.clone()),
            KnownBlock::Callable(..)
            | KnownBlock::PartialCallable(..)
            | KnownBlock::CustomBlock { .. } => {
                emit_message(
                    context,
                    GrazeMessage::Warning(
                        GrazeWarning::Specific(
                            GrazeWarningKind::CallableAsInput,
                            format!("Cannot reasonably use KnownBlock {self:?} as an input parameter, maybe you meant to call it instead.").into(),
                            pos_range
                        ),
                        None
                    ),
                    GrazeMessageSetting::Warnings,
                );
                KnownBlockInput::PrimitiveInput("".into())
            }
            KnownBlock::SingletonReporter {
                opcode,
                params,
                field: _,
                assign: _,
                bind_info: _,
            } => KnownBlockInput::SimpleBlock(opcode, params),
            KnownBlock::Empty => KnownBlockInput::Empty,
        }
    }

    fn resolve_for_field<'a>(
        &'a self,
        pos_range: PosRange,
        context: &'a mut codegen::core::GrazeSb3GeneratorContext,
    ) -> (codegen::project_json::Sb3FieldValue, &'a HashSet<u32>) {
        use codegen::project_json::{Sb3FieldValue, Sb3Primitive};
        match self {
            KnownBlock::Variable {
                canonical_name,
                id,
                assign: _,
                bind_info: _,
            } => (
                Sb3FieldValue::WithId {
                    value: Sb3Primitive::String(canonical_name.to_string()),
                    id: id.to_string(),
                },
                &*VARIABLE_CATEGORIES,
            ),
            KnownBlock::List { canonical_name, id } => (
                Sb3FieldValue::WithId {
                    value: Sb3Primitive::String(canonical_name.to_string()),
                    id: id.to_string(),
                },
                &*LIST_CATEGORIES,
            ),
            KnownBlock::FieldValue { value, categories } => (value.clone(), categories),
            KnownBlock::BlockRef { id } => {
                emit_message(
                    context,
                    GrazeMessage::Warning(
                        GrazeWarning::Specific(
                            GrazeWarningKind::BlockRefAsField,
                            format!("Cannot reasonably use KnownBlock {self:?} as a field parameter, maybe you meant to use it as a different parameter.").into(),
                            pos_range
                        ),
                        None
                    ),
                    GrazeMessageSetting::Warnings,
                );
                // No need for a second warning, therefore `ANY_CATEGORIES` is used
                (Sb3FieldValue::Normal(id.into()), &*ANY_CATEGORIES)
            }
            KnownBlock::PrimitiveBlock { value } => match value {
                codegen::project_json::Sb3PrimitiveBlock::Number(sb3_primitive)
                | codegen::project_json::Sb3PrimitiveBlock::PositiveNumber(sb3_primitive)
                | codegen::project_json::Sb3PrimitiveBlock::PositiveInteger(sb3_primitive)
                | codegen::project_json::Sb3PrimitiveBlock::Integer(sb3_primitive)
                | codegen::project_json::Sb3PrimitiveBlock::Angle(sb3_primitive)
                | codegen::project_json::Sb3PrimitiveBlock::Color(sb3_primitive)
                | codegen::project_json::Sb3PrimitiveBlock::String(sb3_primitive) => {
                    let cow_str = sb3_primitive.as_cow_str();
                    (
                        Sb3FieldValue::Normal(sb3_primitive.clone()),
                        context
                            .field_entry_categories
                            .get(&*cow_str)
                            .unwrap_or_else(|| &*NO_CATEGORIES),
                    )
                }
                codegen::project_json::Sb3PrimitiveBlock::Broadcast { name, id } => (
                    Sb3FieldValue::WithId {
                        value: Sb3Primitive::String(name.clone()),
                        id: id.clone(),
                    },
                    &*BROADCAST_CATEGORIES,
                ),
                codegen::project_json::Sb3PrimitiveBlock::Variable {
                    name,
                    id,
                    x: _,
                    y: _,
                } => (
                    Sb3FieldValue::WithId {
                        value: Sb3Primitive::String(name.clone()),
                        id: id.clone(),
                    },
                    &*VARIABLE_CATEGORIES,
                ),
                codegen::project_json::Sb3PrimitiveBlock::List {
                    name,
                    id,
                    x: _,
                    y: _,
                } => (
                    Sb3FieldValue::WithId {
                        value: Sb3Primitive::String(name.clone()),
                        id: id.clone(),
                    },
                    &*LIST_CATEGORIES,
                ),
            },
            KnownBlock::Callable(..)
            | KnownBlock::PartialCallable(..)
            | KnownBlock::CustomBlock { .. } => {
                emit_message(
                    context,
                    GrazeMessage::Warning(
                        GrazeWarning::Specific(
                            GrazeWarningKind::CallableAsField,
                            format!("Cannot reasonably use KnownBlock {self:?} as a field parameter, maybe you meant to call it instead.").into(),
                            pos_range
                        ),
                        None
                    ),
                    GrazeMessageSetting::Warnings,
                );
                // No need for a second warning, therefore `ANY_CATEGORIES` is used
                (Sb3FieldValue::Normal("".into()), &*ANY_CATEGORIES)
            }
            KnownBlock::Empty => {
                emit_message(
                    context,
                    GrazeMessage::Warning(
                        GrazeWarning::Specific(
                            GrazeWarningKind::EmptyExpressionAsField,
                            format!("Cannot reasonably use KnownBlock {self:?} as a field parameter, maybe you meant to use it as a different parameter.").into(),
                            pos_range
                        ),
                        None
                    ),
                    GrazeMessageSetting::Warnings,
                );
                // No need for a second warning, therefore `ANY_CATEGORIES` is used
                (Sb3FieldValue::Normal("".into()), &*ANY_CATEGORIES)
            }
            KnownBlock::SingletonReporter {
                opcode: _,
                params: _,
                field,
                assign: _,
                bind_info: _,
            } => {
                field
                    .clone()
                    // TODO: Check what categories might fit
                    // Issue: #48
                    .map(|value| (value, &* NO_CATEGORIES))
                    .unwrap_or_else(|| {
                        emit_message(
                            context,
                            GrazeMessage::Warning(
                                GrazeWarning::Specific(
                                    GrazeWarningKind::NonFieldSingletonAsField,
                                    format!("Cannot reasonably use KnownBlock {self:?} as a field parameter, maybe you meant to use it as a different parameter.").into(),
                                    pos_range
                                ),
                                None
                            ),
                            GrazeMessageSetting::Warnings,
                        );
                        // No need for a second warning, therefore `ANY_CATEGORIES` is used
                        (Sb3FieldValue::Normal("".into()), &* ANY_CATEGORIES)
                    })
            }
        }
    }

    fn resolve_for_call_block<'a>(
        &'a self,
        _context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> Option<CallableKnownBlockSignature<'a>> {
        const NO_PARTIALS: &Vec<(CallBlockParam, KnownBlock)> = &Vec::new();
        match self {
            KnownBlock::Callable(opcode, params) => Some(CallableKnownBlockSignature(
                opcode,
                params,
                NO_PARTIALS,
                None,
            )),
            KnownBlock::PartialCallable(opcode, values, params) => {
                Some(CallableKnownBlockSignature(opcode, params, values, None))
            }
            KnownBlock::SingletonReporter { opcode, params, .. } => {
                Some(CallableKnownBlockSignature(
                    opcode,
                    static_ref_of_const!(Vec::new(), Vec<CallBlockParam>),
                    params,
                    None,
                ))
            }
            KnownBlock::CustomBlock {
                proccode,
                call_params,
                params,
                is_warp,
            } => Some(CallableKnownBlockSignature(
                static_ref_of_const!(literal!("procedures_call"), IString),
                call_params,
                NO_PARTIALS,
                Some((proccode, params, is_warp)),
            )),
            KnownBlock::Variable { .. }
            | KnownBlock::List { .. }
            | KnownBlock::FieldValue { .. }
            | KnownBlock::BlockRef { .. }
            | KnownBlock::PrimitiveBlock { .. }
            | KnownBlock::Empty => None,
        }
    }

    fn resolve_for_assignment<'a>(
        &'a self,
        _context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> &'a SimpleCallableKnownBlockSignature {
        match self {
            KnownBlock::Variable {
                canonical_name: _,
                id: _,
                assign,
                bind_info: _,
            }
            | KnownBlock::SingletonReporter {
                opcode: _,
                params: _,
                field: _,
                assign: Some(assign),
                bind_info: _,
            } => assign,
            KnownBlock::List { .. }
            | KnownBlock::FieldValue { .. }
            | KnownBlock::BlockRef { .. }
            | KnownBlock::PrimitiveBlock { .. }
            | KnownBlock::Callable(..)
            | KnownBlock::PartialCallable(..)
            | KnownBlock::SingletonReporter { .. }
            | KnownBlock::CustomBlock { .. }
            | KnownBlock::Empty => todo!(), // warn user about incorrect usage
        } // TODO: Implement assignment of x, y etc
        // Issue: #12
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SymbolTable {
    pub table: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { table: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            table: Vec::with_capacity(capacity),
        }
    }

    pub fn reserve(&mut self, additional: usize) {
        self.table.reserve(additional);
    }

    pub fn new_symbol(&mut self, symbol: Symbol) -> SymbolId {
        self.table.push(symbol);
        SymbolId(self.table.len() - 1)
    }

    pub fn new_child_symbol(
        &mut self,
        parent: SymbolId,
        child_name: IString,
        known_block: Option<Rc<KnownBlock>>,
        hash_map_cap: usize,
    ) -> SymbolId {
        let namespace = HashMap::with_capacity(hash_map_cap);
        let symbol = self.new_symbol(Symbol {
            known_block,
            namespace,
            parent,
        });
        self.insert_child(parent, child_name, symbol);
        symbol
    }

    pub fn get_child<Q>(&self, symbol: SymbolId, child_name: &Q) -> Option<SymbolId>
    where
        Q: ?Sized + Hash + Eq,
        IString: Borrow<Q>,
    {
        self[symbol].namespace.get(child_name).copied()
    }

    pub fn insert_child(
        &mut self,
        symbol: SymbolId,
        child_name: IString,
        child: SymbolId,
    ) -> Option<SymbolId> {
        self[child].parent = symbol;
        self[symbol].namespace.insert(child_name, child)
    }

    pub fn insert_alias(
        &mut self,
        symbol: SymbolId,
        child_name: IString,
        child: SymbolId,
    ) -> Option<SymbolId> {
        self[symbol].namespace.insert(child_name, child)
    }
}

impl Index<SymbolId> for SymbolTable {
    type Output = Symbol;
    fn index(&self, index: SymbolId) -> &Self::Output {
        &self.table[index.0]
    }
}

impl IndexMut<SymbolId> for SymbolTable {
    fn index_mut(&mut self, index: SymbolId) -> &mut Self::Output {
        &mut self.table[index.0]
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Serialize, Deserialize)]
pub struct SymbolId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub known_block: Option<Rc<KnownBlock>>,
    pub namespace: HashMap<IString, SymbolId>,
    pub parent: SymbolId,
}

// #[derive(Debug, Clone)]
// pub enum Symbol {
//     Namespace(
//         HashMap<IString, Rc<RefCell<Symbol>>>,
//         Weak<RefCell<Symbol>>,
//     ),
//     KnownBlock(
//         Box<KnownBlock>,
//         HashMap<IString, Rc<RefCell<Symbol>>>,
//         Weak<RefCell<Symbol>>,
//     ),
//     Alias(Weak<RefCell<Symbol>>, Weak<RefCell<Symbol>>),
// }

// impl Symbol {
//     pub fn new_namespace() -> Self {
//         Self::Namespace(HashMap::new(), Weak::new())
//     }

//     pub fn get_block(&self) -> Option<&KnownBlock> {
//         match self {
//             Symbol::KnownBlock(known_block, ..) => Some(known_block),
//             Symbol::Namespace(..) | Symbol::Alias(..) => None,
//         }
//     }

//     pub fn unalias(this: &Rc<RefCell<Self>>) -> Option<Rc<RefCell<Self>>> {
//         match &*this.borrow() {
//             Symbol::Namespace(..) | Symbol::KnownBlock(..) => Some(this.clone()),
//             Symbol::Alias(weak, _) => weak.upgrade().and_then(|this| Symbol::unalias(&this)),
//         }
//     }

//     pub fn get_child(&self, child_name: &IString) -> Option<Rc<RefCell<Symbol>>> {
//         match self {
//             Symbol::Namespace(hash_map, ..) | Symbol::KnownBlock(_, hash_map, ..) => {
//                 hash_map.get(child_name).cloned()
//             }
//             Symbol::Alias(alias, ..) => alias
//                 .upgrade()
//                 .and_then(|value| value.borrow().get_child(child_name)),
//         }
//     }

//     /// Clones the children and allocates them in a dedicated Vec
//     pub fn get_children_cloned(&self) -> Vec<Rc<RefCell<Symbol>>> {
//         match self {
//             Symbol::Namespace(hash_map, ..) | Symbol::KnownBlock(_, hash_map, ..) => {
//                 hash_map.values().cloned().collect()
//             }
//             Symbol::Alias(alias, ..) => alias
//                 .upgrade()
//                 .map(|value| value.borrow().get_children_cloned())
//                 .unwrap_or_default(),
//         }
//     }

//     /// Clones the children and keys and allocates them in a dedicated Vec
//     pub fn get_entries_cloned(&self) -> Vec<(IString, Rc<RefCell<Symbol>>)> {
//         match self {
//             Symbol::Namespace(hash_map, ..) | Symbol::KnownBlock(_, hash_map, ..) => hash_map
//                 .iter()
//                 .map(|(key, value)| (key.clone(), value.clone()))
//                 .collect(),
//             Symbol::Alias(alias, ..) => alias
//                 .upgrade()
//                 .map(|value| value.borrow().get_entries_cloned())
//                 .unwrap_or_default(),
//         }
//     }

//     pub fn get_parent(&self) -> &Weak<RefCell<Symbol>> {
//         match self {
//             Symbol::Namespace(_, parent_ref)
//             | Symbol::KnownBlock(_, _, parent_ref)
//             | Symbol::Alias(_, parent_ref) => parent_ref,
//         }
//     }

//     pub fn replace_parent(&mut self, parent: Weak<RefCell<Symbol>>) -> Weak<RefCell<Symbol>> {
//         match self {
//             Symbol::Namespace(_, parent_ref)
//             | Symbol::KnownBlock(_, _, parent_ref)
//             | Symbol::Alias(_, parent_ref) => std::mem::replace(parent_ref, parent),
//         }
//     }

//     pub fn insert_child(
//         this: &Rc<RefCell<Self>>,
//         child_name: IString,
//         child: Rc<RefCell<Symbol>>,
//     ) -> Option<Rc<RefCell<Symbol>>> {
//         child.borrow_mut().replace_parent(Rc::downgrade(this));
//         match &mut *this.borrow_mut() {
//             Symbol::Namespace(hash_map, ..) => hash_map.insert(child_name, child),
//             Symbol::KnownBlock(_, hash_map, ..) => hash_map.insert(child_name, child),
//             Symbol::Alias(alias, ..) => alias
//                 .upgrade()
//                 .and_then(|value| Self::insert_child(&value, child_name, child)),
//         }
//     }

//     pub fn remove_child(
//         this: &Rc<RefCell<Self>>,
//         child_name: &IString,
//     ) -> Option<Rc<RefCell<Symbol>>> {
//         match &mut *this.borrow_mut() {
//             Symbol::Namespace(hash_map, ..) => hash_map.remove(child_name),
//             Symbol::KnownBlock(_, hash_map, ..) => hash_map.remove(child_name),
//             Symbol::Alias(alias, ..) => alias
//                 .upgrade()
//                 .and_then(|value| Self::remove_child(&value, child_name)),
//         }
//     }

//     pub fn clear_children(this: &Rc<RefCell<Self>>) {
//         let alias = match &mut *this.borrow_mut() {
//             Symbol::Namespace(hash_map, ..) | Symbol::KnownBlock(_, hash_map, ..) => {
//                 hash_map.clear();
//                 return;
//             }
//             Symbol::Alias(alias, ..) => alias.upgrade(),
//         };
//         if let Some(alias) = alias {
//             Self::clear_children(&alias);
//         }
//     }

//     // Whether this symbol is an alias
//     pub fn is_alias(&self) -> bool {
//         matches!(self, Symbol::Alias(..))
//     }
// }

#[derive(Debug, Clone)]
pub struct BroadcastDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
}

impl BroadcastDescriptor {
    pub fn derive_related_data<T: Rng>(&self, rng: &mut T) -> (Symbol, TargetAttachment) {
        let name = self
            .canonical_name
            .as_ref()
            .map(IString::to_string)
            .unwrap_or_else(|| self.name.to_string());
        let id = generate_random_id_as_string(rng);
        (
            Symbol {
                known_block: Some(Rc::new(KnownBlock::PrimitiveBlock {
                    value: Sb3PrimitiveBlock::Broadcast {
                        name: name.clone(),
                        id: id.clone(),
                    },
                })),
                namespace: HashMap::new(),
                parent: Default::default(),
            },
            TargetAttachment::Broadcast { name, id },
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TargetSymbolDescriptor {
    Var(VarDescriptor),
    List(ListDescriptor),
    CustomBlockDescriptor(CustomBlockDescriptor),
    Costume(CostumeDescriptor),
    Backdrop(BackdropDescriptor),
    Sound(SoundDescriptor),
}

impl TargetSymbolDescriptor {
    pub fn compute_hash(path: &str) -> Result<String, std::io::Error> {
        use std::fs::File;
        let mut file = File::open(path)?;
        let mut data: Vec<u8> = Vec::new();
        file.read_to_end(&mut data)?;
        let digest = md5::compute(data);
        let hex_digest = format!("{:x}", digest);
        Ok(hex_digest)
    }

    pub fn derive_related_data<T: Rng>(
        &self,
        rng: &mut T,
        namespace: &mut Namespace,
    ) -> Result<
        (
            Symbol,
            Option<codegen::project_json::TargetAttachment>,
            Option<AssetFile>,
        ),
        std::io::Error,
    > {
        pub fn get_file_extension_unwrapped(this: &TargetSymbolDescriptor) -> &str {
            match this {
                TargetSymbolDescriptor::Costume(CostumeDescriptor {
                    name: _,
                    canonical_name: _,
                    source,
                })
                | TargetSymbolDescriptor::Backdrop(BackdropDescriptor {
                    name: _,
                    canonical_name: _,
                    source,
                })
                | TargetSymbolDescriptor::Sound(SoundDescriptor {
                    name: _,
                    canonical_name: _,
                    source,
                }) => {
                    // TODO: better ext detections
                    // Issue: #11
                    source.as_str().rsplit_once('.').unwrap_or(("", "")).1
                }
                _ => unreachable!(),
            }
        }
        if let TargetSymbolDescriptor::CustomBlockDescriptor(descriptor) = self {
            let proccode = descriptor.canonical_name.clone().unwrap_or_else(|| {
                if descriptor.args.is_empty() {
                    return descriptor.name.clone();
                }
                let mut proccode =
                    String::with_capacity(descriptor.name.len() + 3 * descriptor.args.len());
                proccode.push_str(descriptor.name.as_str());
                for arg in &descriptor.args {
                    proccode.push_str(" %");
                    proccode.push(match &arg.kind {
                        CustomBlockParamKindValue::Number => 'n',
                        CustomBlockParamKindValue::String => 's',
                        CustomBlockParamKindValue::Boolean => 'b',
                    });
                }
                proccode.into()
            });
            let mut call_params = Vec::with_capacity(descriptor.args.len());
            let mut params = Vec::with_capacity(descriptor.args.len());
            for arg in &descriptor.args {
                let is_bool = arg.kind == CustomBlockParamKindValue::Boolean;
                let arg_id = codegen::ids::generate_random_id_string(rng);
                call_params.push(CallBlockParam {
                    kind: grazelang_library::CallBlockParamKind::Input {
                        default: (!is_bool).then(|| {
                            grazelang_library::project_json::Sb3PrimitiveBlock::String("".into())
                        }),
                    },
                    name: arg_id.clone(),
                });
                params.push((
                    arg_id,
                    if is_bool {
                        HasShadow::No
                    } else {
                        HasShadow::Yes
                    },
                ));
            }
            return Ok((
                Symbol {
                    known_block: Some(Rc::new(KnownBlock::CustomBlock {
                        proccode: proccode.clone(),
                        call_params: call_params.clone(),
                        params: params.clone(),
                        is_warp: descriptor.is_warp,
                    })),
                    namespace: HashMap::new(),
                    parent: Default::default(),
                },
                Some(
                    grazelang_library::project_json::TargetAttachment::CustomBlock(
                        descriptor.name.clone(),
                        KnownBlock::CustomBlock {
                            proccode,
                            call_params,
                            params,
                            is_warp: descriptor.is_warp,
                        },
                    ),
                ),
                None,
            ));
        }
        match self {
            TargetSymbolDescriptor::Costume(descriptor) => Some(&descriptor.source),
            TargetSymbolDescriptor::Backdrop(descriptor) => Some(&descriptor.source),
            TargetSymbolDescriptor::Sound(descriptor) => Some(&descriptor.source),
            _ => None,
        }
        .map(|value| Self::compute_hash(value.as_str()))
        .transpose()?
        .map(|value| {
            let file_ext = get_file_extension_unwrapped(self);
            let md5ext = format!("{}.{}", value, file_ext);
            Ok((
                Symbol {
                    known_block: Some(Rc::new(match self {
                        // TODO: implement specific known blocks here
                        // Issue: #10
                        TargetSymbolDescriptor::Costume(CostumeDescriptor {
                            name,
                            canonical_name,
                            source: _,
                        }) => KnownBlock::FieldValue {
                            value: codegen::project_json::Sb3FieldValue::Normal(
                                codegen::project_json::Sb3Primitive::String(
                                    canonical_name.as_ref().unwrap_or(name).to_string(),
                                ),
                            ),
                            categories: HashSet::from([COSTUMES_CATEGORY_ID]),
                        },
                        TargetSymbolDescriptor::Backdrop(BackdropDescriptor {
                            name,
                            canonical_name,
                            source: _,
                        }) => KnownBlock::FieldValue {
                            value: codegen::project_json::Sb3FieldValue::Normal(
                                codegen::project_json::Sb3Primitive::String(
                                    canonical_name.as_ref().unwrap_or(name).to_string(),
                                ),
                            ),
                            categories: HashSet::from([
                                BACKDROPS_CATEGORY_ID,
                                BACKDROP_TARGETS_CATEGORY_ID,
                            ]),
                        },
                        TargetSymbolDescriptor::Sound(SoundDescriptor {
                            name,
                            canonical_name,
                            source: _,
                        }) => KnownBlock::FieldValue {
                            value: codegen::project_json::Sb3FieldValue::Normal(
                                codegen::project_json::Sb3Primitive::String(
                                    canonical_name.as_ref().unwrap_or(name).to_string(),
                                ),
                            ),
                            categories: HashSet::from([SOUNDS_CATEGORY_ID]),
                        },
                        _ => unreachable!(),
                    })),
                    namespace: HashMap::new(),
                    parent: Default::default(),
                },
                match self {
                    // TODO: implement specific known blocks here
                    // Issue: #9
                    TargetSymbolDescriptor::Costume(CostumeDescriptor {
                        name,
                        canonical_name,
                        source: _,
                    })
                    | TargetSymbolDescriptor::Backdrop(BackdropDescriptor {
                        name,
                        canonical_name,
                        source: _,
                    }) => {
                        Some(codegen::project_json::TargetAttachment::Costume(
                            codegen::project_json::Sb3Costume {
                                asset_id: value.clone(),
                                name: canonical_name.as_ref().unwrap_or(name).to_string(),
                                md5ext: md5ext.clone(),
                                data_format: file_ext.to_string(),
                                bitmap_resolution: Some(1.0), // TODO: better default and more control
                                // Issue: #8
                                rotation_center_x: 0.0,
                                rotation_center_y: 0.0,
                            },
                        ))
                    }
                    TargetSymbolDescriptor::Sound(SoundDescriptor {
                        name,
                        canonical_name,
                        source: _,
                    }) => {
                        Some(codegen::project_json::TargetAttachment::Sound(
                            codegen::project_json::Sb3Sound {
                                asset_id: value.clone(),
                                name: canonical_name.as_ref().unwrap_or(name).to_string(),
                                md5ext: md5ext.clone(),
                                data_format: file_ext.to_string(),
                                rate: 48000.0, // TODO: better default and more control
                                // Issue: #7
                                sample_count: 1124,
                            },
                        ))
                    }
                    _ => unreachable!(),
                },
                match self {
                    TargetSymbolDescriptor::Costume(CostumeDescriptor {
                        name: _,
                        canonical_name: _,
                        source,
                    })
                    | TargetSymbolDescriptor::Backdrop(BackdropDescriptor {
                        name: _,
                        canonical_name: _,
                        source,
                    })
                    | TargetSymbolDescriptor::Sound(SoundDescriptor {
                        name: _,
                        canonical_name: _,
                        source,
                    }) => Some(AssetFile {
                        file_name: md5ext,
                        file_path: source.clone(),
                    }),
                    _ => unreachable!(),
                },
            ))
        })
        .unwrap_or_else(|| {
            let id = codegen::ids::generate_random_id_string(rng);
            match self {
                TargetSymbolDescriptor::Var(var_descriptor) => {
                    let canonical_name = namespace.introduce_new_symbol(
                        var_descriptor
                            .canonical_name
                            .as_ref()
                            .map(|value| value.to_string()),
                        var_descriptor.name.clone(),
                    );
                    let id_string = id.to_string();
                    Ok((
                        Symbol {
                            known_block: Some(Rc::new(KnownBlock::new_variable(
                                canonical_name.clone(),
                                id,
                                None,
                            ))),
                            namespace: HashMap::new(),
                            parent: Default::default(),
                        },
                        Some(grazelang_library::project_json::TargetAttachment::Var(
                            id_string,
                            Sb3VariableDeclaration {
                                name: canonical_name,
                                value: if var_descriptor.value_is_initial_value {
                                    var_descriptor.value.clone()
                                } else {
                                    "".into()
                                },
                                is_cloud: var_descriptor.is_cloud,
                            },
                        )),
                        None,
                    ))
                }
                TargetSymbolDescriptor::List(list_descriptor) => {
                    let canonical_name = namespace.introduce_new_symbol(
                        list_descriptor
                            .canonical_name
                            .as_ref()
                            .map(|value| value.to_string()),
                        list_descriptor.name.clone(),
                    );
                    let id_string = id.to_string();
                    Ok((
                        Symbol {
                            known_block: Some(Rc::new(KnownBlock::List {
                                canonical_name: canonical_name.clone(),
                                id,
                            })), // TODO: add list length as method
                            // Issue: #6
                            namespace: HashMap::new(),
                            parent: Default::default(),
                        },
                        Some(grazelang_library::project_json::TargetAttachment::List(
                            id_string,
                            Sb3ListDeclaration(
                                canonical_name.clone(),
                                if list_descriptor.value_is_initial_value {
                                    list_descriptor.value.clone()
                                } else {
                                    Vec::new()
                                },
                            ),
                        )),
                        None,
                    ))
                }
                TargetSymbolDescriptor::CustomBlockDescriptor(_)
                | TargetSymbolDescriptor::Costume(_)
                | TargetSymbolDescriptor::Backdrop(_)
                | TargetSymbolDescriptor::Sound(_) => unreachable!(),
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Target {
    Sprite {
        name: IString,
        canonical_name: Option<IString>,
        symbols: HashMap<IString, TargetSymbolDescriptor>,
    },
    Stage {
        symbols: HashMap<IString, TargetSymbolDescriptor>,
    },
}

impl Target {
    pub fn new_sprite(name: IString, canonical_name: Option<IString>) -> Self {
        Self::Sprite {
            name,
            canonical_name,
            symbols: Default::default(),
        }
    }

    pub fn borrow_symbols_mut(&mut self) -> &mut HashMap<IString, TargetSymbolDescriptor> {
        match self {
            Target::Sprite {
                name: _,
                canonical_name: _,
                symbols,
            }
            | Target::Stage { symbols } => symbols,
        }
    }

    pub fn borrow_symbols(&self) -> &HashMap<IString, TargetSymbolDescriptor> {
        match self {
            Target::Sprite {
                name: _,
                canonical_name: _,
                symbols,
            }
            | Target::Stage { symbols } => symbols,
        }
    }

    pub fn get_field_name(&self) -> &str {
        match self {
            Target::Sprite {
                name,
                canonical_name: _,
                symbols: _,
            } => name.as_str(),
            Target::Stage { symbols: _ } => "_stage_", // TODO: check whether this is correct
                                                       // Issue: #5
        }
    }

    pub fn get_namespace_name(&self) -> &IString {
        match self {
            Target::Sprite {
                name,
                canonical_name: _,
                symbols: _,
            } => name,
            Target::Stage { symbols: _ } => codegen::core::STAGE_ISTRING,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum GrazeMessageSetting {
    // Maybe there will be settings inbetween later
    #[default]
    /// tag: 255, is default
    All,
    /// tag: 9
    Infos,
    /// tag: 6
    Warnings,
    /// tag: 3
    Errors,
    /// tag: 2
    ExitOnError,
    /// tag: 1
    ExitOnErrorUnlogged,
    /// tag: 0
    None,
}

impl GrazeMessageSetting {
    pub fn get_numeric(&self) -> u8 {
        match self {
            GrazeMessageSetting::All => 255,
            GrazeMessageSetting::Infos => 9,
            GrazeMessageSetting::Warnings => 6,
            GrazeMessageSetting::Errors => 3,
            GrazeMessageSetting::ExitOnError => 2,
            GrazeMessageSetting::ExitOnErrorUnlogged => 1,
            GrazeMessageSetting::None => 0,
        }
    }
}

impl PartialOrd for GrazeMessageSetting {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for GrazeMessageSetting {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_numeric().cmp(&other.get_numeric())
    }
}

#[derive(Debug, Clone)]
pub struct ParseContext {
    // If stage is in this, it should be the first element
    pub parsed_targets: VecDeque<Target>,
    pub broadcasts: HashMap<IString, BroadcastDescriptor>,
    pub next_target: Option<Target>,
    pub global_symbols: HashMap<IString, TargetSymbolDescriptor>,
    pub random_seed: <Xoshiro256StarStar as SeedableRng>::Seed,
    pub messages: Vec<GrazeMessage>,
    pub settings: GrazeSettings,
    pub successful: bool,
}

impl ParseContext {
    pub fn new(
        settings: GrazeSettings,
        random_seed: <Xoshiro256StarStar as SeedableRng>::Seed,
    ) -> Self {
        Self {
            parsed_targets: VecDeque::from([Target::Stage {
                symbols: HashMap::new(),
            }]),
            broadcasts: HashMap::new(),
            global_symbols: HashMap::new(),
            next_target: None,
            random_seed,
            messages: Vec::new(),
            successful: true,
            settings,
        }
    }
}

impl Default for ParseContext {
    fn default() -> Self {
        Self::new(Default::default(), Default::default())
    }
}
