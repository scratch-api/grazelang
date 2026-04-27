use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    io::Read,
    rc::{Rc, Weak},
};

use arcstr::{ArcStr as IString, literal};
pub use grazelang_library::KnownBlock;
use grazelang_library::{
    CallBlockParam, CallableKnownBlockSignature, HasShadow, KnownBlockInput,
    SimpleCallableKnownBlockSignature,
    project_json::{Sb3ListDeclaration, Sb3Primitive, Sb3VariableDeclaration},
};
use rand::{Rng, SeedableRng};
use rand_xoshiro::Xoshiro256StarStar;
use serde::{Deserialize, Serialize};

use crate::{codegen, names::Namespace, parser::ast::CustomBlockParamKindValue};

pub type IdString = IString;

macro_rules! static_ref_of_const {
    ($expr:expr, $type_:ty) => {{
        const VALUE: &$type_ = &$expr;
        VALUE
    }};
}

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
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> KnownBlockInput<'a>;

    fn resolve_for_field(
        &self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> codegen::project_json::Sb3FieldValue;

    fn resolve_for_call_block<'a>(
        &'a self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> CallableKnownBlockSignature<'a>;

    fn resolve_for_assignment<'a>(
        &'a self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> &'a SimpleCallableKnownBlockSignature;
}

impl ResolveKnownBlock for KnownBlock {
    fn resolve_for_input<'a>(
        &'a self,
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
                // TODO: possibly set x and y
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
                // TODO: possibly set x and y
                KnownBlockInput::PrimitiveInput(Sb3PrimitiveBlock::List {
                    name: canonical_name.to_string(),
                    id: id.to_string(),
                    x: None,
                    y: None,
                })
            }
            KnownBlock::FieldValue { value } => {
                // TODO: warn user about possibly incorrect usage in some cases
                KnownBlockInput::Menu(value.clone())
            }
            KnownBlock::BlockRef { id } => KnownBlockInput::BlockRef(id.clone()),
            KnownBlock::PrimitiveBlock { value } => KnownBlockInput::PrimitiveInput(value.clone()),
            KnownBlock::Callable(..)
            | KnownBlock::PartialCallable(..)
            | KnownBlock::CustomBlock { .. } => {
                // TODO: warn user about probably incorrect usage
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

    fn resolve_for_field(
        &self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> codegen::project_json::Sb3FieldValue {
        use codegen::project_json::{Sb3FieldValue, Sb3Primitive};
        match self {
            KnownBlock::Variable {
                canonical_name,
                id,
                assign: _,
                bind_info: _,
            }
            | KnownBlock::List { canonical_name, id } => Sb3FieldValue::WithId {
                value: Sb3Primitive::String(canonical_name.to_string()),
                id: id.to_string(),
            },
            KnownBlock::FieldValue { value } => value.clone(),
            KnownBlock::BlockRef { id } => {
                Sb3FieldValue::Normal(id.into()) // TODO: warn user about probably incorrect usage.
            }
            KnownBlock::PrimitiveBlock { value } => {
                // TODO: warn user about possibly incorrect usage
                match value {
                    codegen::project_json::Sb3PrimitiveBlock::Number(sb3_primitive)
                    | codegen::project_json::Sb3PrimitiveBlock::PositiveNumber(sb3_primitive)
                    | codegen::project_json::Sb3PrimitiveBlock::PositiveInteger(sb3_primitive)
                    | codegen::project_json::Sb3PrimitiveBlock::Integer(sb3_primitive)
                    | codegen::project_json::Sb3PrimitiveBlock::Angle(sb3_primitive)
                    | codegen::project_json::Sb3PrimitiveBlock::Color(sb3_primitive)
                    | codegen::project_json::Sb3PrimitiveBlock::String(sb3_primitive) => {
                        Sb3FieldValue::Normal(sb3_primitive.clone())
                    }
                    codegen::project_json::Sb3PrimitiveBlock::Broadcast { name, id }
                    | codegen::project_json::Sb3PrimitiveBlock::Variable {
                        name,
                        id,
                        x: _,
                        y: _,
                    }
                    | codegen::project_json::Sb3PrimitiveBlock::List {
                        name,
                        id,
                        x: _,
                        y: _,
                    } => Sb3FieldValue::WithId {
                        value: Sb3Primitive::String(name.clone()),
                        id: id.clone(),
                    },
                }
            }
            KnownBlock::Callable(..)
            | KnownBlock::PartialCallable(..)
            | KnownBlock::CustomBlock { .. }
            | KnownBlock::Empty => {
                // TODO: warn user about probably incorrect usage
                Sb3FieldValue::Normal("".into())
            }
            KnownBlock::SingletonReporter {
                opcode: _,
                params: _,
                field,
                assign: _,
                bind_info: _,
            } => {
                // TODO: warn user about incorrect usage if no field supplied
                field
                    .clone()
                    .unwrap_or_else(|| codegen::project_json::Sb3FieldValue::Normal("".into()))
            }
        }
    }

    fn resolve_for_call_block<'a>(
        &'a self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> CallableKnownBlockSignature<'a> {
        const NO_PARTIALS: &Vec<(CallBlockParam, KnownBlock)> = &Vec::new();
        match self {
            KnownBlock::Callable(opcode, params) => {
                CallableKnownBlockSignature(opcode, params, NO_PARTIALS, None)
            }
            KnownBlock::PartialCallable(opcode, values, params) => {
                CallableKnownBlockSignature(opcode, params, values, None)
            }
            KnownBlock::SingletonReporter { opcode, params, .. } => CallableKnownBlockSignature(
                opcode,
                static_ref_of_const!(Vec::new(), Vec<CallBlockParam>),
                params,
                None,
            ),
            KnownBlock::CustomBlock {
                proccode,
                call_params,
                params,
                is_warp,
            } => CallableKnownBlockSignature(
                static_ref_of_const!(literal!("procedures_call"), IString),
                call_params,
                NO_PARTIALS,
                Some((proccode, params, is_warp)),
            ),
            KnownBlock::Variable { .. }
            | KnownBlock::List { .. }
            | KnownBlock::FieldValue { .. }
            | KnownBlock::BlockRef { .. }
            | KnownBlock::PrimitiveBlock { .. }
            | KnownBlock::Empty => todo!(), // warn user about incorrect usage
        }
    }

    fn resolve_for_assignment<'a>(
        &'a self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
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
    }
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Namespace(HashMap<IString, Rc<RefCell<Symbol>>>, Weak<RefCell<Symbol>>),
    KnownBlock(
        Box<KnownBlock>,
        HashMap<IString, Rc<RefCell<Symbol>>>,
        Weak<RefCell<Symbol>>,
    ),
    Alias(Weak<RefCell<Symbol>>, Weak<RefCell<Symbol>>),
}

impl Symbol {
    pub fn new_namespace() -> Self {
        Self::Namespace(HashMap::new(), Weak::new())
    }

    pub fn get_block(&self) -> Option<&KnownBlock> {
        match self {
            Symbol::KnownBlock(known_block, ..) => Some(known_block),
            Symbol::Namespace(..) | Symbol::Alias(..) => None,
        }
    }

    pub fn unalias(this: &Rc<RefCell<Self>>) -> Option<Rc<RefCell<Self>>> {
        match &*this.borrow() {
            Symbol::Namespace(..) | Symbol::KnownBlock(..) => Some(this.clone()),
            Symbol::Alias(weak, _) => weak.upgrade().and_then(|this| Symbol::unalias(&this)),
        }
    }

    pub fn get_child(&self, child_name: &IString) -> Option<Rc<RefCell<Symbol>>> {
        match self {
            Symbol::Namespace(hash_map, ..) | Symbol::KnownBlock(_, hash_map, ..) => {
                hash_map.get(child_name).cloned()
            }
            Symbol::Alias(alias, ..) => alias
                .upgrade()
                .and_then(|value| value.borrow().get_child(child_name)),
        }
    }

    /// Clones the children and allocates them in a dedicated Vec
    pub fn get_children_cloned(&self) -> Vec<Rc<RefCell<Symbol>>> {
        match self {
            Symbol::Namespace(hash_map, ..) | Symbol::KnownBlock(_, hash_map, ..) => {
                hash_map.values().cloned().collect()
            }
            Symbol::Alias(alias, ..) => alias
                .upgrade()
                .map(|value| value.borrow().get_children_cloned())
                .unwrap_or_default(),
        }
    }

    /// Clones the children and keys and allocates them in a dedicated Vec
    pub fn get_entries_cloned(&self) -> Vec<(IString, Rc<RefCell<Symbol>>)> {
        match self {
            Symbol::Namespace(hash_map, ..) | Symbol::KnownBlock(_, hash_map, ..) => hash_map
                .iter()
                .map(|(key, value)| (key.clone(), value.clone()))
                .collect(),
            Symbol::Alias(alias, ..) => alias
                .upgrade()
                .map(|value| value.borrow().get_entries_cloned())
                .unwrap_or_default(),
        }
    }

    pub fn get_parent(&self) -> &Weak<RefCell<Symbol>> {
        match self {
            Symbol::Namespace(_, parent_ref)
            | Symbol::KnownBlock(_, _, parent_ref)
            | Symbol::Alias(_, parent_ref) => parent_ref,
        }
    }

    pub fn replace_parent(&mut self, parent: Weak<RefCell<Symbol>>) -> Weak<RefCell<Symbol>> {
        match self {
            Symbol::Namespace(_, parent_ref)
            | Symbol::KnownBlock(_, _, parent_ref)
            | Symbol::Alias(_, parent_ref) => std::mem::replace(parent_ref, parent),
        }
    }

    pub fn insert_child(
        this: &Rc<RefCell<Self>>,
        child_name: IString,
        child: Rc<RefCell<Symbol>>,
    ) -> Option<Rc<RefCell<Symbol>>> {
        child.borrow_mut().replace_parent(Rc::downgrade(this));
        match &mut *this.borrow_mut() {
            Symbol::Namespace(hash_map, ..) => hash_map.insert(child_name, child),
            Symbol::KnownBlock(_, hash_map, ..) => hash_map.insert(child_name, child),
            Symbol::Alias(alias, ..) => alias
                .upgrade()
                .and_then(|value| Self::insert_child(&value, child_name, child)),
        }
    }

    pub fn remove_child(
        this: &Rc<RefCell<Self>>,
        child_name: &IString,
    ) -> Option<Rc<RefCell<Symbol>>> {
        match &mut *this.borrow_mut() {
            Symbol::Namespace(hash_map, ..) => hash_map.remove(child_name),
            Symbol::KnownBlock(_, hash_map, ..) => hash_map.remove(child_name),
            Symbol::Alias(alias, ..) => alias
                .upgrade()
                .and_then(|value| Self::remove_child(&value, child_name)),
        }
    }

    pub fn clear_children(this: &Rc<RefCell<Self>>) {
        let alias = match &mut *this.borrow_mut() {
            Symbol::Namespace(hash_map, ..) | Symbol::KnownBlock(_, hash_map, ..) => {
                hash_map.clear();
                return;
            }
            Symbol::Alias(alias, ..) => alias.upgrade(),
        };
        if let Some(alias) = alias {
            Self::clear_children(&alias);
        }
    }

    // Whether this symbol is an alias
    pub fn is_alias(&self) -> bool {
        matches!(self, Symbol::Alias(..))
    }
}

#[derive(Debug, Clone)]
pub struct BroadcastDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub known_block: Option<Rc<RefCell<Symbol>>>,
}

impl BroadcastDescriptor {
    pub fn derive_symbol<T: Rng>(&self, rng: &mut T) -> Symbol {
        todo!()
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

    pub fn derive_symbol_and_attachment<T: Rng>(
        &self,
        rng: &mut T,
        namespace: &mut Namespace,
    ) -> Result<(Symbol, Option<codegen::project_json::TargetAttachment>), std::io::Error> {
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
                let arg_id = codegen::ids::generate_random_id(rng);
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
                Symbol::KnownBlock(
                    Box::new(KnownBlock::CustomBlock {
                        proccode: proccode.clone(),
                        call_params: call_params.clone(),
                        params: params.clone(),
                        is_warp: descriptor.is_warp,
                    }),
                    HashMap::new(),
                    Weak::new(),
                ),
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
            Ok((
                Symbol::KnownBlock(
                    Box::new(match self {
                        // TODO: implement specific known blocks here
                        TargetSymbolDescriptor::Costume(..)
                        | TargetSymbolDescriptor::Backdrop(..)
                        | TargetSymbolDescriptor::Sound(..) => KnownBlock::FieldValue {
                            value: codegen::project_json::Sb3FieldValue::Normal(
                                codegen::project_json::Sb3Primitive::String(value.clone()),
                            ),
                        },
                        _ => unreachable!(),
                    }),
                    HashMap::new(),
                    Weak::new(),
                ),
                match self {
                    // TODO: implement specific known blocks here
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
                                md5ext: format!("{}.{}", value, file_ext),
                                data_format: file_ext.to_string(),
                                bitmap_resolution: Some(1.0), // TODO: better default and more control
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
                                md5ext: format!("{}.{}", value, file_ext),
                                data_format: file_ext.to_string(),
                                rate: 48000.0, // TODO: better default and more control
                                sample_count: 1124,
                            },
                        ))
                    }
                    _ => unreachable!(),
                }, // TODO: add asset
            ))
        })
        .unwrap_or_else(|| {
            let id = codegen::ids::generate_random_id(rng);
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
                        Symbol::KnownBlock(
                            Box::new(KnownBlock::new_variable(canonical_name.clone(), id, None)),
                            HashMap::new(),
                            Weak::new(),
                        ),
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
                        Symbol::KnownBlock(
                            Box::new(KnownBlock::List {
                                canonical_name: canonical_name.clone(),
                                id,
                            }), // TODO: add list length as method
                            HashMap::new(),
                            Weak::new(),
                        ),
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

    pub fn get_field_name(&self) -> String {
        match self {
            Target::Sprite {
                name,
                canonical_name: _,
                symbols: _,
            } => name.to_string(),
            Target::Stage { symbols: _ } => "_stage_".to_string(), // TODO: check whether this is correct
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

#[derive(Debug, Clone)]
pub struct ParseContext {
    // If stage is in this, it should be the first element
    pub parsed_targets: VecDeque<Target>,
    pub broadcasts: HashMap<IString, BroadcastDescriptor>,
    // pub root_symbol: ActualSymbol<'static>,
    pub next_target: Option<Target>,
    pub global_symbols: HashMap<IString, TargetSymbolDescriptor>,
    pub random_seed: <Xoshiro256StarStar as SeedableRng>::Seed,
}

impl ParseContext {
    pub fn new() -> Self {
        Self {
            parsed_targets: VecDeque::from([Target::Stage {
                symbols: HashMap::new(),
            }]),
            broadcasts: HashMap::new(),
            // root_symbol: ActualSymbol::Namespace(HashMap::from([(
            //     literal!("sprites"),
            //     Rc::new(RefCell::new(ActualSymbol::Sprites)),
            // )])),
            global_symbols: HashMap::new(),
            next_target: None,
            random_seed: Default::default(),
        }
    }

    // pub fn get_stage_mut(&mut self) -> &mut Target {
    //     if matches!(self.next_target, Some(Target::Stage { symbols: _ })) {
    //         return self.next_target.as_mut().unwrap();
    //     }
    //     &mut self.parsed_targets[0]
    // }
}

impl Default for ParseContext {
    fn default() -> Self {
        Self::new()
    }
}
