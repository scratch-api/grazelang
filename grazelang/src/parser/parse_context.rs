use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{HashMap, VecDeque},
    io::Read,
    rc::{Rc, Weak},
};

use arcstr::{ArcStr as IString, literal};
use rand::{Rng, SeedableRng};
use rand_xoshiro::Xoshiro256StarStar;
use serde::{Deserialize, Serialize};

use crate::{codegen, names::Namespace, parser::ast::Literal};

pub type IdString = IString;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Primitive {
    String(IString),
    Number(IString),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CustomBlockDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CostumeDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub source: Literal,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BackdropDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub source: Literal,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SoundDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub source: Literal,
}

/// Resolved symbols or primitive expressions like strings for usage in inputs or fields
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum KnownBlock {
    Variable {
        canonical_name: String,
        id: IdString,
    },
    List {
        canonical_name: String,
        id: IdString,
    },
    /// Cannot be used for variables in the context of an input
    FieldValue {
        value: codegen::project_json::Sb3FieldValue,
    },
    BlockRef {
        id: IdString,
    },
    /// Intended for primitive expressions like strings but can be used for variables etc aswell
    PrimitiveBlock {
        value: codegen::project_json::Sb3PrimitiveBlock,
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
        field: Option<codegen::project_json::Sb3FieldValue>,
        assign: Option<SimpleCallableKnownBlockSignature>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum KnownBlockInput<'a> {
    PrimitiveInput(codegen::project_json::Sb3PrimitiveBlock),
    BlockRef(IdString),
    SimpleBlock(&'a IString, &'a Vec<(CallBlockParam, KnownBlock)>),
    Menu(codegen::project_json::Sb3FieldValue),
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
    pub Option<Vec<(CallBlockParam, KnownBlock)>>,
);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CallBlockParamKind {
    Input {
        default: Option<codegen::project_json::Sb3PrimitiveBlock>,
    },
    Field,
    MenuInput {
        opcode: IString,
        field_name: IString,
        default: codegen::project_json::Sb3FieldValue,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CallBlockParam {
    pub kind: CallBlockParamKind,
    pub name: IString,
}

impl KnownBlock {
    pub fn resolve_for_input<'a>(
        &'a self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> KnownBlockInput<'a> {
        use codegen::project_json::Sb3PrimitiveBlock;
        match self {
            KnownBlock::Variable { canonical_name, id } => {
                // TODO: possibly set x and y
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
            KnownBlock::Callable(..) | KnownBlock::PartialCallable(..) => {
                // TODO: warn user about probably incorrect usage
                KnownBlockInput::PrimitiveInput("".into())
            }
            KnownBlock::SingletonReporter {
                opcode,
                params,
                field: _,
                assign: _,
            } => KnownBlockInput::SimpleBlock(opcode, params),
        }
    }

    pub fn resolve_for_field(
        &self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> codegen::project_json::Sb3FieldValue {
        use codegen::project_json::{Sb3FieldValue, Sb3Primitive};
        match self {
            KnownBlock::Variable { canonical_name, id } => Sb3FieldValue::WithId {
                value: Sb3Primitive::String(canonical_name.to_string()),
                id: id.to_string(),
            },
            KnownBlock::List { canonical_name, id } => Sb3FieldValue::WithId {
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
            KnownBlock::Callable(..) | KnownBlock::PartialCallable(..) => {
                // TODO: warn user about probably incorrect usage
                Sb3FieldValue::Normal("".into())
            }
            KnownBlock::SingletonReporter {
                opcode: _,
                params: _,
                field,
                assign: _,
            } => {
                // TODO: warn user about incorrect usage if no field supplied
                field
                    .clone()
                    .unwrap_or_else(|| codegen::project_json::Sb3FieldValue::Normal("".into()))
            }
        }
    }

    pub fn resolve_for_call_block<'a>(
        &'a self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> CallableKnownBlockSignature<'a> {
        const EMPTY_KNOWN_PARAM_VEC: &Vec<(CallBlockParam, KnownBlock)> = &Vec::new();
        match self {
            KnownBlock::Callable(opcode, params) => {
                CallableKnownBlockSignature(opcode, params, EMPTY_KNOWN_PARAM_VEC)
            }
            KnownBlock::PartialCallable(opcode, values, params) => {
                CallableKnownBlockSignature(opcode, params, values)
            }
            KnownBlock::Variable { .. }
            | KnownBlock::List { .. }
            | KnownBlock::FieldValue { .. }
            | KnownBlock::BlockRef { .. }
            | KnownBlock::PrimitiveBlock { .. }
            | KnownBlock::SingletonReporter { .. } => todo!(), // warn user about incorrect usage
        }
    }

    pub fn resolve_for_assignment<'a>(
        &'a self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> Cow<'a, SimpleCallableKnownBlockSignature> {
        // TODO: is cow actually the right choice here?
        match self {
            KnownBlock::Variable { canonical_name, id } => {
                Cow::Owned(SimpleCallableKnownBlockSignature(
                    literal!("data_setvariableto"),
                    CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some("0".into()),
                        },
                        name: literal!("VALUE"),
                    },
                    Some(vec![(
                        CallBlockParam {
                            kind: CallBlockParamKind::Field,
                            name: literal!("VARIABLE"),
                        },
                        KnownBlock::FieldValue {
                            value: codegen::project_json::Sb3FieldValue::WithId {
                                value: (canonical_name as &str).into(),
                                id: id.to_string(),
                            },
                        },
                    )]),
                ))
            }
            KnownBlock::SingletonReporter {
                opcode: _,
                params: _,
                field: _,
                assign: Some(assign),
            } => Cow::Borrowed(assign),
            KnownBlock::List { .. }
            | KnownBlock::FieldValue { .. }
            | KnownBlock::BlockRef { .. }
            | KnownBlock::PrimitiveBlock { .. }
            | KnownBlock::Callable(..)
            | KnownBlock::PartialCallable(..)
            | KnownBlock::SingletonReporter { .. } => todo!(), // warn user about incorrect usage
        } // TODO: Implement assignment of x, y etc
    }
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Namespace(HashMap<IString, Rc<RefCell<Symbol>>>, Weak<RefCell<Symbol>>),
    KnownBlock(
        Box<KnownBlock>,
        Option<HashMap<IString, Rc<RefCell<Symbol>>>>,
        Weak<RefCell<Symbol>>,
    ),
    Sprites(Weak<RefCell<Symbol>>),
    Alias(Weak<RefCell<Symbol>>, Weak<RefCell<Symbol>>), // TODO: use Weak for Alias
}

impl Symbol {
    pub fn new_namespace() -> Self {
        Self::Namespace(HashMap::new(), Weak::new())
    }

    pub fn get_block(&self) -> Option<&KnownBlock> {
        match self {
            Symbol::KnownBlock(known_block, ..) => Some(known_block),
            Symbol::Namespace(..) | Symbol::Sprites(..) | Symbol::Alias(..) => None,
        }
    }

    pub fn get_child(&self, child_name: &IString) -> Option<Rc<RefCell<Symbol>>> {
        match self {
            Symbol::Namespace(hash_map, ..) => hash_map.get(child_name).cloned(),
            Symbol::KnownBlock(_, hash_map, ..) => hash_map
                .as_ref()
                .and_then(|value| value.get(child_name).cloned()),
            Symbol::Alias(alias, ..) => alias
                .upgrade()
                .and_then(|value| value.borrow().get_child(child_name)),
            Symbol::Sprites(..) => None,
        }
    }

    pub fn get_parent(&self) -> &Weak<RefCell<Symbol>> {
        match self {
            Symbol::Namespace(_, parent_ref)
            | Symbol::KnownBlock(_, _, parent_ref)
            | Symbol::Sprites(parent_ref)
            | Symbol::Alias(_, parent_ref) => parent_ref,
        }
    }

    pub fn replace_parent(&mut self, parent: Weak<RefCell<Symbol>>) -> Weak<RefCell<Symbol>> {
        match self {
            Symbol::Namespace(_, parent_ref)
            | Symbol::KnownBlock(_, _, parent_ref)
            | Symbol::Sprites(parent_ref)
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
            Symbol::KnownBlock(_, hash_map, ..) => {
                hash_map.get_or_insert_default().insert(child_name, child)
            }
            Symbol::Alias(alias, ..) => alias
                .upgrade()
                .and_then(|value| Self::insert_child(&value, child_name, child)),
            Symbol::Sprites(..) => None,
        }
    }

    pub fn is_dependent(&self) -> bool {
        matches!(self, Symbol::Alias(..) | Symbol::Sprites(..))
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
    pub fn compute_hash(path: IString) -> Result<String, std::io::Error> {
        use std::fs::File;
        let mut file = File::open(path.to_string())?;
        let mut data: Vec<u8> = Vec::new();
        file.read_to_end(&mut data)?;
        let digest = md5::compute(data);
        let hex_digest = format!("{:x}", digest);
        Ok(hex_digest)
    }

    pub fn derive_symbol<T: Rng>(
        &self,
        rng: &mut T,
        namespace: &mut Namespace,
    ) -> Result<Symbol, std::io::Error> {
        if let TargetSymbolDescriptor::CustomBlockDescriptor(descriptor) = self {
            let id = descriptor
                .canonical_name
                .clone()
                .unwrap_or_else(|| descriptor.name.clone()); // TODO: use arguments
            return todo!(); // TODO: implement custom block actual symbol
        }
        Ok(match self {
            TargetSymbolDescriptor::Costume(descriptor) => Some(&descriptor.source),
            TargetSymbolDescriptor::Backdrop(descriptor) => Some(&descriptor.source),
            TargetSymbolDescriptor::Sound(descriptor) => Some(&descriptor.source),
            _ => None,
        }
        .map(|value| Self::compute_hash(value.cast_to_string()))
        .transpose()?
        .map(|value| {
            Symbol::KnownBlock(
                Box::new(match self {
                    // TODO: implement specific known blocks here
                    TargetSymbolDescriptor::Costume(..)
                    | TargetSymbolDescriptor::Backdrop(..)
                    | TargetSymbolDescriptor::Sound(..) => KnownBlock::FieldValue {
                        value: codegen::project_json::Sb3FieldValue::Normal(codegen::project_json::Sb3Primitive::String(value)),
                    },
                    _ => unreachable!(),
                }),
                None,
                Weak::new(),
            )
        })
        .unwrap_or_else(|| {
            let id = codegen::ids::generate_random_id(rng);
            match self {
                TargetSymbolDescriptor::Var(var_descriptor) => Symbol::KnownBlock(
                    Box::new(KnownBlock::Variable {
                        canonical_name: namespace.introduce_new_symbol(
                            var_descriptor
                                .canonical_name
                                .as_ref()
                                .map(|value| value.to_string()),
                            var_descriptor.name.clone(),
                        ),
                        id,
                    }),
                    None,
                    Weak::new(),
                ),
                TargetSymbolDescriptor::List(list_descriptor) => Symbol::KnownBlock(
                    Box::new(KnownBlock::List {
                        canonical_name: namespace.introduce_new_symbol(
                            list_descriptor
                                .canonical_name
                                .as_ref()
                                .map(|value| value.to_string()),
                            list_descriptor.name.clone(),
                        ),
                        id,
                    }), // TODO: add list length as method
                    None,
                    Weak::new(),
                ),
                TargetSymbolDescriptor::CustomBlockDescriptor(custom_block_descriptor) => todo!(),
                TargetSymbolDescriptor::Costume(_)
                | TargetSymbolDescriptor::Backdrop(_)
                | TargetSymbolDescriptor::Sound(_) => unreachable!(),
            }
        }))
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

    pub fn get_namespace_name(&self) -> IString {
        match self {
            Target::Sprite {
                name,
                canonical_name: _,
                symbols: _,
            } => name.clone(),
            Target::Stage { symbols: _ } => literal!("stage"),
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
