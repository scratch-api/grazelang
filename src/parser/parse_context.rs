use core::hash;
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    hint,
    rc::{Rc, Weak},
};

use arcstr::{ArcStr as IString, literal};
use rand::{Rng, SeedableRng};
use rand_xoshiro::Xoshiro256StarStar;
use serde::{Deserialize, Serialize};

use crate::{
    codegen,
    parser::ast::{Identifier, Literal},
};

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
        canonical_name: IString,
        id: IdString,
    },
    List {
        canonical_name: IString,
        id: IdString,
    },
    /// Cannot be used for variables in the context of an input
    FieldValue {
        value: codegen::project_json::Sb3FieldValue,
    },
    BlockRef {
        id: IdString,
    },
    /// Intended for primitive expressions like strings but can be used variables etc aswell
    PrimitiveBlock {
        value: codegen::project_json::Sb3PrimitiveBlock,
    },
} // TODO: Implement menus and calling known blocks

impl KnownBlock {
    pub fn resolve_for_input(
        &self,
        context: &mut codegen::core::GrazeSb3GeneratorContext,
    ) -> codegen::project_json::Sb3InputRepr {
        use codegen::project_json::{Sb3InputRepr, Sb3PrimitiveBlock};
        match self {
            KnownBlock::Variable { canonical_name, id } => {
                // TODO: possibly set x and y
                Sb3InputRepr::PrimitiveBlock(Sb3PrimitiveBlock::Variable {
                    name: canonical_name.to_string(),
                    id: id.to_string(),
                    x: None,
                    y: None,
                })
            }
            KnownBlock::List { canonical_name, id } => {
                // TODO: possibly set x and y
                Sb3InputRepr::PrimitiveBlock(Sb3PrimitiveBlock::List {
                    name: canonical_name.to_string(),
                    id: id.to_string(),
                    x: None,
                    y: None,
                })
            }
            KnownBlock::FieldValue { value } => {
                // TODO: warn user about possibly incorrect usage
                Sb3InputRepr::PrimitiveBlock(Sb3PrimitiveBlock::String(
                    match value {
                        codegen::project_json::Sb3FieldValue::Normal(sb3_primitive) => {
                            sb3_primitive
                        }
                        codegen::project_json::Sb3FieldValue::WithId { value, id } => value,
                    }
                    .clone(),
                ))
            }
            KnownBlock::BlockRef { id } => Sb3InputRepr::Reference(id.to_string()),
            KnownBlock::PrimitiveBlock { value } => Sb3InputRepr::PrimitiveBlock(value.clone()),
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
                // TODO: warn user about possibly incorrect usage
                Sb3FieldValue::Normal(Sb3Primitive::String(id.to_string()))
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum ActualSymbol {
    Namespace(
        HashMap<IString, Rc<RefCell<ActualSymbol>>>,
        Weak<RefCell<ActualSymbol>>,
    ),
    KnownBlock(
        KnownBlock,
        Option<HashMap<IString, Rc<RefCell<ActualSymbol>>>>,
        Weak<RefCell<ActualSymbol>>,
    ),
    Sprites(Weak<RefCell<ActualSymbol>>),
    Alias(Rc<RefCell<ActualSymbol>>, Weak<RefCell<ActualSymbol>>),
}

impl ActualSymbol {
    pub fn new_namespace() -> Self {
        Self::Namespace(HashMap::new(), Weak::new())
    }

    pub fn get_block(&self) -> Option<&KnownBlock> {
        match self {
            ActualSymbol::KnownBlock(known_block, ..) => Some(known_block),
            ActualSymbol::Namespace(..) | ActualSymbol::Sprites(..) | ActualSymbol::Alias(..) => {
                None
            }
        }
    }

    pub fn get_child(&self, child_name: &IString) -> Option<Rc<RefCell<ActualSymbol>>> {
        match self {
            ActualSymbol::Namespace(hash_map, ..) => {
                hash_map.get(child_name).map(|value| value.clone())
            }
            ActualSymbol::KnownBlock(_, hash_map, ..) => hash_map
                .as_ref()
                .and_then(|value| value.get(child_name).map(|value| value.clone())),
            ActualSymbol::Alias(alias, ..) => alias
                .borrow()
                .get_child(child_name)
                .map(|value| value.clone()),
            ActualSymbol::Sprites(..) => None,
        }
    }

    pub fn replace_parent(
        &mut self,
        parent: Weak<RefCell<ActualSymbol>>,
    ) -> Weak<RefCell<ActualSymbol>> {
        match self {
            ActualSymbol::Namespace(_, parent_ref)
            | ActualSymbol::KnownBlock(_, _, parent_ref)
            | ActualSymbol::Sprites(parent_ref)
            | ActualSymbol::Alias(_, parent_ref) => std::mem::replace(parent_ref, parent),
        }
    }

    pub fn insert_child(
        this: &Rc<RefCell<Self>>,
        child_name: IString,
        child: Rc<RefCell<ActualSymbol>>,
    ) -> Option<Rc<RefCell<ActualSymbol>>> {
        child.borrow_mut().replace_parent(Rc::downgrade(this));
        match &mut *this.borrow_mut() {
            ActualSymbol::Namespace(hash_map, ..) => hash_map.insert(child_name, child),
            ActualSymbol::KnownBlock(_, hash_map, ..) => {
                hash_map.get_or_insert_default().insert(child_name, child)
            }
            ActualSymbol::Alias(alias, ..) => Self::insert_child(alias, child_name, child),
            ActualSymbol::Sprites(..) => None,
        }
    }

    pub fn is_dependent(&self) -> bool {
        matches!(self, ActualSymbol::Alias(..) | ActualSymbol::Sprites(..))
    }
}

#[derive(Debug, Clone)]
pub struct BroadcastDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub known_block: Option<Rc<RefCell<ActualSymbol>>>,
}

impl BroadcastDescriptor {
    pub fn derive_actual_symbol<T: Rng>(&self, rng: &mut T) -> ActualSymbol {
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
    pub fn compute_hash(path: IString) -> IdString {
        todo!()
    }

    pub fn derive_actual_symbol<T: Rng>(&self, rng: &mut T) -> ActualSymbol {
        if let TargetSymbolDescriptor::CustomBlockDescriptor(descriptor) = self {
            let id = descriptor
                .canonical_name
                .clone()
                .unwrap_or_else(|| descriptor.name.clone()); // TODO: use arguments
            return todo!(); // TODO: implement custom block actual symbol
        }
        match self {
            TargetSymbolDescriptor::Costume(descriptor) => Some(&descriptor.source),
            TargetSymbolDescriptor::Backdrop(descriptor) => Some(&descriptor.source),
            TargetSymbolDescriptor::Sound(descriptor) => Some(&descriptor.source),
            _ => None,
        }
        .map(|value| Self::compute_hash(value.cast_to_string()))
        .map(|value| {
            ActualSymbol::KnownBlock(
                match self {
                    TargetSymbolDescriptor::Costume(_) => {
                        todo!() // TODO: implement costume known blocks
                    }
                    TargetSymbolDescriptor::Backdrop(_) => {
                        todo!() // TODO: implement backdrop known blocks
                    }
                    TargetSymbolDescriptor::Sound(_) => {
                        todo!() // TODO: implement sound known blocks
                    }
                    _ => unsafe { std::hint::unreachable_unchecked() },
                },
                None,
                Weak::new()
            )
        })
        .unwrap_or_else(|| {
            let id = codegen::ids::generate_random_id(rng);
            match self {
                TargetSymbolDescriptor::Var(var_descriptor) => {
                    ActualSymbol::KnownBlock(todo!(), None, Weak::new())
                }
                TargetSymbolDescriptor::List(list_descriptor) => todo!(),
                TargetSymbolDescriptor::CustomBlockDescriptor(custom_block_descriptor) => todo!(),
                TargetSymbolDescriptor::Costume(_)
                | TargetSymbolDescriptor::Backdrop(_)
                | TargetSymbolDescriptor::Sound(_) => unsafe { hint::unreachable_unchecked() },
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
