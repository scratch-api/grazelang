use std::collections::{HashMap, VecDeque};

use arcstr::ArcStr as IString;
use rand::SeedableRng;
use rand_xoshiro::Xoshiro256StarStar;
use serde::{Deserialize, Serialize};

use crate::parser::ast::Literal;

pub type IDString = IString;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Primitive {
    String(IString),
    Number(IString),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub id: Option<IDString>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub id: Option<IDString>,
}

type CustomBlockDescriptor = ();

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CostumeDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub id: Option<IDString>,
    pub source: Literal,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BackdropDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub id: Option<IDString>,
    pub source: Literal,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SoundDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub id: Option<IDString>,
    pub source: Literal,
}

type TopLevelSymbol = ();

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BroadcastDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub id: Option<IDString>,
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
    pub fn assign_id(&mut self, new_id: Option<IDString>) {
        match self {
            TargetSymbolDescriptor::Var(descriptor) => {
                descriptor.id = new_id;
            }
            TargetSymbolDescriptor::List(descriptor) => {
                descriptor.id = new_id;
            }
            TargetSymbolDescriptor::CustomBlockDescriptor(descriptor) => {

                // TODO
            }
            TargetSymbolDescriptor::Costume(descriptor) => {
                descriptor.id = new_id;
            }
            TargetSymbolDescriptor::Backdrop(descriptor) => {
                descriptor.id = new_id;
            }
            TargetSymbolDescriptor::Sound(descriptor) => {
                descriptor.id = new_id;
            }
        }
    }

    pub fn compute_hash(path: IString) -> IDString {
        todo!()
    }
    
    pub fn derive_id_if_asset(&self) -> Option<IDString> {
        match self {
            TargetSymbolDescriptor::Costume(descriptor) => {
                Some(&descriptor.source)
            }
            TargetSymbolDescriptor::Backdrop(descriptor) => {
                Some(&descriptor.source)
            }
            TargetSymbolDescriptor::Sound(descriptor) => {
                Some(&descriptor.source)
            }
            _ => None
        }.map(|value| Self::compute_hash(value.cast_to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
    #[inline]
    pub fn borrow_symbols_mut<'a>(
        &'a mut self,
    ) -> &'a mut HashMap<IString, TargetSymbolDescriptor> {
        match self {
            Target::Sprite {
                name: _,
                canonical_name: _,
                symbols,
            } => symbols,
            Target::Stage { symbols } => symbols,
        }
    }
}

impl From<Target> for RawTarget {
    fn from(value: Target) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RawTarget {}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParseContext {
    // If stage is in this, it must be the first element
    pub parsed_targets: VecDeque<Target>,
    pub broadcasts: HashMap<IString, BroadcastDescriptor>,
    pub top_level_symbols: HashMap<IString, TopLevelSymbol>,
    pub next_target: Option<Target>,
    pub random_seed: <Xoshiro256StarStar as SeedableRng>::Seed,
}

impl ParseContext {
    pub fn new() -> Self {
        Self {
            parsed_targets: VecDeque::from([Target::Stage {
                symbols: HashMap::new(),
            }]),
            broadcasts: HashMap::new(),
            top_level_symbols: HashMap::new(),
            next_target: None,
            random_seed: Default::default(),
        }
    }

    pub fn get_stage_mut<'a>(&'a mut self) -> &'a mut Target {
        if matches!(self.next_target, Some(Target::Stage { symbols: _ })) {
            return self.next_target.as_mut().unwrap();
        }
        &mut self.parsed_targets[0]
    }
}
