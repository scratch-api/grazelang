use std::{
    collections::{HashMap, VecDeque},
    vec,
};

use arcstr::{ArcStr as IString};
use serde::{Deserialize, Serialize};

type IDString = IString;

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
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BackdropDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub id: Option<IDString>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SoundDescriptor {
    pub name: IString,
    pub canonical_name: Option<IString>,
    pub id: Option<IDString>,
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
    pub fn borrow_symbols_mut<'a>(&'a mut self) -> &'a mut HashMap<IString, TargetSymbolDescriptor> {
        match self {
            Target::Sprite {
                name: _,
                canonical_name: _,
                symbols,
            } => {
                symbols
            }
            Target::Stage { symbols } => {
                symbols
            }
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
    pub broadcasts: Vec<BroadcastDescriptor>,
    pub top_level_symbols: HashMap<IString, TopLevelSymbol>,
    pub next_target: Option<Target>,
}

impl ParseContext {
    pub fn new() -> Self {
        Self {
            parsed_targets: VecDeque::from([Target::Stage { symbols: HashMap::new() }]),
            broadcasts: vec![],
            top_level_symbols: HashMap::new(),
            next_target: None,
        }
    }

    pub fn get_stage_mut<'a>(&'a mut self) -> &'a mut Target {
        if matches!(self.next_target, Some(Target::Stage { symbols: _ })) {
            return self.next_target.as_mut().unwrap()
        }
        &mut self.parsed_targets[0]
    }
}
