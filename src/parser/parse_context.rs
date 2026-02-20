use std::collections::HashMap;

use serde::{Serialize, Deserialize};
use arcstr::ArcStr as IString;


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Primitive {
    String(IString),
    Number(IString),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Var {
    pub name: IString,
    pub initial_value: Primitive
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct List {
    pub name: IString,
    pub initial_value: Vec<Primitive>
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Target {
    Sprite {
        name: IString,
        vars: HashMap<String, Var>,
        lists: HashMap<String, List>,
    },
    Backdrop {
        vars: HashMap<String, Var>,
        lists: HashMap<String, List>,
    }
}

impl From<Target> for RawTarget {
    fn from(value: Target) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RawTarget {

}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParseContext {
    pub targets: Vec<Target>,
}

impl ParseContext {
    pub fn new() -> Self {
        Self {
            targets: vec![]
        }
    }
}
