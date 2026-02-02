use std::{collections::HashMap};
use serde::{Deserialize, Serialize};

type SymbolContent = ();
type ActualNameType = String;
type OriginalNameType = String;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Namespace {
    pub assigned_names: HashMap<OriginalNameType, ActualNameType>,
    pub used_names: HashMap<ActualNameType, OriginalNameType>,
    pub symbol_contents: HashMap<OriginalNameType, SymbolContent>
}

impl Namespace {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Namespace {
    pub fn get_name_for(&mut self, original_name: OriginalNameType) -> ActualNameType {
        if let Some(name) = self.assigned_names.get(&original_name) {
            return name.to_string()
        }
        let mut num = 2;
        while self.used_names.contains_key(&format!("{}_{}", original_name, num)) {
            num += 1;
        }
        let name = format!("{}_{}", original_name, num);
        self.assign_name_for(original_name, name)
    }
    /** Panics when reassigning or reusing a name. */
    pub fn assign_name_for(&mut self, original_name: OriginalNameType, name: ActualNameType) -> ActualNameType {
        if self.used_names.contains_key(&name) {
            panic!("Name \"{}\" is not unique in this namespace.", name);
        }
        if self.assigned_names.contains_key(&original_name) {
            panic!("Name \"{}\" has already been assigned a name in this namespace.", name);
        }
        self.assigned_names.insert(original_name.clone(), name.clone());
        self.used_names.insert(name.clone(), original_name);
        name
    }
}
