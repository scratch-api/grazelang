use std::collections::HashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Namespace {
    pub assigned_names: HashMap<String, String>,
    pub used_names: HashMap<String, String>
}

impl Namespace {
    pub fn get_name_for(&mut self, original_name: String) -> String {
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
    pub fn assign_name_for(&mut self, original_name: String, name: String) -> String {
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
