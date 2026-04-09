use arcstr::ArcStr as IString;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

type ActualName = String;
type OriginalName = IString;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Namespace {
    pub used_names: HashMap<ActualName, OriginalName>,
}

impl Namespace {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Namespace {
    // pub fn get_name_for(&mut self, original_name: OriginalName) -> ActualName {
    //     if let Some(name) = self.assigned_names.get(&original_name) {
    //         return name.to_string()
    //     }
    //     let mut num = 2;
    //     while self.used_names.contains_key(&format!("{}_{}", original_name, num)) {
    //         num += 1;
    //     }
    //     let name = format!("{}_{}", original_name, num);
    //     self.assign_name_for(original_name, name)
    // }
    pub fn introduce_new_symbol(
        &mut self,
        canonical_name: Option<ActualName>,
        name: OriginalName,
    ) -> ActualName {
        if let Some(canonical_name) = canonical_name {
            return self.assign_name_for(name, canonical_name);
        }
        self.introduce_new_name(name)
    }
    /** Panics when reusing a name. */
    pub fn assign_name_for(&mut self, original_name: OriginalName, name: ActualName) -> ActualName {
        if self.used_names.contains_key(&name) {
            panic!("Name \"{}\" is not unique in this namespace.", name);
        }
        self.used_names.insert(name.clone(), original_name);
        name
    }
    pub fn introduce_new_name(&mut self, original_name: OriginalName) -> ActualName {
        let mut converted_name = original_name.to_string();
        if !self.used_names.contains_key(&converted_name) {
            return self.assign_name_for(original_name, converted_name);
        }
        let mut num = 2;
        let name = loop {
            let name = format!("{}_{}", original_name, num);
            if !self.used_names.contains_key(&name) {
                break name;
            }
            num += 1;
        };
        self.assign_name_for(original_name, name)
    }
}
