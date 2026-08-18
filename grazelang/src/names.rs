use arcstr::ArcStr as IString;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

type ActualName = String;
type OriginalName = IString;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Namespace {
    pub used_names: HashMap<ActualName, OriginalName>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct IStringNamespace {
    pub used_names: HashMap<IString, IString>,
}

// TODO: Better names for these different types of namespaces
// Issue: #95

impl Namespace {
    pub fn new() -> Self {
        Self::default()
    }
}

impl IStringNamespace {
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
    pub fn assign_name_for(&mut self, original_name: OriginalName, name: ActualName) -> ActualName {
        // Disabled to prevent panics. There is also a warning for this scenario.
        // if self.used_names.contains_key(&name) {
        //     panic!("Name \"{}\" is not unique in this namespace.", name);
        // }
        self.used_names.insert(name.clone(), original_name);
        name
    }
    pub fn introduce_new_name(&mut self, original_name: OriginalName) -> ActualName {
        use std::fmt::Write;
        let converted_name = original_name.to_string();
        if !self.used_names.contains_key(&converted_name) {
            return self.assign_name_for(original_name, converted_name);
        }
        let mut num = 2;
        let mut name = String::with_capacity(original_name.len() + 2);
        let name = loop {
            name.clear();
            write!(name, "{original_name}_{num}").unwrap();
            if !self.used_names.contains_key(&name) {
                break name;
            }
            num += 1;
        };
        self.assign_name_for(original_name, name)
    }
}

impl IStringNamespace {
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
        canonical_name: Option<IString>,
        name: IString,
    ) -> IString {
        if let Some(canonical_name) = canonical_name {
            return self.assign_name_for(name, canonical_name);
        }
        self.introduce_new_name(name)
    }

    pub fn assign_name_for(&mut self, original_name: IString, name: IString) -> IString {
        // Disabled to prevent panics. There is also a warning for this scenario.
        // if self.used_names.contains_key(&name) {
        //     panic!("Name \"{}\" is not unique in this namespace.", name);
        // }
        self.used_names.insert(name.clone(), original_name);
        name
    }

    pub fn convert_to_snake_case(name: IString) -> IString {
        let (alphanumeric, uppercase) = name.chars().fold((true, true), |(a, b), c| {
            (
                a && (c.is_ascii_alphanumeric() || c == '_'),
                b && c.is_uppercase(),
            )
        });
        if alphanumeric {
            return name;
        }
        let mut new_name = String::with_capacity(name.len());
        if uppercase {
            let mut alphanumeric = name
                .chars()
                .next()
                .map(|c| c.is_ascii_alphanumeric())
                .unwrap_or(true);
            for c in name.chars() {
                if c.is_ascii_alphanumeric() {
                    new_name.push(c);
                    alphanumeric = true;
                } else if alphanumeric {
                    alphanumeric = false;
                    new_name.push('_');
                }
            }
        } else {
            let (mut alphanumeric, mut uppercase) = name
                .chars()
                .next()
                .map(|c| (c.is_ascii_alphanumeric(), c.is_ascii_uppercase()))
                .unwrap_or((true, false));
            for c in name.chars() {
                if c.is_ascii_alphanumeric() {
                    if !uppercase && c.is_ascii_uppercase() {
                        new_name.push('_');
                    }
                    new_name.push(c.to_ascii_lowercase());
                    alphanumeric = true;
                    uppercase = c.is_ascii_uppercase();
                } else if alphanumeric {
                    uppercase = true;
                    alphanumeric = true;
                    new_name.push('_');
                }
            }
        }
        new_name.into()
    }

    pub fn introduce_new_name(&mut self, original_name: IString) -> IString {
        let original_name = Self::convert_to_snake_case(original_name);
        if !self.used_names.contains_key(&original_name) {
            return self.assign_name_for(original_name.clone(), original_name);
        }
        let mut num = 2;
        let name = loop {
            let name = format!("{}_{}", original_name, num);
            if !self.used_names.contains_key(name.as_str()) {
                break name.into();
            }
            num += 1;
        };
        self.assign_name_for(original_name, name)
    }
}

type NameIdentifier = String;
type BidirectionalNamespaceOriginalName = String;
type BidirectionalNamespaceActualName = IString;

#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
pub struct BidirectionalNamespace {
    pub used_names: HashMap<BidirectionalNamespaceActualName, BidirectionalNamespaceOriginalName>,
    pub assigned_names: HashMap<NameIdentifier, BidirectionalNamespaceActualName>,
}

impl BidirectionalNamespace {
    pub fn new() -> Self {
        Self::default()
    }
}

impl BidirectionalNamespace {
    pub fn get_symbol(
        &mut self,
        original_name: &str,
        name_identifier: &str,
    ) -> BidirectionalNamespaceActualName {
        use std::fmt::Write;
        if let Some(actual_name) = self.assigned_names.get(name_identifier) {
            return actual_name.clone();
        }
        if !self.used_names.contains_key(original_name) {
            let actual_name = original_name.into();
            return self
                .assign_name(
                    original_name.to_string(),
                    name_identifier.to_string(),
                    actual_name,
                )
                .clone();
        }
        let mut actual_name = String::with_capacity(original_name.len() + 2);
        let mut i = 2;
        loop {
            actual_name.clear();
            write!(&mut actual_name, "{original_name}_{i}").unwrap();
            if !self.used_names.contains_key(actual_name.as_str()) {
                return self
                    .assign_name(
                        original_name.to_string(),
                        name_identifier.to_string(),
                        actual_name.into(),
                    )
                    .clone();
            }
            i += 1;
        }
    }

    fn assign_name(
        &mut self,
        original_name: BidirectionalNamespaceOriginalName,
        name_identifier: NameIdentifier,
        actual_name: BidirectionalNamespaceActualName,
    ) -> &BidirectionalNamespaceActualName {
        self.used_names.insert(actual_name.clone(), original_name);
        self.assigned_names
            .entry(name_identifier)
            .or_insert(actual_name)
    }
}
