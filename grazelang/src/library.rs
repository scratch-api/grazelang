use std::{
    cell::RefCell,
    collections::HashMap,
    ops::DerefMut,
    rc::{Rc, Weak},
};

use arcstr::ArcStr as IString;
use grazelang_library::{AliasSegment, LibraryItem, LibraryItemValue};
use grazelang_library_parser::generate_library;

use crate::parser::parse_context::Symbol;

pub fn get_generated_library() -> HashMap<String, LibraryItem> {
    generate_library!("schemas/toolbox_schema.json")
}

pub fn convert_generated_library(
    library: HashMap<String, LibraryItem>,
) -> impl Iterator<Item = (IString, Rc<RefCell<Symbol>>)> {
    pub fn recursively_convert(
        namespace: LibraryItem,
        aliases: &mut Vec<(Rc<RefCell<Symbol>>, Vec<AliasSegment>)>,
    ) -> Rc<RefCell<Symbol>> {
        let (my_symbol, alias_content) = match namespace.value {
            Some(LibraryItemValue::Alias(alias)) => (
                Rc::new(RefCell::new(Symbol::Alias(Weak::new(), Weak::new()))),
                Some(alias),
            ),
            Some(LibraryItemValue::KnownBlock(known_block)) => (
                Rc::new(RefCell::new(Symbol::KnownBlock(
                    known_block,
                    HashMap::new(),
                    Weak::new(),
                ))),
                None,
            ),
            None => (Rc::new(RefCell::new(Symbol::new_namespace())), None),
        };
        if let Some(alias_content) = alias_content {
            aliases.push((my_symbol.clone(), alias_content));
        }
        for (child_name, child) in namespace.namespace {
            let child = recursively_convert(child, aliases);
            Symbol::insert_child(&my_symbol, child_name.into(), child);
        }
        my_symbol
    }
    let root = Rc::new(RefCell::new(Symbol::new_namespace()));
    let mut aliases = Vec::new();
    library.into_iter().for_each(|(name, namespace)| {
        Symbol::insert_child(
            &root,
            name.as_str().into(),
            recursively_convert(namespace, &mut aliases),
        );
    });
    for (alias_symbol, segments) in aliases {
        let mut current = alias_symbol.borrow().get_parent().upgrade().unwrap();
        for segment in segments {
            current = match segment {
                AliasSegment::Super => current.borrow().get_parent().upgrade().unwrap(),
                AliasSegment::Child(child) => current.borrow().get_child(&child.into()).unwrap(),
            }
        }
        if let Symbol::Alias(target, _) = alias_symbol.borrow_mut().deref_mut() {
            *target = Rc::downgrade(&current);
        }
    }
    let root = Rc::try_unwrap(root).unwrap().into_inner();
    match root {
        Symbol::Namespace(namespace, _) => namespace.into_iter(),
        _ => unreachable!(),
    }
}

/// Output is not guaranteed to be correct
pub fn get_standard_library_namespace_count() -> usize {
    10
}

pub fn get_standard_library_namespaces() -> impl Iterator<Item = (IString, Rc<RefCell<Symbol>>)> {
    convert_generated_library(get_generated_library())
}
