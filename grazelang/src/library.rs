use std::{cell::RefCell, collections::HashMap, rc::Rc};

use arcstr::ArcStr as IString;
use grazelang_library::{LibraryItem, LibraryItemValue};
use grazelang_library_parser::generate_library;

use crate::parser::parse_context::Symbol;

pub fn get_generated_library() -> HashMap<String, LibraryItem> {
    generate_library!("schemas/test_schema.json")
}

pub fn convert_generated_library(
    library: HashMap<String, LibraryItem>,
) -> impl Iterator<Item = (IString, Rc<RefCell<Symbol>>)> {
    pub fn recursively_convert(
        namespace: LibraryItem,
        symbol_stack: &mut Vec<Rc<RefCell<Symbol>>>,
    ) {
        let my_symbol = Rc::new(RefCell::new(match namespace.value {
            Some(LibraryItemValue::Alias(alias)) => {
                let mut current = symbol_stack.last().unwrap().clone();
                for segment in alias {
                    current = match segment {
                        grazelang_library::AliasSegment::Super => {
                            current.borrow().get_parent().upgrade().unwrap()
                        }
                        grazelang_library::AliasSegment::Child(child) => {
                            current.borrow().get_child(&child.into()).unwrap()
                        }
                    }
                }
                Symbol::Alias(
                    Rc::downgrade(&current),
                    Rc::downgrade(symbol_stack.last().unwrap()),
                )
            }
            Some(LibraryItemValue::KnownBlock(known_block)) => Symbol::KnownBlock(
                known_block,
                None,
                Rc::downgrade(symbol_stack.last().unwrap()),
            ),
            None => Symbol::new_namespace(),
        }));
        symbol_stack.push(my_symbol.clone());
        for (child_name, child) in namespace.namespace {
            recursively_convert(child, symbol_stack);
            let child = symbol_stack.pop().unwrap();
            Symbol::insert_child(&my_symbol, child_name.into(), child);
        }
    }
    library.into_iter().map(|(name, namespace)| {
        let namespace_root = Rc::new(RefCell::new(Symbol::new_namespace()));
        let mut symbol_stack = vec![namespace_root];
        recursively_convert(namespace, &mut symbol_stack);
        (name.into(), symbol_stack.pop().unwrap())
    })
}

/// Output is not guaranteed to be correct
pub fn get_standard_library_namespace_count() -> usize {
    9
}

pub fn get_standard_library_namespaces() -> impl Iterator<Item = (IString, Rc<RefCell<Symbol>>)> {
    convert_generated_library(get_generated_library())
}
