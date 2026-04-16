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

fn rc_as_usize<T>(rc: &Rc<T>) -> usize {
    std::rc::Rc::<T>::as_ptr(rc) as usize
}

pub fn convert_generated_library(
    library: HashMap<String, LibraryItem>,
) -> impl Iterator<Item = (IString, Rc<RefCell<Symbol>>)> {
    pub fn recursively_convert(
        namespace: LibraryItem,
        aliases: &mut HashMap<usize, Vec<AliasSegment>>,
    ) -> Rc<RefCell<Symbol>> {
        let mut alias_content = None::<Vec<AliasSegment>>;
        let my_symbol = Rc::new(RefCell::new(match namespace.value {
            Some(LibraryItemValue::Alias(alias)) => {
                alias_content.replace(alias);
                Symbol::Alias(Weak::new(), Weak::new())
            }
            Some(LibraryItemValue::KnownBlock(known_block)) => {
                Symbol::KnownBlock(known_block, HashMap::new(), Weak::new())
            }
            None => Symbol::new_namespace(),
        }));
        if let Some(alias_content) = alias_content {
            aliases.insert(rc_as_usize(&my_symbol), alias_content);
        }
        for (child_name, child) in namespace.namespace {
            let child = recursively_convert(child, aliases);
            Symbol::insert_child(&my_symbol, child_name.into(), child);
        }
        my_symbol
    }
    pub fn recursively_convert_aliases(
        symbol: Rc<RefCell<Symbol>>,
        aliases: &mut HashMap<usize, Vec<AliasSegment>>,
        symbol_stack: &mut Vec<Rc<RefCell<Symbol>>>,
    ) {
        if let Some(alias) = aliases.remove(&rc_as_usize(&symbol))
            && let Symbol::Alias(target, _) = symbol.borrow_mut().deref_mut()
        {
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
            *target = Rc::downgrade(&current);
        }
        symbol_stack.push(symbol.clone());
        for child in symbol.borrow().get_children_cloned() {
            recursively_convert_aliases(child, aliases, symbol_stack);
            symbol_stack.pop().unwrap();
        }
    }
    let root = Rc::new(RefCell::new(Symbol::new_namespace()));
    let mut aliases = HashMap::new();
    library.into_iter().for_each(|(name, namespace)| {
        Symbol::insert_child(
            &root,
            name.as_str().into(),
            recursively_convert(namespace, &mut aliases),
        );
    });
    let mut symbol_stack = Vec::new();
    recursively_convert_aliases(root, &mut aliases, &mut symbol_stack);
    let root = Rc::try_unwrap(symbol_stack.pop().unwrap())
        .unwrap()
        .into_inner();
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
