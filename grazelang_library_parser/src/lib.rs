use std::{
    collections::{HashMap, HashSet},
    fs,
    path::Path,
};

use arcstr::{ArcStr as IString, literal};
use grazelang_library::{
    BACKDROP_TARGETS_CATEGORY_ID, BACKDROPS_CATEGORY_ID, BROADCASTS_CATEGORY_ID,
    CLONABLES_CATEGORY_ID, COLLIDERS_CATEGORY_ID, COSTUMES_CATEGORY_ID, DESTINATIONS_CATEGORY_ID,
    DIRECTIONS_CATEGORY_ID, LISTS_CATEGORY_ID, LOCATIONS_CATEGORY_ID, LibraryItem, NO_CATEGORY_ID,
    OBJECTS_CATEGORY_ID, PEN_PROPERTIES_CATEGORY_ID, PROPERTIES_CATEGORY_ID, SOUNDS_CATEGORY_ID,
    VARIABLES_CATEGORY_ID,
};
use proc_macro::TokenStream;
use quote::quote;
use sha3::{Digest, Sha3_256};
use syn::{LitStr, parse_macro_input};

use crate::parser::{LibraryCache, merge_associated_item, process_toolbox_category};
mod parser;

macro_rules! implement_generate_library {
    ($input:ident, $use_cache:ident, $create_cache:ident) => {{
        let input_lit = parse_macro_input!($input as LitStr);
        let relative_path = input_lit.value();

        let manifest_dir =
            std::env::var("CARGO_MANIFEST_DIR").expect("Failed to get CARGO_MANIFEST_DIR");
        let full_path = Path::new(&manifest_dir).join(&relative_path);

        let json_str = fs::read_to_string(&full_path)
            .unwrap_or_else(|_| panic!("Failed to read file at {:?}", full_path));

        let (hex_hash, output_cache_path) = implement_create_hash_and_cache_path!(
            manifest_dir,
            relative_path,
            json_str,
            $use_cache,
            $create_cache
        );
        implement_use_cache!(output_cache_path, hex_hash, $use_cache);

        let v: Vec<crate::parser::ToolboxCategory> =
            serde_json::from_str(&json_str).expect("Failed to parse JSON");

        let mut library = HashMap::with_capacity(10);
        let mut menus = HashMap::new();
        let mut menu_category_ids = HashMap::<IString, u32>::from([
            (literal!(""), NO_CATEGORY_ID),
            (literal!("variables"), VARIABLES_CATEGORY_ID),
            (literal!("lists"), LISTS_CATEGORY_ID),
            (literal!("broadcasts"), BROADCASTS_CATEGORY_ID),
            (literal!("costumes"), COSTUMES_CATEGORY_ID),
            (literal!("backdrops"), BACKDROPS_CATEGORY_ID),
            (literal!("backdrop_targets"), BACKDROP_TARGETS_CATEGORY_ID),
            (literal!("sounds"), SOUNDS_CATEGORY_ID),
            (literal!("destinations"), DESTINATIONS_CATEGORY_ID),
            (literal!("directions"), DIRECTIONS_CATEGORY_ID),
            (literal!("clonables"), CLONABLES_CATEGORY_ID),
            (literal!("colliders"), COLLIDERS_CATEGORY_ID),
            (literal!("locations"), LOCATIONS_CATEGORY_ID),
            (literal!("properties"), PROPERTIES_CATEGORY_ID),
            (literal!("objects"), OBJECTS_CATEGORY_ID),
            (literal!("pen_properties"), PEN_PROPERTIES_CATEGORY_ID),
        ]);
        let mut category_entries = HashMap::<u32, HashSet<String>>::new();

        for namespace in v {
            let (category_name, category, associated_menus) = process_toolbox_category(namespace, &mut category_entries, &mut menu_category_ids);
            for (key, value) in associated_menus {
                match menus.entry(key) {
                    std::collections::hash_map::Entry::Vacant(v) => {
                        v.insert(value);
                    }
                    std::collections::hash_map::Entry::Occupied(mut o) => {
                        merge_associated_item(o.get_mut(), value);
                    }
                }
            }
            library.insert(category_name, category);
        }

        library.insert("menus".to_string(), LibraryItem {
            namespace: menus,
            value: None,
        });

        let library_keys = library.keys();
        let library_values = library.values();

        let category_entry_stream = expand_category_entries(category_entries);

        let expanded = quote! {
            (::std::collections::HashMap::from([#( (#library_keys.to_string(), #library_values) ),*]), #category_entry_stream)
        };

        implement_create_cache!(output_cache_path, hex_hash, expanded, $create_cache);

        TokenStream::from(expanded)
    }};
}

fn expand_category_entries(
    category_entries: HashMap<u32, HashSet<String>>,
) -> proc_macro2::TokenStream {
    let keys = category_entries.keys();
    let mut values = Vec::with_capacity(category_entries.len());
    for value in category_entries.values() {
        let value = value.iter();
        values.push(quote! {
            ::std::collections::HashSet::from([#( ::arcstr::literal!(#value) ),*])
        });
    }
    quote! {
        ::std::collections::HashMap::from([#( (#keys, #values) ),*])
    }
}

macro_rules! implement_create_hash_and_cache_path {
    ($dir:expr, $rel_path:expr, $json_str:expr, yes) => {
        (
            {
                let hash = Sha3_256::digest($json_str.as_bytes());
                base16ct::lower::encode_string(hash.as_slice())
            },
            Path::new(&$dir).join(&($rel_path + ".out_cached.json")),
        )
    };
    ($dir:expr, $rel_path:expr, $json_str:expr, no_use_cache, no_create_cache) => {
        ((), ())
    };
    ($dir:expr, $rel_path:expr, $json_str:expr, use_cache, no_create_cache) => {
        implement_create_hash_and_cache_path!($dir, $rel_path, $json_str, yes)
    };
    ($dir:expr, $rel_path:expr, $json_str:expr, no_use_cache, create_cache) => {
        implement_create_hash_and_cache_path!($dir, $rel_path, $json_str, yes)
    };
    ($dir:expr, $rel_path:expr, $json_str:expr, use_cache, create_cache) => {
        implement_create_hash_and_cache_path!($dir, $rel_path, $json_str, yes)
    };
}

macro_rules! implement_use_cache {
    ($path:expr, $hex_hash:expr, use_cache) => {
        if $path.is_file() {
            let output_json_str = fs::read_to_string(&$path)
                .unwrap_or_else(|_| panic!("Failed to read file at {:?}", $path));

            if let Ok(cache) = serde_json::from_str::<LibraryCache>(&output_json_str)
                && cache.hash == $hex_hash
            {
                return cache.value.parse().unwrap();
            }
        }
    };
    ($path:expr, $hex_hash:expr, no_use_cache) => {};
}

macro_rules! implement_create_cache {
    ($path:expr, $hex_hash:expr, $expanded:expr, create_cache) => {
        fs::write(
            $path,
            serde_json::to_string(&LibraryCache {
                hash: $hex_hash,
                value: $expanded.to_string(),
            })
            .unwrap(),
        )
        .unwrap();
    };
    ($path:expr, $hex_hash:expr, $expanded:expr, no_create_cache) => {};
}

#[proc_macro]
pub fn generate_library(input: TokenStream) -> TokenStream {
    implement_generate_library!(input, use_cache, create_cache)
}

#[proc_macro]
#[expect(unused_variables)]
pub fn generate_library_no_cache(input: TokenStream) -> TokenStream {
    implement_generate_library!(input, no_use_cache, no_create_cache)
}

#[proc_macro]
pub fn generate_library_no_create_cache(input: TokenStream) -> TokenStream {
    implement_generate_library!(input, use_cache, no_create_cache)
}

#[proc_macro]
pub fn generate_library_no_use_cache(input: TokenStream) -> TokenStream {
    implement_generate_library!(input, no_use_cache, create_cache)
}
