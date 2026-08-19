use std::{
    collections::{HashMap, HashSet},
    fs,
    path::Path,
};

use arcstr::{ArcStr as IString, literal};
use grazelang_types::{
    BACKDROP_TARGETS_CATEGORY_ID, BACKDROPS_CATEGORY_ID, BROADCASTS_CATEGORY_ID,
    CLONABLES_CATEGORY_ID, COLLIDERS_CATEGORY_ID, COSTUMES_CATEGORY_ID, ConstantExprLibraryItem,
    DESTINATIONS_CATEGORY_ID, DIRECTIONS_CATEGORY_ID, INTEGERS_CATEGORY_ID, LISTS_CATEGORY_ID,
    LOCATIONS_CATEGORY_ID, LibraryItem, NO_CATEGORY_ID, OBJECTS_CATEGORY_ID,
    PEN_PROPERTIES_CATEGORY_ID, PROPERTIES_CATEGORY_ID, SOUNDS_CATEGORY_ID, VARIABLES_CATEGORY_ID,
    library_parser::{
        self, LibraryCache, merge_associated_item, process_constant_expr_toolbox_category,
        process_toolbox_category,
    },
};
use proc_macro::TokenStream;
use quote::quote;
use sha3::{Digest, Sha3_256};
use syn::{LitStr, parse_macro_input};

const NO_CATEGORY_STRING: &IString = &literal!("");
const VARIABLES_CATEGORY_STRING: &IString = &literal!("variables");
const LISTS_CATEGORY_STRING: &IString = &literal!("lists");
const BROADCASTS_CATEGORY_STRING: &IString = &literal!("broadcasts");
const COSTUMES_CATEGORY_STRING: &IString = &literal!("costumes");
const BACKDROPS_CATEGORY_STRING: &IString = &literal!("backdrops");
const BACKDROP_TARGETS_CATEGORY_STRING: &IString = &literal!("backdrop_targets");
const SOUNDS_CATEGORY_STRING: &IString = &literal!("sounds");
const DESTINATIONS_CATEGORY_STRING: &IString = &literal!("destinations");
const DIRECTIONS_CATEGORY_STRING: &IString = &literal!("directions");
const CLONABLES_CATEGORY_STRING: &IString = &literal!("clonables");
const COLLIDERS_CATEGORY_STRING: &IString = &literal!("colliders");
const LOCATIONS_CATEGORY_STRING: &IString = &literal!("locations");
const PROPERTIES_CATEGORY_STRING: &IString = &literal!("properties");
const OBJECTS_CATEGORY_STRING: &IString = &literal!("objects");
const PEN_PROPERTIES_CATEGORY_STRING: &IString = &literal!("pen_properties");
const INTEGERS_CATEGORY_STRING: &IString = &literal!("integers");

macro_rules! implement_generate_library {
    ($input:ident, $use_cache:ident, $create_cache:ident) => {{
        let input_lit = parse_macro_input!($input as LitStr);
        let relative_path = input_lit.value();

        let manifest_dir =
            std::env::var("CARGO_MANIFEST_DIR").expect("Failed to get CARGO_MANIFEST_DIR");
        let full_path = Path::new(&manifest_dir).join(&relative_path);

        let toml_str = fs::read_to_string(&full_path)
            .unwrap_or_else(|_| panic!("Failed to read file at {:?}", full_path));

        let (hex_hash, output_cache_path) = implement_create_hash_and_cache_path!(
            manifest_dir,
            relative_path,
            toml_str,
            $use_cache,
            $create_cache
        );
        implement_use_cache!(output_cache_path, hex_hash, $use_cache);

        let source_library: library_parser::Library =
            toml::from_str(&toml_str).expect("Failed to parse JSON");
        let v: Vec<library_parser::ToolboxCategory> = source_library.categories;

        let mut library = HashMap::with_capacity(10);
        let mut menus = HashMap::new();
        let mut menu_category_ids = HashMap::<IString, u32>::from([
            (NO_CATEGORY_STRING.clone(), NO_CATEGORY_ID),
            (VARIABLES_CATEGORY_STRING.clone(), VARIABLES_CATEGORY_ID),
            (LISTS_CATEGORY_STRING.clone(), LISTS_CATEGORY_ID),
            (BROADCASTS_CATEGORY_STRING.clone(), BROADCASTS_CATEGORY_ID),
            (COSTUMES_CATEGORY_STRING.clone(), COSTUMES_CATEGORY_ID),
            (BACKDROPS_CATEGORY_STRING.clone(), BACKDROPS_CATEGORY_ID),
            (BACKDROP_TARGETS_CATEGORY_STRING.clone(), BACKDROP_TARGETS_CATEGORY_ID),
            (SOUNDS_CATEGORY_STRING.clone(), SOUNDS_CATEGORY_ID),
            (DESTINATIONS_CATEGORY_STRING.clone(), DESTINATIONS_CATEGORY_ID),
            (DIRECTIONS_CATEGORY_STRING.clone(), DIRECTIONS_CATEGORY_ID),
            (CLONABLES_CATEGORY_STRING.clone(), CLONABLES_CATEGORY_ID),
            (COLLIDERS_CATEGORY_STRING.clone(), COLLIDERS_CATEGORY_ID),
            (LOCATIONS_CATEGORY_STRING.clone(), LOCATIONS_CATEGORY_ID),
            (PROPERTIES_CATEGORY_STRING.clone(), PROPERTIES_CATEGORY_ID),
            (OBJECTS_CATEGORY_STRING.clone(), OBJECTS_CATEGORY_ID),
            (PEN_PROPERTIES_CATEGORY_STRING.clone(), PEN_PROPERTIES_CATEGORY_ID),
            (INTEGERS_CATEGORY_STRING.clone(), INTEGERS_CATEGORY_ID),
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
        let extensions = source_library.required_extensions.iter();
        let expanded = quote! {
            (::std::collections::HashMap::from([#( (#library_keys.to_string(), #library_values) ),*]), #category_entry_stream, [#( #extensions ),*])
        };
        implement_create_cache!(output_cache_path, hex_hash, expanded, $create_cache);
        TokenStream::from(expanded)
    }};
}

macro_rules! implement_generate_constant_expr_library {
    ($input:ident, $use_cache:ident, $create_cache:ident) => {{
        let input_lit = parse_macro_input!($input as LitStr);
        let relative_path = input_lit.value();
        let manifest_dir =
            std::env::var("CARGO_MANIFEST_DIR").expect("Failed to get CARGO_MANIFEST_DIR");
        let full_path = Path::new(&manifest_dir).join(&relative_path);
        let toml_str = fs::read_to_string(&full_path)
            .unwrap_or_else(|_| panic!("Failed to read file at {:?}", full_path));
        let (hex_hash, output_cache_path) = implement_create_hash_and_cache_path_for_constant_expr!(
            manifest_dir,
            relative_path,
            toml_str,
            $use_cache,
            $create_cache
        );
        implement_use_cache!(output_cache_path, hex_hash, $use_cache);

        let source_library: library_parser::Library =
            toml::from_str(&toml_str).expect("Failed to parse JSON");
        let v: Vec<library_parser::ToolboxCategory> = source_library.categories;

        let mut library = HashMap::with_capacity(10);
        let mut menus = HashMap::new();
        for namespace in v {
            let (category_name, category) =
                process_constant_expr_toolbox_category(namespace, &mut menus);
            library.insert(category_name, category);
        }
        library.insert(
            "menus".to_string(),
            ConstantExprLibraryItem {
                namespace: menus,
                value: None,
            },
        );
        let library_keys = library.keys();
        let library_values = library.values();
        let expanded = quote! {
            ::grazelang_types::ConstantExprLibraryItem {
                namespace: ::std::collections::HashMap::from([#((#library_keys.to_string(), #library_values)),*]),
                value: ::std::option::Option::None,
            }
        };
        implement_create_cache!(output_cache_path, hex_hash, expanded, $create_cache);
        TokenStream::from(expanded)
    }}
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
    ($dir:expr, $rel_path:expr, $toml_str:expr, yes) => {
        (
            {
                let hash = Sha3_256::digest($toml_str.as_bytes());
                base16ct::lower::encode_string(hash.as_slice())
            },
            Path::new(&$dir).join(&($rel_path + ".out_cached.json")),
        )
    };
    ($dir:expr, $rel_path:expr, $toml_str:expr, no_use_cache, no_create_cache) => {
        ((), ())
    };
    ($dir:expr, $rel_path:expr, $toml_str:expr, use_cache, no_create_cache) => {
        implement_create_hash_and_cache_path!($dir, $rel_path, $toml_str, yes)
    };
    ($dir:expr, $rel_path:expr, $toml_str:expr, no_use_cache, create_cache) => {
        implement_create_hash_and_cache_path!($dir, $rel_path, $toml_str, yes)
    };
    ($dir:expr, $rel_path:expr, $toml_str:expr, use_cache, create_cache) => {
        implement_create_hash_and_cache_path!($dir, $rel_path, $toml_str, yes)
    };
}

macro_rules! implement_create_hash_and_cache_path_for_constant_expr {
    ($dir:expr, $rel_path:expr, $toml_str:expr, yes) => {
        (
            {
                let hash = Sha3_256::digest($toml_str.as_bytes());
                base16ct::lower::encode_string(hash.as_slice())
            },
            Path::new(&$dir).join(&($rel_path + ".constant_expr.out_cached.json")),
        )
    };
    ($dir:expr, $rel_path:expr, $toml_str:expr, no_use_cache, no_create_cache) => {
        ((), ())
    };
    ($dir:expr, $rel_path:expr, $toml_str:expr, use_cache, no_create_cache) => {
        implement_create_hash_and_cache_path_for_constant_expr!($dir, $rel_path, $toml_str, yes)
    };
    ($dir:expr, $rel_path:expr, $toml_str:expr, no_use_cache, create_cache) => {
        implement_create_hash_and_cache_path_for_constant_expr!($dir, $rel_path, $toml_str, yes)
    };
    ($dir:expr, $rel_path:expr, $toml_str:expr, use_cache, create_cache) => {
        implement_create_hash_and_cache_path_for_constant_expr!($dir, $rel_path, $toml_str, yes)
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
        let _ = fs::write(
            $path,
            serde_json::to_string(&LibraryCache {
                hash: $hex_hash,
                value: $expanded.to_string(),
            })
            .unwrap(),
        );
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

#[proc_macro]
pub fn generate_constant_expr_library(input: TokenStream) -> TokenStream {
    implement_generate_constant_expr_library!(input, use_cache, create_cache)
}

#[proc_macro]
#[expect(unused_variables)]
pub fn generate_constant_expr_library_no_cache(input: TokenStream) -> TokenStream {
    implement_generate_constant_expr_library!(input, no_use_cache, no_create_cache)
}

#[proc_macro]
pub fn generate_constant_expr_library_no_create_cache(input: TokenStream) -> TokenStream {
    implement_generate_constant_expr_library!(input, use_cache, no_create_cache)
}

#[proc_macro]
pub fn generate_constant_expr_library_no_use_cache(input: TokenStream) -> TokenStream {
    implement_generate_constant_expr_library!(input, no_use_cache, create_cache)
}

#[proc_macro]
pub fn generate_dynamic_generate_library(input: TokenStream) -> TokenStream {
    let library_path = quote! { ::grazelang_types };
    let parser_path = quote! { #library_path::library_parser };
    let qualifier = if input.is_empty() {
        quote! {}
    } else {
        let qualifier = parse_macro_input!(input as syn::Visibility);
        quote! { #qualifier }
    };
    let no_category_string_2 = NO_CATEGORY_STRING.as_str();
    let variables_category_string_2 = VARIABLES_CATEGORY_STRING.as_str();
    let lists_category_string_2 = LISTS_CATEGORY_STRING.as_str();
    let broadcasts_category_string_2 = BROADCASTS_CATEGORY_STRING.as_str();
    let costumes_category_string_2 = COSTUMES_CATEGORY_STRING.as_str();
    let backdrops_category_string_2 = BACKDROPS_CATEGORY_STRING.as_str();
    let backdrop_targets_category_string_2 = BACKDROP_TARGETS_CATEGORY_STRING.as_str();
    let sounds_category_string_2 = SOUNDS_CATEGORY_STRING.as_str();
    let destinations_category_string_2 = DESTINATIONS_CATEGORY_STRING.as_str();
    let directions_category_string_2 = DIRECTIONS_CATEGORY_STRING.as_str();
    let clonables_category_string_2 = CLONABLES_CATEGORY_STRING.as_str();
    let colliders_category_string_2 = COLLIDERS_CATEGORY_STRING.as_str();
    let locations_category_string_2 = LOCATIONS_CATEGORY_STRING.as_str();
    let properties_category_string_2 = PROPERTIES_CATEGORY_STRING.as_str();
    let objects_category_string_2 = OBJECTS_CATEGORY_STRING.as_str();
    let pen_properties_category_string_2 = PEN_PROPERTIES_CATEGORY_STRING.as_str();
    let integers_category_string_2 = INTEGERS_CATEGORY_STRING.as_str();
    quote! {
        #qualifier fn dynamic_generate_library(
            path: &::std::path::Path,
            use_cache: ::std::primitive::bool,
            create_cache: ::std::primitive::bool,
        ) -> Option<(
                ::std::collections::HashMap<::std::string::String, #library_path::LibraryItem>,
                ::std::collections::HashMap<::std::primitive::u32, ::std::collections::HashSet<::std::string::String>>,
                ::std::vec::Vec<::std::string::String>,
            )> {
            let full_path = path;
            let toml_str = ::std::fs::read_to_string(&full_path)
                .ok()?;
            let (hex_hash, output_cache_path) = (
                {
                    let hash = <::sha3::Sha3_256 as ::sha3::Digest>::digest(toml_str.as_bytes());
                    ::base16ct::lower::encode_string(hash.as_slice())
                },
                {
                    let mut path = full_path.with_added_extension("out_cached");
                    path.add_extension("dyn");
                    path.add_extension("json");
                    path
                },
            );
            if use_cache && output_cache_path.is_file() {
                let output_json_str = ::std::fs::read_to_string(&output_cache_path)
                    .ok()?;
                if let ::std::result::Result::Ok(cache) = ::serde_json::from_str::<#parser_path::DynamicLibraryCache>(&output_json_str)
                    && cache.hash == hex_hash
                {
                    return Some(cache.value);
                }
            }
            let source_library: #parser_path::Library = ::toml::from_str(&toml_str).expect("Failed to parse JSON");
            let v: ::std::vec::Vec<#parser_path::ToolboxCategory> = source_library.categories;
            let mut library = ::std::collections::HashMap::with_capacity(10);
            let mut menus = ::std::collections::HashMap::new();
            let mut menu_category_ids = ::std::collections::HashMap::<::arcstr::ArcStr, ::std::primitive::u32>::from([
                (::arcstr::literal!(#no_category_string_2), #library_path::NO_CATEGORY_ID),
                (::arcstr::literal!(#variables_category_string_2), #library_path::VARIABLES_CATEGORY_ID),
                (::arcstr::literal!(#lists_category_string_2), #library_path::LISTS_CATEGORY_ID),
                (::arcstr::literal!(#broadcasts_category_string_2), #library_path::BROADCASTS_CATEGORY_ID),
                (::arcstr::literal!(#costumes_category_string_2), #library_path::COSTUMES_CATEGORY_ID),
                (::arcstr::literal!(#backdrops_category_string_2), #library_path::BACKDROPS_CATEGORY_ID),
                (
                    ::arcstr::literal!(#backdrop_targets_category_string_2),
                    #library_path::BACKDROP_TARGETS_CATEGORY_ID,
                ),
                (::arcstr::literal!(#sounds_category_string_2), #library_path::SOUNDS_CATEGORY_ID),
                (
                    ::arcstr::literal!(#destinations_category_string_2),
                    #library_path::DESTINATIONS_CATEGORY_ID,
                ),
                (::arcstr::literal!(#directions_category_string_2), #library_path::DIRECTIONS_CATEGORY_ID),
                (::arcstr::literal!(#clonables_category_string_2), #library_path::CLONABLES_CATEGORY_ID),
                (::arcstr::literal!(#colliders_category_string_2), #library_path::COLLIDERS_CATEGORY_ID),
                (::arcstr::literal!(#locations_category_string_2), #library_path::LOCATIONS_CATEGORY_ID),
                (::arcstr::literal!(#properties_category_string_2), #library_path::PROPERTIES_CATEGORY_ID),
                (::arcstr::literal!(#objects_category_string_2), #library_path::OBJECTS_CATEGORY_ID),
                (
                    ::arcstr::literal!(#pen_properties_category_string_2),
                    #library_path::PEN_PROPERTIES_CATEGORY_ID,
                ),
                (::arcstr::literal!(#integers_category_string_2), #library_path::INTEGERS_CATEGORY_ID),
            ]);
            let mut category_entries = ::std::collections::HashMap::<::std::primitive::u32, ::std::collections::HashSet<::std::string::String>>::new();
            for namespace in v {
                let (category_name, category, associated_menus) =
                #parser_path::process_toolbox_category(namespace, &mut category_entries, &mut menu_category_ids);
                for (key, value) in associated_menus {
                    match menus.entry(key) {
                        ::std::collections::hash_map::Entry::Vacant(v) => {
                            v.insert(value);
                        }
                        ::std::collections::hash_map::Entry::Occupied(mut o) => {
                            #parser_path::merge_associated_item(o.get_mut(), value);
                        }
                    }
                }
                library.insert(category_name, category);
            }
            library.insert(
                "menus".to_string(),
                #library_path::LibraryItem {
                    namespace: menus,
                    value: None,
                },
            );
            if create_cache {
                let dynamic_library_cache = #parser_path::DynamicLibraryCache {
                    hash: hex_hash,
                    value: (library, category_entries, source_library.required_extensions),
                };
                let _ = ::std::fs::write(
                    output_cache_path,
                    ::serde_json::to_string(&dynamic_library_cache).ok()?,
                );
                return Some(dynamic_library_cache.value);
            }
            Some((library, category_entries, source_library.required_extensions))
        }
    }.into()
}
