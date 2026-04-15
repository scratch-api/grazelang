use std::{fs, path::Path};

use proc_macro::TokenStream;
use quote::quote;
use sha3::{Digest, Sha3_256};
use syn::{LitStr, parse_macro_input};

use crate::parser::LibraryCache;
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

        let library: ::std::collections::HashMap<
            ::std::string::String,
            ::grazelang_library::LibraryItem,
        > = v.into_iter().map(|value| value.into()).collect();

        let library_keys = library.keys();
        let library_values = library.values();

        let expanded = quote! {
            ::std::collections::HashMap::from([#( (#library_keys.to_string(), #library_values) ),*])
        };

        implement_create_cache!(output_cache_path, hex_hash, expanded, $create_cache);

        TokenStream::from(expanded)
    }};
}

macro_rules! implement_create_hash_and_cache_path {
    ($dir:expr, $rel_path:expr, $json_str:expr, yes) => {
        (
            {
                let hash = Sha3_256::digest($json_str.as_bytes());
                base16ct::lower::encode_string(hash.as_slice())
            },
            Path::new(&$dir).join(&($rel_path + ".out_cached")),
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
#[allow(unused_variables)]
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
