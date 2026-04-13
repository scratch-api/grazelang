use std::{collections::HashMap, fs, path::Path};

use proc_macro::TokenStream;
use quote::quote;
use sha3::{Digest, Sha3_256};
use syn::{LitStr, parse_macro_input};

use crate::parser::LibraryCache;
mod parser;

#[proc_macro]
pub fn generate_library(input: TokenStream) -> TokenStream {
    let input_lit = parse_macro_input!(input as LitStr);
    let relative_path = input_lit.value();

    let manifest_dir =
        std::env::var("CARGO_MANIFEST_DIR").expect("Failed to get CARGO_MANIFEST_DIR");
    let full_path = Path::new(&manifest_dir).join(&relative_path);

    let json_str = fs::read_to_string(&full_path)
        .unwrap_or_else(|_| panic!("Failed to read file at {:?}", full_path));

    let hash = Sha3_256::digest(json_str.as_bytes());
    let hex_hash = base16ct::lower::encode_string(hash.as_slice());

    let output_path = Path::new(&manifest_dir).join(&(relative_path + ".out_cached"));
    if output_path.is_file() {
        let output_json_str = fs::read_to_string(&output_path)
            .unwrap_or_else(|_| panic!("Failed to read file at {:?}", output_path));

        if let Ok(cache) = serde_json::from_str::<LibraryCache>(&output_json_str)
            && cache.hash == hex_hash
        {
            return cache.value.parse().unwrap();
        }
    }

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

    fs::write(
        output_path,
        serde_json::to_string(&LibraryCache {
            hash: hex_hash,
            value: expanded.to_string(),
        })
        .unwrap(),
    )
    .unwrap();

    TokenStream::from(expanded)
}
