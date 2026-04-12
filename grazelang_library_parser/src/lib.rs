use std::{fs, path::Path};

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, LitStr};
mod parser;

// TEMPORARY
#[proc_macro]
pub fn generate_json_fn(input: TokenStream) -> TokenStream {
    let input_lit = parse_macro_input!(input as LitStr);
    let relative_path = input_lit.value();
    
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .expect("Failed to get CARGO_MANIFEST_DIR");
    let full_path = Path::new(&manifest_dir).join(&relative_path);
    
    let json_str = fs::read_to_string(&full_path)
        .unwrap_or_else(|_| panic!("Failed to read file at {:?}", full_path));

    let v: Vec<crate::parser::ToolboxCategory> = serde_json::from_str(&json_str)
        .expect("Failed to parse JSON");

    let debug = format!("{:?}", v);

    let expanded = quote! {
        pub fn get_debug_repr() -> &'static str {
            #debug
        }
    };

    TokenStream::from(expanded)
}