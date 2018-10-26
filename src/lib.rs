#![recursion_limit = "1024"]

#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use syn::{AttributeArgs, Item};

mod common;
mod enum_flags;
mod int_enums;
mod int_eval;

#[proc_macro_attribute]
pub fn auto_enum(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as AttributeArgs);
    let input = parse_macro_input!(item as Item);
    int_enums::expand_int_enum(&args, &input)
        .unwrap_or_else(compile_error)
        .into()
}

#[proc_macro_attribute]
pub fn enum_flags(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as AttributeArgs);
    let input = parse_macro_input!(item as Item);
    enum_flags::expand_enum_flags(&args, &input)
        .unwrap_or_else(compile_error)
        .into()
}

fn compile_error(message: String) -> proc_macro2::TokenStream {
    quote! {
        compile_error!(#message);
    }
}
