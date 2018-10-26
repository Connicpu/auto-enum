//! Provides automatic enum implementations that are convenient for working with FFI
//! 
//! ```
//! #[macro_use]
//! extern crate auto_enum;
//! extern crate checked_enum;
//! 
//! #[auto_enum(u32, checked)]
//! pub enum SweepDirection {
//!     CounterClockwise = 0,
//!     Clockwise = 1,
//! }
//! 
//! # fn main() {
//! assert_eq!(SweepDirection::from_u32(0), Some(SweepDirection::CounterClockwise));
//! # }
//! ```
//! 
//! ```
//! #[macro_use]
//! extern crate auto_enum;
//! 
//! #[enum_flags(u32)]
//! pub enum BitmapOptions {
//!     TARGET = 0x1,
//!     CANNOT_DRAW = 0x2,
//!     CPU_READ = 0x4,
//!     GDI_COMPATIBLE = 0x8,
//! }
//! 
//! # fn main() {
//! let flags = BitmapOptions::TARGET | BitmapOptions::GDI_COMPATIBLE;
//! assert!(flags.is_set(BitmapOptions::TARGET));
//! assert!(!flags.is_set(BitmapOptions::CPU_READ));
//! # }
//! ```

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
