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
/// Generates methods for converting an enum value to and from
/// its integer representation safely. Defaults to `u32` representation.
/// 
/// ### Additional parameters
/// 
/// `#[auto_enum({representation} [, checked])]`
/// 
/// `{representation}` may be any unsigned integral type from 8 to 64 (including usize).
/// 
/// If the `checked` flag is specified it will generate an implementation of
/// `checked_enum::CheckedEnum`. The crate must be included to use this option.
/// 
/// ### Basic usage
/// 
/// ```
/// # #[macro_use]
/// # extern crate auto_enum;
/// #[auto_enum]
/// pub enum Foo {
///     Bar,
///     Baz,
/// }
/// 
/// # fn main() {
/// assert_eq!(Foo::Bar.to_u32(), 0);
/// assert_eq!(Foo::Baz.to_u32(), 1);
/// 
/// assert_eq!(Foo::from_u32(0), Some(Foo::Bar));
/// assert_eq!(Foo::from_u32(1), Some(Foo::Baz));
/// assert_eq!(Foo::from_u32(2), None);
/// # }
/// ```
pub fn auto_enum(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as AttributeArgs);
    let input = parse_macro_input!(item as Item);
    int_enums::expand_int_enum(&args, &input)
        .unwrap_or_else(compile_error)
        .into()
}

#[proc_macro_attribute]
/// Generates a bitflag wrapper struct containing the specified representation type. Representation
/// defaults to u32.
/// 
/// ### Additional parameters
/// 
/// `#[enum_flags({representation})]`
/// 
/// `{representation}` may be any unsigned integral type from 8 to 64 (including usize).
/// It defaults to u32 if unspecified.
/// 
/// ### Basic usage
/// 
/// ```
/// # #[macro_use]
/// # extern crate auto_enum;
/// #[enum_flags]
/// pub enum Foo {
///     BAR = 0b001, // These flag values are the ones that would be
///     BAZ = 0b010, // assigned automatically by the macro if they
///     QUX = 0b100, // were not specified explicitly.
///     
///     NONE = 0,
///     ALL = BAR | BAZ | QUX,
/// }
/// 
/// # fn main() {
/// let mut flags = Foo::NONE;
/// 
/// assert_eq!(flags.is_set(Foo::BAR), false);
/// assert_eq!(flags.is_set(Foo::BAZ), false);
/// assert_eq!(flags.is_set(Foo::QUX), false);
/// 
/// flags |= Foo::BAR | Foo::QUX;
/// 
/// assert_eq!(flags.is_set(Foo::BAR), true);
/// assert_eq!(flags.is_set(Foo::BAZ), false);
/// assert_eq!(flags.is_set(Foo::QUX), true);
/// 
/// flags ^= Foo::BAR | Foo::BAZ;
/// 
/// assert_eq!(flags.is_set(Foo::BAR), false);
/// assert_eq!(flags.is_set(Foo::BAZ), true);
/// assert_eq!(flags.is_set(Foo::QUX), true);
/// 
/// flags &= Foo::BAZ;
/// 
/// assert_eq!(flags.is_set(Foo::BAR), false);
/// assert_eq!(flags.is_set(Foo::BAZ), true);
/// assert_eq!(flags.is_set(Foo::QUX), false);
/// 
/// flags = !flags;
/// 
/// assert_eq!(flags.is_set(Foo::BAR), true);
/// assert_eq!(flags.is_set(Foo::BAZ), false);
/// assert_eq!(flags.is_set(Foo::QUX), true);
/// 
/// flags |= Foo::ALL;
/// 
/// assert_eq!(flags.is_set(Foo::BAR), true);
/// assert_eq!(flags.is_set(Foo::BAZ), true);
/// assert_eq!(flags.is_set(Foo::QUX), true);
/// 
/// flags.clear(Foo::ALL);
/// 
/// assert_eq!(flags.is_set(Foo::BAR), false);
/// assert_eq!(flags.is_set(Foo::BAZ), false);
/// assert_eq!(flags.is_set(Foo::QUX), false);
/// # }
/// ```
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
