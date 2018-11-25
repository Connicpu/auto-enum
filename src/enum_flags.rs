use proc_macro2::TokenStream;
use syn::spanned::Spanned;
use syn::{Attribute, AttributeArgs, Fields, Ident, Item, ItemEnum, LitInt, Meta, NestedMeta};

use common::Repr;
use int_eval::int_eval;

pub fn expand_enum_flags(args: &AttributeArgs, input: &Item) -> Result<TokenStream, String> {
    let input = match input {
        Item::Enum(input) => input,
        _ => return Err("#[enum_flags] may only be used on `enum` types".into()),
    };

    let meta = parse_meta(args)?;
    let flags = parse_flags(input, &meta)?;

    let extrameta = &input.attrs;
    let repr = meta.repr.ident();
    let name = &input.ident;
    let vis = &input.vis;
    let rvis = std::iter::repeat(vis);

    let struct_body = quote! {
        #[repr(transparent)]
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #( #extrameta )*
        #vis struct #name ( pub #repr );
    };

    let fmeta = flags.iter().map(|Flag { meta, .. }| meta);
    let fname: &Vec<_> = &flags.iter().map(|Flag { name, .. }| name).collect();
    let fdisc: &Vec<_> = &flags.iter().map(|Flag { disc, .. }| disc).collect();
    let mask = flags.iter().fold(0u64, |a, f| (a | f.disc.value()));

    let rname = std::iter::repeat(name);
    let rname2 = std::iter::repeat(name);
    let struct_impl = quote! {
        impl #name {
            #(
                #( #fmeta )*
                #rvis const #fname : #rname = #rname2 ( #fdisc );
            )*

            #[inline(always)]
            /// Test whether the given flags are set on this instance. Eqivalent to 
            /// `value & flags == flags`.
            pub fn is_set(self, flags: Self) -> bool {
                self & flags == flags
            }

            #[inline(always)]
            /// Clear the given flags from this instance. Equivalent to `value &= !flags;`.
            pub fn clear(&mut self, flags: Self) {
                *self &= !flags;
            }

            #[inline(always)]
            /// Checks whether any flags are set in the bitfield representation that don't exist
            /// in the enumeration definition. Returns true if only valid flags are set.
            pub fn validate(self) -> bool {
                const MASK: #repr = #mask as #repr;
                self.0 & !MASK == 0
            }
        }
    };

    let trait_impl = quote! {
        impl std::ops::Not for #name {
            type Output = Self;
            #[inline(always)]
            fn not(self) -> Self {
                #name ( !self.0 )
            }
        }
        impl std::ops::BitAnd for #name {
            type Output = Self;
            #[inline(always)]
            fn bitand(self, rhs: Self) -> Self {
                #name ( self.0 & rhs.0 )
            }
        }
        impl std::ops::BitAndAssign for #name {
            #[inline(always)]
            fn bitand_assign(&mut self, rhs: Self) {
                self.0 &= rhs.0;
            }
        }
        impl std::ops::BitOr for #name {
            type Output = Self;
            #[inline(always)]
            fn bitor(self, rhs: Self) -> Self {
                #name ( self.0 | rhs.0 )
            }
        }
        impl std::ops::BitOrAssign for #name {
            #[inline(always)]
            fn bitor_assign(&mut self, rhs: Self) {
                self.0 |= rhs.0;
            }
        }
        impl std::ops::BitXor for #name {
            type Output = Self;
            #[inline(always)]
            fn bitxor(self, rhs: Self) -> Self {
                #name ( self.0 ^ rhs.0 )
            }
        }
        impl std::ops::BitXorAssign for #name {
            #[inline(always)]
            fn bitxor_assign(&mut self, rhs: Self) {
                self.0 ^= rhs.0;
            }
        }
        impl Default for #name {
            #[inline(always)]
            /// Defaults to no flags being set.
            fn default() -> Self {
                #name (0)
            }
        }
    };

    let fname2 = fname;
    let rname = std::iter::repeat(name);
    let dbg_name_open = format!("{}(", name);
    let debug_impl = quote! {
        impl std::fmt::Debug for #name {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.write_str(#dbg_name_open)?;
                let mut first = true;
                #(
                    if self.is_set(#rname :: #fname) {
                        if first {
                            first = false;
                        } else {
                            fmt.write_str(" | ")?;
                        }
                        fmt.write_str(stringify!(#fname2))?;
                    }
                )*
                if first {
                    fmt.write_str("NONE")?;
                }
                fmt.write_str(")")?;
                Ok(())
            }
        }
    };

    Ok(quote! {
        #struct_body
        #struct_impl
        #trait_impl
        #debug_impl
    })
}

#[derive(Debug)]
struct Flag {
    meta: Vec<Attribute>,
    name: Ident,
    disc: LitInt,
}

fn parse_flags(input: &ItemEnum, meta: &MetaInfo) -> Result<Vec<Flag>, String> {
    let mut flags = Vec::new();
    let mut next_flag = 1;

    for var in &input.variants {
        if var.fields != Fields::Unit {
            return Err(format!(
                "Variant `{}` has fields, this is not allowed for enum_flags",
                var.ident
            ));
        }

        let value = match &var.discriminant {
            Some((_, expr)) => int_eval(expr)?,
            None => match next_flag {
                0 => {
                    return Err("Integer overflow in flag calculation. Use a larger \
                                repr or set your flag values manually."
                        .into())
                }
                flag => flag,
            },
        };
        next_flag = meta.repr.truncate(value << 1);

        flags.push(Flag {
            meta: var.attrs.clone(),
            name: var.ident.clone(),
            disc: LitInt::new(
                meta.repr.truncate(value),
                meta.repr.suffix(),
                var.discriminant
                    .as_ref()
                    .map(|(_, d)| d.span())
                    .unwrap_or(var.ident.span()),
            ),
        })
    }

    Ok(flags)
}

struct MetaInfo {
    pub repr: Repr,
}

fn parse_meta(args: &AttributeArgs) -> Result<MetaInfo, String> {
    let mut repr = Repr::U32;

    for arg in args {
        let meta = match arg {
            NestedMeta::Meta(meta) => meta,
            NestedMeta::Literal(lit) => {
                return Err(format!(
                    "Unexpected literal `{}` in attribute",
                    quote! { #lit }
                ))
            }
        };

        let arg = match meta {
            Meta::Word(ident) => {
                let val = ident.to_string();
                match &val[..] {
                    _ => repr = Repr::parse(&ident.to_string())?,
                }
                continue;
            }
            Meta::NameValue(arg) => arg,
            Meta::List(_) => return Err("Unexpected attribute list".into()),
        };

        let attr = arg.ident.to_string();
        match &attr[..] {
            _ => return Err(format!("Unexpected attribute argument `{}`", attr)),
        }
    }

    Ok(MetaInfo { repr })
}
