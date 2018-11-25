use proc_macro2::TokenStream;
use syn::spanned::Spanned;
use syn::{
    Attribute, AttributeArgs, Expr, ExprLit, Fields, Ident, Item, ItemEnum, Lit, LitInt, Meta,
    NestedMeta,
};

use common::Repr;

pub fn expand_int_enum(args: &AttributeArgs, input: &Item) -> Result<TokenStream, String> {
    let input = match input {
        Item::Enum(input) => input,
        _ => return Err("#[auto_enum] may only be used on `enum` types".into()),
    };

    let gen = &input.generics;
    if gen.type_params().count() + gen.lifetimes().count() + gen.const_params().count() != 0 {
        return Err("#[auto_enum] does not support parameterized types".into());
    }

    let meta = parse_meta(args)?;
    let variants = parse_variants(input, &meta)?;

    let enumtok = input.enum_token;
    let extrameta = &input.attrs;
    let repr = meta.repr.ident();
    let name = &input.ident;
    let ispub = &input.vis;

    let vmeta = variants.iter().map(|Var { meta, .. }| meta);
    let vname: &Vec<_> = &variants.iter().map(|Var { name, .. }| name).collect();
    let vdisc: &Vec<_> = &variants.iter().map(|Var { disc, .. }| disc).collect();

    let enum_body = quote_spanned! {
        input.span() =>
        #[repr(#repr)]
        #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #( #extrameta )*
        #ispub #enumtok #name {
            #(
                #( #vmeta )*
                #vname = #vdisc,
            )*
        }
    };

    let to_fn = Ident::new(&format!("to_{}", repr), repr.span());
    let from_fn = Ident::new(&format!("from_{}", repr), repr.span());
    let enum_impl = quote! {
        impl #name {
            #[inline(always)]
            /// Convert this enum to its underlying value.
            #ispub fn #to_fn (self) -> #repr {
                self as #repr
            }

            #[inline]
            /// Attempt to convert this enum from an underlying value. Returns None if
            /// the value is not valid for this enumeration.
            #ispub fn #from_fn (val: #repr) -> Option<Self> {
                use self :: #name :: *;
                match val {
                    #(
                        #vdisc => Option::Some(#vname),
                    )*
                    _ => Option::None,
                }
            }
        }
    };

    let checked_impl = if meta.checked {
        quote! {
            impl checked_enum::CheckedEnum for #name {
                type Storage = #repr;
                #[inline(always)]
                fn try_from_storage(val: Self::Storage) -> Option<Self> {
                    #name :: #from_fn (val)
                }
                #[inline(always)]
                fn to_storage(self) -> Self::Storage {
                    #name :: #to_fn (self)
                }
            }
        }
    } else {
        quote!{}
    };

    Ok(quote! {
        #enum_body
        #enum_impl
        #checked_impl
    })
}

struct Var {
    meta: Vec<Attribute>,
    name: Ident,
    disc: LitInt,
}

fn parse_variants(input: &ItemEnum, meta: &MetaInfo) -> Result<Vec<Var>, String> {
    let mut vars = Vec::new();
    let mut next_val = 0u64;

    for var in input.variants.iter() {
        if var.fields != Fields::Unit {
            return Err(format!(
                "Variant `{}` has fields, this is not allowed for auto_enum",
                var.ident
            ));
        }

        let disc = match &var.discriminant {
            Some((
                _,
                Expr::Lit(ExprLit {
                    lit: Lit::Int(lit), ..
                }),
            )) => lit.clone(),
            None => LitInt::new(next_val, meta.repr.suffix(), var.ident.span()),
            _ => {
                return Err("Only integer literals are supported for auto_enum discriminants".into())
            }
        };
        next_val = meta
            .repr
            .truncate((disc.value() as i64).wrapping_add(meta.incr) as u64);

        vars.push(Var {
            meta: var.attrs.clone(),
            name: var.ident.clone(),
            disc,
        })
    }

    Ok(vars)
}

struct MetaInfo {
    pub repr: Repr,
    pub incr: i64,
    pub checked: bool,
}

fn parse_meta(args: &AttributeArgs) -> Result<MetaInfo, String> {
    let mut repr = Repr::U32;
    let mut incr = 1;
    let mut checked = false;

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
                    "checked" => checked = true,
                    _ => repr = Repr::parse(&ident.to_string())?,
                }
                continue;
            }
            Meta::NameValue(arg) => arg,
            Meta::List(_) => return Err("Unexpected attribute list".into()),
        };

        let attr = arg.ident.to_string();
        match &attr[..] {
            "increment" => match &arg.lit {
                Lit::Int(i) => incr = i.value() as i64,
                _ => return Err(format!("Unexpected literal for `increment`")),
            },
            "decrement" => match &arg.lit {
                Lit::Int(i) => incr = -(i.value() as i64),
                _ => return Err(format!("Unexpected literal for `decrement`")),
            },
            _ => return Err(format!("Unexpected attribute argument `{}`", attr)),
        }
    }

    Ok(MetaInfo {
        repr,
        incr,
        checked,
    })
}
