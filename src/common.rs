use proc_macro2::Span;
use syn::{Ident, IntSuffix};

#[derive(Copy, Clone)]
pub enum Repr {
    U8,
    U16,
    U32,
    U64,
    Usize,
}

impl Repr {
    pub fn parse(val: &str) -> Result<Repr, String> {
        Ok(match val {
            "u8" => Repr::U8,
            "u16" => Repr::U16,
            "u32" => Repr::U32,
            "u64" => Repr::U64,
            "usize" => Repr::Usize,
            _ => return Err(format!("Invalid enum representation `{}`", val)),
        })
    }

    pub fn ident(self) -> Ident {
        use self::Repr::*;
        let name = match self {
            U8 => "u8",
            U16 => "u16",
            U32 => "u32",
            U64 => "u64",
            Usize => "usize",
        };

        Ident::new(name, Span::call_site())
    }

    pub fn truncate(self, val: u64) -> u64 {
        use self::Repr::*;
        match self {
            U8 => val as u8 as u64,
            U16 => val as u16 as u64,
            U32 => val as u32 as u64,
            U64 => val,
            Usize => val as usize as u64,
        }
    }

    pub fn suffix(self) -> IntSuffix {
        use self::Repr::*;
        match self {
            U8 => IntSuffix::U8,
            U16 => IntSuffix::U16,
            U32 => IntSuffix::U32,
            U64 => IntSuffix::U64,
            Usize => IntSuffix::Usize,
        }
    }
}
