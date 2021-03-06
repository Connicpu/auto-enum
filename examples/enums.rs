#[macro_use]
extern crate auto_enum;

#[auto_enum]
pub enum Bar {
    None = 0,
    Some = 1,
}

#[enum_flags(u32)]
pub enum Foo {
    A,
    B,
    C,
    D,
}

fn main() {
    let flags = Foo::B | Foo::D;

    println!("{}", flags.is_set(Foo::A));
    println!("{}", flags.is_set(Foo::B));
    println!("{}", flags.is_set(Foo::C));
    println!("{}", flags.is_set(Foo::D));
}
