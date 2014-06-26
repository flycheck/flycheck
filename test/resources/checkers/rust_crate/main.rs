#![crate_type="bin"]
#![crate_id="flycheck"]

mod bar;
mod foo;

pub fn main() {
    foo::foo();
    println!("Hello, world!");
}
