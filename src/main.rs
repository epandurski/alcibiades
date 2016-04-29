extern crate regex;

pub mod utils;
pub mod basetypes;
pub mod bitsets;
pub mod movegen;

fn main() {
    // use regex::Regex;
    // let c = [(true, true); 2];
    use basetypes::*;
    println!("Board -> {}", std::mem::size_of::<Option<Square>>());
}
