extern crate regex;

pub mod utils;
pub mod basetypes;
pub mod bitsets;
pub mod board;

fn main() {
    // use regex::Regex;
    // let c = [(true, true); 2];
    use board::Board;
    println!("Board -> {}", std::mem::size_of::<Board>());
}
