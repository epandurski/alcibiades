extern crate regex;

pub mod position;
pub mod basetypes;
pub mod bitsets;
pub mod notation;

fn main() {
    // use regex::Regex;
    // let c = [(true, true); 2];
    use position::board::Board;
    println!("Board -> {}", std::mem::size_of::<Board>());
}
