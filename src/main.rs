#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod position;
pub mod basetypes;
pub mod bitsets;
pub mod notation;
pub mod uci;

fn main() {
    // use regex::Regex;
    // let c = [(true, true); 2];
    use position::Position;
    Position::from_fen("k7/8/8/8/7P/8/8/7K w - h3 0 1").is_ok();
    println!("Board -> {}", 1);
}
