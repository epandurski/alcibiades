#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod position;
pub mod basetypes;
pub mod bitsets;
pub mod notation;
pub mod uci;
pub mod engine;

use std::process::exit;
use engine::DummyEngineFactory;

fn main() {
    // use regex::Regex;
    // let c = [(true, true); 2];
    // use position::Position;
    // Position::from_fen("k7/8/8/8/7P/8/8/7K w - h3 0 1").is_ok();
    // println!("Board -> {}", 1);

    if let Ok(mut uci_loop) = uci::Server::wait_for_hanshake(&DummyEngineFactory) {
        match uci_loop.serve() {
            Ok(_) => {
                exit(0);
            }
            Err(_) => {
                exit(1);
            }
        }
    }
    exit(1);
}


