#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod basetypes;
pub mod castling_rights;
pub mod chess_move;
pub mod bitsets;
pub mod position;
pub mod uci;
pub mod engine;
pub mod tt;

use std::process::exit;
use engine::EngineFactory;

fn main() {
    if let Ok(mut uci_loop) = uci::Server::wait_for_hanshake(&EngineFactory) {
        match uci_loop.serve() {
            Ok(_) => {
                exit(0);
            }
            Err(_) => {
                exit(1);
            }
        }
    }
    exit(2);
}


