#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod basetypes;
pub mod chess_move;
pub mod position;
pub mod search;
pub mod uci;

use std::process::exit;


/// The version of the program.
pub const VERSION: &'static str = "0.1";


/// The author of the program.
pub const AUTHOR: &'static str = "Evgeni Pandurski";


fn main() {
    if let Ok(mut uci_loop) = uci::Server::wait_for_hanshake(search::EngineFactory) {
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
