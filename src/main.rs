#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod basetypes;
pub mod castling_rights;
pub mod bitsets;
pub mod notation;
pub mod position;
pub mod uci;
pub mod engine;
pub mod tt;

use std::process::exit;
use engine::DummyEngineFactory;

fn main() {
    // use regex::Regex;
    // let c = [(true, true); 2];
    // use position::Position;
    // Position::from_fen("k7/8/8/8/7P/8/8/7K w - h3 0 1").is_ok();
    // println!("Board -> {}", 1);
    
    // use position::board::Board;
    // let mut b = Board::from_fen("rnbq1bn1/pppP3k/8/3P2B1/2B5/5N2/PPPN1PP1/2K4R b - - 0 1").ok().unwrap();
    
    // use position::board::Board;
    // use position::chess_move::*;
    // let mut stack = MoveStack::new();
    // let mut b = Board::from_fen("rnbq1bn1/pppP3k/8/3P2B1/2B5/5N2/PPPN1PP1/2K4R b - - 0 1").ok().unwrap();
    // b.generate_moves(true, &mut stack);
    // let m = stack.pop().unwrap();
    // b.do_move(m);

    if let Ok(mut uci_loop) = uci::Server::wait_for_hanshake(&DummyEngineFactory::new()) {
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


