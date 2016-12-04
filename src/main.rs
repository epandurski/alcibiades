#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod chesstypes;
pub mod uci;
pub mod board;
pub mod search;
pub mod engine;

use std::process::exit;
use search::searchers::{Deepening, StandardSrch};
use search::tt::StandardTt;
use search::Position;
use board::evaluators::RandomEval;
use board::move_generators::StandardMgen;
use engine::run_server;

fn main() {
    exit(match run_server::<Deepening<StandardSrch<StandardTt,
                                                   Position<StandardMgen<RandomEval>>>>>() {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
