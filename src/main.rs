#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod chesstypes;
pub mod uci;
pub mod board;
pub mod search;
pub mod move_generation;
pub mod engine;

use std::process::exit;
use search::searchers::{Deepening, StandardSrch};
use search::tt::StandardTt;
use search::quiescence::StandardQsearch;
use move_generation::{Position, Generator};
use board::evaluators::RandomEval;
use engine::run_server;

fn main() {
    exit(match run_server::<Deepening<StandardSrch<StandardTt,
                                                   Position<StandardQsearch<Generator<RandomEval>>>>>>() {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
