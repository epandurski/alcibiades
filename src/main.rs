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
use search::Deepening;
use search::stock::{StdHashTable, StdQsearch, StdSearchExecutor, StdMoveGenerator};
use move_generation::Position;
use board::evaluators::RandomEval;
use engine::run_server;

fn main() {
    type SearchNode = Position<StdQsearch<StdMoveGenerator<RandomEval>>>;
    exit(match run_server::<Deepening<StdSearchExecutor<StdHashTable, SearchNode>>>() {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
