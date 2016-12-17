#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod chesstypes;
pub mod uci;
pub mod utils;
pub mod search;
pub mod engine;

use std::process::exit;
use search::{Deepening, Position};
use search::stock::{StdHashTable, StdQsearch, StdSearchExecutor, StdMoveGenerator, RandomEvaluator};
use engine::run_server;

fn main() {
    type SearchNode = Position<StdQsearch<StdMoveGenerator<RandomEvaluator>>>;
    exit(match run_server::<Deepening<StdSearchExecutor<StdHashTable, SearchNode>>>() {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
