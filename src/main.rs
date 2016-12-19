#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod board;
pub mod uci;
pub mod utils;
pub mod search;
pub mod engine;

use std::process::exit;
use engine::run_server;
use search::Deepening;
use search::stock::*;

fn main() {
    type SearchNode = StdSearchNode<StdQsearch<StdMoveGenerator<RandomEvaluator>>>;
    exit(match run_server::<Deepening<StdSearchExecutor<StdHashTable, SearchNode>>>() {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
