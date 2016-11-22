#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod chesstypes;
pub mod board;
pub mod search;
pub mod engine;

use std::process::exit;
use search::aspiration::Multipv;
use search::searchers::StandardSearcher;
use search::tt::Tt;
use board::Position;
use board::evaluators::RandomEvaluator;
use engine::run;

fn main() {
    exit(match run::<Multipv<StandardSearcher<Tt, Position<RandomEvaluator>>>>() {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
