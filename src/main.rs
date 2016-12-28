#[macro_use]
extern crate alcibiades;

use std::process::exit;
pub use alcibiades::*;
pub use alcibiades::stock::*;

fn main() {
    type SearchNode = StdSearchNode<StdQsearch<StdMoveGenerator<SimpleEvaluator>>>;
    type SearchExecutor = Deepening<StdSearchExecutor<StdHashTable, SearchNode>>;
    exit(match engine::run_uci::<SearchExecutor, StdTimeManager>("Alcibiades 0.2",
                                                                 "Evgeni Pandurski") {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
