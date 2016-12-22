#[macro_use]
extern crate alcibiades;

use std::process::exit;
pub use alcibiades::*;
pub use alcibiades::stock::*;

fn main() {
    type SearchNode = StdSearchNode<StdQsearch<StdMoveGenerator<RandomEvaluator>>>;
    type SearchExecutor = Deepening<StdSearchExecutor<StdHashTable, SearchNode>>;
    exit(match engine::run::<SearchExecutor>("Alcibiades 0.1", "Evgeni Pandurski") {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
