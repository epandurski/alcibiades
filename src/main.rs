extern crate alcibiades;

use alcibiades::*;
use alcibiades::stock::*;

fn main() {
    type SearchNode = StdSearchNode<StdQsearch<StdMoveGenerator<SimpleEvaluator>>>;
    type SearchExecutor = Deepening<StdSearchExecutor<StdHashTable, SearchNode>>;
    engine::run_uci::<SearchExecutor, StdTimeManager>("Alcibiades 0.2", "Evgeni Pandurski");
}
