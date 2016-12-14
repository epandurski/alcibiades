use chesstypes::*;
use board::MoveGenerator;


pub struct QsearchResult<T> {
    value: Value,
    searched_nodes: u64,
    hints: T,
}

/// A trait used to perform quiescence searches.
pub trait Qsearch {
    type Hints;
    type MoveGenerator: MoveGenerator;

    /// TODO
    fn qsearch(position: Self::MoveGenerator,
               halfmove_clock: u8,
               depth: Depth,
               lower_bound: Value,
               upper_bound: Value,
               static_evaluation: Value)
               -> QsearchResult<Self::Hints>;
}
