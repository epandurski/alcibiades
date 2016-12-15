//! Facilities for implementing quiescence searching.
//!
//! Most chess programs, at the end of the main search perform a more
//! limited quiescence search, containing fewer moves. The purpose of
//! this search is to only evaluate "quiet" positions, or positions
//! where there are no winning tactical moves to be made. Simply
//! stopping your search when you reach the desired depth and then
//! evaluate, is very dangerous. Consider the situation where the last
//! move is QxP. If you stop there and evaluate, you might think that
//! you have won a pawn. But what if you were to search one move
//! deeper and find that the next move is PxQ? You didn't win a pawn,
//! you actually lost a queen. Hence the need to make sure that you
//! are evaluating only quiescent (quiet) positions.

use chesstypes::*;
use board::MoveGenerator;


/// Parameters describing a quiescence search.
pub struct QsearchParams<T: MoveGenerator> {
    /// The root position for the search.
    pub position: T,

    /// The requested search depth.
    ///
    /// This is the depth at which the main search stops and the
    /// quiescence search takes on. It should be between `DEPTH_MIN`
    /// and `DEPTH_MAX`. Normally, it will be zero or less. The
    /// quiescence search implementation may decide to perform less
    /// thorough analysis when `depth` is smaller.
    pub depth: Depth,

    /// The lower bound for the new search.
    ///
    /// Should be no lesser than `VALUE_MIN`.
    pub lower_bound: Value,

    /// The upper bound for the new search.
    ///
    /// Should be greater than `lower_bound`, but no greater than
    /// `VALUE_MAX`.
    pub upper_bound: Value,

    /// Position's static evaluation, or `VALUE_UNKNOWN`.
    ///
    /// Saves the re-calculation if position's static evaluation is
    /// already available.
    pub static_evaluation: Value,
}


/// Results from a quiescence search.
pub struct QsearchResult<T> {
    /// The calculated evaluation for the analyzed position.
    pub value: Value,

    /// The number of positions that were searched in order to
    /// calculate the evaluation.
    pub searched_nodes: u64,

    /// Auxiliary hints regarding the analyzed position.
    pub hints: T,
}


/// A trait used to perform quiescence searches.
pub trait Qsearch {
    type MoveGenerator: MoveGenerator;
    type Hints;

    /// TODO
    fn qsearch(params: QsearchParams<Self::MoveGenerator>) -> QsearchResult<Self::Hints>;
}
