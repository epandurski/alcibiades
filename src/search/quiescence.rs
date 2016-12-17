//! Facilities for implementing quiescence searching.
//!
//! Quiescence search is a restricted search which considers only a
//! limited set of moves (for example: winning captures, pawn
//! promotions to queen, check evasions). The goal is to statically
//! evaluate only "quiet" positions (positions where there are no
//! winning tactical moves to be made). Although this search can
//! cheaply and correctly resolve many simple tactical issues, it is
//! completely blind to the more complex ones.
//!
//! To implement your own quiescence search routine, you must define a
//! type that implements the `Qsearch` trait.

use uci::SetOption;
use search::*;


/// Parameters describing a quiescence search.
pub struct QsearchParams<'a, T: MoveGenerator + 'a> {
    /// A mutable reference to the root position for the search.
    ///
    /// **Important note:** The search routine may use this reference
    /// to do and undo moves, but when the search is finished, all
    /// played moves must be taken back so that the board is restored
    /// to its original state.
    pub position: &'a mut T,

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


/// A trait for performing quiescence searches.
pub trait Qsearch: SetOption + Send {
    /// The type of move generator that the implementation works with.
    type MoveGenerator: MoveGenerator;

    /// The type of result object that the search produces.
    type QsearchResult: QsearchResult;

    /// Performs a quiescence search and returns a result object.
    fn qsearch(params: QsearchParams<Self::MoveGenerator>) -> Self::QsearchResult;
}
