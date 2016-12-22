//! Facilities for implementing search executors.

use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use uci::SetOption;
use moves::*;
use value::*;
use depth::*;
use hash_table::*;
use search_node::*;


/// Parameters describing a search.
///
/// **Important note:** `lower_bound` and `upper_bound` fields
/// together give the interval within which an as precise as possible
/// evaluation is required. If during the search is determined that
/// the exact evaluation is outside of this interval, the search may
/// return a value that is closer to the the interval bounds than the
/// exact evaluation, but always staying on the correct side of the
/// interval (i.e. "fail-soft").
#[derive(Clone)]
pub struct SearchParams<T: SearchNode> {
    /// A number identifying the search.
    pub search_id: usize,

    /// The root position for the search.
    pub position: T,

    /// The requested search depth.
    ///
    /// Should be between `DEPTH_MIN` and `DEPTH_MAX`.
    pub depth: Depth,

    /// The lower bound for the search.
    ///
    /// Should be no lesser than `VALUE_MIN`.
    pub lower_bound: Value,

    /// The upper bound for the search.
    ///
    /// Should be greater than `lower_bound`, but no greater than
    /// `VALUE_MAX`.
    pub upper_bound: Value,

    /// Restricts the analysis to the supplied list of moves only.
    ///
    /// * All moves in the list should be legal.
    ///
    /// * The same move should not occur more than once.
    ///
    /// * If the root position is final, the supplied list of moves
    ///   should be empty.
    ///
    /// The behavior of the search is *undefined* if the root position
    /// is not final, but `searchmoves` is empty.
    pub searchmoves: Vec<Move>,
}


/// A progress report from a search.
#[derive(Clone)]
pub struct SearchReport<T> {
    /// The ID assigned to the search.
    ///
    /// Should be the same for all reports from a given search.
    pub search_id: usize,

    /// The number of positions searched so far.
    ///
    /// Should be no lesser than the value sent in the previous
    /// report.
    pub searched_nodes: u64,

    /// The search depth completed so far.
    ///
    /// Should be no lesser than the value sent in the previous
    /// report, and no greater than the requested search depth. If the
    /// search has not been forcefully terminated, the last reported
    /// `depth` should be the requested search depth.
    ///
    /// **Note:** Depth-first searches should send `0` in all reports
    /// except the last one.
    pub depth: Depth,

    /// The evaluation of the root position so far, or `VALUE_UNKNOWN`
    /// if not available.
    ///
    /// If the search has not been forcefully terminated, the last
    /// report should contain the calculated final evaluation.
    ///
    /// **Note:** Depth-first searches should send `VALUE_UNKNOWN` in
    /// all reports except the last one.
    pub value: Value,

    /// Whether the search is done.
    ///
    /// Should be `false` for all reports except the last one.
    pub done: bool,

    /// Auxiliary data.
    ///
    /// For example, this may contain the principal variation(s)
    /// calculated do far.
    pub data: T,
}


/// A trait for executing consecutive searches in different starting
/// positions.
///
/// Chess programs must rely on some type of search in order to play
/// reasonably. Searching involves looking ahead at different move
/// sequences and evaluating the positions after making the
/// moves. Normally, this is done by traversing and min-maxing a
/// tree-like data-structure by some algorithm. To implement your own
/// search algorithm, you must define a type that implements the
/// `SearchExecutor` trait.
/// 
/// Here is what the engine does on each move:
///
/// 1. Calls `start_search`.
///
/// 2. Continues calling `wait_report` and `try_recv_report`
///    periodically, until the returned report indicates that the
///    search is done.
///
/// 3. Obtains the principal variation(s) from search reports, or
///    directly from the transposition table.
pub trait SearchExecutor: SetOption {
    /// The type of transposition (hash) table that the implementation
    /// works with.
    type HashTable: HashTable;

    /// The type of search node that the implementation works with.
    type SearchNode: SearchNode;

    /// The type of auxiliary data that search progress reports carry.
    type ReportData;

    /// Creates a new instance.
    fn new(tt: Arc<Self::HashTable>) -> Self;

    /// Starts a new search.
    ///
    /// After calling `start_search`, `wait_report` and
    /// `try_recv_report` will be called periodically until the
    /// returned report indicates that the search is done. A new
    /// search will not be started until the previous search is done.
    ///
    /// **Important note:** The executing search must send periodic
    /// reports, informing about its current progress. Also, the
    /// executing search must continuously update the transposition
    /// table so that, at each moment, it contains the results of the
    /// work done so far.
    fn start_search(&mut self, params: SearchParams<Self::SearchNode>);

    /// Attempts to return a search progress report without blocking.
    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError>;

    /// Waits until a search progress report is available, timing out
    /// after a specified duration or earlier.
    fn wait_report(&self, duration: Duration);

    /// Requests the termination of the current search.
    ///
    /// Can be called more than once for the same search. After
    /// calling `terminate_search`, `wait_report` and
    /// `try_recv_report` will continue to be called periodically
    /// until the returned report indicates that the search is done.
    fn terminate_search(&mut self);
}


/// A sequence of moves from some starting position, together with the
/// value assigned to the final position.
pub struct Variation {
    /// A sequence of moves from some starting position.
    pub moves: Vec<Move>,

    /// The value assigned to the final position.
    ///
    /// The value is from the point of view of player that has the
    /// move in the starting position.
    pub value: Value,

    /// The accuracy of the assigned value.
    pub bound: BoundType,
}


/// Extracts the principal variation for a given position from the
/// transposition table.
///
/// The principal variation (PV) is the sequence of moves that the
/// engine considers best and therefore expects to be played.
///
/// **Important note:** Evaluations under `-9999`, or over `9999` will
/// be chopped.
pub fn extract_pv<T: HashTable, N: SearchNode>(tt: &T, position: &N, depth: Depth) -> Variation {
    assert!(depth <= DEPTH_MAX, "invalid depth: {}", depth);
    let mut p = position.clone();
    let mut our_turn = true;
    let mut root_value = VALUE_UNKNOWN;
    let mut leaf_value = 9999;
    let mut leaf_bound = BOUND_UPPER;
    let mut pv_moves = Vec::new();

    'move_extraction: while let Some(entry) = tt.probe(p.hash()) {
        let pv_length = pv_moves.len() as i8;

        // Before considering the next value from the transposition
        // table, we make sure that it is reliable enough, and at
        // least as reliable as the one we already have.
        if entry.depth() >= depth - pv_length &&
           (entry.bound() == BOUND_EXACT ||
            root_value == VALUE_UNKNOWN && entry.bound() != BOUND_NONE) {

            // Get the next value and the bound type. (Note that in
            // half of the cases the value stored in `entry` is from
            // other side's perspective. Also, note that we chop
            // values under -9999 or over 9999.)
            if our_turn {
                leaf_value = entry.value();
                leaf_bound = entry.bound();
            } else {
                leaf_value = -entry.value();
                leaf_bound = match entry.bound() {
                    BOUND_UPPER => BOUND_LOWER,
                    BOUND_LOWER => BOUND_UPPER,
                    x => x,
                };
            }
            debug_assert!(leaf_value != VALUE_UNKNOWN);
            if leaf_value <= -9999 {
                leaf_value = -9999;
                if leaf_bound == BOUND_UPPER {
                    leaf_bound = BOUND_EXACT
                }
            } else if leaf_value >= 9999 {
                leaf_value = 9999;
                if leaf_bound == BOUND_LOWER {
                    leaf_bound = BOUND_EXACT
                }
            }
            if root_value == VALUE_UNKNOWN {
                root_value = leaf_value;
            }

            // Continue the move extraction cycle until `depth` is
            // reached or `leaf_value` has diverged from `root_value`.
            if pv_length < depth && leaf_value == root_value {
                if let Some(m) = p.try_move_digest(entry.move_digest()) {
                    if p.do_move(m) {
                        pv_moves.push(m);
                        if entry.bound() == BOUND_EXACT {
                            our_turn = !our_turn;
                            continue 'move_extraction;
                        }
                    }
                }
            }
        }
        break 'move_extraction;
    }

    Variation {
        value: if root_value != VALUE_UNKNOWN {
            root_value
        } else {
            leaf_value
        },
        bound: leaf_bound,
        moves: pv_moves,
    }
}
