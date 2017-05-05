//! Defines search-related types and traits.

use std::thread;
use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::{Sender, Receiver, TryRecvError};
use uci::SetOption;
use moves::Move;
use value::*;
use depth::*;
use hash_table::*;
use search_node::SearchNode;


/// Parameters describing a search.
///
/// **Important note:** `lower_bound` and `upper_bound` fields
/// together give the interval within which an as precise as possible
/// evaluation is required. If during the search is determined that
/// the exact evaluation is outside of this interval, the search may
/// return a value that is closer to the the interval bounds than the
/// exact evaluation, but always staying on the correct side of the
/// interval (i.e. "fail-soft" semantics).
#[derive(Clone, Debug)]
pub struct SearchParams<T: SearchNode> {
    /// A number identifying the search.
    pub search_id: usize,

    /// The root position for the search.
    pub position: T,

    /// The requested search depth.
    ///
    /// Should be between `0` and `DEPTH_MAX`.
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
#[derive(Clone, Debug)]
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
    /// Should be no lesser than `0`. Also, no lesser than the value
    /// sent in the previous report, and no greater than the requested
    /// search depth. If the search has not been forcefully stopped,
    /// the last reported `depth` should be the requested search
    /// depth.
    ///
    /// **Note:** Depth-first searches should send `0` in all reports
    /// except the last one.
    pub depth: Depth,

    /// The evaluation of the root position so far, or `VALUE_UNKNOWN`
    /// if not available.
    ///
    /// If the search has not been forcefully stopped, the last report
    /// should contain the calculated final evaluation.
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
    /// For example, this may contain calculated principal
    /// variation(s).
    pub data: T,
}


/// A trait for executing iterative deepening searches.
///
/// Chess programs must rely on some type of search in order to play
/// reasonably. Searching involves looking ahead at different move
/// sequences and evaluating the positions after making the
/// moves. Normally, this is done by traversing and min-maxing a
/// tree-like data-structure by some algorithm. To implement your own
/// search algorithm, you must define a type that implements either
/// `Search` or `SearchExecutor` trait.
///
/// There are two types of searches that should be distinguished:
///
/// * **Depth-first search** (the `Search` trait).
///
///   Starts at the root and explores as far as possible along each
///   branch before backtracking.
///
/// * **Iterative deepening search** (the `SearchExecutor` trait).
///
///   A depth-first search is executed with a depth of one ply, then
///   the depth is incremented and another search is executed. This
///   process is repeated until the search is terminated or the
///   requested search depth is reached. In case of a terminated
///   search, the engine can always fall back to the move selected in
///   the last iteration of the search.
///
///   You can use `stock::Deepening` to turn a depth-first searcher
///   into a deepening searcher.
pub trait SearchExecutor: SetOption {
    /// The type of transposition (hash) table that the implementation
    /// works with.
    type HashTable: HashTable;

    /// The type of search node that the implementation works with.
    type SearchNode: SearchNode;

    /// The type of auxiliary data that search progress reports carry.
    type ReportData;

    /// Creates a new instance.
    ///
    /// `tt` gives a transposition table for the new search executor
    /// to work with.
    fn new(tt: Arc<Self::HashTable>) -> Self;

    /// Starts a new search.
    ///
    /// This method must not block the current thread. After calling
    /// `start_search`, `wait_report` and `try_recv_report` will be
    /// called periodically until the returned report indicates that
    /// the search is done. A new search will not be started until the
    /// previous search is done.
    ///
    /// **Important note:** The executing search must generate
    /// periodic reports, informing about its current progress. Also,
    /// the executing search must continuously update the
    /// transposition table so that, at each moment, it contains the
    /// results of the work done so far.
    fn start_search(&mut self, params: SearchParams<Self::SearchNode>);

    /// Waits until a search progress report is available, timing out
    /// after a specified duration or earlier.
    fn wait_report(&self, timeout_after: Duration);

    /// Attempts to return a search progress report without blocking.
    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError>;

    /// Sends a message to the currently executing search.
    ///
    /// The message format is not specified, but the implementation
    /// **must** meet the following requirements:
    ///
    /// * Unrecognized messages are ignored.
    ///
    /// * The message `"TERMINATE"` is recognized as a request to
    ///   terminate the current search.
    ///
    /// * Receiving two or more termination requests for the same
    ///   search does not cause any problems.
    ///
    /// **Note:** Normally, after sending one or more `"TERMINATE"`
    /// messages, `wait_report` and `try_recv_report` methods will
    /// continue to be called periodically until the returned report
    /// indicates that the search is done.
    fn send_message(&mut self, msg: &str);
}


/// A trait used to spawn depth-first searching threads.
///
/// Chess programs must rely on some type of search in order to play
/// reasonably. Searching involves looking ahead at different move
/// sequences and evaluating the positions after making the
/// moves. Normally, this is done by traversing and min-maxing a
/// tree-like data-structure by some algorithm. To implement your own
/// search algorithm, you must define a type that implements either
/// `Search` or `SearchExecutor` trait.
///
/// There are two types of searches that should be distinguished:
///
/// * **Depth-first search** (the `Search` trait).
///
///   Starts at the root and explores as far as possible along each
///   branch before backtracking.
///
/// * **Iterative deepening search** (the `SearchExecutor` trait).
///
///   A depth-first search is executed with a depth of one ply, then
///   the depth is incremented and another search is executed. This
///   process is repeated until the search is terminated or the
///   requested search depth is reached. In case of a terminated
///   search, the engine can always fall back to the move selected in
///   the last iteration of the search.
///
///   You can use `stock::Deepening` to turn a depth-first searcher
///   into a deepening searcher.
pub trait Search: SetOption {
    /// The type of transposition (hash) table that the implementation
    /// works with.
    type HashTable: HashTable;

    /// The type of search node that the implementation works with.
    type SearchNode: SearchNode;

    /// The type of auxiliary data that search progress reports carry.
    type ReportData;

    /// Spawns a new depth-first searching thread.
    ///
    /// * `params` specifies the exact parameters for the new search
    ///   -- starting position, search depth etc.
    ///
    /// * `tt` supplies a transposition table instance.
    ///
    ///   The search thread must continuously update `tt` so that, at
    ///   each moment, it contains the results of the work done so
    ///   far.
    ///
    /// * `reports_tx` gives the sending-half of progress reports'
    ///   channel.
    ///
    ///   The search thread must send periodic reports to
    ///   `reports_tx`, informing about the current progress of the
    ///   search.
    ///
    /// * `messages_rx` gives the receiving-half of control messages'
    ///   channel.
    ///
    ///   Control messages' format is not specified, but the
    ///   implementation **must** meet the following requirements:
    ///
    ///   * Unrecognized messages are ignored.
    ///
    ///   * The message `"TERMINATE"` is recognized as a request to
    ///     terminate the search.
    ///
    ///   * Receiving two or more termination requests does not cause
    ///     problems.
    fn spawn(params: SearchParams<Self::SearchNode>,
             tt: Arc<Self::HashTable>,
             reports_tx: Sender<SearchReport<Self::ReportData>>,
             messages_rx: Receiver<String>) -> thread::JoinHandle<Value>;
}
