//! Facilities for implementing game-tree searching.
//!
//! Chess programs must rely on some type of search in order to play
//! reasonably. Searching involves looking ahead at different move
//! sequences and evaluating the positions after making the
//! moves. Normally, this is done by traversing and min-maxing a
//! tree-like data-structure by some algorithm.
//!
//! To implement your own search algorithm, you must define a type
//! that implements the `SearchExecutor` trait.

mod move_stack;
mod position;
pub mod searchers;
pub mod tt;

use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use uci::SetOption;
use chesstypes::*;
use board::*;
use board::notation::NotationError;

pub use self::move_stack::MoveStack;
pub use self::position::Position;


/// The maximum search depth in half-moves.
pub const DEPTH_MAX: u8 = 63;


/// Parameters describing a new search.
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
    /// A number identifying the new search.
    pub search_id: usize,

    /// The root position for the new search.
    pub position: T,

    /// The requested search depth.
    ///
    /// Should be no greater than `DEPTH_MAX`.
    pub depth: u8,

    /// The lower bound for the new search.
    ///
    /// Should be no lesser than `VALUE_MIN`.
    pub lower_bound: Value,

    /// The upper bound for the new search.
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
    pub depth: u8,

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
    /// For example, this may contain the primary variation(s)
    /// calculated do far.
    pub data: T,
}


/// A trait for transposition tables.
/// 
/// Chess programs, during their brute-force search, encounter the
/// same positions again and again, but from different sequences of
/// moves, which is called a "transposition". When the search
/// encounters a transposition, it is beneficial to "remember" what
/// was determined last time the position was examined, rather than
/// redoing the entire search again. For this reason, chess programs
/// have a transposition table, which is a large hash table storing
/// information about positions previously searched, how deeply they
/// were searched, and what we concluded about them.
pub trait HashTable: Sync + Send {
    type Entry: HashTableEntry;

    /// Creates a new transposition table.
    ///
    /// `size_mb` is the desired size in Mbytes.
    fn new(size_mb: Option<usize>) -> Self;

    /// Signals that a new search is about to begin.
    fn new_search(&self);

    /// Stores data by key.
    ///
    /// After being stored, the data can be retrieved by `probe`. This
    /// is not guaranteed though, because the entry might have been
    /// overwritten in the meantime.
    fn store(&self, key: u64, mut data: Self::Entry);

    /// Probes for data by key.
    fn probe(&self, key: u64) -> Option<Self::Entry>;

    /// Removes all entries in the table.
    fn clear(&self);
}


/// A trait for transposition table entries.
pub trait HashTableEntry: Copy {
    /// The type of auxiliary data that the implementation works with.
    type AuxData;

    /// Creates a new instance.
    ///
    /// * `value` -- The value assigned to the position. Must not be
    ///   `VALUE_UNKNOWN`.
    ///
    /// * `bound` -- The accuracy of the assigned `value`.
    ///
    /// * `depth` -- The depth of search. Must be no greater than
    ///   `DEPTH_MAX`.
    ///
    /// * `move_digest` -- Best or refutation move digest, or `0` if
    ///   no move is available.
    ///
    /// * `data` -- Auxiliary data.
    fn new(value: Value,
           bound: BoundType,
           depth: u8,
           move_digest: MoveDigest,
           data: Self::AuxData)
           -> Self;

    /// Returns the `value` passed to the constructor.
    fn value(&self) -> Value;

    /// Returns the `bound` passed to the constructor.
    fn bound(&self) -> BoundType;

    /// Returns the `depth` passed to the constructor.
    fn depth(&self) -> u8;

    /// Returns the `move_digest` passed to the constructor.
    fn move_digest(&self) -> MoveDigest;

    /// Returns the auxiliary data passed to the constructor.
    fn data(&self) -> Self::AuxData;
}


/// A trait for executing consecutive searches in different starting
/// positions.
///
/// Here is what the engine does on each move:
///
/// 1. Calls `start_search`.
///
/// 2. Continues calling `wait_report` and `try_recv_report`
///    periodically, until the returned report indicates that the
///    search is done.
///
/// 3. Obtains the primary variation(s) from search reports, or
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


/// A trait for chess positions.
///
/// `SearchNode` presents a convenient interface to the tree-searching
/// algorithm. A `SearchNode` can generate all possible moves (plus a
/// "null move") in the current position, play a selected move and
/// take it back. It can also quickly (without doing extensive
/// tree-searching) evaluate the chances of the sides, so that the
/// tree-searching algorithm can use this evaluation to assign
/// realistic game outcomes to its leaf nodes.
///
/// **Important note:** Repeating positions are considered a draw
/// after the first repetition, not after the second one as the chess
/// rules prescribe. In order to compensate for that,
/// `SearchNode::from_history` "forgets" all positions that have
/// occurred exactly once. Also, the newly created instance is never
/// deemed a draw due to repetition or rule-50.
pub trait SearchNode: Send + Clone + SetOption {
    /// The type of static evaluator that the implementation works
    /// with.
    type BoardEvaluator: BoardEvaluator;

    /// Instantiates a new chess position from playing history.
    ///
    /// `fen` should be the Forsyth–Edwards Notation of a legal
    /// starting position. `moves` should be an iterator over all the
    /// moves that were played from that position. The move format is
    /// long algebraic notation. Examples: `e2e4`, `e7e5`, `e1g1`
    /// (white short castling), `e7e8q` (for promotion).
    fn from_history(fen: &str, moves: &mut Iterator<Item = &str>) -> Result<Self, NotationError>;

    /// Returns an almost unique hash value for the position.
    ///
    /// The returned value is good for use as transposition table key.
    fn hash(&self) -> u64;

    /// Returns a reference to the underlying `Board` instance.
    fn board(&self) -> &Board;

    /// Returns the number of half-moves since the last piece capture
    /// or pawn advance.
    fn halfmove_clock(&self) -> u8;

    /// Returns if the side to move is in check.
    fn is_check(&self) -> bool;

    /// Returns a reference to a static evaluator bound to the current
    /// position.
    fn evaluator(&self) -> &Self::BoardEvaluator;

    /// Evaluates a final position.
    ///
    /// In final positions this method will return the correct value
    /// of the position (`0` for a draw, `VALUE_MIN` for a
    /// checkmate). A position is guaranteed to be final if
    /// `generate_moves` method generates no legal moves. (It may
    /// generate some pseudo-legal moves, but if none of them is
    /// legal, then the position is final.)
    fn evaluate_final(&self) -> Value;

    /// Performs quiescence search and returns an evaluation.
    ///
    /// Quiescence search is a restricted search which considers only
    /// a limited set of moves (for example: winning captures, pawn
    /// promotions to queen, check evasions). The goal is to
    /// statically evaluate only "quiet" positions (positions where
    /// there are no winning tactical moves to be made). Although this
    /// search can cheaply and correctly resolve many simple tactical
    /// issues, it is completely blind to the more complex ones.
    ///
    /// `lower_bound` and `upper_bound` together give the interval
    /// within which an as precise as possible evaluation is
    /// required. If during the calculation is determined that the
    /// exact evaluation is outside of this interval, this method may
    /// return a value that is closer to the the interval bounds than
    /// the exact evaluation, but always staying on the correct side
    /// of the interval. `static_evaluation` should be position's
    /// static evaluation, or `VALUE_UNKNOWN`.
    ///
    /// The first slot in the returned tuple is the calculated
    /// evaluation. It will always be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`. For repeated and rule-50 positions it will
    /// be `0`. The second slot in the returned tuple is the number of
    /// positions that were searched in order to calculate the
    /// evaluation.
    ///
    /// **Important note:** This method will return a reliable result
    /// even when the side to move is in check.
    fn evaluate_quiescence(&self,
                           lower_bound: Value,
                           upper_bound: Value,
                           static_evaluation: Value)
                           -> (Value, u64);

    /// Returns the likely evaluation change (material) to be lost or
    /// gained as a result of a given move.
    ///
    /// This method performs static exchange evaluation (SEE). It
    /// examines the consequence of a series of exchanges on the
    /// destination square after a given move. A positive returned
    /// value indicates a "winning" move. For example, "PxQ" will
    /// always be a win, since the pawn side can choose to stop the
    /// exchange after its pawn is recaptured, and still be ahead. SEE
    /// is just an evaluation calculated without actually trying moves
    /// on the board, and therefore the returned value might be
    /// incorrect.
    ///
    /// The move passed to this method must have been generated by
    /// `generate_moves`, `try_move_digest`, or `null_move` methods
    /// for the current position on the board.
    fn evaluate_move(&self, m: Move) -> Value;

    /// Generates all legal moves, possibly including some
    /// pseudo-legal moves too.
    ///
    /// A pseudo-legal move is a move that is otherwise legal, except
    /// it might leave the king in check. Every legal move is a
    /// pseudo-legal move, but not every pseudo-legal move is legal.
    /// The generated moves will be added to `moves`. If all of the
    /// moves generated by this methods are illegal (this means that
    /// `do_move(m)` returns `false` for all of them), then the
    /// position is final, and `evaluate_final()` will return its
    /// correct value.
    ///
    /// **Important note:** No moves will be generated in repeated and
    /// rule-50 positions.
    fn generate_moves<T: AddMove>(&self, moves: &mut T);

    /// Checks if `move_digest` represents a pseudo-legal move.
    ///
    /// If a move `m` exists that would be generated by
    /// `generate_moves` if called for the current position, and for
    /// that move `m.digest() == move_digest`, this method will
    /// return `Some(m)`. Otherwise it will return `None`. This is
    /// useful when playing moves from the transposition table,
    /// without calling `generate_moves`.
    fn try_move_digest(&self, move_digest: MoveDigest) -> Option<Move>;

    /// Returns a null move.
    ///
    /// "Null move" is a pseudo-move that changes only the side to
    /// move. It is sometimes useful to include a speculative null
    /// move in the search tree so as to achieve more aggressive
    /// pruning. Null moves are represented as king's moves for which
    /// the origin and destination squares are the same.
    fn null_move(&self) -> Move;

    /// Plays a move on the board.
    ///
    /// It the move leaves the king in check, `false` is returned
    /// without updating the board. Otherwise the board is updated and
    /// `true` is returned. The move passed to this method must have
    /// been generated by `generate_moves`, `try_move_digest`, or
    /// `null_move` methods for the current position on the board.
    ///
    /// **Important note:** For null moves, if the position is a draw
    /// due to repetition or rule-50, `do_move` will return `false`.
    fn do_move(&mut self, m: Move) -> bool;

    /// Takes back the last played move.
    fn undo_move(&mut self);

    /// Returns all legal moves in the position.
    ///
    /// No moves are returned for repeated and rule-50 positions.
    ///
    /// **Important note:** This method is slower than
    /// `generate_moves` because it ensures that all returned moves
    /// are legal.
    fn legal_moves(&self) -> Vec<Move> {
        let mut position = self.clone();
        let mut legal_moves = Vec::with_capacity(96);
        let mut v = Vec::with_capacity(96);
        position.generate_moves(&mut v);
        for m in v.iter() {
            if position.do_move(*m) {
                legal_moves.push(*m);
                position.undo_move();
            }
        }
        legal_moves
    }
}


/// `BOUND_EXACT`, `BOUND_LOWER`, `BOUND_UPPER`, or `BOUND_NONE`.
///
/// For the majority of chess positions our evaluations will be more
/// or less inaccurate, and there is nothing we can do about it. But
/// sometimes we know that a given evaluation is probably inaccurate,
/// and we know the sign of the error. `BoundType` defines the
/// direction of such **known inaccuracies**.
///
/// # Constants:
///
/// * `BOUND_EXACT` means that the evaluation is exact (as far as we know).
///
/// * `BOUND_LOWER` means that the real value is greater or equal to
///    the evaluation (as far as we know).
///
/// * `BOUND_UPPER` means that the real value is lesser or equal to
///   the evaluation (as far as we know).
///
/// * `BOUND_NONE` means that the real value can be anything.
pub type BoundType = u8;

pub const BOUND_NONE: BoundType = 0;
pub const BOUND_LOWER: BoundType = 0b01;
pub const BOUND_UPPER: BoundType = 0b10;
pub const BOUND_EXACT: BoundType = BOUND_UPPER | BOUND_LOWER;


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


/// Extracts the primary variation for a given position from the
/// transposition table.
///
/// **Important note:** Evaluations under `-9999`, or over `9999` will
/// be chopped.
pub fn extract_pv<T: HashTable, N: SearchNode>(tt: &T, position: &N, depth: u8) -> Variation {
    assert!(depth <= DEPTH_MAX, "invalid depth: {}", depth);
    let mut p = position.clone();
    let mut our_turn = true;
    let mut root_value = VALUE_UNKNOWN;
    let mut leaf_value = 9999;
    let mut leaf_bound = BOUND_UPPER;
    let mut pv_moves = Vec::new();

    'move_extraction: while let Some(entry) = tt.probe(p.hash()) {
        let pv_length = pv_moves.len() as u8;

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
