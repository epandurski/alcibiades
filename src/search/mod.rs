//! Basic facilities for implementing game-tree searchers.
//!
//! # Usage
//!
//! To execute a game search, instantiate one of the following types:
//!
//! * `DeepeningSearcher<StandardSearcher>`
//! * `DeepeningSearcher<AspirationSearcher<StandardSearcher>>`
//! * `DeepeningSearcher<MultipvSearcher<StandardSearcher>>`
//!
//! then:
//!
//! 1. Call the `start_search` method.
//!
//! 2. Continue calling `wait_report` and `try_recv_report` methods
//! periodically, until the search is done.
//!
//! 3. Usually, when the search is done (or at least partially
//! completed), `extract_pv` will be called to obtain the primary
//! variation from the transposition table.
//!
//! # Example:
//! ```rust
//! use std::time::Duration;
//! use tt::*;
//! use search::*;
//! use board::evaluation::MaterialEvaluator;
//! use board::rules::Position;
//!
//! let mut tt = Tt::new();
//! tt.resize(16);
//! let tt = Arc::new(tt);
//! let fen = "8/8/8/8/8/7k/7q/7K w - - 0 1";
//! let position = Box::new(Position::<MaterialEvaluator>::from_fen(fen).ok().unwrap());
//! let mut searcher: DeepeningSearcher<AspirationSearcher<StandardSearcher>> =
//!     DeepeningSearcher::new(tt.clone());
//! searcher.start_search(SearchParams {
//!     search_id: 0,
//!     position: position.copy(),
//!     depth: 10,
//!     lower_bound: VALUE_MIN,
//!     upper_bound: VALUE_MAX,
//!     searchmoves: position.legal_moves(),
//!     variation_count: 1,
//! });
//! loop {
//!     searcher.wait_report(Duration::from_millis(20));
//!     if let Ok(report) = searcher.try_recv_report() {
//!         // Process the report here!
//!         if report.done {
//!             break;
//!         }
//!     }
//!     // Do something else here!
//! }
//! let pv = extract_pv(&tt, position.as_ref(), 10);
//! ```
pub mod deepening;
pub mod standard;

use std::cmp::Ordering;
use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use basetypes::*;
use moves::*;
use tt::*;
pub use self::deepening::DeepeningSearcher;
pub use self::deepening::MultipvSearcher;
pub use self::deepening::AspirationSearcher;
pub use self::standard::StandardSearcher;


/// Parameters describing a new search.
pub struct SearchParams {
    /// A number identifying the new search.
    pub search_id: usize,

    /// The root position for the new search.
    pub position: Box<SearchNode>,

    /// The requested search depth.
    pub depth: u8,

    /// The lower bound for the new search (alpha).
    pub lower_bound: Value,

    /// The upper bound for the new search (beta).
    pub upper_bound: Value,

    /// Restricts the analysis to the supplied list of moves only.
    ///
    /// The same move should not occur more than once, and all
    /// supplied moves should be legal. The behavior of the new search
    /// is undefined if `searchmoves` is empty, but the supplied root
    /// position is not final.
    pub searchmoves: Vec<Move>,

    /// Specifies how many best lines of play to calculate (for the
    /// multi-PV mode).
    ///
    /// Must be greater than zero.
    pub variation_count: usize,
}

impl Clone for SearchParams {
    fn clone(&self) -> Self {
        SearchParams {
            position: self.position.copy(),
            searchmoves: self.searchmoves.clone(),
            ..*self
        }
    }
}


/// A progress report from a search.
#[derive(Clone)]
pub struct SearchReport {
    /// The ID assigned to search.
    pub search_id: usize,

    /// The number of positions searched so far.
    pub searched_nodes: NodeCount,

    /// The search depth completed so far.
    pub depth: u8,

    /// The evaluation of the root position so far, or `VALUE_UNKNOWN`
    /// if not available.
    pub value: Value,

    /// The `searchmoves` list sorted by descending move strength (see
    /// `SearchParams`), or an empty list.
    pub sorted_moves: Vec<Move>,

    /// `true` if the search is done, `false` otherwise.
    pub done: bool,
}


/// A trait for executing consecutive searches in different starting
/// positions.
pub trait SearchExecutor {
    /// Creates a new instance.
    fn new(tt: Arc<Tt>) -> Self;

    /// Starts a new search.
    ///
    /// After calling `start_search`, `try_recv_report` must be called
    /// periodically until the returned report indicates that the
    /// search is done. A new search must not be started until the
    /// previous search is done.
    fn start_search(&mut self, params: SearchParams);

    /// Attempts to return a search progress report without blocking.
    fn try_recv_report(&mut self) -> Result<SearchReport, TryRecvError>;

    /// Waits until a search progress report is available, timing out
    /// after a specified duration or earlier.
    fn wait_report(&self, duration: Duration);

    /// Requests the termination of the current search.
    ///
    /// After calling `terminate`, `try_recv_report` must continue to
    /// be called periodically until the returned report indicates
    /// that the search is done.
    fn terminate_search(&mut self);
}


/// A trait for interacting with chess positions.
///
/// `SearchNode` presents a convenient interface to the tree-searching
/// algorithm. A `SearchNode` can generate the all possible moves
/// (plus a "null move") in the current position, play a selected move
/// and take it back. It can also quickly (without doing extensive
/// tree-searching) evaluate the chances of the sides, so that the
/// tree-searching algorithm can use this evaluation to assign
/// realistic game outcomes to its leaf nodes.
///
/// **Important note:** Repeating positions are considered a draw
/// after the first repetition, not after the second one as the chess
/// rules prescribe. This is done in the sake of efficiency.

pub trait SearchNode: Send {
    /// Returns an almost unique hash value for the position.
    ///
    /// **Important notes:** 1) Two positions that differ in their
    /// sets of previously repeated, still reachable boards will have
    /// different hashes. 2) Two positions that differ only in their
    /// number of played moves without capturing piece or advancing a
    /// pawn will have equal hashes, as long as they both are far from
    /// the rule-50 limit.
    fn hash(&self) -> u64;

    /// Returns a description of the placement of the pieces on the
    /// board.
    fn pieces(&self) -> &PiecesPlacement;

    /// Returns the side to move.
    fn to_move(&self) -> Color;

    /// Returns the castling rights.
    fn castling(&self) -> CastlingRights;

    /// Returns the file on which an en-passant pawn capture is
    /// possible.
    fn en_passant_file(&self) -> Option<File>;

    /// Returns if the side to move is in check.
    fn is_check(&self) -> bool;

    /// Returns if the side to move is unlikely to be in zugzwang.
    ///
    /// In many endgame positions there is a relatively high
    /// probability of zugzwang occurring. For such positions, this
    /// method will return `false`. For all "normal" positions it will
    /// return `true`. This is useful when deciding if it is safe to
    /// try a "null move".
    fn is_zugzwang_unlikely(&self) -> bool;

    /// Evaluates a final position.
    ///
    /// In final positions this method will return the correct value
    /// of the position (`0` for a draw, `VALUE_MIN` for a
    /// checkmate). A position is guaranteed to be final if
    /// `generate_moves` method generates no legal moves. (It may
    /// generate some pseudo-legal moves, but if none of them is
    /// legal, then the position is final.)
    ///
    /// **Important note:** Repeated and rule-50 positions are
    /// considered final (a draw).
    fn evaluate_final(&self) -> Value;

    /// Statically evaluates the position.
    ///
    /// This method considers only static material and positional
    /// properties of the position. If the position is dynamic, with
    /// pending tactical threats, this function will return a grossly
    /// incorrect evaluation.
    ///
    /// The returned value will be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`. For repeated and rule-50 positions `0` is
    /// returned.
    fn evaluate_static(&self) -> Value;

    /// Performs a "quiescence search" and returns an evaluation.
    ///
    /// The "quiescence search" is a restricted search which considers
    /// only a limited set of moves (winning captures, pawn promotions
    /// to queen, check evasions). The goal is to statically evaluate
    /// only "quiet" positions (positions where there are no winning
    /// tactical moves to be made). Although this search can cheaply
    /// and correctly resolve many tactical issues, it is blind to
    /// other simple tactical threats like most kinds of forks,
    /// checks, even a checkmate in one move.
    ///
    /// `lower_bound` and `upper_bound` together give the interval
    /// within which an as precise as possible evaluation is
    /// required. If during the calculation is determined that the
    /// exact evaluation is outside of this interval, this method may
    /// return a value that is closer to the the interval bounds than
    /// the exact evaluation, but always staying on the correct side
    /// of the interval. `static_evaluation` should be the value
    /// returned by `self.evaluate_static()`, or `VALUE_UNKNOWN`.
    ///
    /// The returned value will be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`. For repeated and rule-50 positions `0` is
    /// returned.
    ///
    /// **Note:** This method will return a reliable result even when
    /// the side to move is in check. In this case the all possible
    /// check evasions will be tried. It will will never use the
    /// static evaluation value when in check.
    fn evaluate_quiescence(&self,
                           lower_bound: Value,
                           upper_bound: Value,
                           static_evaluation: Value)
                           -> (Value, NodeCount);

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
    /// The move passed to this method **must** have been generated by
    /// `generate_moves`, `try_move_digest`, or `null_move` methods
    /// for the current position on the board.
    fn evaluate_move(&self, m: Move) -> Value;

    /// Generates pseudo-legal moves.
    ///
    /// A pseudo-legal move is a move that is otherwise legal, except
    /// it might leave the king in check. Every legal move is a
    /// pseudo-legal move, but not every pseudo-legal move is legal.
    /// The generated moves will be pushed to `move_stack`. If all of
    /// the moves generated by this methods are illegal (this means
    /// that `do_move(m)` returns `false` for all of them), then the
    /// position is final, and `evaluate_final()` will return its
    /// correct value.
    ///
    /// **Important note:** Repeated and rule-50 positions are
    /// considered final (and therefore, this method generates no
    /// moves).
    fn generate_moves(&self, move_stack: &mut MoveStack);

    /// Returns a null move.
    ///
    /// "Null move" is a pseudo-move that changes only the side to
    /// move. It is sometimes useful to include a speculative null
    /// move in the search tree so as to achieve more aggressive
    /// pruning.
    fn null_move(&self) -> Move;

    /// Checks if `move_digest` represents a pseudo-legal move.
    ///
    /// If a move `m` exists that would be generated by
    /// `generate_moves` if called for the current position, and for
    /// that move `m.digest() == move_digest`, this method will
    /// return `Some(m)`. Otherwise it will return `None`. This is
    /// useful when playing moves from the transposition table,
    /// without calling `generate_moves`.
    fn try_move_digest(&self, move_digest: MoveDigest) -> Option<Move>;

    /// Plays a move on the board.
    ///
    /// It verifies if the move is legal. If the move is legal, the
    /// board is updated and `true` is returned. If the move is
    /// illegal, `false` is returned without updating the board. The
    /// move passed to this method **must** have been generated by
    /// `generate_moves`, `try_move_digest`, or `null_move` methods
    /// for the current position on the board.
    ///
    /// Moves generated by the `null_move` method are exceptions. For
    /// them `do_move(m)` will return `false` only if the king is in
    /// check or the position is a draw due to repetition or rule-50.
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
    fn legal_moves(&self) -> Vec<Move>;

    /// Returns an exact copy of the position.
    fn copy(&self) -> Box<SearchNode>;
}

/// A sequence of moves from some starting position, together with the
/// value assigned to the final position.
pub struct Variation {
    /// A sequence of moves from some starting position.
    pub moves: Vec<Move>,

    /// The value assigned to the final position.
    /// 
    /// **Important note:** Values under `-9999` or over `9999` may be
    /// chopped, because they often look ugly in GUIs.
    pub value: Value,

    /// The accuracy of the assigned value.
    pub bound: BoundType,
}


/// Extracts the primary variation for a given position from the
/// transposition table and returns it.
pub fn extract_pv(tt: &Tt, position: &SearchNode, depth: u8) -> Variation {
    let mut p = position.copy();
    let mut our_turn = true;
    let mut root_value = VALUE_UNKNOWN;
    let mut leaf_value = -9999;
    let mut leaf_bound = BOUND_LOWER;
    let mut pv_moves = Vec::new();

    'move_extraction: while let Some(entry) = tt.probe(p.hash()) {
        if entry.bound() != BOUND_NONE {
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
            if pv_moves.len() < depth as usize && leaf_value == root_value {
                if let Some(m) = p.try_move_digest(entry.move16()) {
                    if p.do_move(m) {
                        pv_moves.push(m);
                        if leaf_bound == BOUND_EXACT {
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
        bound: match leaf_value.cmp(&root_value) {
            Ordering::Greater => BOUND_LOWER,
            Ordering::Less => BOUND_UPPER,
            Ordering::Equal => leaf_bound,
        },
        moves: pv_moves,
    }
}


/// A helper function used by the sub-modules. It checks if the two
/// supplied lists of moves contain the same moves, possibly in
/// different order.
fn contains_same_moves(list1: &Vec<Move>, list2: &Vec<Move>) -> bool {
    let mut list1 = list1.clone();
    let mut list2 = list2.clone();
    list1.sort();
    list2.sort();
    list1 == list2
}


/// A helper function used by the sub-modules. It checks if there are
/// moves in the supplied list that occur more than once.
fn contains_dups(list: &Vec<Move>) -> bool {
    let mut l = list.clone();
    l.sort();
    l.dedup();
    l.len() < list.len()
}
