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
pub mod deepening;
pub mod searchers;
pub mod tt;

use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use chesstypes::*;
use engine::SetOption;

pub use self::move_stack::MoveStack;


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
/// interval.
#[derive(Clone)]
pub struct SearchParams<T: SearchNode> {
    /// A number identifying the new search.
    pub search_id: usize,

    /// The root position for the new search.
    pub position: T,

    /// The requested search depth.
    ///
    /// No greater than `DEPTH_MAX`.
    pub depth: u8,

    /// The lower bound for the new search.
    ///
    /// No lesser than `VALUE_MIN`.
    pub lower_bound: Value,

    /// The upper bound for the new search.
    ///
    /// Greater than `lower_bound`, no greater than `VALUE_MAX`.
    pub upper_bound: Value,

    /// Restricts the analysis to the supplied list of moves only.
    ///
    /// The same move should not occur more than once, and all
    /// supplied moves should be legal. The behavior of the new search
    /// is undefined if `searchmoves` is empty, but the supplied root
    /// position is not final.
    pub searchmoves: Vec<Move>,
}


/// A progress report from a search.
#[derive(Clone)]
pub struct SearchReport {
    /// The ID assigned to search.
    pub search_id: usize,

    /// The number of positions searched so far.
    pub searched_nodes: u64,

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
    /// Creates a new instance.
    ///
    /// * `value` -- The value assigned to the position. Should not be
    ///   `VALUE_UNKNOWN`.
    ///
    /// * `bound` -- The accuracy of the assigned `value`.
    ///
    /// * `depth` -- The depth of search. Should be no greater than
    ///   `DEPTH_MAX`.
    ///
    /// * `move_digest` -- Best or refutation move digest, or `0` if
    ///   no move is available.
    ///
    /// * `eval_value` -- The calculated static evaluation for the
    ///   position, or `VALUE_UNKNOWN`.
    fn new(value: Value,
           bound: BoundType,
           depth: u8,
           move_digest: MoveDigest,
           eval_value: Value)
           -> Self;

    fn value(&self) -> Value;
    fn bound(&self) -> BoundType;
    fn depth(&self) -> u8;
    fn move_digest(&self) -> MoveDigest;
    fn eval_value(&self) -> Value;
}


/// A trait for executing consecutive searches in different starting
/// positions.
///
/// Here is what the engine does on each move:
///
/// 1. The engine calls the `start_search` method.
///
/// 2. The engine continues calling `wait_report` and
///    `try_recv_report` methods periodically, until the returned
///    report indicates that the search is done.
///
/// 3. On each completed search depth, the primary variation is
///    obtained from the transposition table.
pub trait SearchExecutor: SetOption {
    /// The type of transposition (hash) table that the implementation
    /// works with.
    type HashTable: HashTable;
    
    /// The type of search node that the implementation works with.
    type SearchNode: SearchNode;

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
    fn try_recv_report(&mut self) -> Result<SearchReport, TryRecvError>;

    /// Waits until a search progress report is available, timing out
    /// after a specified duration or earlier.
    fn wait_report(&self, duration: Duration);

    /// Requests the termination of the current search.
    ///
    /// After calling `terminate`, `wait_report` and `try_recv_report`
    /// will continue to be called periodically until the returned
    /// report indicates that the search is done.
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
/// rules prescribe. This is done in the sake of efficiency. In order
/// to compensate for that, `SearchNode::create` should "forget" all
/// positions that have occurred exactly once. Also, the newly created
/// instance should never be deemed a draw due to repetition or
/// rule-50.
pub trait SearchNode: Send + Sized + Clone + SetOption {
    /// Instantiates a new chess position from playing history.
    ///
    /// `fen` should be the Forsyth–Edwards Notation of a legal
    /// starting position. `moves` should be an iterator over all the
    /// moves that were played from that position. The move format is
    /// long algebraic notation. Examples: `e2e4`, `e7e5`, `e1g1`
    /// (white short castling), `e7e8q` (for promotion).
    fn create(fen: &str, moves: &mut Iterator<Item = &str>) -> Result<Self, String>;
    
    /// Returns an almost unique hash value for the position.
    ///
    /// The returned value is good for use as transposition table key.
    fn hash(&self) -> u64;

    /// Returns a description of the placement of the pieces on the
    /// board.
    fn pieces(&self) -> &PiecesPlacement;

    /// Returns the side to move.
    fn to_move(&self) -> Color;

    /// Returns the castling rights.
    fn castling_rights(&self) -> CastlingRights;

    /// If the previous move was a double pawn push, returns double
    /// pushed pawn's file.
    fn en_passant_file(&self) -> Option<File>;

    /// Returns the number of half-moves since the last piece capture
    /// or pawn advance.
    fn halfmove_clock(&self) -> u8;

    /// Returns the count of half-moves since the beginning of the
    /// game.
    ///
    /// At the beginning of the game it starts at `0`, and is
    /// incremented after anyone's move.
    fn halfmove_count(&self) -> u16;

    /// Returns if the side to move is in check.
    fn is_check(&self) -> bool;

    /// Returns if the side to move is unlikely to be in zugzwang.
    ///
    /// In many endgame positions there is a relatively high
    /// probability of zugzwang occurring. For such positions, this
    /// method will return `false`. This is useful when deciding if it
    /// is safe to try a "null move".
    fn is_zugzwang_unlikely(&self) -> bool;

    /// Evaluates a final position.
    ///
    /// In final positions this method will return the correct value
    /// of the position (`0` for a draw, `VALUE_MIN` for a
    /// checkmate). A position is guaranteed to be final if
    /// `generate_moves` method generates no legal moves. (It may
    /// generate some pseudo-legal moves, but if none of them is
    /// legal, then the position is final.)
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

    /// Performs quiescence search and returns an evaluation.
    ///
    /// Quiescence search is a restricted search which considers only
    /// a limited set of moves (for example: winning captures, pawn
    /// promotions to queen, check evasions). The goal is to
    /// statically evaluate only "quiet" positions (positions where
    /// there are no winning tactical moves to be made). Although this
    /// search can cheaply and correctly resolve many tactical issues,
    /// it is blind to other simple tactical threats like most kinds
    /// of forks, checks, even a checkmate in one move.
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
    /// **Important note:** No moves will be generated in repeated and
    /// rule-50 positions.
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
    fn legal_moves(&self) -> Vec<Move>;
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
