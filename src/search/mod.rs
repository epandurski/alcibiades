//! Facilities for implementing game-tree searchers.
//!
//! Chess programs must rely on some type of search in order to play
//! reasonably. Searching involves looking ahead at different move
//! sequences and evaluating the positions after making the
//! moves. Normally, this is done by traversing and min-maxing a
//! tree-like data-structure by some algorithm.
//!
//! To implement your own search algorithm, you must define a type
//! that implements the `SearchExecutor` trait.

pub mod deepening;
pub mod searchers;

use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use std::slice;
use chesstypes::*;
use tt::*;
use uci::SetOption;


/// Parameters describing a new search.
///
/// **Important note:** `lower_bound` and `upper_bound` fields
/// together give the interval within which an as precise as possible
/// evaluation is required. If during the search is determined that
/// the exact evaluation is outside of this interval, the search may
/// return a value that is closer to the the interval bounds than the
/// exact evaluation, but always staying on the correct side of the
/// interval.
pub struct SearchParams {
    /// A number identifying the new search.
    pub search_id: usize,

    /// The root position for the new search.
    pub position: Box<SearchNode>,

    /// The requested search depth.
    ///
    /// No greater than `DEPTH_MAX`.
    pub depth: u8,

    /// The lower bound for the new search.
    ///
    /// Lesser than `upper_bound`, no lesser than `VALUE_MIN`.
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
///
/// **Important note:** The executing search must send periodic
/// reports, informing about its current progress. Also, the executing
/// search must continuously update the transposition table so that,
/// at each moment, it contains the results of the work done so far.
pub trait SearchExecutor: SetOption {
    /// Creates a new instance.
    fn new(tt: Arc<Tt>) -> Self;

    /// Starts a new search.
    ///
    /// After calling `start_search`, `try_recv_report` will be called
    /// periodically until the returned report indicates that the
    /// search is done. A new search will not be started until the
    /// previous search is done.
    fn start_search(&mut self, params: SearchParams);

    /// Attempts to return a search progress report without blocking.
    fn try_recv_report(&mut self) -> Result<SearchReport, TryRecvError>;

    /// Waits until a search progress report is available, timing out
    /// after a specified duration or earlier.
    fn wait_report(&self, duration: Duration);

    /// Requests the termination of the current search.
    ///
    /// After calling `terminate`, `try_recv_report` will continue to
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
    fn castling_rights(&self) -> CastlingRights;

    /// Returns the file on which an en-passant pawn capture is
    /// possible.
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
    /// The first slot in the returned tuple is the calculated
    /// evaluation. It will always be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`. For repeated and rule-50 positions it will
    /// be `0`. The second slot in the returned tuple is the number of
    /// positions that were searched in order to calculate the
    /// evaluation.
    ///
    /// **Note:** This method will return a reliable result even when
    /// the side to move is in check. In this case the all possible
    /// check evasions will be tried. It will will never use the
    /// static evaluation value when in check.
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


/// A trait for instantiating chess positions.
pub trait SearchNodeFactory: SetOption {
    type T: SearchNode;

    /// Creates a new instance from a Forsyth–Edwards Notation (FEN)
    /// string.
    ///
    /// Verifies that the position is legal.
    fn from_fen(fen: &str) -> Result<Self::T, String>;

    /// Creates a new instance from playing history.
    ///
    /// `fen` should be the Forsyth–Edwards Notation of a legal
    /// starting position. `moves` should be an iterator over all the
    /// moves that were played from that position. The move format is
    /// long algebraic notation. Examples: `e2e4`, `e7e5`, `e1g1`
    /// (white short castling), `e7e8q` (for promotion).
    fn from_history(fen: &str, moves: &mut Iterator<Item = &str>) -> Result<Self::T, String>;
}


/// Stores a list of moves for each position in a given line of play.
pub struct MoveStack {
    moves: Vec<Move>,
    savepoints: Vec<usize>,
    first_move_index: usize,
}

impl MoveStack {
    /// Creates a new (empty) instance.
    pub fn new() -> MoveStack {
        MoveStack {
            moves: Vec::with_capacity(32 * 64),
            savepoints: Vec::with_capacity(32),
            first_move_index: 0,
        }
    }

    /// Saves the current move list and replaces it with an empty one.
    ///
    /// This method can be called many times. At each call the current
    /// move list will be saved to the stack of lists that can later
    /// be restored. After calling `save` the new current move list
    /// will be empty.
    #[inline]
    pub fn save(&mut self) {
        self.savepoints.push(self.first_move_index);
        self.first_move_index = self.moves.len();
    }

    /// Restores the last saved move list.
    ///
    /// The current move list is lost.
    ///
    /// # Panics
    ///
    /// Panics if there are no saved move lists left.
    #[inline]
    pub fn restore(&mut self) {
        self.moves.truncate(self.first_move_index);
        self.first_move_index = self.savepoints.pop().unwrap();
    }

    /// Returns the number of saved move lists.
    ///
    /// The number of saved move lists starts at zero. It is
    /// incremented on each call to `save`, and decremented on each
    /// call to `restore`.
    #[inline]
    pub fn ply(&self) -> usize {
        self.savepoints.len()
    }

    /// Clears the current move list, removing all moves from it.
    #[inline]
    pub fn clear(&mut self) {
        self.moves.truncate(self.first_move_index);
    }

    /// Clears the current move list and deletes all saved move lists.
    #[inline]
    pub fn clear_all(&mut self) {
        self.moves.clear();
        self.savepoints.clear();
        self.first_move_index = 0;
    }

    /// Returns the number of moves in the current move list.
    #[inline]
    pub fn len(&self) -> usize {
        debug_assert!(self.moves.len() >= self.first_move_index);
        self.moves.len() - self.first_move_index
    }

    /// Appends a move to the end of the current move list.
    #[inline]
    pub fn push(&mut self, m: Move) {
        debug_assert!(self.moves.len() >= self.first_move_index);
        self.moves.push(m);
    }

    /// Removes the last move from the current move list and returns
    /// it, or `None` if empty.
    #[inline]
    pub fn pop(&mut self) -> Option<Move> {
        debug_assert!(self.moves.len() >= self.first_move_index);
        if self.moves.len() > self.first_move_index {
            self.moves.pop()
        } else {
            None
        }
    }

    /// Removes a specific move from the current move list and returns
    /// it.
    ///
    /// This method tries to find a move `m` for which `m.digest() ==
    /// move_digest`. Then it removes it from the current move list,
    /// and returns it. If such move is not found, `None` is returned.
    #[inline]
    pub fn remove(&mut self, move_digest: MoveDigest) -> Option<Move> {
        debug_assert!(self.moves.len() >= self.first_move_index);

        // The last move in `self.moves` will take the place of the
        // removed move.
        let last_move = if let Some(last) = self.moves.last() {
            *last
        } else {
            return None;
        };

        let m;
        'moves: loop {
            for curr in self.iter_mut() {
                if curr.digest() == move_digest {
                    m = *curr;
                    *curr = last_move;
                    break 'moves;
                }
            }
            return None;
        }
        debug_assert!(!self.moves.is_empty());
        self.moves.pop();
        Some(m)
    }

    /// Removes the move with the highest value from the current move
    /// list and returns it.
    ///
    /// Returns `None` if the current move list is empty.
    #[inline]
    pub fn remove_best(&mut self) -> Option<Move> {
        debug_assert!(self.moves.len() >= self.first_move_index);
        let moves = &mut self.moves;
        if moves.len() > self.first_move_index {
            let last = moves.len() - 1;
            unsafe {
                let mut best_move = *moves.get_unchecked(last);
                let mut curr = last;
                loop {
                    if *moves.get_unchecked(curr) > best_move {
                        // Swap the new best move candidate (`curr`)
                        // with the previous candidate (`last`).
                        *moves.get_unchecked_mut(last) = *moves.get_unchecked_mut(curr);
                        *moves.get_unchecked_mut(curr) = best_move;
                        best_move = *moves.get_unchecked(last);
                    }
                    if curr == self.first_move_index {
                        break;
                    }
                    curr -= 1;
                }
                moves.pop();
                return Some(best_move);
            }
        }
        None
    }

    /// Returns an iterator over each move in the current move list.
    #[inline]
    pub fn iter(&self) -> slice::Iter<Move> {
        self.moves[self.first_move_index..].iter()
    }

    /// Returns an iterator that allows modifying each move in the
    /// current move list.
    #[inline]
    pub fn iter_mut(&mut self) -> slice::IterMut<Move> {
        self.moves[self.first_move_index..].iter_mut()
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


#[cfg(test)]
mod tests {
    use super::*;
    use chesstypes::*;
    const NO_ENPASSANT_FILE: usize = 8;

    #[test]
    fn test_move_stack() {
        let m = Move::new(MOVE_NORMAL,
                          E2,
                          E4,
                          0,
                          NO_PIECE,
                          PAWN,
                          CastlingRights::new(0),
                          NO_ENPASSANT_FILE,
                          0);
        let mut s = MoveStack::new();
        assert!(s.remove_best().is_none());
        s.save();
        s.push(m);
        assert_eq!(s.remove_best().unwrap(), m);
        assert!(s.remove_best().is_none());
        s.restore();
        assert!(s.remove_best().is_none());
        s.push(m);
        s.push(m);
        s.save();
        s.push(m);
        s.restore();
        assert_eq!(s.remove_best().unwrap(), m);
        assert_eq!(s.remove_best().unwrap(), m);
        assert!(s.remove_best().is_none());
    }
}
