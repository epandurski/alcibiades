//! Facilities for implementing static position evaluation and move
//! generation.
//!
//! # Static position evaluation
//!
//! An evaluation function is used to heuristically determine the
//! relative value of a position, i.e. the chances of winning. If we
//! could see to the end of the game in every line, the evaluation
//! would only have values of "loss", "draw", and "win". In practice,
//! however, we do not know the exact value of a position, so we must
//! make an approximation. Beginning chess players learn to do this
//! starting with the value of the pieces themselves. Computer
//! evaluation functions also use the value of the material as the
//! most significant aspect and then add other considerations.
//!
//! Static evaluation is an evaluation that considers only the static
//! material and positional properties of the current position,
//! without analyzing any tactical variations. Therefore, if the
//! position has pending tactical threats, the static evaluation will
//! be grossly incorrect.
//!
//! Writing a new static evaluator is as simple as defining a type
//! that implements the `BoardEvaluator` trait.
//!
//! # Move generation
//!
//! TODO

pub mod tables;
pub mod bitsets;
pub mod evaluators;
pub mod generators;
pub mod notation;

use chesstypes::*;
use uci::SetOption;
use self::notation::{parse_fen, NotationError};


/// Holds a chess position.
#[derive(Clone)]
pub struct Board {
    /// The placement of the pieces on the board.
    pub pieces: PiecesPlacement,

    /// The side to move.
    pub to_move: Color,

    /// The castling rights for both players.
    pub castling_rights: CastlingRights,

    /// If the previous move was a double pawn push, contains pushed
    /// pawn's file (a value between 0 and 7). Otherwise contains `8`.
    pub enpassant_file: usize,

    /// The set of all occupied squares on the board.
    ///
    /// Always equals `self.pieces.color[WHITE] |
    /// self.pieces.color[BLACK]`. Deserves a field on its own because
    /// it is very frequently needed.
    pub occupied: Bitboard,
}

impl Board {
    /// Creates a new instance from a Forsyth–Edwards Notation (FEN)
    /// string.
    pub fn from_fen(fen: &str) -> Result<Board, NotationError> {
        parse_fen(fen).map(|x| x.0)
    }
}


/// A trait used to statically evaluate positions.
pub trait BoardEvaluator: Clone + Send + SetOption {
    /// Creates a new instance and binds it to a given position.
    ///
    /// When a new instance is created, it is bound to a particular
    /// chess position (given by the `board` parameter). And for a
    /// moment, this is the only position that can be correctly
    /// evaluated. The instance then can be re-bound to the next (or
    /// the previous) position in the line of play by issuing calls to
    /// `will_do_move` and `done_move` methods (or respectively,
    /// `will_undo_move` and `undone_move` methods) .
    fn new(board: &Board) -> Self;

    /// Evaluates the the position to which the instance is bound.
    ///
    /// `board` points to the position to which the instance is bound.
    /// `halfmove_clock` gives the number of half-moves since the last
    /// piece capture or pawn advance.
    ///
    /// The returned value must be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`.
    fn evaluate(&self, board: &Board, halfmove_clock: u8) -> Value;

    /// Returns whether the position is zugzwangy.
    ///
    /// In many endgame positions there is a relatively high
    /// probability of zugzwang occurring. For such positions, this
    /// method returns `true`.
    fn is_zugzwangy(&self, board: &Board, halfmove_clock: u8) -> bool;

    /// Updates evaluator's state to keep up with a move that will be
    /// played.
    ///
    /// `board` points to the position to which the instance is bound.
    ///
    /// `m` is a legal move, or (if not in check) a "null move".
    #[inline]
    #[allow(unused_variables)]
    fn will_do_move(&mut self, board: &Board, m: Move) {}

    /// Updates evaluator's state to keep up with a move that was
    /// played.
    ///
    /// `board` points to the new position to which the instance is
    /// bound.
    #[inline]
    #[allow(unused_variables)]
    fn done_move(&mut self, board: &Board, m: Move) {}

    /// Updates evaluator's state to keep up with a move that will be
    /// taken back.
    ///
    /// `board` points to the position to which the instance is bound.
    #[inline]
    #[allow(unused_variables)]
    fn will_undo_move(&mut self, board: &Board, m: Move) {}

    /// Updates evaluator's state in accordance with a move that was
    /// taken back.
    ///
    /// `board` points to the new position to which the instance is
    /// bound.
    #[inline]
    #[allow(unused_variables)]
    fn undone_move(&mut self, board: &Board, m: Move) {}
}


/// A trait for move generators.
///
/// A `MoveGenerator` can generate all possible moves in the current
/// position, play a selected move, and take it back. It can also play
/// a "null move" which can be used to selectively prune the search
/// tree. `MoveGenerator` is unaware of repeating positions and
/// rule-50.
pub trait MoveGenerator: Sized + Send + Clone + SetOption {
    /// The type of static evaluator that the implementation works
    /// with.
    type BoardEvaluator: BoardEvaluator;

    /// Creates a new instance from a `Board` instance.
    ///
    /// Returns `None` if the position is illegal.
    fn from_board(board: Board) -> Option<Self>;

    /// Returns a reference to the underlying `Board` instance.
    #[inline(always)]
    fn board(&self) -> &Board;

    /// Returns the Zobrist hash value for the underlying `Board`
    /// instance.
    ///
    /// Zobrist hashing is a technique to transform a board position
    /// into a number of a fixed length, with an equal distribution
    /// over all possible numbers, invented by Albert Zobrist. The key
    /// property of this method is that two similar positions generate
    /// entirely different hash numbers.
    ///
    /// **Important note:** This method calculates the hash value
    /// "from scratch", which sometimes can be too slow. (See
    /// `do_move` method.)
    fn board_hash(&self) -> u64;

    /// Returns a reference to a static evaluator bound to the current
    /// position.
    fn evaluator(&self) -> &Self::BoardEvaluator;

    /// Returns a bitboard of all pieces and pawns of color `us` that
    /// attack `square`.
    fn attacks_to(&self, us: Color, square: Square) -> Bitboard;

    /// Returns a bitboard of all enemy pieces and pawns that attack
    /// the king.
    fn checkers(&self) -> Bitboard;

    /// Generates pseudo-legal moves.
    ///
    /// A pseudo-legal move is a move that is otherwise legal, except
    /// it might leave the king in check. Every legal move is a
    /// pseudo-legal move, but not every pseudo-legal move is legal.
    /// The generated moves will be pushed to `move_stack`. When `all`
    /// is `true`, all pseudo-legal moves will be generated. When
    /// `all` is `false`, only **check evasions** and **quiescence
    /// search moves** will be generated.
    fn generate_moves<S: PushMove>(&self, all: bool, move_stack: &mut S);

    /// Checks if `move_digest` represents a pseudo-legal move.
    ///
    /// If a move `m` exists that would be generated by
    /// `generate_moves` if called for the current position on the
    /// board, and for that move `m.digest() == move_digest`, this
    /// method will return `Some(m)`. Otherwise it will return
    /// `None`. This is useful when playing moves from the
    /// transposition table, without calling `generate_moves`.
    fn try_move_digest(&self, move_digest: MoveDigest) -> Option<Move>;

    /// Returns a null move.
    ///
    /// "Null move" is a pseudo-move that changes nothing on the board
    /// except the side to move. It is sometimes useful to include a
    /// speculative null move in the search tree so as to achieve more
    /// aggressive pruning.
    fn null_move(&self) -> Move;

    /// Plays a move on the board.
    ///
    /// It verifies if the move is legal. If the move is legal, the
    /// board is updated and an `u64` value is returned, which should
    /// be XOR-ed with old board's hash value to obtain new board's
    /// hash value. If the move is illegal, `None` is returned without
    /// updating the board. The move passed to this method **must**
    /// have been generated by `generate_moves`, `try_move_digest`, or
    /// `null_move` methods for the current position on the board.
    ///
    /// Moves generated by the `null_move` method are exceptions. For
    /// them `do_move(m)` will return `None` if and only if the king
    /// is in check.
    fn do_move(&mut self, m: Move) -> Option<u64>;

    /// Takes back a previously played move.
    ///
    /// The move passed to this method **must** be the last move passed
    /// to `do_move`.
    fn undo_move(&mut self, m: Move);
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_fen() {
        Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_ok();
    }
}
