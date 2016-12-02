//! Facilities for implementing static position evaluation.
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

pub mod tables;
pub mod bitsets;
pub mod evaluators;

use chesstypes::*;
use uci::SetOption;


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
    pub en_passant_file: usize,

    /// The set of all occupied squares on the board.
    ///
    /// Always equals `self.pieces.color[WHITE] |
    /// self.pieces.color[BLACK]`. Deserves a field on its own because
    /// it is very frequently needed.
    pub occupied: Bitboard,
}

impl Board {
    /// Creates a new instance.
    pub fn from_raw_parts(pieces: &PiecesPlacement,
                          to_move: Color,
                          castling_rights: CastlingRights,
                          en_passant_square: Option<Square>)
                          -> Result<Board, String> {
        let en_passant_rank = match to_move {
            WHITE => RANK_6,
            BLACK => RANK_3,
            _ => return Err(format!("illegal position")),
        };
        let en_passant_file = match en_passant_square {
            None => 8,
            Some(x) if x <= 63 && rank(x) == en_passant_rank => file(x),
            _ => return Err(format!("illegal position")),
        };
        Ok(Board {
            pieces: *pieces,
            to_move: to_move,
            castling_rights: castling_rights,
            en_passant_file: en_passant_file,
            occupied: pieces.color[WHITE] | pieces.color[BLACK],
        })
    }

    /// Creates a new instance from a Forsyth–Edwards Notation (FEN)
    /// string.
    pub fn from_fen(fen: &str) -> Result<Board, String> {
        let parts = try!(parse_fen(fen).map_err(|_| fen));
        let (ref pieces, to_move, castling_rights, en_passant_square, _, _) = parts;
        Board::from_raw_parts(pieces, to_move, castling_rights, en_passant_square)
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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_fen() {
        Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_ok();
    }
}
