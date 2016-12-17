//! Implementations of `Evaluator` trait.

use std::hash::{Hasher, SipHasher};
use chesstypes::*;
use uci::SetOption;
use board::*;
use board::bitsets::*;
use search::Evaluator;


/// A simple evaluator that considers only the material available on
/// the board.
#[derive(Clone)]
pub struct MaterialEval;

impl SetOption for MaterialEval {}

impl Evaluator for MaterialEval {
    #[allow(unused_variables)]
    fn new(board: &Board) -> MaterialEval {
        MaterialEval
    }

    fn evaluate(&self, board: &Board) -> Value {
        use board::bitsets::*;
        const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];
        let piece_type = board.pieces.piece_type;
        let color = board.pieces.color;
        let us = board.to_move;
        let them = 1 ^ us;
        let mut result = 0;
        for piece in QUEEN..NO_PIECE {
            result += PIECE_VALUES[piece] *
                      (pop_count(piece_type[piece] & color[us]) as i16 -
                       pop_count(piece_type[piece] & color[them]) as i16);
        }
        result
    }

    #[allow(unused_variables)]
    #[inline]
    fn is_zugzwangy(&self, board: &Board) -> bool {
        false
    }
}


/// A simple evaluator that adds a random number to the available
/// material.
#[derive(Clone)]
pub struct RandomEval;

impl SetOption for RandomEval {}

impl Evaluator for RandomEval {
    #[allow(unused_variables)]
    fn new(board: &Board) -> RandomEval {
        RandomEval
    }

    fn evaluate(&self, board: &Board) -> Value {
        const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];
        let piece_type = board.pieces.piece_type;
        let color = board.pieces.color;
        let us = board.to_move;
        let them = 1 ^ us;
        let mut result = 0;
        for piece in QUEEN..NO_PIECE {
            result += PIECE_VALUES[piece] *
                      (pop_count(piece_type[piece] & color[us]) as i16 -
                       pop_count(piece_type[piece] & color[them]) as i16);
        }
        let mut hasher = SipHasher::new();
        hasher.write_u64(board.occupied);
        result + (hasher.finish() >> 59) as i16
    }

    #[allow(unused_variables)]
    #[inline]
    fn is_zugzwangy(&self, board: &Board) -> bool {
        false
    }
}
