//! Implementations of `BoardEvaluator` trait.

use std::hash::{Hasher, SipHasher};
use chesstypes::*;
use uci::SetOption;
use board::*;
use board::bitsets::*;


/// A simple evaluator that considers only the material available on
/// the board.
#[derive(Clone)]
pub struct MaterialEval;

impl SetOption for MaterialEval {}

impl BoardEvaluator for MaterialEval {
    #[allow(unused_variables)]
    fn new(board: &Board) -> MaterialEval {
        MaterialEval
    }

    #[allow(unused_variables)]
    fn evaluate(&self, board: &Board, halfmove_clock: u8) -> Value {
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

    #[inline]
    #[allow(unused_variables)]
    fn is_zugzwangy(&self, board: &Board, halfmove_clock: u8) -> bool {
        false
    }
}


/// A simple evaluator that adds a random number to the available
/// material.
#[derive(Clone)]
pub struct RandomEval;

impl SetOption for RandomEval {}

impl BoardEvaluator for RandomEval {
    #[allow(unused_variables)]
    fn new(board: &Board) -> RandomEval {
        RandomEval
    }

    #[allow(unused_variables)]
    fn evaluate(&self, board: &Board, halfmove_clock: u8) -> Value {
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

    #[inline]
    #[allow(unused_variables)]
    fn is_zugzwangy(&self, board: &Board, halfmove_clock: u8) -> bool {
        false
    }
}
