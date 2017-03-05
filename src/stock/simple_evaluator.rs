//! Implements the `Evaluator` trait.

use std::hash::Hasher;
use std::collections::hash_map::DefaultHasher;
use uci::SetOption;
use board::*;
use value::*;
use evaluator::Evaluator;
use bitsets::*;


/// A simple evaluator that adds a random number to the available
/// material.
#[derive(Clone)]
pub struct SimpleEvaluator;

impl SetOption for SimpleEvaluator {}

impl Evaluator for SimpleEvaluator {
    #[allow(unused_variables)]
    fn new(position: &Board) -> SimpleEvaluator {
        SimpleEvaluator
    }

    fn evaluate(&self, position: &Board) -> Value {
        const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];
        let piece_type = position.pieces.piece_type;
        let color = position.pieces.color;
        let us = position.to_move;
        let them = 1 ^ us;
        let mut result = 0;
        for piece in QUEEN..PIECE_NONE {
            result += PIECE_VALUES[piece] *
                      (pop_count(piece_type[piece] & color[us]) as i16 -
                       pop_count(piece_type[piece] & color[them]) as i16);
        }
        let mut hasher = DefaultHasher::new();
        hasher.write_u64(position.occupied);
        result + (hasher.finish() >> 59) as i16
    }

    #[allow(unused_variables)]
    #[inline]
    fn is_zugzwangy(&self, position: &Board) -> bool {
        false
    }
}
