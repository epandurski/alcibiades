//! Implements static board evaluation (not implemented yet).

use std::hash::{Hasher, SipHasher};
use basetypes::*;
use moves::*;
use uci::{OptionName, OptionDescription};
use position::bitsets::*;
use position::move_generation::Board;


/// A trait for announcing configuration options, and changing
/// configuration parameters.
pub trait SetOption {
    /// Returns all supported configuration options.
    fn options() -> Vec<(OptionName, OptionDescription)> {
        vec![]
    }

    /// Sets a new value for a given configuration parameter.
    #[allow(unused_variables)]
    fn set_option(&mut self, name: &str, value: &str) {}
}


/// A trait used to statically evaluate positions.
pub trait BoardEvaluator: Clone + Send + SetOption {
    /// Creates a new instance and binds it to a given position.
    ///
    /// When a new instance is created, it is bound to a particular
    /// chess position (given by the `board` parameter). And for a
    /// moment, this the only position that can be correctly
    /// evaluated. The instance then can be re-bound to the next (or
    /// the previous) position in the line of play by issuing calls to
    /// `will_do_move` and `done_move` methods (or respectively,
    /// `will_undo_move` and `undone_move` methods) .
    fn new(board: &Board<Self>) -> Self;

    /// Evaluates the the position to which the instance is bound.
    /// 
    /// `board` points to the position to which the instance is bound.
    /// 
    /// The returned value must be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`.

    fn evaluate(&self, board: &Board<Self>) -> Value;
    /// Updates evaluator's state to keep up with a move that will be
    /// played.
    ///
    /// `board` points to the position to which the instance is bound.
    ///
    /// `m` is a legal move, or (if not in check) a "null move".
    #[inline]
    #[allow(unused_variables)]
    fn will_do_move(&mut self, board: &Board<Self>, m: Move) {}

    /// Updates evaluator's state to keep up with a move that was
    /// played.
    ///
    /// `board` points to the position to which the instance is bound.
    #[inline]
    #[allow(unused_variables)]
    fn done_move(&mut self, board: &Board<Self>, m: Move) {}

    /// Updates evaluator's state to keep up with a move that will be
    /// taken back.
    ///
    /// `board` points to the position to which the instance is bound.
    #[inline]
    #[allow(unused_variables)]
    fn will_undo_move(&mut self, board: &Board<Self>, m: Move) {}

    /// Updates evaluator's state in accordance with a move that was
    /// taken back.
    ///
    /// `board` points to the position to which the instance is bound.
    #[inline]
    #[allow(unused_variables)]
    fn undone_move(&mut self, board: &Board<Self>, m: Move) {}
}


#[derive(Clone)]
pub struct RandomEvaluator;

impl SetOption for RandomEvaluator {}

impl BoardEvaluator for RandomEvaluator {
    #[allow(unused_variables)]
    fn new(board: &Board<RandomEvaluator>) -> RandomEvaluator {
        RandomEvaluator
    }

    fn evaluate(&self, board: &Board<RandomEvaluator>) -> Value {
        const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];
        let piece_type = board.pieces().piece_type;
        let color = board.pieces().color;
        let us = board.to_move();
        let them = 1 ^ us;
        let mut result = 0;
        for piece in QUEEN..NO_PIECE {
            result += PIECE_VALUES[piece] *
                      (pop_count(piece_type[piece] & color[us]) as i16 -
                       pop_count(piece_type[piece] & color[them]) as i16);
        }
        let mut hasher = SipHasher::new();
        hasher.write_u64(board.occupied());
        result + (hasher.finish() >> 59) as i16
    }
}


// use rand;
// use rand::distributions::{Sample, Range};
// let mut rng = rand::thread_rng();
// let mut between = Range::new(0, 10);
// let x = between.sample(&mut rng);
