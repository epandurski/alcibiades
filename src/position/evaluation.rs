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


/// A trait used to statically evaluate the current board.
pub trait BoardEvaluator: Clone + Send + SetOption {
    /// Creates a new instance and binds it to a given position on the
    /// board.
    fn new(board: &Board<Self>) -> Self;

    /// Updates evaluator's internal state in accordance with a move
    /// that will be played.
    ///
    /// This method updates the internally maintained properties of
    /// the current position, so that the next call to `evaluate` can
    /// calculate the correct evaluation as quickly as possible.
    ///
    /// **Important note:** `m` must be a legal move. `board` must
    /// point to a board that represents exactly the same position as
    /// the one to which the evaluator's instance is
    /// bound. `will_do_move` must be called before the move `m` is
    /// actually played, and after that, the move must actually be
    /// played.
    #[inline]
    #[allow(unused_variables)]
    fn will_do_move(&mut self, board: &Board<Self>, m: Move) {}

    /// Updates evaluator's internal state in accordance with a move
    /// that will be taken back.
    ///
    /// This method updates the internally maintained properties of
    /// the current position, so that the next call to `evaluate` can
    /// calculate the correct evaluation as quickly as possible.
    ///
    /// **Important note:** `m` must the last played move. `board`
    /// must point to a board that represents exactly the same
    /// position as the one to which the evaluator's instance is
    /// bound. `will_undo_move` must be called before the move `m` is
    /// actually taken back, and after that, the move must actually be
    /// taken back.
    #[inline]
    #[allow(unused_variables)]
    fn will_undo_move(&mut self, board: &Board<Self>, m: Move) {}

    #[inline]
    #[allow(unused_variables)]
    fn done_move(&mut self, board: &Board<Self>, m: Move) {}

    #[inline]
    #[allow(unused_variables)]
    fn undone_move(&mut self, board: &Board<Self>, m: Move) {}

    /// Statically evaluates the board.
    /// 
    /// The returned value will be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`.
    /// 
    /// **Important note:** `board` must point to a board that
    /// represents exactly the same position as the one to which the
    /// evaluator's instance is bound.
    fn evaluate(&self, board: &Board<Self>) -> Value;
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
