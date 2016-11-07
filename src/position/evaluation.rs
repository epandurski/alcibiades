//! Implements static board evaluation (not implemented yet).

use std::hash::{Hasher, SipHasher};
use std::ptr;
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
///
/// The static evaluation of the current board will consider only the
/// static material and positional properties of the position on the
/// board. If the position is dynamic, with pending tactical threats,
/// the static evaluation will return a grossly incorrect
/// evaluation. Therefore, it should be relied upon only for
/// reasonably "quiet" positions.
pub trait BoardEvaluator: Clone + SetOption + Send {
    /// Creates a new instance.
    fn new() -> Self;

    /// Binds the instance to a given board.
    fn set_board(&mut self, board: *const Board);

    /// Updates the internal state in accordance with a move that will
    /// be played on the board.
    ///
    /// This method updates the internally maintained attributes of
    /// the position, so that the next call to `evaluate` can give the
    /// correct evaluation of the board. `do_move` must be called
    /// before the move `m` is actually played on the board.
    #[allow(unused_variables)]
    fn do_move(&mut self, m: Move) {}

    /// Updates the internal state in accordance with a move that will
    /// be taken back.
    ///
    /// This method updates the internally maintained attributes of
    /// the position, so that the next call to `evaluate` can give the
    /// correct evaluation of the board. `undo_move` must be called
    /// before the move `m` is actually taken back.
    #[allow(unused_variables)]
    fn undo_move(&mut self, m: Move) {}

    /// Statically evaluates the board.
    /// 
    /// The returned value will be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`.
    fn evaluate(&self) -> Value;
}


#[derive(Clone)]
pub struct RandomEvaluator {
    board: *const Board,
}


impl SetOption for RandomEvaluator {}


unsafe impl Send for RandomEvaluator {}


impl BoardEvaluator for RandomEvaluator {
    fn new() -> RandomEvaluator {
        RandomEvaluator { board: ptr::null() }
    }

    fn set_board(&mut self, board: *const Board) {
        self.board = board;
    }

    fn evaluate(&self) -> Value {
        const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];
        let board = unsafe { self.board.as_ref().unwrap() };
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

/// Statically evaluates the board.
///
/// This method considers only static material and positional
/// properties of the position. If the position is dynamic, with
/// pending tactical threats, this function will return a grossly
/// incorrect evaluation. Therefore, it should be relied upon only
/// for reasonably "quiet" positions.
/// 
/// The returned value will always be between `VALUE_EVAL_MIN` and
/// `VALUE_EVAL_MAX`.
#[inline]
pub fn evaluate_board(board: &Board) -> Value {
    // TODO: Implement a real evaluation.

    const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];

    if board.checkers() != 0 {
        // In positions under check we can be vastly incorrect with no
        // negative consequences.
        return 0;
    }

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


// use rand;
// use rand::distributions::{Sample, Range};
// let mut rng = rand::thread_rng();
// let mut between = Range::new(0, 10);
// let x = between.sample(&mut rng);
