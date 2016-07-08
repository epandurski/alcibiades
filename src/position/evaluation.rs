//! Implements static board evaluation.

use std::hash::{Hasher, SipHasher};
use basetypes::*;
use bitsets::*;
use position::board::Board;


/// Statically evaluates the board.
///
/// This method considers only static material and positional
/// properties of the position. If the position is dynamic, with
/// pending tactical threats, this function will return a grossly
/// incorrect evaluation. Therefore, it should be relied upon only
/// for reasonably "quiet" positions.
/// 
/// `lower_bound` and `upper_bound` together give the interval
/// within which an as precise as possible evaluation is
/// required. If during the calculation it is determined that the
/// evaluation is outside this interval, this method may return
/// any value outside of the interval (including the bounds), but
/// always staying on the correct side of the interval.
#[inline]
pub fn evaluate_board(board: &Board, lower_bound: Value, upper_bound: Value) -> Value {
    assert!(lower_bound <= upper_bound);
    // TODO: Implement a real evaluation.
    
    const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];
    
    let piece_type = board.piece_type();
    let color = board.color();
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
