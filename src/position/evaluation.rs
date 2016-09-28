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
/// The returned value will always be between -19999 and 19999.
#[inline]
pub fn evaluate_board(board: &Board) -> Value {
    // TODO: Implement a real evaluation.
    
    const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];
    
    if board.checkers() != 0 {
        // In positions under check we can be vastly incorrect with no
        // negative consequences.
        return -19999;
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
    result + (hasher.finish() >> 60) as i16
}


// use rand;
// use rand::distributions::{Sample, Range};
// let mut rng = rand::thread_rng();
// let mut between = Range::new(0, 10);
// let x = between.sample(&mut rng);


