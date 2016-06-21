use basetypes::*;
use bitsets::*;
use position::board::Board;


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
    result
}
