//! Implements the `Evaluator` trait.

use uci::SetOption;
use moves::*;
use board::*;
use value::*;
use evaluator::Evaluator;
use bitsets::*;


/// A simple evaluator that adds a random number to the available
/// material.
#[derive(Clone)]
pub struct SimpleEvaluator {
    material: Value,
}

impl SetOption for SimpleEvaluator {}

impl Evaluator for SimpleEvaluator {
    fn new(position: &Board) -> SimpleEvaluator {
        let us = position.to_move;
        let them = 1 ^ us;
        let color = position.pieces.color;
        let mut material = 0;
        for piece in QUEEN..PIECE_NONE {
            let occupied = position.pieces.piece_type[piece];
            let count_us = pop_count(occupied & color[us]) as i16;
            let count_them = pop_count(occupied & color[them]) as i16;
            material += PIECE_VALUES[piece] * (count_us - count_them);
        }
        SimpleEvaluator { material: material }
    }

    #[inline]
    fn done_move(&mut self, _: &Board, m: Move) {
        self.material = -(self.material + gained_material(m));
    }

    #[inline]
    fn undone_move(&mut self, _: &Board, m: Move) {
        self.material = -self.material - gained_material(m);
    }

    #[inline]
    fn evaluate(&self, position: &Board) -> Value {
        let k = (position.occupied >> 32 ^ position.occupied) as u32;
        let random_number = (k.wrapping_mul(2654435769) >> 27) as Value;
        self.material + random_number
    }

    #[allow(unused_variables)]
    #[inline]
    fn is_zugzwangy(&self, position: &Board) -> bool {
        false
    }
}


const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];


#[inline]
fn gained_material(m: Move) -> Value {
    if m.move_type() == MOVE_PROMOTION {
        // the value of the captured piece
        PIECE_VALUES[m.captured_piece()]

        // add the value of the newly promoted piece
        + PIECE_VALUES[Move::piece_from_aux_data(m.aux_data())]

        // subtract the value of the promoted pawn
        - PIECE_VALUES[PAWN]
    } else {
        unsafe { *PIECE_VALUES.get_unchecked(m.captured_piece()) }
    }
}
