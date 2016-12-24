//! Implements the extraction of the principal variation from the
//! transposition table.

use std::cmp::min;
use moves::Move;
use value::*;
use depth::*;
use hash_table::*;
use search_node::SearchNode;


/// A sequence of moves from some starting position, together with the
/// value assigned to the final position.
pub struct Variation {
    /// A sequence of moves from some starting position.
    pub moves: Vec<Move>,

    /// The value assigned to the final position.
    ///
    /// The value is from the point of view of player that has the
    /// move in the starting position.
    pub value: Value,

    /// The accuracy of the assigned value.
    pub bound: BoundType,
}


/// Extracts the principal variation for a given position from the
/// transposition table.
///
/// The principal variation (PV) is the sequence of moves that the
/// engine considers best and therefore expects to be played.
pub fn extract_pv<T: HashTable, N: SearchNode>(tt: &T, position: &N) -> Variation {
    let mut p = position.clone();
    let mut our_turn = true;
    let mut moves = Vec::with_capacity(32);
    let mut root_value = VALUE_UNKNOWN;
    let mut value = VALUE_MAX;
    let mut bound = BOUND_UPPER;
    let mut depth = DEPTH_MAX + 1;

    'move_extraction: while let Some(e) = tt.probe(p.hash()) {
        depth = min(depth - 1, e.depth());

        if e.bound() == BOUND_EXACT || root_value == VALUE_UNKNOWN && e.bound() != BOUND_NONE {
            // In half of the cases the value is from other side's perspective.
            if our_turn {
                value = e.value();
                bound = e.bound();
            } else {
                value = -e.value();
                bound = match e.bound() {
                    BOUND_UPPER => BOUND_LOWER,
                    BOUND_LOWER => BOUND_UPPER,
                    b => b,
                };
            }
            assert!(value != VALUE_UNKNOWN);

            // Set the root value on the first iteration.
            if root_value == VALUE_UNKNOWN {
                root_value = value;
            }

            // Consider adding current entry's hash move to the
            // PV. There are 3 conditions for this:
            //
            // 1) The depth limit has not been reached yet.
            // 2) The move is either best move or a refutation move.
            // 3) The value has not diverged from the root value.
            if depth > 0 && e.bound() & BOUND_LOWER != 0 &&
               match root_value {
                v if v < VALUE_EVAL_MIN => v as isize == value as isize + moves.len() as isize,
                v if v > VALUE_EVAL_MAX => v as isize == value as isize - moves.len() as isize,
                v => v == value,
            } {
                // Verify that the hash move is legal.
                if let Some(m) = p.try_move_digest(e.move_digest()) {
                    if p.do_move(m) {
                        moves.push(m);
                        if e.bound() == BOUND_EXACT {
                            our_turn = !our_turn;
                            continue 'move_extraction;
                        }
                    }
                }
            }
        }
        break 'move_extraction;
    }

    Variation {
        value: if root_value != VALUE_UNKNOWN {
            root_value
        } else {
            value
        },
        bound: bound,
        moves: moves,
    }
}
