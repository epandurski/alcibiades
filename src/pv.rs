//! Implements the extraction of the principal variation from the
//! transposition table.

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
///
/// **Important note:** Evaluations under `-9999`, or over `9999` will
/// be chopped.
pub fn extract_pv<T: HashTable, N: SearchNode>(tt: &T, position: &N, depth: Depth) -> Variation {
    assert!(depth <= DEPTH_MAX, "invalid depth: {}", depth);
    let mut p = position.clone();
    let mut our_turn = true;
    let mut root_value = VALUE_UNKNOWN;
    let mut leaf_value = 9999;
    let mut leaf_bound = BOUND_UPPER;
    let mut pv_moves = Vec::new();

    'move_extraction: while let Some(entry) = tt.probe(p.hash()) {
        let pv_length = pv_moves.len() as i8;

        // Before considering the next value from the transposition
        // table, we make sure that it is reliable enough, and at
        // least as reliable as the one we already have.
        if entry.depth() >= depth - pv_length &&
           (entry.bound() == BOUND_EXACT ||
            root_value == VALUE_UNKNOWN && entry.bound() != BOUND_NONE) {

            // Get the next value and the bound type. (Note that in
            // half of the cases the value stored in `entry` is from
            // other side's perspective. Also, note that we chop
            // values under -9999 or over 9999.)
            if our_turn {
                leaf_value = entry.value();
                leaf_bound = entry.bound();
            } else {
                leaf_value = -entry.value();
                leaf_bound = match entry.bound() {
                    BOUND_UPPER => BOUND_LOWER,
                    BOUND_LOWER => BOUND_UPPER,
                    x => x,
                };
            }
            debug_assert!(leaf_value != VALUE_UNKNOWN);
            if leaf_value <= -9999 {
                leaf_value = -9999;
                if leaf_bound == BOUND_UPPER {
                    leaf_bound = BOUND_EXACT
                }
            } else if leaf_value >= 9999 {
                leaf_value = 9999;
                if leaf_bound == BOUND_LOWER {
                    leaf_bound = BOUND_EXACT
                }
            }
            if root_value == VALUE_UNKNOWN {
                root_value = leaf_value;
            }

            // Continue the move extraction cycle until `depth` is
            // reached or `leaf_value` has diverged from `root_value`.
            if pv_length < depth && leaf_value == root_value {
                if let Some(m) = p.try_move_digest(entry.move_digest()) {
                    if p.do_move(m) {
                        pv_moves.push(m);
                        if entry.bound() == BOUND_EXACT {
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
            leaf_value
        },
        bound: leaf_bound,
        moves: pv_moves,
    }
}
