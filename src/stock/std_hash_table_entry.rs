//! Implements `StdHashTableEntry`.

use value::*;
use depth::*;
use hash_table::*;
use moves::MoveDigest;


/// Implements the `HashTableEntry` trait.
#[derive(Copy, Clone, Debug)]
pub struct StdHashTableEntry {
    value: Value,
    bound: BoundType,
    depth: Depth,
    move_digest: MoveDigest,
    static_eval: Value,
}

impl HashTableEntry for StdHashTableEntry {
    #[inline]
    fn new(value: Value,
           bound: BoundType,
           depth: Depth,
           move_digest: MoveDigest)
           -> StdHashTableEntry {
        Self::with_static_eval(value, bound, depth, move_digest, VALUE_UNKNOWN)
    }

    #[inline]
    fn with_static_eval(value: Value,
                        bound: BoundType,
                        depth: Depth,
                        move_digest: MoveDigest,
                        static_eval: Value)
                        -> StdHashTableEntry {
        debug_assert!(value != VALUE_UNKNOWN);
        debug_assert!(bound <= 0b11);
        debug_assert!(DEPTH_MIN <= depth && depth <= DEPTH_MAX);
        StdHashTableEntry {
            value: value,
            bound: bound,
            depth: depth,
            move_digest: move_digest,
            static_eval: static_eval,
        }
    }

    #[inline]
    fn value(&self) -> Value {
        self.value
    }

    #[inline]
    fn bound(&self) -> BoundType {
        self.bound
    }

    #[inline]
    fn depth(&self) -> Depth {
        self.depth
    }

    #[inline]
    fn move_digest(&self) -> MoveDigest {
        self.move_digest
    }

    #[inline]
    fn set_move_digest(&mut self, move_digest: MoveDigest) {
        self.move_digest = move_digest;
    }

    /// Returns the `static_eval` passed to the constructor.
    #[inline]
    fn static_eval(&self) -> Value {
        self.static_eval
    }

    #[inline]
    fn importance(&self) -> i16 {
        // Positions with higher search depths are scored higher.
        self.depth as i16

        // Positions with exact evaluations are given slight advantage.
        + (if self.bound == BOUND_EXACT {
            1
        } else {
            0
        })
    }
}
