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
    depth: i8,
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
        debug_assert!(VALUE_MIN <= value && value <= VALUE_MAX);
        debug_assert!(bound <= 0b11);
        debug_assert!(DEPTH_MIN <= depth && depth <= DEPTH_MAX);
        StdHashTableEntry {
            value: value,
            bound: bound,
            depth: depth as i8,
            move_digest: move_digest,
            static_eval: VALUE_UNKNOWN,
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
        self.depth as Depth
    }

    #[inline]
    fn move_digest(&self) -> MoveDigest {
        self.move_digest
    }

    #[inline]
    fn static_eval(&self) -> Value {
        self.static_eval
    }

    #[inline]
    fn set_value(&mut self, value: Value) {
        debug_assert!(VALUE_MIN <= value && value <= VALUE_MAX);
        self.value = value;
    }

    #[inline]
    fn set_bound(&mut self, bound: BoundType) {
        debug_assert!(bound <= 0b11);
        self.bound = bound;
    }

    #[inline]
    fn set_depth(&mut self, depth: Depth) {
        debug_assert!(DEPTH_MIN <= depth && depth <= DEPTH_MAX);
        self.depth = depth as i8;
    }

    #[inline]
    fn set_move_digest(&mut self, move_digest: MoveDigest) {
        self.move_digest = move_digest;
    }

    /// Sets position's static evaluation.
    #[inline]
    fn set_static_eval(&mut self, static_eval: Value) {
        self.static_eval = static_eval;
    }
}
