//! Defines types and traits related to transposition tables.

use std::cmp::min;
use moves::{Move, MoveDigest};
use value::*;
use depth::*;
use search_node::SearchNode;


/// `BOUND_EXACT`, `BOUND_LOWER`, `BOUND_UPPER`, or `BOUND_NONE`.
///
/// For the majority of chess positions our evaluations will be more
/// or less inaccurate, and there is nothing we can do about it. But
/// sometimes we know that a given evaluation is probably inaccurate,
/// and we know the sign of the error. `BoundType` defines the
/// direction of such **known inaccuracies**.
///
/// # Constants:
///
/// * `BOUND_EXACT` means that the evaluation is exact (as far as we know).
///
/// * `BOUND_LOWER` means that the real value is greater or equal to
///    the evaluation (as far as we know).
///
/// * `BOUND_UPPER` means that the real value is lesser or equal to
///   the evaluation (as far as we know).
///
/// * `BOUND_NONE` means that the real value can be anything.
pub type BoundType = u8;

pub const BOUND_NONE: BoundType = 0;
pub const BOUND_LOWER: BoundType = 0b01;
pub const BOUND_UPPER: BoundType = 0b10;
pub const BOUND_EXACT: BoundType = BOUND_UPPER | BOUND_LOWER;


/// A trait for transposition tables.
///
/// Chess programs, during their brute-force search, encounter the
/// same positions again and again, but from different sequences of
/// moves, which is called a "transposition". When the search
/// encounters a transposition, it is beneficial to "remember" what
/// was determined last time the position was examined, rather than
/// redoing the entire search again. For this reason, chess programs
/// have a transposition table, which is a large hash table storing
/// information about positions previously searched, how deeply they
/// were searched, and what we concluded about them. To implement your
/// own transposition table, you must define a type that implements
/// the `HashTable` trait.
pub trait HashTable: Sync + Send {
    type Entry: HashTableEntry;

    /// Creates a new transposition table.
    ///
    /// `size_mb` is the desired size in Mbytes.
    fn new(size_mb: Option<usize>) -> Self;

    /// Signals that a new search is about to begin.
    fn new_search(&self);

    /// Stores data by key.
    ///
    /// After being stored, the data can be retrieved by `probe`. This
    /// is not guaranteed though, because the entry might have been
    /// overwritten in the meantime.
    fn store(&self, key: u64, data: Self::Entry);

    /// Probes for data by key.
    fn probe(&self, key: u64) -> Option<Self::Entry>;

    /// Removes all entries in the table.
    fn clear(&self);

    /// Extracts the principal variation for a given position.
    ///
    /// The principal variation (PV) is the sequence of moves that the
    /// engine considers best and therefore expects to be played.
    fn extract_pv<T: SearchNode>(&self, position: &T) -> Variation {
        let mut p = position.clone();
        let mut our_turn = true;
        let mut moves = Vec::with_capacity(32);
        let mut root_value = VALUE_UNKNOWN;
        let mut value = VALUE_MAX;
        let mut bound = BOUND_UPPER;
        let mut depth = DEPTH_MAX + 1;

        'move_extraction: while let Some(e) = self.probe(p.hash()) {
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
                // PV. There are 2 conditions for this:
                //
                // 1) The depth limit has not been reached yet.
                // 2) The value has not diverged from the root value.
                if depth > 0 &&
                   match root_value {
                    v if v < VALUE_EVAL_MIN => v as isize == value as isize + moves.len() as isize,
                    v if v > VALUE_EVAL_MAX => v as isize == value as isize - moves.len() as isize,
                    v => v == value,
                } {
                    // Verify that the hash move is legal.
                    if let Some(m) = p.try_move_digest(e.move_digest()) {
                        if p.do_move(m) {
                            moves.push(m);

                            // Note: we continue expanding the PV only on best moves.
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
}


/// A trait for transposition table entries.
pub trait HashTableEntry: Copy + Send {
    /// Creates a new instance.
    ///
    /// * `value` -- The value assigned to the position. Must not be
    ///   `VALUE_UNKNOWN`.
    ///
    /// * `bound` -- The accuracy of the assigned value.
    ///
    /// * `depth` -- The depth of search. Must be between `DEPTH_MIN`
    ///   and `DEPTH_MAX`.
    ///
    /// * `move_digest` -- Best or refutation move digest, or
    ///   `MoveDigest::invalid()` if no move is available.
    fn new(value: Value, bound: BoundType, depth: Depth, move_digest: MoveDigest) -> Self;

    /// Creates a new instance.
    ///
    /// The only difference between this function and `new` is that
    /// this function requires one additional parameter:
    ///
    /// * `static_eval` -- Position's static evaluation, or
    ///   `VALUE_UNKNOWN`.
    ///
    /// **Important note:** `static_eval` will be ignored if there is
    /// no field allotted for it in the underlying memory structure.
    fn with_static_eval(value: Value,
                        bound: BoundType,
                        depth: Depth,
                        move_digest: MoveDigest,
                        static_eval: Value)
                        -> Self;

    /// Returns the value assigned to the position.
    fn value(&self) -> Value;

    /// Returns the accuracy of the assigned value.
    fn bound(&self) -> BoundType;

    /// Returns the search depth for the assigned value.
    fn depth(&self) -> Depth;

    /// Returns best or refutation move digest, or
    /// `MoveDigest::invalid()` if no move is available.
    fn move_digest(&self) -> MoveDigest;

    /// Returns position's static evaluation, or `VALUE_UNKNOWN`.
    fn static_eval(&self) -> Value;

    /// Sets a new best or refutation move digest.
    ///
    /// Transposition tables may use this method when they overwrite
    /// an old record for the same position, and want to keep the move
    /// from the old record.
    fn set_move_digest(&mut self, move_digest: MoveDigest);

    /// Returns the relative importance of the entry.
    ///
    /// Transposition tables may use this method to improve their
    /// record replacement strategy. Normally, when possible, entries
    /// with lower `importance` will be replaced before entries with
    /// higher `importance`. Therefore this method should try to
    /// return higher values for entries that are move likely to save
    /// CPU work in the future. For example, positions analyzed to a
    /// higher depth are probably more "important" than those analyzed
    /// to a lower depth.
    #[inline]
    fn importance(&self) -> i16 {
        match self.bound() {
            BOUND_EXACT => self.depth() as i16,
            BOUND_NONE => 0,
            _ => self.depth() as i16 - 1,
        }
    }
}


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
