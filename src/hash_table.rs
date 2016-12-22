//! Defines types and traits related to transposition tables.

use moves::*;
use value::*;
use depth::*;


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
    fn store(&self, key: u64, mut data: Self::Entry);

    /// Probes for data by key.
    fn probe(&self, key: u64) -> Option<Self::Entry>;

    /// Removes all entries in the table.
    fn clear(&self);
}


/// A trait for transposition table entries.
pub trait HashTableEntry: Copy {
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
    /// * `move_digest` -- Best or refutation move digest, or `0` if
    ///   no move is available.
    fn new(value: Value, bound: BoundType, depth: Depth, move_digest: MoveDigest) -> Self;

    /// Creates a new instance.
    ///
    /// The only difference between this function and `new` is that
    /// this function requires one additional parameter:
    ///
    /// * `eval_value` -- Position's static evaluation, or
    ///   `VALUE_UNKNOWN`.
    ///
    /// **Important note:** `eval_value` will be ignored if there is
    /// no field allotted for it in the underlying memory structure.
    fn with_eval_value(value: Value,
                       bound: BoundType,
                       depth: Depth,
                       move_digest: MoveDigest,
                       eval_value: Value)
                       -> Self;

    /// Returns the value assigned to the position.
    fn value(&self) -> Value;

    /// Returns the accuracy of the assigned value.
    fn bound(&self) -> BoundType;

    /// Returns the search depth for the assigned value.
    fn depth(&self) -> Depth;

    /// Returns best or refutation move digest, or `0` if no move is
    /// available.
    fn move_digest(&self) -> MoveDigest;

    /// Returns position's static evaluation, or `VALUE_UNKNOWN`.
    fn eval_value(&self) -> Value;
}
