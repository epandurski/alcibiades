//! Defines types and constants related to position evaluation.


/// Evaluation value in centipawns.
///
/// Positive values mean that the position is favorable for the side
/// to move. Negative values mean the position is favorable for the
/// other side (not to move). A value of `0` means that the chances
/// are equal. For example: a value of `100` might mean that the side
/// to move is a pawn ahead.
///
/// # Constants:
///
/// * `VALUE_UNKNOWN` has the special meaning of "unknown value".
///
/// * `VALUE_MAX` designates a checkmate (a win).
///
/// * `VALUE_MIN` designates a checkmate (a loss).
///
/// * Values bigger than `VALUE_EVAL_MAX` designate a win by
///   inevitable checkmate.
///
/// * Values smaller than `VALUE_EVAL_MIN` designate a loss by
///   inevitable checkmate.
pub type Value = i16;

/// Equals `-32768` and has the special meaning of "unknown value".
pub const VALUE_UNKNOWN: Value = VALUE_MIN - 1;

/// Equals `32767` and designates a checkmate (a win).
pub const VALUE_MAX: Value = ::std::i16::MAX;

/// Equals `-32767` and designates a checkmate (a loss).
pub const VALUE_MIN: Value = -VALUE_MAX;

/// Equals `29999`, values bigger than that designate a win by
/// inevitable checkmate.
pub const VALUE_EVAL_MAX: Value = 29999;

/// Equals `-29999`, values smaller than that designate a loss by
/// inevitable checkmate.
pub const VALUE_EVAL_MIN: Value = -VALUE_EVAL_MAX;


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

/// Means that the real value can be anything.
pub const BOUND_NONE: BoundType = 0;

/// Means that the real value is greater or equal to the evaluation.
pub const BOUND_LOWER: BoundType = 0b01;

/// Means that the real value is lesser or equal to the evaluation.
pub const BOUND_UPPER: BoundType = 0b10;

/// Means that the evaluation is exact.
pub const BOUND_EXACT: BoundType = BOUND_UPPER | BOUND_LOWER;
