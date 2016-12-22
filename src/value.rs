//! Defines the `Value` type and its related constants.


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

pub const VALUE_UNKNOWN: Value = VALUE_MIN - 1;
pub const VALUE_MAX: Value = ::std::i16::MAX;
pub const VALUE_MIN: Value = -VALUE_MAX;
pub const VALUE_EVAL_MAX: Value = 29999;
pub const VALUE_EVAL_MIN: Value = -VALUE_EVAL_MAX;
