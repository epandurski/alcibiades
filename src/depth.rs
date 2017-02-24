//! Defines the `Depth` type and its related constants.


/// Search depth in half-moves.
///
/// A value of this type can be interpreted in two ways:
///
/// *  **Remaining search depth.**
///
///    The remaining search depth tells how many half-moves should be
///    added to the current line of play before a leaf node is
///    reached. Usually, searches are started with some positive
///    number as their remaining search depth. This number is
///    decremented when a move is tried, and the search routine is
///    called recursively. When the remaining depth becomes zero (or
///    less), a leaf node has been reached and a quiescence search is
///    performed to obtain reliable evaluation. Sometimes depth
///    reductions are applied for less interesting moves. This means
///    that the remaining search depth is decreased by more than
///    one. Thus, if such reductions are applied near leaf nodes, the
///    remaining search depth may become negative.
///
/// *  **Completed search depth.**
///
///    The completed search depth tells the depth in half-moves to
///    which a position has been analyzed. Negative values are
///    possible, meaning that a quiescence search with a negative
///    remaining search depth has been executed for the position.
///
/// # Limits:
///
/// * `DEPTH_MAX` is the maximum allowed search depth in half-moves (a
///   positive number).
///
/// * `DEPTH_MIN` is the minimum allowed search depth in half-moves (a
///   negative number).
pub type Depth = i16;

pub const DEPTH_MIN: Depth = -32;
pub const DEPTH_MAX: Depth = 95;
pub const DEPTH_ONE_PLY: Depth = 1;
