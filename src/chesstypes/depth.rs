//! Defines the search depth type and its constants.


/// Remaining search depth in half-moves.
///
/// The remaining search depth tells how many half-moves should be
/// added to the current line of play before a leaf node is
/// reached. Usually, searches are started with some positive number
/// as their remaining search depth. This number is decreased when a
/// move is tried, and the search routine is called recursively. When
/// the remaining depth becomes zero (or less), a leaf node is reached
/// and an evaluation is assigned to it. Sometimes depth reductions
/// are applied to less interesting moves. This means that the
/// remaining search depth is decreased by more than one. The
/// remaining search depth may become negative if such reductions are
/// applied near the leafs.
///
/// # Constants:
///
/// * `DEPTH_MAX` -- the maximum search depth in half-moves
///   (a positive number).
///
/// * `DEPTH_MIN` -- the minimum search depth in half-moves
///   (a negative number).
pub type Depth = i8;

/// Equals `-32` -- the minimum search depth in half-moves.
pub const DEPTH_MIN: u8 = 0;

/// Equals `95` -- the maximum search depth in half-moves.
pub const DEPTH_MAX: u8 = 63;
