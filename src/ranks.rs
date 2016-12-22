//! Defines a constant for every rank on the board.

use board::Square;

pub const RANK_1: usize = 0;
pub const RANK_2: usize = 1;
pub const RANK_3: usize = 2;
pub const RANK_4: usize = 3;
pub const RANK_5: usize = 4;
pub const RANK_6: usize = 5;
pub const RANK_7: usize = 6;
pub const RANK_8: usize = 7;

/// Returns the rank of a given square.
///
/// The returned number will be between 0 and 7 (0 is rank 1, 7 is rank 8).
#[inline(always)]
pub fn rank(square: Square) -> usize {
    debug_assert!(square <= 63);
    square >> 3
}
