//! Defines a constant for every file on the board.

use board::Square;

pub const FILE_A: usize = 0;
pub const FILE_B: usize = 1;
pub const FILE_C: usize = 2;
pub const FILE_D: usize = 3;
pub const FILE_E: usize = 4;
pub const FILE_F: usize = 5;
pub const FILE_G: usize = 6;
pub const FILE_H: usize = 7;

/// Returns the file of a given square.
///
/// The returned number will be between 0 and 7 (0 is file A, 7 is file H).
#[inline(always)]
pub fn file(square: Square) -> usize {
    debug_assert!(square <= 63);
    square % 8
}
