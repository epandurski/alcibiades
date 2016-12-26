//! Functions and constants for working with bit-sets.
//!
//! **Note:** "LSB" means "least significant `1` bit".

use board::{Square, Bitboard};


/// Empty set of squares.
pub const BB_EMPTY_SET: Bitboard = 0;

/// The set of all 64 squares on the board.
pub const BB_UNIVERSAL_SET: Bitboard = 0xffffffffffffffff;

// Ranks 1-8.
pub const BB_RANK_1: Bitboard = 0b11111111;
pub const BB_RANK_2: Bitboard = BB_RANK_1 << 8;
pub const BB_RANK_3: Bitboard = BB_RANK_2 << 8;
pub const BB_RANK_4: Bitboard = BB_RANK_3 << 8;
pub const BB_RANK_5: Bitboard = BB_RANK_4 << 8;
pub const BB_RANK_6: Bitboard = BB_RANK_5 << 8;
pub const BB_RANK_7: Bitboard = BB_RANK_6 << 8;
pub const BB_RANK_8: Bitboard = BB_RANK_7 << 8;

// Files A-H.
pub const BB_FILE_A: Bitboard = 0x0101010101010101;
pub const BB_FILE_B: Bitboard = BB_FILE_A << 1;
pub const BB_FILE_C: Bitboard = BB_FILE_B << 1;
pub const BB_FILE_D: Bitboard = BB_FILE_C << 1;
pub const BB_FILE_E: Bitboard = BB_FILE_D << 1;
pub const BB_FILE_F: Bitboard = BB_FILE_E << 1;
pub const BB_FILE_G: Bitboard = BB_FILE_F << 1;
pub const BB_FILE_H: Bitboard = BB_FILE_G << 1;

/// Rank 1 and rank 8.
pub const BB_PAWN_PROMOTION_RANKS: Bitboard = BB_RANK_1 | BB_RANK_8;

/// The main diagonal (A1-H8)
pub const BB_MAIN_DIAG: u64 = 0x8040201008040201;

/// The main anti-diagonal (A8-H1).
pub const BB_MAIN_ANTI_DIAG: u64 = 0x0102040810204080;


/// Returns the LSB of a value.
///
/// The way to calculate this is: `lsb = x & -x;`.
///
/// If `x` is `0` this function returns `0`.
///
/// # Examples
///
/// ```text
/// 
///        x         &        -x         =      lsb(x)
/// . . . . . . . .     1 1 1 1 1 1 1 1     . . . . . . . .
/// . . 1 . 1 . . .     1 1 . 1 . 1 1 1     . . . . . . . .
/// . 1 . . . 1 . .     1 . 1 1 1 . 1 1     . . . . . . . .
/// . . . . . . . .     1 1 1 1 1 1 1 1     . . . . . . . .
/// . 1 . . . 1 . .  &  1 . 1 1 1 . 1 1  =  . . . . . . . .
/// . . 1 . 1 . . .     . . 1 1 . 1 1 1     . . 1 . . . . .
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// ```
#[inline(always)]
pub fn lsb(x: Bitboard) -> Bitboard {
    x & x.wrapping_neg()
}


/// Resets the LSB of a value to zero.
///
/// The way to calculate this is: `x_with_reset_lsb = x & (x - 1);`.
///
/// If `x` is `0` this function does nothing.
///
/// # Examples:
///
/// ```text
/// 
///       x          &      (x - 1)      =  x_with_reset_lsb(x)
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// . . 1 . 1 . . .     . . 1 . 1 . . .     . . 1 . 1 . . .
/// . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// . 1 . . . 1 . .  &  . 1 . . . 1 . .  =  . 1 . . . 1 . .
/// . . 1 . 1 . . .     1 1 . . 1 . . .     . . . . 1 . . .
/// . . . . . . . .     1 1 1 1 1 1 1 1     . . . . . . . .
/// . . . . . . . .     1 1 1 1 1 1 1 1     . . . . . . . .
/// ```
#[inline(always)]
pub fn reset_lsb(x: &mut Bitboard) {
    *x &= x.wrapping_sub(1);
}


/// Returns a mask with all bits above the LSB set to `1`.
///
/// The way to calculate this is: `above_lsb = x ^ -x;`.
///
/// If `x` is `0` this function returns `0`.
/// 
/// # Examples:
///
/// ```text
/// 
///       x          ^        -x         =  above_lsb(x)
/// . . . . . . . .     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
/// . . 1 . 1 . . .     1 1 . 1 . 1 1 1     1 1 1 1 1 1 1 1
/// . 1 . . . 1 . .     1 . 1 1 1 . 1 1     1 1 1 1 1 1 1 1
/// . . . . . . . .     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
/// . 1 . . . 1 . .  ^  1 . 1 1 1 . 1 1  =  1 1 1 1 1 1 1 1
/// . . 1 . 1 . . .     . . 1 1 . 1 1 1     . . . 1 1 1 1 1
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// ```
#[inline(always)]
pub fn above_lsb(x: Bitboard) -> Bitboard {
    x ^ x.wrapping_neg()
}


/// Returns a mask with all bits below and including the LSB set to
/// `1`.
///
/// The way to calculate this is: `below_lsb_including = x ^ (x - 1);`.
///
/// If `x` is `0` this function returns `0xffffffffffffffff`.
/// 
/// # Examples:
///
/// ```text
///       x          ^      (x - 1)      =  below_lsb_including(x)
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// . . 1 . 1 . . .     . . 1 . 1 . . .     . . . . . . . .
/// . 1 . . . 1 . .     . 1 . . . 1 . .     . . . . . . . .
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// . 1 . . . 1 . .  ^  . 1 . . . 1 . .  =  . . . . . . . .
/// . . 1 . 1 . . .     1 1 . . 1 . . .     1 1 1 . . . . .
/// . . . . . . . .     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
/// . . . . . . . .     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
/// ```
#[inline(always)]
pub fn below_lsb_including(x: Bitboard) -> Bitboard {
    x ^ x.wrapping_sub(1)
}


/// Returns a mask with all bits above and including the LSB set to
/// `1`.
///
/// The way to calculate this is: `above_lsb_including = x | -x;`.
///
/// If `x` is `0` this function returns `0`.
/// 
/// # Examples:
/// ```text
///       x          |        -x         =  above_lsb_including(x)
/// . . . . . . . .     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
/// . . 1 . 1 . . .     1 1 . 1 . 1 1 1     1 1 1 1 1 1 1 1
/// . 1 . . . 1 . .     1 . 1 1 1 . 1 1     1 1 1 1 1 1 1 1
/// . . . . . . . .     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
/// . 1 . . . 1 . .  |  1 . 1 1 1 . 1 1  =  1 1 1 1 1 1 1 1
/// . . 1 . 1 . . .     . . 1 1 . 1 1 1     . . 1 1 1 1 1 1
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// . . . . . . . .     . . . . . . . .     . . . . . . . .
/// ```
#[inline(always)]
pub fn above_lsb_including(x: Bitboard) -> Bitboard {
    x | x.wrapping_neg()
}


/// Returns a mask with all bits below the LSB set to `1`.
///
/// The way to calculate this is: `below_lsb = !x & (x - 1);`.
///
/// If `x` is `0` this function returns `0xffffffffffffffff`.
/// 
/// # Examples:
///
/// ```text
///      !x          &      (x - 1)      =  below_lsb(x)
/// 1 1 1 1 1 1 1 1     . . . . . . . .     . . . . . . . .
/// 1 1 . 1 . 1 1 1     . . 1 . 1 . . .     . . . . . . . .
/// 1 . 1 1 1 . 1 1     . 1 . . . 1 . .     . . . . . . . .
/// 1 1 1 1 1 1 1 1     . . . . . . . .     . . . . . . . .
/// 1 . 1 1 1 . 1 1  &  . 1 . . . 1 . .  =  . . . . . . . .
/// 1 1 . 1 . 1 1 1     1 1 . . 1 . . .     1 1 . . . . . .
/// 1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
/// 1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
/// ```
#[inline(always)]
pub fn below_lsb(x: Bitboard) -> Bitboard {
    !x & x.wrapping_sub(1)
}


/// Shifts a value with a signed number.
///
/// Returns `x << s` if `s` is positive, and `x >> s` if `s` is
/// negative.
#[inline(always)]
pub fn gen_shift(x: Bitboard, s: isize) -> Bitboard {
    if s > 0 {
        x << s
    } else {
        x >> -s
    }
}


/// Returns the binary position of the LSB (bit-scan-forward).
///
/// `b` must not be zero, otherwise this function will panic or return
/// garbage.
///
/// # Examples:
/// ```
/// # use alcibiades::utils::bitsets::*;
/// assert_eq!(bsf(0b100100), 2);
/// ```
#[inline(always)]
pub fn bsf(b: Bitboard) -> Square {
    debug_assert!(b != 0);
    b.trailing_zeros() as Square
}


/// Returns the binary position of the LSB (bit-scan-forward), and
/// resets the LSB to zero.
///
/// `b` must not be zero, otherwise this function will panic or return
/// garbage.
/// 
/// # Examples:
/// ```
/// # use alcibiades::utils::bitsets::*;
/// let mut x = 0b100100;
/// assert_eq!(bsf_reset(&mut x), 2);
/// assert_eq!(x, 0b100000);
/// ```
#[inline(always)]
pub fn bsf_reset(b: &mut Bitboard) -> Square {
    debug_assert!(*b != 0);
    let x = lsb(*b);
    *b ^= x;
    bsf(x)
}


/// Returns the number of `1`s in the binary representation of a
/// value.
///
/// # Examples:
/// ```
/// # use alcibiades::utils::bitsets::*;
/// assert_eq!(pop_count(0b100101), 3);
/// ```
#[inline(always)]
pub fn pop_count(b: Bitboard) -> usize {
    b.count_ones() as usize
}


/// Returns the set of squares on the same rank as the given square.
#[inline(always)]
pub fn bb_rank(square: Square) -> Bitboard {
    BB_RANK_1 << ((square >> 3) << 3)
}


/// Returns the set of squares on the same file as the given square.
#[inline(always)]
pub fn bb_file(square: Square) -> Bitboard {
    BB_FILE_A << (square % 8)
}


/// Returns the set of squares on the same diagonal as the given square.
///
/// Diagonals go from white's queen-side to black's king-side (A1-H8
/// for example).
#[inline]
pub fn bb_diag(square: Square) -> Bitboard {
    let diag_index = ((square >> 3).wrapping_sub(square % 8)) & 15;
    if diag_index <= 7 {
        BB_MAIN_DIAG << (diag_index << 3)
    } else {
        BB_MAIN_DIAG >> ((16 - diag_index) << 3)
    }
}


/// Returns the set of squares on the same anti-diagonal as the given square.
///
/// Anti-diagonals go from white's king-side to black's queen-side
/// (H1-A8 for example).
#[inline]
pub fn bb_anti_diag(square: Square) -> Bitboard {
    let diag_index = ((square >> 3) + (square % 8)) ^ 7;
    if diag_index <= 7 {
        BB_MAIN_ANTI_DIAG >> (diag_index << 3)
    } else {
        BB_MAIN_ANTI_DIAG << ((16 - diag_index) << 3)
    }
}


/// Returns the set of squares that are attacked by a rook from a
/// given square.
///
/// This function calculates and returns the set of squares that are
/// attacked by a rook from the square `from_square`, on a board which
/// is occupied with pieces according to the `occupied` bitboard. It
/// does not matter if `from_square` is occupied or not.
///
/// **Important note:** This function calcuates the set of attacked
/// squares "from scratch". This can be too slow for some use cases.
pub fn bb_rook_attacks(from_square: Square, occupied: Bitboard) -> Bitboard {
    bb_line_attacks(bb_file(from_square), from_square, occupied) |
    bb_line_attacks(bb_rank(from_square), from_square, occupied)
}


/// Returns the set of squares that are attacked by a bishop from a
/// given square.
///
/// This function calculates and returns the set of squares that are
/// attacked by a bishop from the square `from_square`, on a board
/// which is occupied with pieces according to the `occupied`
/// bitboard. It does not matter if `from_square` is occupied or not.
///
/// **Important note:** This function calcuates the set of attacked
/// squares "from scratch". This can be too slow for some use cases.
pub fn bb_bishop_attacks(from_square: Square, occupied: Bitboard) -> Bitboard {
    bb_line_attacks(bb_diag(from_square), from_square, occupied) |
    bb_line_attacks(bb_anti_diag(from_square), from_square, occupied)
}


/// A helper function for `bb_rook_attacks` and `bb_bishop_attacks`.
///
/// This function calculates the set of squares, lying on a single
/// straight line (a file, rank, diagonal, or anti-diagonal), that a
/// piece can attack from a given square and given board occupancy. To
/// accomplish this it uses some insanely beautiful bit manipulations
/// that are almost indistinguishable from magic.
fn bb_line_attacks(line: Bitboard, from_square: Square, occupied: Bitboard) -> Bitboard {
    let from_square_bb = 1u64 << from_square;
    debug_assert!(from_square_bb & line != 0);
    let potential_blockers = occupied & line;
    let forward = potential_blockers.wrapping_sub(from_square_bb.wrapping_mul(2));
    let rev = reverse(reverse(potential_blockers)
                          .wrapping_sub(reverse(from_square_bb).wrapping_mul(2)));
    (forward ^ rev) & line
}


/// Reverses the order of the bits in a 64-bit number.
fn reverse(mut v: u64) -> u64 {
    v = ((v >> 1) & 0x5555555555555555) | ((v & 0x5555555555555555) << 1);
    v = ((v >> 2) & 0x3333333333333333) | ((v & 0x3333333333333333) << 2);
    v = ((v >> 4) & 0x0F0F0F0F0F0F0F0F) | ((v & 0x0F0F0F0F0F0F0F0F) << 4);
    v = ((v >> 8) & 0x00FF00FF00FF00FF) | ((v & 0x00FF00FF00FF00FF) << 8);
    v = ((v >> 16) & 0x0000FFFF0000FFFF) | ((v & 0x0000FFFF0000FFFF) << 16);
    ((v >> 32) & 0x00000000FFFFFFFF) | ((v & 0x00000000FFFFFFFF) << 32)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lsb() {
        assert_eq!(lsb(0x100100), 0x100);
        assert_eq!(lsb(0x8000000000000000), 0x8000000000000000);
        assert_eq!(lsb(0xf800000000000000), 0x0800000000000000);
        assert_eq!(lsb(0), 0);
        let mut x = 0x100100u64;
        reset_lsb(&mut x);
        assert_eq!(x, 0x100000);
        reset_lsb(&mut x);
        assert_eq!(x, 0);
        reset_lsb(&mut x);
        assert_eq!(x, 0);
        assert_eq!(above_lsb(0), 0);
        assert_eq!(above_lsb(0b11101000), 0xfffffffffffffff0);
        assert_eq!(above_lsb(0x8000000000000000), 0);
        assert_eq!(below_lsb_including(0), 0xffffffffffffffff);
        assert_eq!(below_lsb_including(0b11101000), 0b1111);
        assert_eq!(below_lsb(0), 0xffffffffffffffff);
        assert_eq!(below_lsb(0b11101000), 0b111);
        assert_eq!(above_lsb_including(0), 0);
        assert_eq!(above_lsb_including(0b1010000), 0xfffffffffffffff0);
        assert_eq!(above_lsb_including(0x8000000000000000), 0x8000000000000000);
        assert_eq!(pop_count(0), 0);
        assert_eq!(pop_count(0b1001101), 4);
        assert_eq!(pop_count(0xffffffffffffffff), 64);
    }

    #[test]
    fn test_bitscan() {
        assert_eq!(bsf(0b1001101), 0);
        assert_eq!(bsf(0b1001000), 3);
        assert_eq!(bsf(0xf000000000000000), 60);
        let mut b = 0b1100100;
        assert_eq!(bsf_reset(&mut b), 2);
        assert_eq!(b, 0b1100000);
    }

}
