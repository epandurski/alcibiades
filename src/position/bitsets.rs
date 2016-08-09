//! Implements useful functions for manipulating bit-sets.
//!
//! **Note:** The term "LS1B" used in the code means "least
//! significant `1` bit".

use std::num::Wrapping;
use basetypes::*;


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

// Rank 1 or 8.
pub const BB_PAWN_PROMOTION_RANKS: Bitboard = BB_RANK_1 | BB_RANK_8;

// The main diagonal (A1-H8), and the main anti-diagonal (A8-H1).
pub const BB_MAIN_DIAG: u64 = 0x8040201008040201;
pub const BB_MAIN_ANTI_DIAG: u64 = 0x0102040810204080;


/// Returns only LS1B of a value.
///
/// The way to calculate this is: `ls1b(x) = x & -x;`.
///
/// If `x` is `0` this function returns `0`.
///
/// # Examples
///
/// ```text
/// 
///        x         &        -x         =      ls1b(x)
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
pub fn ls1b(x: Bitboard) -> Bitboard {
    x & (Wrapping(0) - Wrapping(x)).0
}


/// Resets the LS1B of a value to zero.
///
/// The way to calculate this is: `x_with_reset_ls1b = x & (x - 1);`.
///
/// If `x` is `0` this function does nothing.
///
/// # Examples
///
/// ```text
/// 
///       x          &      (x - 1)      =  x_with_reset_ls1b(x)
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
pub fn reset_ls1b(x: &mut Bitboard) {
    *x &= (Wrapping(*x) - Wrapping(1)).0;
}


/// Returns a mask with all bits above the LS1B set to `1`.
///
/// The way to calculate this is: `above_ls1b_mask(x) = x ^ -x;`.
///
/// If `x` is `0` this function returns `0`.
/// 
/// # Examples:
///
/// ```text
/// 
///       x          ^        -x         =  above_LS1B_mask(x)
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
pub fn above_ls1b_mask(x: Bitboard) -> Bitboard {
    x ^ (Wrapping(0) - Wrapping(x)).0
}


/// Returns a mask with all bits below and including the LS1B set to
/// `1`.
///
/// The way to calculate this is: `below_lsb1_mask_including(x) = x ^ (x - 1);`.
///
/// If `x` is `0` this function returns `0xffffffffffffffff`.
/// 
/// # Examples:
///
/// ```text
///       x          ^      (x - 1)      =  below_lsb1_mask_including(x)
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
pub fn below_lsb1_mask_including(x: Bitboard) -> Bitboard {
    x ^ (Wrapping(x) - Wrapping(1)).0
}


/// Returns a mask with all bits above and including the LS1B set to
/// `1`.
///
/// The way to calculate this is: `above_lsb1_mask_including(x) = x |
/// -x;`.
///
/// If `x` is `0` this function returns `0`.
/// 
/// # Examples:
/// ```text
///       x          |        -x         =  above_lsb1_mask_including(x)
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
pub fn above_lsb1_mask_including(x: Bitboard) -> Bitboard {
    x | (Wrapping(0) - Wrapping(x)).0
}


/// Returns a mask with all bits below the LS1B set to `1`.
///
/// The way to calculate this is: `below_lsb1_mask(x) = !x & (x - 1);`.
///
/// If `x` is `0` this function returns `0xffffffffffffffff`.
/// 
/// # Examples:
///
/// ```text
///      !x          &      (x - 1)      =  below_lsb1_mask(x)
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
pub fn below_lsb1_mask(x: Bitboard) -> Bitboard {
    !x & (Wrapping(x) - Wrapping(1)).0
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


/// Returns the binary position of the LS1B in a value.
///
/// `b` must not be zero, otherwise this function will panic or return
/// garbage.
///
/// # Examples:
/// ```
/// assert_eq!(bitscan_forward(0b100100), 2);
/// ```
#[inline(always)]
pub fn bitscan_forward(b: Bitboard) -> Square {
    assert!(b != 0);
    b.trailing_zeros() as Square
}


/// Returns the binary position of the LS1B, and resets the LS1B to
/// zero.
///
/// `b` must not be zero, otherwise this function will panic or return
/// garbage.
/// 
/// # Examples:
/// ```
/// let mut x = 0b100100;
/// assert_eq!(bitscan_forward_and_reset(&mut x), 2);
/// assert_eq!(x, 0b100000);
/// ```
#[inline(always)]
pub fn bitscan_forward_and_reset(b: &mut Bitboard) -> Square {
    assert!(*b != 0);
    let ls1b_position = b.trailing_zeros() as Square;
    *b ^= 1 << ls1b_position;
    ls1b_position
}

/// Returns the binary position of the only binary `1` in a value.
///
/// If `b` is a number in the form `2 ** x`, this function will return
/// `x`. Otherwise, it will panic or return garbage.
#[inline(always)]
pub fn bitscan_1bit(b: Bitboard) -> Square {
    assert!(b != 0);
    assert_eq!(b, ls1b(b));
    b.trailing_zeros() as Square
}

/// Returns the number of `1`s in the binary representation of a
/// value.
///
/// # Examples:
/// ```
/// assert_eq!(pop_count(0b100101), 3);
/// ```
#[inline(always)]
pub fn pop_count(b: Bitboard) -> usize {
    b.count_ones() as usize
}


/// Returns the set of squares on the same rank as `square`.
pub fn bb_rank(square: Square) -> Bitboard {
    BB_RANK_1 << (8 * (square / 8))
}


/// Returns the set of squares on the same file as `square`.
pub fn bb_file(square: Square) -> Bitboard {
    BB_FILE_A << (square % 8)
}


/// Returns the set of squares on the same diagonal as `square`.
///
/// Diagonals go from white's queen-side to black's king-side (A1-H8
/// for example).
pub fn bb_diag(square: Square) -> Bitboard {
    let diag_index = ((square / 8).wrapping_sub(square % 8)) & 15;
    if diag_index <= 7 {
        BB_MAIN_DIAG << (8 * diag_index)
    } else {
        BB_MAIN_DIAG >> (8 * (16 - diag_index))
    }
}


/// Returns the set of squares on the same anti-diagonal as `square`.
///
/// Anti-diagonals go from white's king-side to black's queen-side
/// (H1-A8 for example).
pub fn bb_anti_diag(square: Square) -> Bitboard {
    let diag_index = ((square / 8) + (square % 8)) ^ 7;
    if diag_index <= 7 {
        BB_MAIN_ANTI_DIAG >> (8 * diag_index)
    } else {
        BB_MAIN_ANTI_DIAG << (8 * (16 - diag_index))
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ls1b_functions() {
        assert_eq!(ls1b(0x100100), 0x100);
        assert_eq!(ls1b(0x8000000000000000), 0x8000000000000000);
        assert_eq!(ls1b(0xf800000000000000), 0x0800000000000000);
        assert_eq!(ls1b(0), 0);
        let mut x = 0x100100u64;
        reset_ls1b(&mut x);
        assert_eq!(x, 0x100000);
        reset_ls1b(&mut x);
        assert_eq!(x, 0);
        reset_ls1b(&mut x);
        assert_eq!(x, 0);
        assert_eq!(above_ls1b_mask(0), 0);
        assert_eq!(above_ls1b_mask(0b11101000), 0xfffffffffffffff0);
        assert_eq!(above_ls1b_mask(0x8000000000000000), 0);
        assert_eq!(below_lsb1_mask_including(0), 0xffffffffffffffff);
        assert_eq!(below_lsb1_mask_including(0b11101000), 0b1111);
        assert_eq!(below_lsb1_mask(0), 0xffffffffffffffff);
        assert_eq!(below_lsb1_mask(0b11101000), 0b111);
        assert_eq!(above_lsb1_mask_including(0), 0);
        assert_eq!(above_lsb1_mask_including(0b1010000), 0xfffffffffffffff0);
        assert_eq!(above_lsb1_mask_including(0x8000000000000000),
                   0x8000000000000000);
        assert_eq!(pop_count(0), 0);
        assert_eq!(pop_count(0b1001101), 4);
        assert_eq!(pop_count(0xffffffffffffffff), 64);
    }

    #[test]
    fn test_bitscan() {
        assert_eq!(bitscan_forward(0b1001101), 0);
        assert_eq!(bitscan_forward(0b1001000), 3);
        assert_eq!(bitscan_forward(0xf000000000000000), 60);
        let mut b = 0b1100100;
        assert_eq!(bitscan_forward_and_reset(&mut b), 2);
        assert_eq!(b, 0b1100000);
        assert_eq!(bitscan_1bit(0b1000), bitscan_forward(0b1000));
    }

}
