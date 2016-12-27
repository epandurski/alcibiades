//! Defines constants and functions for working with bitboards.
//!
//! `u64` bit-sets called *bitboards* can be used to represent a set
//! of squares on the chessboard. This module defines utility
//! functions and constants for working with `u64` bit-sets.
//!
//! **Note:** "LSB" means "least significant `1` bit".

use board::{Square, Bitboard};


/// Empty set of squares.
pub const BB_NONE: Bitboard = 0;

/// The set of all 64 squares on the board.
pub const BB_ALL: Bitboard = 0xffffffffffffffff;

/// The squares on rank 1.
pub const BB_RANK_1: Bitboard = 0b11111111;
/// The squares on rank 2.
pub const BB_RANK_2: Bitboard = BB_RANK_1 << 8;
/// The squares on rank 3.
pub const BB_RANK_3: Bitboard = BB_RANK_2 << 8;
/// The squares on rank 4.
pub const BB_RANK_4: Bitboard = BB_RANK_3 << 8;
/// The squares on rank 5.
pub const BB_RANK_5: Bitboard = BB_RANK_4 << 8;
/// The squares on rank 6.
pub const BB_RANK_6: Bitboard = BB_RANK_5 << 8;
/// The squares on rank 7.
pub const BB_RANK_7: Bitboard = BB_RANK_6 << 8;
/// The squares on rank 8.
pub const BB_RANK_8: Bitboard = BB_RANK_7 << 8;

/// The squares on file A.
pub const BB_FILE_A: Bitboard = 0x0101010101010101;
/// The squares on file B.
pub const BB_FILE_B: Bitboard = BB_FILE_A << 1;
/// The squares on file C.
pub const BB_FILE_C: Bitboard = BB_FILE_B << 1;
/// The squares on file D.
pub const BB_FILE_D: Bitboard = BB_FILE_C << 1;
/// The squares on file E.
pub const BB_FILE_E: Bitboard = BB_FILE_D << 1;
/// The squares on file F.
pub const BB_FILE_F: Bitboard = BB_FILE_E << 1;
/// The squares on file G.
pub const BB_FILE_G: Bitboard = BB_FILE_F << 1;
/// The squares on file H.
pub const BB_FILE_H: Bitboard = BB_FILE_G << 1;

/// The squares on the main diagonal (A1-H8).
pub const BB_MAIN_DIAG: Bitboard = 0x8040201008040201;

/// The squares on the main anti-diagonal (H1-A8).
pub const BB_MAIN_ANTI_DIAG: Bitboard = 0x0102040810204080;


/// Returns the LSB of a value.
///
/// The way to calculate this is: `lsb = x & -x;`.
///
/// If `x` is `0` this function returns `0`.
///
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// assert_eq!(lsb(0b10100), 0b100);
/// assert_eq!(lsb(0), 0);
/// ```
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
/// If `*x` is `0` this function does nothing.
///
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// let mut x = 0b100100;
/// reset_lsb(&mut x);
/// assert_eq!(x, 0b100000);
/// reset_lsb(&mut x);
/// assert_eq!(x, 0);
/// reset_lsb(&mut x);
/// assert_eq!(x, 0);
/// ```
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
/// ```rust
/// # use alcibiades::bitsets::*;
/// assert_eq!(above_lsb(0b10100), !0b111);
/// assert_eq!(above_lsb(0), 0);
/// ```
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
/// ```rust
/// # use alcibiades::bitsets::*;
/// assert_eq!(below_lsb_including(0b10100), 0b111);
/// assert_eq!(below_lsb_including(0), !0);
/// ```
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
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// assert_eq!(above_lsb_including(0b10100), !0b11);
/// assert_eq!(above_lsb_including(0), 0);
/// ```
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
/// ```rust
/// # use alcibiades::bitsets::*;
/// assert_eq!(below_lsb(0b10100), 0b11);
/// assert_eq!(below_lsb(0), !0);
/// ```
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
/// Returns `x << s` if `s` is positive, `x >> s` if `s` is
/// negative, `x` if `s` is zero.
///
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// assert_eq!(gen_shift(0b101, 1), 0b1010);
/// assert_eq!(gen_shift(0b101, -1), 0b10);
/// assert_eq!(gen_shift(0b101, 0), 0b101);
/// ```
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
/// If `x` is `0` this function returns `64`.
///
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// assert_eq!(bsf(0b100100), 2);
/// assert_eq!(bsf(0), 64);
/// ```
#[inline(always)]
pub fn bsf(x: Bitboard) -> Square {
    x.trailing_zeros() as Square
}


/// Returns the binary position of the LSB (bit-scan-forward), and
/// resets the LSB to zero.
///
/// If `*x` is `0` this function returns `64`.
/// 
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// let mut x = 0b100100;
/// assert_eq!(bsf_reset(&mut x), 2);
/// assert_eq!(x, 0b100000);
/// assert_eq!(bsf_reset(&mut x), 5);
/// assert_eq!(x, 0);
/// assert_eq!(bsf_reset(&mut x), 64);
/// assert_eq!(x, 0);
/// ```
#[inline(always)]
pub fn bsf_reset(x: &mut Bitboard) -> Square {
    let a = lsb(*x);
    *x ^= a;
    bsf(a)
}


/// Returns the number of `1`s in the binary representation of a
/// value.
///
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// assert_eq!(pop_count(0b100101), 3);
/// assert_eq!(pop_count(0), 0);
/// ```
#[inline(always)]
pub fn pop_count(b: Bitboard) -> usize {
    b.count_ones() as usize
}


/// Returns the set of squares on the same rank as the given square.
///
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// # use alcibiades::squares::*;
/// assert_eq!(bb_rank(E4), BB_RANK_4);
/// ```
#[inline(always)]
pub fn bb_rank(square: Square) -> Bitboard {
    BB_RANK_1 << ((square >> 3) << 3)
}


/// Returns the set of squares on the same file as the given square.
///
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// # use alcibiades::squares::*;
/// assert_eq!(bb_file(E4), BB_FILE_E);
/// ```
#[inline(always)]
pub fn bb_file(square: Square) -> Bitboard {
    BB_FILE_A << (square % 8)
}


/// Returns the set of squares on the same diagonal as the given square.
///
/// Diagonals go from white's queen-side to black's king-side (A1-H8
/// for example).
///
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// # use alcibiades::squares::*;
/// assert_eq!(bb_diag(D4), BB_MAIN_DIAG);
/// ```
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
///
/// # Examples:
///
/// ```rust
/// # use alcibiades::bitsets::*;
/// # use alcibiades::squares::*;
/// assert_eq!(bb_anti_diag(E4), BB_MAIN_ANTI_DIAG);
/// ```
#[inline]
pub fn bb_anti_diag(square: Square) -> Bitboard {
    let diag_index = ((square >> 3) + (square % 8)) ^ 7;
    if diag_index <= 7 {
        BB_MAIN_ANTI_DIAG >> (diag_index << 3)
    } else {
        BB_MAIN_ANTI_DIAG << ((16 - diag_index) << 3)
    }
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
