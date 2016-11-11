//! Defines types and constants related to castling.

use std::fmt;
use super::*;


/// `QUEENSIDE` or `KINGSIDE`.
pub type CastlingSide = usize;

pub const QUEENSIDE: CastlingSide = 0;
pub const KINGSIDE: CastlingSide = 1;


/// Holds information about which player can castle on which side.
///
/// The castling rights are held in a `usize` value. The lowest 4 bits
/// of the value contain the whole needed information. It is laid out
/// in the following way:
///
/// ```text
///  usize                    3   2   1   0
///  +----------------------+---+---+---+---+
///  |                      |   |   |   |   |
///  |    Unused (zeros)    |Castling flags |
///  |                      |   |   |   |   |
///  +----------------------+---+---+---+---+
///
///  bit 0 -- if set, white can castle on queen-side;
///  bit 1 -- if set, white can castle on king-side;
///  bit 2 -- if set, black can castle on queen-side;
///  bit 3 -- if set, black can castle on king-side.
/// ```
#[derive(Clone, Copy, Debug)]
pub struct CastlingRights(usize);

impl CastlingRights {
    /// Creates a new instance.
    ///
    /// The least significant 4 bits of `value` are used as a raw
    /// value for the new instance.
    #[inline(always)]
    pub fn new(value: usize) -> CastlingRights {
        CastlingRights(value & 0b1111)
    }

    /// Returns the contained raw value (between 0 and 15).
    #[inline(always)]
    pub fn value(&self) -> usize {
        self.0
    }

    /// Grants a given player the right to castle on a given side.
    ///
    /// This method returns `true` if the player did not have the
    /// right to castle on the given side before this method was
    /// called, and `false` otherwise.
    pub fn grant(&mut self, player: Color, side: CastlingSide) -> bool {
        assert!(player <= 1);
        assert!(side <= 1);
        let rights_before = self.0;
        let granted = 1 << (player << 1) << side;
        self.0 |= granted;

        granted & !rights_before != 0
    }

    /// Updates the castling rights after played move.
    ///
    /// `orig_square` and `dest_square` describe the played move.
    #[inline(always)]
    pub fn update(&mut self, orig_square: Square, dest_square: Square) {
        debug_assert!(orig_square <= 63);
        debug_assert!(dest_square <= 63);
        const WQ: usize = (1 << (WHITE << 1) << QUEENSIDE);
        const WK: usize = (1 << (WHITE << 1) << KINGSIDE);
        const W: usize = WQ | WK;
        const BQ: usize = (1 << (BLACK << 1) << QUEENSIDE);
        const BK: usize = (1 << (BLACK << 1) << KINGSIDE);
        const B: usize = BQ | BK;

        // On each move, the value of `CASTLING_RELATION` for the
        // origin and destination squares should be AND-ed with the
        // castling rights value, to derive the updated castling
        // rights.
        const CASTLING_RELATION: [usize; 64] = [
            !WQ, !0, !0, !0, !W, !0, !0, !WK,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !BQ, !0, !0, !0, !B, !0, !0, !BK
        ];
        self.0 &= CASTLING_RELATION[orig_square] & CASTLING_RELATION[dest_square];
    }

    /// Returns if a given player has the rights to castle on a given
    /// side.
    #[inline(always)]
    pub fn can_castle(&self, player: Color, side: CastlingSide) -> bool {
        debug_assert!(player <= 1);
        debug_assert!(side <= 1);
        (1 << (player << 1) << side) & self.0 != 0
    }
}

impl fmt::Display for CastlingRights {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut value = self.value();
        for s in ["Q", "K", "q", "k"].iter() {
            if value & 1 == 1 {
                try!(f.write_str(s));
            }
            value >>= 1;
        }
        Ok(())
    }
}
