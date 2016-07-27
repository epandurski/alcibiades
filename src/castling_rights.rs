//! Implements castling rights logic.

use basetypes::*;


/// Holds information about which player is allowed to castle on which
/// side.
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
#[derive(Debug)]
#[derive(Clone, Copy)]
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
    /// This method returns `true` if the player had the right to
    /// castle on the given side before this method was called, and
    /// `false` otherwise.
    pub fn grant(&mut self, player: Color, side: CastlingSide) -> bool {
        if player > 1 || side > 1 {
            panic!("invalid arguments");
        }
        let before = self.0;
        let mask = 1 << (player << 1) << side;
        self.0 |= mask;
        !before & mask == 0
    }

    /// Updates the castling rights after played move.
    ///
    /// `orig_square` and `dest_square` describe the played move.
    #[inline]
    pub fn update(&mut self, orig_square: Square, dest_square: Square) {
        assert!(orig_square <= 63);
        assert!(dest_square <= 63);
        // On each move, the value of `CASTLING_RELATION` for the
        // origin and destination squares should be &-ed with the
        // castling rights value, to derive the updated castling
        // rights.
        const CASTLING_RELATION: [usize; 64] = [
            !CASTLE_WHITE_QUEENSIDE, !0, !0, !0,
            !(CASTLE_WHITE_QUEENSIDE | CASTLE_WHITE_KINGSIDE), !0, !0, !CASTLE_WHITE_KINGSIDE,
            !0, !0, !0, !0, !0, !0, !0, !0,
            !0, !0, !0, !0, !0, !0, !0, !0,
            !0, !0, !0, !0, !0, !0, !0, !0,
            !0, !0, !0, !0, !0, !0, !0, !0,
            !0, !0, !0, !0, !0, !0, !0, !0,
            !0, !0, !0, !0, !0, !0, !0, !0,
            !CASTLE_BLACK_QUEENSIDE, !0, !0, !0,
            !(CASTLE_BLACK_QUEENSIDE | CASTLE_BLACK_KINGSIDE), !0, !0, !CASTLE_BLACK_KINGSIDE,
        ];
        self.0 &= unsafe {
            // AND-ing with anything can not corrupt the instance, so
            // we are safe even if `orig_square` and `dest_square` are
            // out of bounds.
            *CASTLING_RELATION.get_unchecked(orig_square) &
            *CASTLING_RELATION.get_unchecked(dest_square)
        };
    }

    /// Returns if a given player can castle on a given side.
    #[inline]
    pub fn can_castle(&self, player: Color, side: CastlingSide) -> bool {
        assert!(player <= 1);
        assert!(side <= 1);
        (1 << (player << 1) << side) & self.0 != 0
    }

    /// Returns a bitboard with potential castling obstacles.
    /// 
    /// Returns a bitboard with the set of squares that should be
    /// vacant in order for the specified (`player`, `side`) castling
    /// move to be possible. If `player` can never castle on `side`,
    /// because the king or the rook had been moved, this method
    /// returns universal set (`0xffffffffffffffff`).
    #[inline]
    pub fn obstacles(&self, player: Color, side: CastlingSide) -> u64 {
        const OBSTACLES: [[u64; 2]; 2] = [[1 << B1 | 1 << C1 | 1 << D1, 1 << F1 | 1 << G1],
                                          [1 << B8 | 1 << C8 | 1 << D8, 1 << F8 | 1 << G8]];
        if self.can_castle(player, side) {
            OBSTACLES[player][side]
        } else {
            // Castling is not allowed, therefore every piece on every
            // square on the board can be considered an obstacle.
            !0
        }
    }
}


/// `QUEENSIDE` of `KINGSIDE`.
pub type CastlingSide = usize;

/// Queen-side castling.
pub const QUEENSIDE: CastlingSide = 0;

/// King-side castling.
pub const KINGSIDE: CastlingSide = 1;


// White can castle on the queen-side.
const CASTLE_WHITE_QUEENSIDE: usize = 1 << 0;

// White can castle on the king-side.
const CASTLE_WHITE_KINGSIDE: usize = 1 << 1;

// Black can castle on the queen-side.
const CASTLE_BLACK_QUEENSIDE: usize = 1 << 2;

// Black can castle on the king-side.
const CASTLE_BLACK_KINGSIDE: usize = 1 << 3;


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_castling_rights() {
        use basetypes::*;
        use bitsets::*;

        let mut c = CastlingRights::new(0b1110);
        assert_eq!(c.can_castle(WHITE, QUEENSIDE), false);
        assert_eq!(c.can_castle(WHITE, KINGSIDE), true);
        assert_eq!(c.can_castle(BLACK, QUEENSIDE), true);
        assert_eq!(c.can_castle(BLACK, KINGSIDE), true);
        c.update(H8, H7);
        assert_eq!(c.can_castle(WHITE, QUEENSIDE), false);
        assert_eq!(c.can_castle(WHITE, KINGSIDE), true);
        assert_eq!(c.can_castle(BLACK, QUEENSIDE), true);
        assert_eq!(c.can_castle(BLACK, KINGSIDE), false);
        assert_eq!(c.value(), 0b0110);
        let granted = c.grant(BLACK, KINGSIDE);
        assert_eq!(granted, false);
        let granted = c.grant(BLACK, KINGSIDE);
        assert_eq!(granted, true);
        assert_eq!(c.value(), 0b1110);
        assert_eq!(c.obstacles(WHITE, QUEENSIDE), BB_UNIVERSAL_SET);
        assert_eq!(c.obstacles(BLACK, QUEENSIDE), 1 << B8 | 1 << C8 | 1 << D8);
    }
}
