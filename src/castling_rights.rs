use basetypes::*;


/// Holds information about which player is allowed to castle on which
/// side.
///
/// `CastlingRights` is a `usize` number. The lowest 4 bits of the
/// value contain the whole needed information. It is laid out the
/// following way:
///
/// ```text
///  usize                                             3           0
///  +-----------------------------------------------+---+---+---+---+
///  |                                               |   |   |   |   |
///  |                   Unused                      |Castling flags |
///  |                                               |   |   |   |   |
///  +-----------------------------------------------+---+---+---+---+
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
    #[inline]
    pub fn new() -> CastlingRights {
        CastlingRights(0)
    }


    /// Returns the contained raw value.
    #[inline]
    pub fn value(&self) -> usize {
        self.0
    }


    /// Grants castling rights according to a given 4-bit mask.
    ///
    /// `mask` is bit-wise OR-ed with the previous value.
    ///
    /// Returns which bits were zero before, and were turned to one.
    pub fn grant(&mut self, mask: usize) -> usize {
        if mask > 0b1111 {
            panic!("invalid mask");
        }
        let before = self.0;
        self.0 |= mask;
        !before & mask
    }


    /// Updates the castling rights with a 4-bit mask.
    ///
    /// `mask` is bit-wise AND-ed with the previous value.
    #[inline]
    pub fn update_with_mask(&mut self, mask: usize) {
        self.0 &= mask;
    }


    /// Returns if a given player can castle on a given side.
    #[inline]
    pub fn can_castle(&self, color: Color, side: CastlingSide) -> bool {
        assert!(color <= 1);
        assert!(side <= 1);
        (1 << (color << 1) << side) & self.0 != 0
    }


    /// Returns a bitboard with potential castling obstacles.
    /// 
    /// Returns a bitboard with the set of squares that should be
    /// vacant in order for the specified (`color`, `side`) castling
    /// move to be possible.
    #[inline]
    pub fn obstacles(&self, color: Color, side: CastlingSide) -> u64 {
        const OBSTACLES: [[u64; 2]; 2] = [[1 << B1 | 1 << C1 | 1 << D1, 1 << F1 | 1 << G1],
                                          [1 << B8 | 1 << C8 | 1 << D8, 1 << F8 | 1 << G8]];
        if self.can_castle(color, side) {
            OBSTACLES[color][side]
        } else {
            // Castling is not allowed, therefore every piece on every
            // square on the board can be considered an obstacle.
            !0
        }
    }


    /// Returns a 2-bit value representing the castling rights for a
    /// given player.
    #[inline]
    pub fn get_for(&self, color: Color) -> usize {
        assert!(color <= 1);
        if color == WHITE {
            self.0 & 0b0011
        } else {
            self.0 >> 2
        }
    }


    /// Sets the castling rights for a given player with a 2-bit
    /// value.
    #[inline]
    pub fn set_for(&mut self, color: Color, rights: usize) {
        assert!(color <= 1);
        if rights > 0b11 {
            // Since the raw value of "CastlingRights" frequently is used
            // as an array index without boundary checking, we guarantee
            // that the raw value is always less than 16 by sanitizing the
            // passed "rights".
            panic!("invalid castling rights");
        }
        self.0 = if color == WHITE {
            self.0 & 0b1100 | rights
        } else {
            self.0 & 0b0011 | rights << 2
        }
    }
}


/// `QUEENSIDE` of `KINGSIDE`.
pub type CastlingSide = usize;

/// Queen-side castling.
pub const QUEENSIDE: CastlingSide = 0;

/// King-side castling.
pub const KINGSIDE: CastlingSide = 1;

/// White can castle on the queen-side.
pub const CASTLE_WHITE_QUEENSIDE: usize = 1 << 0;

/// White can castle on the king-side.
pub const CASTLE_WHITE_KINGSIDE: usize = 1 << 1;

/// Black can castle on the queen-side.
pub const CASTLE_BLACK_QUEENSIDE: usize = 1 << 2;

/// Black can castle on the king-side.
pub const CASTLE_BLACK_KINGSIDE: usize = 1 << 3;


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_castling_rights() {
        use basetypes::*;
        use bitsets::*;

        let mut c = CastlingRights::new();
        c.set_for(WHITE, 0b10);
        c.set_for(BLACK, 0b11);
        assert_eq!(c.can_castle(WHITE, QUEENSIDE), false);
        assert_eq!(c.can_castle(WHITE, KINGSIDE), true);
        assert_eq!(c.can_castle(BLACK, QUEENSIDE), true);
        assert_eq!(c.can_castle(BLACK, KINGSIDE), true);
        c.update_with_mask(!CASTLE_BLACK_KINGSIDE);
        assert_eq!(c.can_castle(WHITE, QUEENSIDE), false);
        assert_eq!(c.can_castle(WHITE, KINGSIDE), true);
        assert_eq!(c.can_castle(BLACK, QUEENSIDE), true);
        assert_eq!(c.can_castle(BLACK, KINGSIDE), false);
        assert_eq!(c.get_for(BLACK), 0b01);
        assert_eq!(c.get_for(WHITE), 0b10);
        assert_eq!(c.value(), 0b0110);
        let granted = c.grant(CASTLE_BLACK_KINGSIDE);
        assert_eq!(granted, CASTLE_BLACK_KINGSIDE);
        let granted = c.grant(CASTLE_BLACK_KINGSIDE);
        assert_eq!(granted, 0);
        assert_eq!(c.value(), 0b1110);
        assert_eq!(c.obstacles(WHITE, QUEENSIDE), UNIVERSAL_SET);
        assert_eq!(c.obstacles(BLACK, QUEENSIDE), 1 << B8 | 1 << C8 | 1 << D8);
    }
}
