use basetypes::*;


// "CastlingRights" holds information about which player (white or
// black) is allowed to castle on which side (queen-side or
// king-side).
//
// The lowest 4 bits of the value contain the whole needed
// information. It is laid out the following way:
//
//  usize                                             4           0
//  +-----------------------------------------------+---+---+---+---+
//  |                                               |   |   |   |   |
//  |                   Unused                      | Castling mask |
//  |                                               |   |   |   |   |
//  +-----------------------------------------------+---+---+---+---+
//
//  bit 0 -- if set, white can castle on queen-side;
//  bit 1 -- if set, white can castle on king-side;
//  bit 2 -- if set, black can castle on queen-side;
//  bit 3 -- if set, black can castle on king-side.
#[derive(Debug)]
#[derive(Clone, Copy)]
pub struct CastlingRights(usize);


impl CastlingRights {
    // Create a new instance.
    #[inline(always)]
    pub fn new() -> CastlingRights {
        CastlingRights(0)
    }


    // Get the contained raw value.
    #[inline(always)]
    pub fn get_value(&self) -> usize {
        self.0
    }


    // Grant castling rights according to a given 4-bit mask (bit-wise
    // OR-ing the previous value with "mask").
    //
    // Returns which bits were zero before, and were turned to one.
    pub fn grant(&mut self, mask: usize) -> usize {
        if mask > 0b1111 {
            panic!("invalid mask");
        }
        let before = self.0;
        self.0 |= mask;
        !before & mask
    }


    // Update the castling rights with a 4-bit mask (bit-wise AND-ing
    // the previous value with "mask").
    #[inline(always)]
    pub fn update_with_mask(&mut self, mask: usize) {
        self.0 &= mask;
    }


    // Return if the player "color" can castle on the given "side".
    #[inline(always)]
    pub fn can_castle(&self, color: Color, side: CastlingSide) -> bool {
        assert!(color <= 1);
        assert!(side <= 1);
        (1 << (color << 1) << side) & self.0 != 0
    }


    // Return a bitboard with the set of squares that should be vacant
    // in order for the specified ("color", "side") castling move to
    // be possible.
    #[inline(always)]
    pub fn obstacles(&self, color: Color, side: CastlingSide) -> u64 {
        if self.can_castle(color, side) {
            let obstacles = if side == QUEENSIDE {
                1 << B1 | 1 << C1 | 1 << D1
            } else {
                1 << F1 | 1 << G1
            };
            if color == WHITE {
                obstacles
            } else {
                obstacles << 56
            }
        } else {
            // Castling is not allowed, therefore every piece on every
            // square on the board can be considered an obstacle.
            return UNIVERSAL_SET;
        }
    }


    // Return a 2-bit value representing the castling rights for the
    // player "color".
    #[inline(always)]
    pub fn get_for(&self, color: Color) -> usize {
        assert!(color <= 1);
        if color == WHITE {
            self.0 & 0b0011
        } else {
            self.0 >> 2
        }
    }


    // Set the castling rights for the player "color" with a 2-bit
    // value ("rights").
    #[inline(always)]
    pub fn set_for(&mut self, color: Color, rights: usize) {
        assert!(color <= 1);
        assert!(rights <= 0b11);
        self.0 = if color == WHITE {
            self.0 & 0b1100 | rights & 0b0011
        } else {
            self.0 & 0b0011 | (rights & 0b0011) << 2
        }
    }
}


// Castling flags
pub const CASTLE_WHITE_QUEENSIDE: usize = 1 << 0;
pub const CASTLE_WHITE_KINGSIDE: usize = 1 << 1;
pub const CASTLE_BLACK_QUEENSIDE: usize = 1 << 2;
pub const CASTLE_BLACK_KINGSIDE: usize = 1 << 3;
