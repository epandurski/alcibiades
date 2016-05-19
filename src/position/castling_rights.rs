use basetypes::*;


// "CastlingRights" holds information about which player (white or
// black) is allowed to castle on which side (queen-side or
// king-side).
//
// The lowest 4 bits of the value contain the whole needed
// information. It is laid out the following way:
//
//  usize                                             3           0
//  +-----------------------------------------------+---+---+---+---+
//  |                                               |   |   |   |   |
//  |                   Unused                      |Castling flags |
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
    #[inline]
    pub fn new() -> CastlingRights {
        CastlingRights(0)
    }


    // Get the contained raw value.
    #[inline]
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
    #[inline]
    pub fn update_with_mask(&mut self, mask: usize) {
        self.0 &= mask;
    }


    // Return if the player "color" can castle on the given "side".
    #[inline]
    pub fn can_castle(&self, color: Color, side: CastlingSide) -> bool {
        assert!(color <= 1);
        assert!(side <= 1);
        (1 << (color << 1) << side) & self.0 != 0
    }


    // Return a bitboard with the set of squares that should be vacant
    // in order for the specified ("color", "side") castling move to
    // be possible.
    #[inline]
    pub fn obstacles(&self, color: Color, side: CastlingSide) -> u64 {
        const OBSTACLES: [[u64; 2]; 2] = [[1 << B1 | 1 << C1 | 1 << D1, 1 << F1 | 1 << G1],
                                          [1 << B8 | 1 << C8 | 1 << D8, 1 << F8 | 1 << G8]];
        if self.can_castle(color, side) {
            OBSTACLES[color][side]
        } else {
            // Castling is not allowed, therefore every piece on every
            // square on the board can be considered an obstacle.
            UNIVERSAL_SET
        }
    }


    // Return a 2-bit value representing the castling rights for the
    // player "color".
    #[inline]
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


// Castling flags
pub const CASTLE_WHITE_QUEENSIDE: usize = 1 << 0;
pub const CASTLE_WHITE_KINGSIDE: usize = 1 << 1;
pub const CASTLE_BLACK_QUEENSIDE: usize = 1 << 2;
pub const CASTLE_BLACK_KINGSIDE: usize = 1 << 3;
