use basetypes::*;

pub const CASTLE_WHITE_QUEENSIDE: usize = 1 << 0;
pub const CASTLE_WHITE_KINGSIDE: usize = 1 << 1;
pub const CASTLE_BLACK_QUEENSIDE: usize = 1 << 2;
pub const CASTLE_BLACK_KINGSIDE: usize = 1 << 3;


#[derive(Debug)]
#[derive(Clone, Copy)]
pub struct CastlingRights(usize);

impl CastlingRights {
    #[inline(always)]
    pub fn new() -> CastlingRights {
        CastlingRights(0)
    }

    pub fn can_castle(&self, color: Color, side: CastlingSide) -> bool {
        self.obstacles(color, side) != UNIVERSAL_SET
    }

    pub fn set_with_mask(&mut self, mask: usize) -> bool {
        if mask > 0b1111 {
            panic!("invalid mask");
        }
        let before = self.0;
        self.0 |= mask;
        self.0 != before
    }

    #[inline(always)]
    pub fn update_with_mask(&mut self, mask: usize) {
        self.0 &= mask;
    }

    #[inline(always)]
    pub fn get_mask(&self) -> usize {
        self.0
    }
    
    #[inline(always)]
    pub fn get_for(&self, color: Color) -> usize {
        assert!(color <= 1);
        if color == WHITE {
            self.0 & 0b0011
        } else {
            self.0 >> 2
        }
    }

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

    #[inline(always)]
    pub fn obstacles(&self, color: Color, side: CastlingSide) -> u64 {
        assert!(color <= 1);
        assert!(side <= 1);
        if (1 << (color << 1) << side) & self.0 == 0 {
            // castling is not allowed, therefore every piece on every
            // square on the board is an obstacle
            return UNIVERSAL_SET;
        }
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
    }

    #[inline]
    pub fn rook_xor_mask(&self, color: Color, side: CastlingSide) -> u64 {
        let mask = match side {
            QUEENSIDE => 1 << A1 | 1 << D1,
            KINGSIDE => 1 << H1 | 1 << F1,
            _ => panic!("invalid castling side"),
        };
        match color {
            WHITE => mask,
            BLACK => mask << 56,
            _ => panic!("invalid color"),
        }
    }
}

