// Basic types
pub type Color = usize; // 0 or 1
pub type File = usize;  // from 0 to 7
pub type Rank = usize;  // from 0 to 7
pub type Square = usize;  // from 0 to 63
pub type PieceType = usize;  // from 0 to 5
pub type MoveType = usize;  // from 0 to 3
pub type CastlingRights = [(bool, bool); 2];  // (King-side, Queen-side)

// Color
pub const WHITE: Color = 0;
pub const BLACK: Color = 1;

// Piece types
pub const QUEEN: PieceType = 0;
pub const ROOK: PieceType = 1;
pub const BISHOP: PieceType = 2;
pub const KNIGHT: PieceType = 3;
pub const KING: PieceType = 4;
pub const PAWN: PieceType = 5;

// Move types
pub const MOVE_NORMAL: MoveType = 0;
pub const MOVE_ENPASSANT: MoveType = 1;
pub const MOVE_PROMOTION: MoveType = 2;
pub const MOVE_CASTLING: MoveType = 3;

// Ranks
pub const RANK_1: Rank = 0;
pub const RANK_2: Rank = 1;
pub const RANK_3: Rank = 2;
pub const RANK_4: Rank = 3;
pub const RANK_5: Rank = 4;
pub const RANK_6: Rank = 5;
pub const RANK_7: Rank = 6;
pub const RANK_8: Rank = 7;

// Files
pub const FILE_A: Rank = 0;
pub const FILE_B: Rank = 1;
pub const FILE_C: Rank = 2;
pub const FILE_D: Rank = 3;
pub const FILE_E: Rank = 4;
pub const FILE_F: Rank = 5;
pub const FILE_G: Rank = 6;
pub const FILE_H: Rank = 7;

// Squares
pub const A1: Square = 0 + 0*8;
pub const B1: Square = 1 + 0*8;
pub const C1: Square = 2 + 0*8;
pub const D1: Square = 3 + 0*8;
pub const E1: Square = 4 + 0*8;
pub const F1: Square = 5 + 0*8;
pub const G1: Square = 6 + 0*8;
pub const H1: Square = 7 + 0*8;

pub const A2: Square = 0 + 1*8;
pub const B2: Square = 1 + 1*8;
pub const C2: Square = 2 + 1*8;
pub const D2: Square = 3 + 1*8;
pub const E2: Square = 4 + 1*8;
pub const F2: Square = 5 + 1*8;
pub const G2: Square = 6 + 1*8;
pub const H2: Square = 7 + 1*8;

pub const A3: Square = 0 + 2*8;
pub const B3: Square = 1 + 2*8;
pub const C3: Square = 2 + 2*8;
pub const D3: Square = 3 + 2*8;
pub const E3: Square = 4 + 2*8;
pub const F3: Square = 5 + 2*8;
pub const G3: Square = 6 + 2*8;
pub const H3: Square = 7 + 2*8;

pub const A4: Square = 0 + 3*8;
pub const B4: Square = 1 + 3*8;
pub const C4: Square = 2 + 3*8;
pub const D4: Square = 3 + 3*8;
pub const E4: Square = 4 + 3*8;
pub const F4: Square = 5 + 3*8;
pub const G4: Square = 6 + 3*8;
pub const H4: Square = 7 + 3*8;

pub const A5: Square = 0 + 4*8;
pub const B5: Square = 1 + 4*8;
pub const C5: Square = 2 + 4*8;
pub const D5: Square = 3 + 4*8;
pub const E5: Square = 4 + 4*8;
pub const F5: Square = 5 + 4*8;
pub const G5: Square = 6 + 4*8;
pub const H5: Square = 7 + 4*8;

pub const A6: Square = 0 + 5*8;
pub const B6: Square = 1 + 5*8;
pub const C6: Square = 2 + 5*8;
pub const D6: Square = 3 + 5*8;
pub const E6: Square = 4 + 5*8;
pub const F6: Square = 5 + 5*8;
pub const G6: Square = 6 + 5*8;
pub const H6: Square = 7 + 5*8;

pub const A7: Square = 0 + 6*8;
pub const B7: Square = 1 + 6*8;
pub const C7: Square = 2 + 6*8;
pub const D7: Square = 3 + 6*8;
pub const E7: Square = 4 + 6*8;
pub const F7: Square = 5 + 6*8;
pub const G7: Square = 6 + 6*8;
pub const H7: Square = 7 + 6*8;

pub const A8: Square = 0 + 7*8;
pub const B8: Square = 1 + 7*8;
pub const C8: Square = 2 + 7*8;
pub const D8: Square = 3 + 7*8;
pub const E8: Square = 4 + 7*8;
pub const F8: Square = 5 + 7*8;
pub const G8: Square = 6 + 7*8;
pub const H8: Square = 7 + 7*8;


pub struct Board {
    pub piece_type: [u64; 6],
    pub color: [u64; 2],
}


pub struct Move(u16);

impl Move {
    #[inline]
    pub fn new(m_type: MoveType, orig: Square, dest: Square, pp_type: PieceType) -> Move {
        assert!(m_type <= 3);
        assert!(orig <= 63);
        assert!(dest <= 63);
        assert!(pp_type <= 3);
        Move( ((m_type << 14) | (pp_type << 12) | (orig << 6) | dest) as u16 )
    }

    #[inline]
    pub fn move_type(&self) -> MoveType {
        ((self.0 & (0b11 << 14)) >> 14) as MoveType
    }

    #[inline]
    pub fn orig_square(&self) -> Square {
        (self.0 & (0b111111 << 6) >> 6) as Square
    }

    #[inline]
    pub fn dest_square(&self) -> Square {
        (self.0 & 0b111111) as Square
    }

    #[inline]
    pub fn promotion_piece_type(&self) -> PieceType {
        (self.0 & (0b11 << 12) >> 12) as PieceType
    }
}


#[inline]
pub fn square(file: File, rank: Rank) -> Square {
    assert!(file < 8);
    assert!(rank < 8);
    rank * 8 + file
}

#[inline]
pub fn rank(square: Square) -> Rank {
    square / 8
}

#[inline]
pub fn file(square: Square) -> File {
    square % 8
}

