// Basic types
pub type Color = usize; // 0 or 1
pub type File = usize;  // from 0 to 7
pub type Rank = usize;  // from 0 to 7
pub type Square = usize;  // from 0 to 63
pub type PieceType = usize;  // from 0 to 5
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

// Move
pub enum MoveType {
    Normal,
    EnPassant,
    Promotion,
    Castling,
}

pub struct Move(u16);

impl Move {
    pub fn new(move_type: MoveType, orig: Square, dest: Square, pp_type: PieceType) -> Move {
        assert!(orig <= 63);
        assert!(dest <= 63);
        assert!(pp_type <= 3);
        let m_type = match move_type {
            MoveType::Normal => 0,
            MoveType::EnPassant => 1,
            MoveType::Promotion => 2,
            MoveType::Castling => 3,
        };
        Move(m_type << 14 | (pp_type << 12) as u16 | (orig << 6) as u16 | (dest as u16))
    }

    pub fn move_type(&self) -> MoveType {
        match (self.0 & (0b11 << 14)) >> 14 {
            0 => MoveType::Normal,
            1 => MoveType::EnPassant,
            2 => MoveType::Promotion,
            3 => MoveType::Castling,
            _ => panic!("Impossible move type"),
        }
    }

    pub fn orig_square(&self) -> Square {
        (self.0 & (0b111111 << 6) >> 6) as Square
    }

    pub fn dest_square(&self) -> Square {
        (self.0 & 0b111111) as Square
    }

    pub fn promotion_piece_type(&self) -> PieceType {
        (self.0 & (0b11 << 12) >> 12) as PieceType
    }
    
}


pub fn square(file: File, rank: Rank) -> Square {
    assert!(file < 8);
    assert!(rank < 8);
    rank * 8 + file
}
