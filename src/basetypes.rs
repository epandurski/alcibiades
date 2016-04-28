pub type Color = usize; // 0 or 1
pub type File = usize;  // from 0 to 7
pub type Rank = usize;  // from 0 to 7
pub type Square = usize;  // from 0 to 63
pub type PieceType = usize;  // from 0 to 5
pub type Bitboard = [[u64; 6]; 2];
pub type CastlingRights = [(bool, bool); 2];  // (King-side, Queen-side)

// Color
pub const WHITE: Color = 0;
pub const BLACK: Color = 1;

// Piece types
pub const KING: PieceType = 0;
pub const QUEEN: PieceType = 1;
pub const ROOK: PieceType = 2;
pub const BISHOP: PieceType = 3;
pub const KNIGHT: PieceType = 4;
pub const PAWN: PieceType = 5;

pub fn square(file: File, rank: Rank) -> Square {
    assert!(file < 8);
    assert!(rank < 8);
    rank * 8 + file
}
