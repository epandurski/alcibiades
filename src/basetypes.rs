// Basic types
pub type Color = usize; // 0 or 1
pub type PieceType = usize;  // from 0 to 5
pub type MoveType = usize;  // from 0 to 3
pub type CastlingSide = usize;  // 0 or 1
pub type Rank = usize;  // from 0 to 7
pub type File = usize;  // from 0 to 7
pub type Square = usize;  // from 0 to 63
pub type Value = i16;

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
pub const NO_PIECE: PieceType = 6;

// Castling sides
pub const QUEENSIDE: CastlingSide = 0;
pub const KINGSIDE: CastlingSide = 1;

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
pub const A1: Square = 0 + 0 * 8;
pub const B1: Square = 1 + 0 * 8;
pub const C1: Square = 2 + 0 * 8;
pub const D1: Square = 3 + 0 * 8;
pub const E1: Square = 4 + 0 * 8;
pub const F1: Square = 5 + 0 * 8;
pub const G1: Square = 6 + 0 * 8;
pub const H1: Square = 7 + 0 * 8;
pub const A2: Square = 0 + 1 * 8;
pub const B2: Square = 1 + 1 * 8;
pub const C2: Square = 2 + 1 * 8;
pub const D2: Square = 3 + 1 * 8;
pub const E2: Square = 4 + 1 * 8;
pub const F2: Square = 5 + 1 * 8;
pub const G2: Square = 6 + 1 * 8;
pub const H2: Square = 7 + 1 * 8;
pub const A3: Square = 0 + 2 * 8;
pub const B3: Square = 1 + 2 * 8;
pub const C3: Square = 2 + 2 * 8;
pub const D3: Square = 3 + 2 * 8;
pub const E3: Square = 4 + 2 * 8;
pub const F3: Square = 5 + 2 * 8;
pub const G3: Square = 6 + 2 * 8;
pub const H3: Square = 7 + 2 * 8;
pub const A4: Square = 0 + 3 * 8;
pub const B4: Square = 1 + 3 * 8;
pub const C4: Square = 2 + 3 * 8;
pub const D4: Square = 3 + 3 * 8;
pub const E4: Square = 4 + 3 * 8;
pub const F4: Square = 5 + 3 * 8;
pub const G4: Square = 6 + 3 * 8;
pub const H4: Square = 7 + 3 * 8;
pub const A5: Square = 0 + 4 * 8;
pub const B5: Square = 1 + 4 * 8;
pub const C5: Square = 2 + 4 * 8;
pub const D5: Square = 3 + 4 * 8;
pub const E5: Square = 4 + 4 * 8;
pub const F5: Square = 5 + 4 * 8;
pub const G5: Square = 6 + 4 * 8;
pub const H5: Square = 7 + 4 * 8;
pub const A6: Square = 0 + 5 * 8;
pub const B6: Square = 1 + 5 * 8;
pub const C6: Square = 2 + 5 * 8;
pub const D6: Square = 3 + 5 * 8;
pub const E6: Square = 4 + 5 * 8;
pub const F6: Square = 5 + 5 * 8;
pub const G6: Square = 6 + 5 * 8;
pub const H6: Square = 7 + 5 * 8;
pub const A7: Square = 0 + 6 * 8;
pub const B7: Square = 1 + 6 * 8;
pub const C7: Square = 2 + 6 * 8;
pub const D7: Square = 3 + 6 * 8;
pub const E7: Square = 4 + 6 * 8;
pub const F7: Square = 5 + 6 * 8;
pub const G7: Square = 6 + 6 * 8;
pub const H7: Square = 7 + 6 * 8;
pub const A8: Square = 0 + 7 * 8;
pub const B8: Square = 1 + 7 * 8;
pub const C8: Square = 2 + 7 * 8;
pub const D8: Square = 3 + 7 * 8;
pub const E8: Square = 4 + 7 * 8;
pub const F8: Square = 5 + 7 * 8;
pub const G8: Square = 6 + 7 * 8;
pub const H8: Square = 7 + 7 * 8;

// We use "u64" bit-sets called bitboards (BB) to represent a set of
// squares on the board. Here are some useful square-sets.
pub const UNIVERSAL_SET: u64 = 0xffffffffffffffff;
pub const EMPTY_SET: u64 = 0;

pub const BB_RANK_1: u64 = 0b11111111;
pub const BB_RANK_2: u64 = BB_RANK_1 << 8;
pub const BB_RANK_3: u64 = BB_RANK_2 << 8;
pub const BB_RANK_4: u64 = BB_RANK_3 << 8;
pub const BB_RANK_5: u64 = BB_RANK_4 << 8;
pub const BB_RANK_6: u64 = BB_RANK_5 << 8;
pub const BB_RANK_7: u64 = BB_RANK_6 << 8;
pub const BB_RANK_8: u64 = BB_RANK_7 << 8;

pub const BB_FILE_A: u64 = 1 << A1 | 1 << A2 | 1 << A3 | 1 << A4 | 1 << A5 | 1 << A6 | 1 << A7 |
                           1 << A8;
pub const BB_FILE_B: u64 = BB_FILE_A << 1;
pub const BB_FILE_C: u64 = BB_FILE_B << 1;
pub const BB_FILE_D: u64 = BB_FILE_C << 1;
pub const BB_FILE_E: u64 = BB_FILE_D << 1;
pub const BB_FILE_F: u64 = BB_FILE_E << 1;
pub const BB_FILE_G: u64 = BB_FILE_F << 1;
pub const BB_FILE_H: u64 = BB_FILE_G << 1;

pub const PAWN_PROMOTION_RANKS: u64 = BB_RANK_1 | BB_RANK_8;

// The maximum number of moves in the move stack. It should be large
// enough so we that never overrun it.
pub const MOVE_STACK_SIZE: usize = 32 * 256;


#[inline]
pub fn square(file: File, rank: Rank) -> Square {
    assert!(file < 8);
    assert!(rank < 8);
    rank * 8 + file
}

#[inline]
pub fn rank(square: Square) -> Rank {
    square >> 3
}

#[inline]
pub fn file(square: Square) -> File {
    square % 8
}
