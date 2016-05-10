// Basic types
pub type Color = usize; // 0 or 1
pub type File = usize;  // from 0 to 7
pub type Rank = usize;  // from 0 to 7
pub type Square = usize;  // from 0 to 63
pub type PieceType = usize;  // from 0 to 5
pub type MoveType = usize;  // from 0 to 3
pub type CastlingSide = usize;  // 0 or 1
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

// Move types
pub const MOVE_NORMAL: MoveType = 0;
pub const MOVE_ENPASSANT: MoveType = 1;
pub const MOVE_PROMOTION: MoveType = 2;
pub const MOVE_CASTLING: MoveType = 3;

// Castling sides
pub const QUEENSIDE: CastlingSide = 0;
pub const KINGSIDE: CastlingSide = 1;

// Castling rights
pub const CASTLE_WHITE_QUEENSIDE: u8 = 1 << 0;
pub const CASTLE_WHITE_KINGSIDE: u8 = 1 << 1;
pub const CASTLE_BLACK_QUEENSIDE: u8 = 1 << 2;
pub const CASTLE_BLACK_KINGSIDE: u8 = 1 << 3;

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

// The maximum number of moves in the move stack. It should be large
// enough so we that never overrun it.
pub const MOVE_STACK_SIZE: usize = 32 * 256;


#[derive(Debug)]
#[derive(Clone, Copy)]
pub struct CastlingRights(u8);

impl CastlingRights {
    #[inline(always)]
    pub fn new() -> CastlingRights {
        CastlingRights(0)
    }

    pub fn can_castle(&self, color: Color, side: CastlingSide) -> bool {
        self.obstacles(color, side) != UNIVERSAL_SET
    }

    pub fn set(&mut self, mask: u8) -> bool {
        let before = self.0;
        self.0 |= mask;
        self.0 != before
    }

    #[inline(always)]
    pub fn clear(&mut self, mask: u8) {
        self.0 &= !mask;
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
        let obstacles = match side {
            0 => 1 << B1 | 1 << C1 | 1 << D1,
            _ => 1 << F1 | 1 << G1,
        };
        match color {
            WHITE => obstacles,
            _ => obstacles << 56,
        }
    }

    #[inline]
    pub fn rook_mask(&self, color: Color, side: CastlingSide) -> u64 {
        let mask = match side {
            0 => 1 << A1 | 1 << D1,
            1 => 1 << H1 | 1 << G1,
            _ => panic!("invalid castling side"),
        };
        match color {
            WHITE => mask,
            BLACK => mask << 56,
            _ => panic!("invalid color"),
        }
    }
}






#[derive(Debug)]
#[derive(Clone, Copy)]
#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub struct Move(u32);

const M_SHIFT_SCORE: u32 = 22;
const M_SHIFT_ORIG_PIECE: u32 = 19;
const M_SHIFT_DEST_PIECE: u32 = 16;
const M_SHIFT_MOVE_TYPE: u32 = 14;
const M_SHIFT_ORIG_SQUARE: u32 = 8;
const M_SHIFT_DEST_SQUARE: u32 = 2;
const M_SHIFT_AUX_DATA: u32 = 0;

const M_MASK_SCORE: u32 = 0b1111111111 << M_SHIFT_SCORE;
const M_MASK_ORIG_PIECE: u32 = 0b111 << M_SHIFT_ORIG_PIECE;
const M_MASK_DEST_PIECE: u32 = 0b111 << M_SHIFT_DEST_PIECE;
const M_MASK_MOVE_TYPE: u32 = 0b11 << M_SHIFT_MOVE_TYPE;
const M_MASK_ORIG_SQUARE: u32 = 0b111111 << M_SHIFT_ORIG_SQUARE;
const M_MASK_DEST_SQUARE: u32 = 0b111111 << M_SHIFT_DEST_SQUARE;
const M_MASK_AUX_DATA: u32 = 0b11 << M_SHIFT_AUX_DATA;

impl Move {
    #[inline(always)]
    pub fn new(score: usize,
               move_type: MoveType,
               orig_piece: PieceType,
               orig_square: Square,
               dest_piece: PieceType,
               dest_square: Square,
               aux_data: usize)
               -> Move {
        assert!(score <= 0b1111111111);
        assert!(move_type <= 0x11);
        assert!(orig_piece < NO_PIECE);
        assert!(orig_square <= 63);
        assert!(dest_piece != KING && orig_piece <= NO_PIECE);
        assert!(dest_square <= 63);
        assert!(aux_data <= 0b11);
        Move((score << M_SHIFT_SCORE | orig_piece << M_SHIFT_ORIG_PIECE |
              dest_piece << M_SHIFT_DEST_PIECE | move_type << M_SHIFT_MOVE_TYPE |
              orig_square << M_SHIFT_ORIG_SQUARE |
              dest_square << M_SHIFT_DEST_SQUARE |
              aux_data << M_SHIFT_AUX_DATA) as u32)
    }

    #[inline(always)]
    pub fn get_score(&self) -> usize {
        ((self.0 & M_MASK_SCORE) >> M_SHIFT_SCORE) as usize
    }

    #[inline(always)]
    pub fn set_score(&mut self, score: usize) {
        assert!(score <= 0b1111111111);
        self.0 |= (score << M_SHIFT_SCORE) as u32;
    }

    #[inline(always)]
    pub fn set_score_bit(&mut self, b: usize) {
        assert!(b <= 9);
        self.0 |= 1 << b << M_SHIFT_SCORE;
    }

    #[inline(always)]
    pub fn clear_score_bit(&mut self, b: usize) {
        assert!(b <= 9);
        self.0 &= !(1 << b << M_SHIFT_SCORE);
    }

    #[inline(always)]
    pub fn move_type(&self) -> MoveType {
        ((self.0 & M_MASK_MOVE_TYPE) >> M_SHIFT_MOVE_TYPE) as MoveType
    }

    #[inline(always)]
    pub fn orig_piece(&self) -> PieceType {
        ((self.0 & M_MASK_ORIG_PIECE) >> M_SHIFT_ORIG_PIECE) as PieceType
    }

    #[inline(always)]
    pub fn orig_square(&self) -> Square {
        ((self.0 & M_MASK_ORIG_SQUARE) >> M_SHIFT_ORIG_SQUARE) as Square
    }

    #[inline(always)]
    pub fn dest_piece(&self) -> PieceType {
        ((self.0 & M_MASK_DEST_PIECE) >> M_SHIFT_DEST_PIECE) as PieceType
    }

    #[inline(always)]
    pub fn dest_square(&self) -> Square {
        ((self.0 & M_MASK_DEST_SQUARE) >> M_SHIFT_DEST_SQUARE) as Square
    }

    #[inline(always)]
    pub fn aux_data(&self) -> usize {
        ((self.0 & M_MASK_AUX_DATA) >> M_SHIFT_AUX_DATA) as usize
    }

    #[inline(always)]
    pub fn promoted_piece(&self) -> PieceType {
        Move::piece_type_from_aux_data(self.aux_data())
    }

    #[inline(always)]
    fn piece_type_from_aux_data(pp_code: usize) -> PieceType {
        match pp_code {
            0 => QUEEN,
            1 => ROOK,
            2 => BISHOP,
            3 => KNIGHT,
            _ => panic!("invalid promoted piece code"),
        }
    }
}


pub struct MoveStack {
    stack: [Move; MOVE_STACK_SIZE],
    top_index: usize,
}

impl MoveStack {
    pub fn new() -> MoveStack {
        use std::mem::uninitialized;
        unsafe {
            MoveStack {
                stack: uninitialized(),
                top_index: 0,
            }
        }
    }

    #[inline(always)]
    pub fn push(&mut self, m: Move) {
        self.stack[self.top_index] = m;
        self.top_index += 1;
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
