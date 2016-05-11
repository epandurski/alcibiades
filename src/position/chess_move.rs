use basetypes::*;

#[derive(Debug)]
#[derive(Clone, Copy)]
#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub struct Move(u32);

const M_SHIFT_SCORE: u32 = 26;
const M_SHIFT_INV_CAPTURED_PIECE: u32 = 23;
const M_SHIFT_PIECE: u32 = 20;
const M_SHIFT_ENPASSANT_FILE: u32 = 16;
const M_SHIFT_MOVE_TYPE: u32 = 14;
const M_SHIFT_ORIG_SQUARE: u32 = 8;
const M_SHIFT_DEST_SQUARE: u32 = 2;
const M_SHIFT_AUX_DATA: u32 = 0;

const M_MASK_SCORE: u32 = 0b111111 << M_SHIFT_SCORE;
const M_MASK_INV_CAPTURED_PIECE: u32 = 0b111 << M_SHIFT_INV_CAPTURED_PIECE;
const M_MASK_PIECE: u32 = 0b111 << M_SHIFT_PIECE;
const M_MASK_ENPASSANT_FILE: u32 = 0b1111 << M_SHIFT_ENPASSANT_FILE;
const M_MASK_MOVE_TYPE: u32 = 0b11 << M_SHIFT_MOVE_TYPE;
const M_MASK_ORIG_SQUARE: u32 = 0b111111 << M_SHIFT_ORIG_SQUARE;
const M_MASK_DEST_SQUARE: u32 = 0b111111 << M_SHIFT_DEST_SQUARE;
const M_MASK_AUX_DATA: u32 = 0b11 << M_SHIFT_AUX_DATA;

impl Move {
    #[inline(always)]
    pub fn new(score: usize,
               move_type: MoveType,
               piece: PieceType,
               orig_square: Square,
               dest_square: Square,
               captured_piece: PieceType,
               en_passant_file: File,
               aux_data: usize)
               -> Move {
        assert!(score <= 0b111111);
        assert!(move_type <= 0x11);
        assert!(piece < NO_PIECE);
        assert!(orig_square <= 63);
        assert!(dest_square <= 63);
        assert!(captured_piece != KING && captured_piece <= NO_PIECE);
        assert!(en_passant_file <= 0b1000);
        assert!(aux_data <= 0b11);
        Move((score << M_SHIFT_SCORE | (!captured_piece & 0b111) << M_SHIFT_INV_CAPTURED_PIECE |
              piece << M_SHIFT_PIECE | en_passant_file << M_SHIFT_ENPASSANT_FILE |
              move_type << M_SHIFT_MOVE_TYPE | orig_square << M_SHIFT_ORIG_SQUARE |
              dest_square << M_SHIFT_DEST_SQUARE |
              aux_data << M_SHIFT_AUX_DATA) as u32)
    }

    #[inline(always)]
    pub fn get_score(&self) -> usize {
        ((self.0 & M_MASK_SCORE) >> M_SHIFT_SCORE) as usize
    }

    #[inline(always)]
    pub fn set_score(&mut self, score: usize) {
        assert!(score <= 0b111111);
        self.0 |= (score << M_SHIFT_SCORE) as u32;
    }

    #[inline(always)]
    pub fn set_score_bit(&mut self, b: usize) {
        assert!(b <= 5);
        self.0 |= 1 << b << M_SHIFT_SCORE;
    }

    #[inline(always)]
    pub fn clear_score_bit(&mut self, b: usize) {
        assert!(b <= 5);
        self.0 &= !(1 << b << M_SHIFT_SCORE);
    }

    #[inline(always)]
    pub fn move_type(&self) -> MoveType {
        ((self.0 & M_MASK_MOVE_TYPE) >> M_SHIFT_MOVE_TYPE) as MoveType
    }

    #[inline(always)]
    pub fn piece(&self) -> PieceType {
        ((self.0 & M_MASK_PIECE) >> M_SHIFT_PIECE) as PieceType
    }

    #[inline(always)]
    pub fn orig_square(&self) -> Square {
        ((self.0 & M_MASK_ORIG_SQUARE) >> M_SHIFT_ORIG_SQUARE) as Square
    }

    #[inline(always)]
    pub fn dest_square(&self) -> Square {
        ((self.0 & M_MASK_DEST_SQUARE) >> M_SHIFT_DEST_SQUARE) as Square
    }

    #[inline(always)]
    pub fn captured_piece(&self) -> PieceType {
        ((!self.0 & M_MASK_INV_CAPTURED_PIECE) >> M_SHIFT_INV_CAPTURED_PIECE) as PieceType
    }

    #[inline(always)]
    pub fn en_passant_file(&self) -> File {
        ((self.0 & M_MASK_ENPASSANT_FILE) >> M_SHIFT_ENPASSANT_FILE) as File
    }

    #[inline(always)]
    pub fn aux_data(&self) -> usize {
        ((self.0 & M_MASK_AUX_DATA) >> M_SHIFT_AUX_DATA) as usize
    }

    #[inline(always)]
    pub fn promoted_piece(&self) -> PieceType {
        Move::piece_from_aux_data(self.aux_data())
    }

    #[inline(always)]
    pub fn piece_from_aux_data(pp_code: usize) -> PieceType {
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


