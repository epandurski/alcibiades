//! Defines data structures describing chess moves.

use std::fmt;
use board::*;


/// `MOVE_ENPASSANT`, `MOVE_PROMOTION`, `MOVE_CASTLING`, or `MOVE_NORMAL`.
pub type MoveType = usize;

pub const MOVE_ENPASSANT: MoveType = 0;
pub const MOVE_PROMOTION: MoveType = 1;
pub const MOVE_CASTLING: MoveType = 2;
pub const MOVE_NORMAL: MoveType = 3;


/// Encodes the minimum needed information that unambiguously
/// describes a move.
///
/// `MoveDigest` is a `u16` number. It is laid out the following way:
///
///  ```text
///   15                                                           0
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
///  | Move  |    Origin square      |   Destination square  | Aux   |
///  | type  |       6 bits          |        6 bits         | data  |
///  | 2 bits|   |   |   |   |   |   |   |   |   |   |   |   | 2 bits|
///  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  ```
///  
/// There are 4 "move type"s: `0`) en-passant capture; `1`) pawn
/// promotion; `2`) castling; `3`) normal move. "Aux data" encodes the
/// type of the promoted piece if the move type is pawn promotion,
/// otherwise it is zero.
#[derive(Debug)]
#[derive(Clone, Copy)]
#[derive(PartialEq, Eq)]
pub struct MoveDigest(u16);

impl MoveDigest {
    /// Creates an invalid move digest instance.
    ///
    /// The returned instance contains `0`. This is sometimes useful
    /// in places where any move is required but no is available.
    #[inline]
    pub fn invalid() -> MoveDigest {
        MoveDigest(0)
    }

    /// Returns the move type.
    #[inline]
    pub fn move_type(&self) -> MoveType {
        ((self.0 & M_MASK_MOVE_TYPE as u16) >> M_SHIFT_MOVE_TYPE) as MoveType
    }

    /// Returns the origin square of the played piece.
    #[inline]
    pub fn orig_square(&self) -> Square {
        ((self.0 & M_MASK_ORIG_SQUARE as u16) >> M_SHIFT_ORIG_SQUARE) as Square
    }

    /// Returns the destination square for the played piece.
    #[inline]
    pub fn dest_square(&self) -> Square {
        ((self.0 & M_MASK_DEST_SQUARE as u16) >> M_SHIFT_DEST_SQUARE) as Square
    }

    /// Returns a value between 0 and 3 representing the auxiliary
    /// data.
    ///
    /// When the move type is pawn promotion, "aux data" encodes the
    /// promoted piece type. For all other move types "aux data" is
    /// zero.
    #[inline]
    pub fn aux_data(&self) -> usize {
        ((self.0 & M_MASK_AUX_DATA as u16) >> M_SHIFT_AUX_DATA) as usize
    }
}


/// Represents a move on the chessboard.
///
/// `Move` is a `u64` number. It contains 3 types of information:
///
/// 1. Information about the played move itself.
///
/// 2. Information needed so as to be able to undo the move and
///    restore the board into the exact same state as before.
///
/// 3. Move ordering info -- moves with higher move score are tried
///    first.
///
/// Bits 0-15 contain the whole information about the move
/// itself. This is called **"move digest"** and is laid out the
/// following way:
///
///  ```text
///   15                                                           0
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
///  | Move  |    Origin square      |   Destination square  | Aux   |
///  | type  |       6 bits          |        6 bits         | data  |
///  | 2 bits|   |   |   |   |   |   |   |   |   |   |   |   | 2 bits|
///  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  ```
///
/// There are 4 "move type"s: `0`) en-passant capture; `1`) pawn
/// promotion; `2`) castling; `3`) normal move. "Aux data" encodes the
/// type of the promoted piece if the move type is pawn promotion,
/// otherwise it is zero.
///
/// Bits 16-31 contain the information needed to undo the move:
///
///  ```text
///   31                                                          16
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
///  |   |   |  Captured |  Played   |   Castling    |   En-passant  |
///  | 0 | 0 |  piece    |  piece    |    rights     |      file     |
///  |   |   |  3 bits   |  3 bits   |    4 bits     |     4 bits    |
///  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  ```
///
/// If the *previous move* was a double pawn push, "en-passant file"
/// contains pushed pawn's file (a value between 0 and 7). Otherwise
/// it contains `8`. "Castling rights" holds the castling rights
/// before the move was played. When "Captured piece" is stored, its
/// bits are inverted, so that MVV-LVA (Most valuable victim -- least
/// valuable aggressor) move ordering is followed for moves that have
/// the same "score".
///
/// Bits 32-63 contain the "score" field, which is used to influence
/// move ordering.
#[derive(Debug)]
#[derive(Clone, Copy)]
#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub struct Move(u64);

impl Move {
    /// Creates a new instance.
    #[inline(always)]
    pub fn new(move_type: MoveType,
               orig_square: Square,
               dest_square: Square,
               aux_data: usize,
               captured_piece: PieceType,
               played_piece: PieceType,
               castling_rights: CastlingRights,
               enpassant_file: usize,
               score: u32)
               -> Move {
        debug_assert!(move_type <= 0x11);
        debug_assert!(played_piece < PIECE_NONE);
        debug_assert!(orig_square <= 63);
        debug_assert!(dest_square <= 63);
        debug_assert!(captured_piece != KING && captured_piece <= PIECE_NONE);
        debug_assert!(enpassant_file <= 8);
        debug_assert!(aux_data <= 0b11);
        debug_assert!(move_type == MOVE_PROMOTION || aux_data == 0);
        debug_assert!(orig_square != dest_square ||
                      move_type == MOVE_NORMAL && captured_piece == PIECE_NONE);

        Move(// The order of those operations could be important to
             // allow more optimizations.
             //
             // Most probably constants:
             (score as u64) << M_SHIFT_SCORE |
             (move_type << M_SHIFT_MOVE_TYPE | aux_data << M_SHIFT_AUX_DATA) as u64 |
             (
                 // Values that most probably *do not* change in cycle:
                 castling_rights.value() << M_SHIFT_CASTLING_RIGHTS |
                 enpassant_file << M_SHIFT_ENPASSANT_FILE |
                 played_piece << M_SHIFT_PIECE |
                 orig_square << M_SHIFT_ORIG_SQUARE |

                 // Values that most probably *do* change in cycle:
                 (!captured_piece & 0b111) << M_SHIFT_CAPTURED_PIECE |
                 dest_square << M_SHIFT_DEST_SQUARE) as u64)
    }

    /// Creates an invalid move instance.
    ///
    /// The returned instance tries to mimic a legal move, but its
    /// move digest equals `MoveDigest::invalid()`. This is sometimes
    /// useful in places where any move is required but no is
    /// available.
    #[inline]
    pub fn invalid() -> Move {
        Move(((!PIECE_NONE & 0b111) << M_SHIFT_CAPTURED_PIECE | KING << M_SHIFT_PIECE) as u64)
    }

    /// Decodes the promoted piece type from the raw value returned by
    /// `aux_data`.
    ///
    /// The interpretation of the raw value is: `0` -- queen, `1` --
    /// rook, `2` -- bishop, `3` -- knight.
    #[inline]
    pub fn piece_from_aux_data(pp_code: usize) -> PieceType {
        debug_assert!(pp_code <= 3);
        QUEEN + pp_code
    }

    /// Assigns a new score for the move.
    #[inline]
    pub fn set_score(&mut self, score: u32) {
        self.0 &= !M_MASK_SCORE;
        self.0 |= (score as u64) << M_SHIFT_SCORE;
    }

    /// Returns the assigned move score.
    #[inline]
    pub fn score(&self) -> u32 {
        (self.0 >> M_SHIFT_SCORE) as u32
    }

    /// Returns the move type.
    #[inline]
    pub fn move_type(&self) -> MoveType {
        (self.0 as usize & M_MASK_MOVE_TYPE) >> M_SHIFT_MOVE_TYPE
    }

    /// Returns the played piece type.
    ///
    /// Castling is considered as king's move.
    #[inline]
    pub fn played_piece(&self) -> PieceType {
        (self.0 as usize & M_MASK_PIECE) >> M_SHIFT_PIECE
    }

    /// Returns the origin square of the played piece.
    #[inline]
    pub fn orig_square(&self) -> Square {
        (self.0 as usize & M_MASK_ORIG_SQUARE) >> M_SHIFT_ORIG_SQUARE
    }

    /// Returns the destination square for the played piece.
    #[inline]
    pub fn dest_square(&self) -> Square {
        (self.0 as usize & M_MASK_DEST_SQUARE) >> M_SHIFT_DEST_SQUARE
    }

    /// Returns the captured piece type.
    #[inline]
    pub fn captured_piece(&self) -> PieceType {
        (!(self.0 as usize) & M_MASK_CAPTURED_PIECE) >> M_SHIFT_CAPTURED_PIECE
    }

    /// If the *previous move* was a double pawn push, returns pushed
    /// pawn's file (a value between 0 and 7). Otherwise returns `8`.
    #[inline]
    pub fn enpassant_file(&self) -> usize {
        (self.0 as usize & M_MASK_ENPASSANT_FILE) >> M_SHIFT_ENPASSANT_FILE
    }

    /// Returns the castling rights as they were before the move was
    /// played.
    #[inline]
    pub fn castling_rights(&self) -> CastlingRights {
        CastlingRights::new(self.0 as usize >> M_SHIFT_CASTLING_RIGHTS)
    }

    /// Returns a value between 0 and 3 representing the auxiliary
    /// data.
    ///
    /// When the move type is pawn promotion, "aux data" encodes the
    /// promoted piece type. For all other move types "aux data" is
    /// zero.
    #[inline]
    pub fn aux_data(&self) -> usize {
        (self.0 as usize & M_MASK_AUX_DATA) >> M_SHIFT_AUX_DATA
    }

    /// Returns the least significant 16 bits of the raw move value.
    #[inline]
    pub fn digest(&self) -> MoveDigest {
        MoveDigest(self.0 as u16)
    }

    /// Returns the algebraic notation of the move.
    ///
    /// Examples: `e2e4`, `e7e5`, `e1g1` (white short castling),
    /// `e7e8q` (for promotion).
    pub fn notation(&self) -> String {
        format!("{}{}{}",
                notation(self.orig_square()),
                notation(self.dest_square()),
                match self.move_type() {
                    MOVE_PROMOTION => ["q", "r", "b", "n"][self.aux_data()],
                    _ => "",
                })
    }

    /// Returns `true` if the move is a pawn advance or a capture,
    /// `false` otherwise.
    #[inline]
    pub fn is_pawn_advance_or_capure(&self) -> bool {
        // We use clever bit manipulations to avoid branches.
        const P: usize = (!PAWN & 0b111) << M_SHIFT_PIECE;
        const C: usize = (!PIECE_NONE & 0b111) << M_SHIFT_CAPTURED_PIECE;
        let v = self.0 as usize;
        (v & M_MASK_PIECE | C) ^ (v & M_MASK_CAPTURED_PIECE | P) >= M_MASK_PIECE
    }

    /// Returns if the move is a null move.
    ///
    /// "Null move" is a pseudo-move that changes nothing on the board
    /// except the side to move. It is sometimes useful to include a
    /// speculative null move in the search tree to achieve more
    /// aggressive pruning. Null moves are represented as king's moves
    /// for which the origin and destination squares are the same.
    #[inline]
    pub fn is_null(&self) -> bool {
        debug_assert!(self.orig_square() != self.dest_square() || self.played_piece() == KING);
        self.orig_square() == self.dest_square() && self.move_type() == MOVE_NORMAL
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.notation())
    }
}


/// A trait for adding moves to move containers.
pub trait AddMove {
    /// Adds a move to the move container.
    fn add_move(&mut self, m: Move);
}

impl AddMove for Vec<Move> {
    #[inline]
    fn add_move(&mut self, m: Move) {
        self.push(m);
    }
}


// Field shifts
const M_SHIFT_SCORE: usize = 32;
const M_SHIFT_CAPTURED_PIECE: usize = 27;
const M_SHIFT_PIECE: usize = 24;
const M_SHIFT_CASTLING_RIGHTS: usize = 20;
const M_SHIFT_ENPASSANT_FILE: usize = 16;
const M_SHIFT_MOVE_TYPE: usize = 14;
const M_SHIFT_ORIG_SQUARE: usize = 8;
const M_SHIFT_DEST_SQUARE: usize = 2;
const M_SHIFT_AUX_DATA: usize = 0;


// Field masks
const M_MASK_CAPTURED_PIECE: usize = 0b111 << M_SHIFT_CAPTURED_PIECE;
const M_MASK_PIECE: usize = 0b111 << M_SHIFT_PIECE;
const M_MASK_ENPASSANT_FILE: usize = 0b1111 << M_SHIFT_ENPASSANT_FILE;
const M_MASK_MOVE_TYPE: usize = 0b11 << M_SHIFT_MOVE_TYPE;
const M_MASK_ORIG_SQUARE: usize = 0b111111 << M_SHIFT_ORIG_SQUARE;
const M_MASK_DEST_SQUARE: usize = 0b111111 << M_SHIFT_DEST_SQUARE;
const M_MASK_AUX_DATA: usize = 0b11 << M_SHIFT_AUX_DATA;
const M_MASK_SCORE: u64 = (::std::u32::MAX as u64) << M_SHIFT_SCORE;


/// Returns the algebraic notation for a given square.
fn notation(square: Square) -> &'static str {
    lazy_static! {
        static ref NOTATION: Vec<String> = (0..64).map(|i| format!("{}{}",
            ["a", "b", "c", "d", "e", "f", "g", "h"][Board::file(i)],
            ["1", "2", "3", "4", "5", "6", "7", "8"][Board::rank(i)])
        ).collect();
    }
    NOTATION[square].as_str()
}


#[cfg(test)]
mod tests {
    use super::*;
    use board::*;
    use squares::*;

    #[test]
    fn moves() {
        let cr = CastlingRights::new(0b1011);
        let mut m = Move::new(MOVE_NORMAL, E2, E4, 0, PIECE_NONE, PAWN, cr, 8, 0);
        let m1 = Move::new(MOVE_NORMAL, F3, E4, 0, KNIGHT, PAWN, cr, 8, ::std::u32::MAX);
        let m2 = Move::new(MOVE_NORMAL, F3, E4, 0, PIECE_NONE, KING, cr, 8, 0);
        let m3 = Move::new(MOVE_PROMOTION, F2, F1, 1, PIECE_NONE, PAWN, cr, 8, 0);
        let m4 = Move::new(MOVE_NORMAL, F2, E3, 0, KNIGHT, BISHOP, cr, 8, 0);
        let m5 = Move::new(MOVE_NORMAL, F2, F2, 0, PIECE_NONE, KING, cr, 8, 0);
        assert!(m1 > m);
        assert!(m2 < m);
        assert_eq!(m.move_type(), MOVE_NORMAL);
        assert_eq!(m.played_piece(), PAWN);
        assert_eq!(m.captured_piece(), PIECE_NONE);
        assert_eq!(m.orig_square(), E2);
        assert_eq!(m.dest_square(), E4);
        assert_eq!(m.enpassant_file(), 8);
        assert_eq!(m.aux_data(), 0);
        assert_eq!(m.castling_rights().value(), 0b1011);
        assert_eq!(m.notation(), "e2e4");
        assert!(!m.is_null());
        assert_eq!(m3.aux_data(), 1);
        assert_eq!(Move::piece_from_aux_data(0), QUEEN);
        assert_eq!(Move::piece_from_aux_data(1), ROOK);
        assert_eq!(Move::piece_from_aux_data(2), BISHOP);
        assert_eq!(Move::piece_from_aux_data(3), KNIGHT);
        let m_copy = m;
        assert_eq!(m, m_copy);
        assert_eq!(m.score(), 0);
        m.set_score(::std::u32::MAX);
        assert_eq!(m.score(), ::std::u32::MAX);
        assert!(m > m_copy);
        m.set_score(0);
        assert_eq!(m.score(), 0);
        assert!(m.is_pawn_advance_or_capure());
        assert!(m1.is_pawn_advance_or_capure());
        assert!(!m2.is_pawn_advance_or_capure());
        assert!(m3.is_pawn_advance_or_capure());
        assert!(m4.is_pawn_advance_or_capure());
        assert!(!m5.is_pawn_advance_or_capure());
        assert!(m5.is_null());
        assert!(MOVE_NORMAL != 0);
        assert!(!Move::invalid().is_null());
        assert_eq!(Move::invalid().digest(), MoveDigest::invalid());
        assert_eq!(m.digest().move_type(), m.move_type());
        assert_eq!(m.digest().orig_square(), m.orig_square());
        assert_eq!(m.digest().dest_square(), m.dest_square());
        assert_eq!(m.digest().aux_data(), m.aux_data());
    }
}
