//! Defines data structures related to chess moves.

use std::slice;
use basetypes::*;
use castling_rights::*;


/// Represents a move on the chessboard.
///
/// `Move` contains 3 types of information:
///
/// 1. Information about the played move itself.
///
/// 2. Information needed so as to be able to undo the move and
///    restore the board into the exact same state as before.
///
/// 3. The move score -- moves with higher score are tried
///    first. Ideally the best move should have the highest score.
///
/// `Move` is a 32-bit unsigned number. The lowest 16 bits contain the
/// whole needed information about the move itself (type 1). And is
/// laid out the following way:
///
///  ```text
///   15                                                           0
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
///  | Move  |    Origin square      |   Destination square  | Aux   |
///  | type  |       6 bits          |        6 bits         | data  |
///  | 2 bits|   |   |   |   |   |   |   |   |   |   |   |   | 2 bits|
///  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |       |
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  ```
///
/// There are 4 "move type"s: `0`) en-passant capture; `1`) pawn
/// promotion; `2`) castling; `3`) normal move. "Aux data" encodes the
/// type of the promoted piece if the move type is a pawn promotion,
/// otherwise it encodes castling rights (see below).
///
/// The highest 16 bits contain the rest ot the info:
///
///  ```text
///   31                                                          16
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
///  | Move  |  Captured | Reser-|  Played   | Cast- |   En-passant  |
///  | score |  piece    |  ved  |  piece    | ling  |      file     |
///  | 2 bits|  3 bits   | 2 bits|  3 bits   | 2 bits|     4 bits    |
///  |   |   |   |   |   |   |   |   |   |   |       |   |   |   |   |
///  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///  ```
///
/// "En-passant file" tells on what vertical line on the board there
/// was a passing pawn before the move was played. If there was no
/// passing pawn, "en-passant file" should be `8`.
///
/// Castling rights are a bit complex. The castling rights for the side
/// that makes the move, before the move was made, are stored in the
/// "Aux data" field. This is OK, because promoting a pawn never
/// changes the moving player's castling rights. The castling rights
/// for the opposite side are stored in "Castling" field. (A move can
/// change the castling rights for the other side when a rook in the
/// corner is captured.)
///
/// When "Captured piece" is stored, its bits are inverted, so that
/// MVV-LVA (Most valuable victim -- least valuable aggressor)
/// ordering of the moves is preserved, even when the "Move score"
/// field stays the same. The "Reserved" field is used to improve the
/// ordering of non-capturing moves.
#[derive(Debug)]
#[derive(Clone, Copy)]
#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub struct Move(u32);


impl Move {
    /// Creates a new instance of `Move`.
    ///
    /// `us` is the side that makes the move. `score` is the assigned
    /// move score (between 0 and 3). `castling` are the castling
    /// rights before the move was played. `en_passant_file` is the
    /// file on which there were a passing pawn before the move was
    /// played (or `8` if there was no passing
    /// pawn). `promoted_piece_code` should be a number between `0`
    /// and `3` and is used only when the `move_type` is a pawn
    /// promotion, otherwise it is ignored.
    #[inline(always)]
    pub fn new(us: Color,
               score: usize,
               move_type: MoveType,
               piece: PieceType,
               orig_square: Square,
               dest_square: Square,
               captured_piece: PieceType,
               en_passant_file: File,
               castling: CastlingRights,
               promoted_piece_code: usize)
               -> Move {
        assert!(us <= 1);
        assert!(score <= 0b11);
        assert!(move_type <= 0x11);
        assert!(piece < NO_PIECE);
        assert!(orig_square <= 63);
        assert!(dest_square <= 63);
        assert!(captured_piece != KING && captured_piece <= NO_PIECE);
        assert!(en_passant_file <= 0b1000);
        assert!(promoted_piece_code <= 0b11);

        // We use the reserved field (2 bits) to properly order
        // "quiet" movies. Moves which destination square is closer to
        // the central ranks are tried first.
        const RESERVED_LOOKUP: [usize; 8] = [0 << M_SHIFT_RESERVED,
                                             1 << M_SHIFT_RESERVED,
                                             1 << M_SHIFT_RESERVED,
                                             2 << M_SHIFT_RESERVED,
                                             2 << M_SHIFT_RESERVED,
                                             1 << M_SHIFT_RESERVED,
                                             1 << M_SHIFT_RESERVED,
                                             0 << M_SHIFT_RESERVED];
        let reserved_shifted = if captured_piece == NO_PIECE {
            unsafe { *RESERVED_LOOKUP.get_unchecked(rank(dest_square)) }
        } else {
            0
        };

        let aux_data = if move_type == MOVE_PROMOTION {
            promoted_piece_code
        } else {
            castling.get_for(us)
        };
        Move((score << M_SHIFT_SCORE | (!captured_piece & 0b111) << M_SHIFT_CAPTURED_PIECE |
              reserved_shifted | piece << M_SHIFT_PIECE |
              castling.get_for(1 ^ us) << M_SHIFT_CASTLING_DATA |
              en_passant_file << M_SHIFT_ENPASSANT_FILE |
              move_type << M_SHIFT_MOVE_TYPE | orig_square << M_SHIFT_ORIG_SQUARE |
              dest_square << M_SHIFT_DEST_SQUARE |
              aux_data << M_SHIFT_AUX_DATA) as u32)
    }

    /// Creates a new instance of `Move` from a raw `u32` value.
    #[inline]
    pub fn from_u32(value: u32) -> Move {
        Move(value)
    }

    /// Assigns a new score for the move (between 0 and 3).
    #[inline]
    pub fn set_score(&mut self, score: usize) {
        assert!(score <= 0b11);
        self.0 &= !M_MASK_SCORE;
        self.0 |= (score << M_SHIFT_SCORE) as u32;
    }

    /// Returns the assigned move score.
    #[inline]
    pub fn score(&self) -> usize {
        ((self.0 & M_MASK_SCORE) >> M_SHIFT_SCORE) as usize
    }

    /// Returns the move type.
    #[inline]
    pub fn move_type(&self) -> MoveType {
        ((self.0 & M_MASK_MOVE_TYPE) >> M_SHIFT_MOVE_TYPE) as MoveType
    }

    /// Returns the played piece type.
    ///
    /// Castling is considered as a king's move.
    #[inline]
    pub fn piece(&self) -> PieceType {
        ((self.0 & M_MASK_PIECE) >> M_SHIFT_PIECE) as PieceType
    }

    /// Returns the origin square of the played piece.
    #[inline]
    pub fn orig_square(&self) -> Square {
        ((self.0 & M_MASK_ORIG_SQUARE) >> M_SHIFT_ORIG_SQUARE) as Square
    }

    /// Returns the destination square for the played piece.
    #[inline]
    pub fn dest_square(&self) -> Square {
        ((self.0 & M_MASK_DEST_SQUARE) >> M_SHIFT_DEST_SQUARE) as Square
    }

    /// Returns the captured piece type.
    #[inline]
    pub fn captured_piece(&self) -> PieceType {
        ((!self.0 & M_MASK_CAPTURED_PIECE) >> M_SHIFT_CAPTURED_PIECE) as PieceType
    }

    /// Returns the file on which there were a passing pawn before the
    /// move was played (or `8` if there was no passing pawn).
    #[inline]
    pub fn en_passant_file(&self) -> File {
        ((self.0 & M_MASK_ENPASSANT_FILE) >> M_SHIFT_ENPASSANT_FILE) as File
    }

    /// Returns a value between 0 and 3 representing the castling
    /// rights for the side that does not make the move, as they were
    /// before the move was played.
    #[inline(always)]
    pub fn castling_data(&self) -> usize {
        ((self.0 & M_MASK_CASTLING_DATA) >> M_SHIFT_CASTLING_DATA) as usize
    }

    /// Returns a value between 0 and 3 representing the auxiliary
    /// data.
    ///
    /// When the move type is pawn promotion, "aux data" encodes the
    /// promoted piece type. For all other move types "aux data"
    /// represents the castling rights for the side that makes the
    /// move, as they were before the move was played.
    #[inline(always)]
    pub fn aux_data(&self) -> usize {
        ((self.0 & M_MASK_AUX_DATA) >> M_SHIFT_AUX_DATA) as usize
    }

    /// Returns a value between 0 and 3 representing the reserved
    /// field.
    #[inline]
    pub fn reserved(&self) -> usize {
        ((self.0 & M_MASK_RESERVED) >> M_SHIFT_RESERVED) as usize
    }

    /// Sets the value of the reserved field to `3` ("killer" flag).
    ///
    /// A "killer" move is a quiet move which caused a beta-cutoff in
    /// a sibling node, or any other earlier branch in the tree with
    /// the same distance to the root. Killer moves are sorted
    /// directly after all captures.
    #[inline]
    pub fn set_killer_flag(&mut self) {
        self.0 |= M_MASK_RESERVED;
    }

    /// Returns `true` if the move is a pawn advance or a capture,
    /// `false` otherwise.
    #[inline]
    pub fn is_pawn_advance_or_capure(&self) -> bool {
        // We use clever bit manipulations to avoid branches.
        const P: u32 = (!(PAWN as u32) & 0b111) << M_SHIFT_PIECE;
        const C: u32 = (!(NO_PIECE as u32) & 0b111) << M_SHIFT_CAPTURED_PIECE;
        (self.0 & M_MASK_PIECE | C) ^ (self.0 & M_MASK_CAPTURED_PIECE | P) >= M_MASK_PIECE
    }

    /// Returns the least significant 16 bits of the raw move value.
    ///
    /// The returned value contains the whole information about the
    /// played move itself. The only missing information is the move
    /// ordering information and the information stored so as to be
    /// able undo the move.
    #[inline(always)]
    pub fn move16(&self) -> u16 {
        self.0 as u16
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

    /// Decodes the promoted piece type from the raw value returned by
    /// `m.aux_data()`.
    ///
    /// The interpretation of the raw value is: `0` -- queen, `1` --
    /// rook, `2` -- bishop, `3` -- knight.
    #[inline]
    pub fn piece_from_aux_data(pp_code: usize) -> PieceType {
        assert!(pp_code <= 3);
        match pp_code {
            0 => QUEEN,
            1 => ROOK,
            2 => BISHOP,
            _ => KNIGHT,
        }
    }
}


/// Consumer of moves.
pub trait MoveSink {
    fn push_move(&mut self, m: Move);
}


impl MoveSink for Vec<Move> {
    #[inline]
    fn push_move(&mut self, m: Move) {
        self.push(m);
    }
}


/// Stores a list of moves for each position in a given line of play.
pub struct MoveStack {
    moves: Vec<Move>,
    savepoints: Vec<(usize, usize)>,
    first_move_index: usize,
}


impl MoveStack {
    /// Creates a new (empty) instance.
    #[inline]
    pub fn new() -> MoveStack {
        MoveStack {
            moves: Vec::with_capacity(32 * 64),
            savepoints: Vec::with_capacity(32),
            first_move_index: 0,
        }
    }

    /// Clears the stack of states, removing all saved move lists.
    #[inline]
    pub fn clear(&mut self) {
        self.moves.clear();
        self.savepoints.clear();
        self.first_move_index = 0;
    }

    /// Clears the current move list, saving it so that it can be
    /// restored.
    ///
    /// This method can be called many times. At each call the current
    /// move list will be cleared, and saved to the stack of states
    /// that can later be restored.
    #[inline]
    pub fn save(&mut self) {
        self.savepoints.push((self.first_move_index, self.moves.len()));
        self.first_move_index = self.moves.len();
    }

    /// Restores the last saved move list.
    ///
    /// The current move list is permanently lost.
    #[inline]
    pub fn restore(&mut self) {
        let savepoint = self.savepoints.pop().unwrap();
        self.first_move_index = savepoint.0;
        self.moves.truncate(savepoint.1);
    }

    /// Returns an iterator that allows modifying each move in the
    /// current move list.
    #[inline]
    pub fn iter_mut(&mut self) -> slice::IterMut<Move> {
        self.moves[self.first_move_index..].iter_mut()
    }

    /// Returns the move with the best score and removes it from the
    /// current move list.
    #[inline]
    pub fn remove_best_move(&mut self) -> Option<Move> {
        let moves = &mut self.moves;
        let i = self.first_move_index;
        let mut m = match moves.get(i) {
            Some(x) => *x,
            None => return None,
        };
        unsafe {
            let mut j = i;
            while j < moves.len() {
                // The current best move candidate is moved to index
                // `i` (swapped with the previous candidate).
                if *moves.get_unchecked(j) > m {
                    *moves.get_unchecked_mut(i) = *moves.get_unchecked_mut(j);
                    *moves.get_unchecked_mut(j) = m;
                    m = *moves.get_unchecked(i);
                }
                j += 1;
            }
            self.first_move_index += 1;
            Some(m)
        }
    }
}


impl MoveSink for MoveStack {
    #[inline]
    fn push_move(&mut self, m: Move) {
        self.moves.push(m);
    }
}


/// `MOVE_ENPASSANT`, `MOVE_PROMOTION`, `MOVE_CASTLING`, or
/// `MOVE_NORMAL`.
pub type MoveType = usize;

/// En-passant capture move type.
pub const MOVE_ENPASSANT: MoveType = 0;

/// Pawn promotion move type.
pub const MOVE_PROMOTION: MoveType = 1;

/// Castling move type.
pub const MOVE_CASTLING: MoveType = 2;

/// Normal move type.
pub const MOVE_NORMAL: MoveType = 3;


// Field shifts
const M_SHIFT_SCORE: u32 = 30;
const M_SHIFT_CAPTURED_PIECE: u32 = 27;
const M_SHIFT_RESERVED: u32 = 25;
const M_SHIFT_PIECE: u32 = 22;
const M_SHIFT_CASTLING_DATA: u32 = 20;
const M_SHIFT_ENPASSANT_FILE: u32 = 16;
const M_SHIFT_MOVE_TYPE: u32 = 14;
const M_SHIFT_ORIG_SQUARE: u32 = 8;
const M_SHIFT_DEST_SQUARE: u32 = 2;
const M_SHIFT_AUX_DATA: u32 = 0;

// Field masks
const M_MASK_SCORE: u32 = 0b11 << M_SHIFT_SCORE;
const M_MASK_CAPTURED_PIECE: u32 = 0b111 << M_SHIFT_CAPTURED_PIECE;
const M_MASK_RESERVED: u32 = 0b11 << M_SHIFT_RESERVED;
const M_MASK_PIECE: u32 = 0b111 << M_SHIFT_PIECE;
const M_MASK_CASTLING_DATA: u32 = 0b11 << M_SHIFT_CASTLING_DATA;
const M_MASK_ENPASSANT_FILE: u32 = 0b1111 << M_SHIFT_ENPASSANT_FILE;
const M_MASK_MOVE_TYPE: u32 = 0b11 << M_SHIFT_MOVE_TYPE;
const M_MASK_ORIG_SQUARE: u32 = 0b111111 << M_SHIFT_ORIG_SQUARE;
const M_MASK_DEST_SQUARE: u32 = 0b111111 << M_SHIFT_DEST_SQUARE;
const M_MASK_AUX_DATA: u32 = 0b11 << M_SHIFT_AUX_DATA;


#[cfg(test)]
mod tests {
    use super::*;
    use basetypes::*;
    use castling_rights::CastlingRights;
    const NO_ENPASSANT_FILE: File = 8;

    #[test]
    fn test_move() {

        let mut cr = CastlingRights::new();
        cr.set_for(WHITE, 0b10);
        cr.set_for(BLACK, 0b11);
        let mut m = Move::new(WHITE,
                              2,
                              MOVE_NORMAL,
                              PAWN,
                              E2,
                              E4,
                              NO_PIECE,
                              NO_ENPASSANT_FILE,
                              cr,
                              0);
        let mut n1 = Move::new(WHITE,
                               2,
                               MOVE_NORMAL,
                               PAWN,
                               F3,
                               E4,
                               KNIGHT,
                               NO_ENPASSANT_FILE,
                               CastlingRights::new(),
                               0);
        let n2 = Move::new(WHITE,
                           2,
                           MOVE_NORMAL,
                           KING,
                           F3,
                           E4,
                           NO_PIECE,
                           NO_ENPASSANT_FILE,
                           CastlingRights::new(),
                           0);
        let n3 = Move::new(BLACK,
                           0,
                           MOVE_PROMOTION,
                           PAWN,
                           F2,
                           F1,
                           NO_PIECE,
                           NO_ENPASSANT_FILE,
                           CastlingRights::new(),
                           1);
        let n4 = Move::new(WHITE,
                           0,
                           MOVE_NORMAL,
                           BISHOP,
                           F2,
                           E3,
                           KNIGHT,
                           NO_ENPASSANT_FILE,
                           CastlingRights::new(),
                           0);
        let n5 = Move::new(WHITE,
                           0,
                           MOVE_NORMAL,
                           PAWN,
                           F2,
                           E3,
                           KNIGHT,
                           NO_ENPASSANT_FILE,
                           CastlingRights::new(),
                           0);
        assert!(n1 > m);
        assert!(n2 < m);
        assert_eq!(m.score(), 2);
        assert_eq!(m.piece(), PAWN);
        assert_eq!(m.captured_piece(), NO_PIECE);
        assert_eq!(m.orig_square(), E2);
        assert_eq!(m.dest_square(), E4);
        assert_eq!(m.en_passant_file(), 8);
        assert_eq!(m.aux_data(), 0b10);
        assert_eq!(m.castling_data(), 0b11);
        let m2 = m;
        assert_eq!(m, m2);
        m.set_score(3);
        assert_eq!(m.score(), 3);
        assert!(m > m2);
        assert_eq!(m.score(), 3);
        m.set_score(0);
        assert_eq!(m.score(), 0);
        assert_eq!(n3.aux_data(), 1);
        assert_eq!(n1.reserved(), 0);
        n1.set_killer_flag();
        assert_eq!(n1.reserved(), 3);
        assert_eq!(n1.move16(), (n1.0 & 0xffff) as u16);
        assert!(m.is_pawn_advance_or_capure());
        assert!(!n2.is_pawn_advance_or_capure());
        assert!(n4.is_pawn_advance_or_capure());
        assert!(n5.is_pawn_advance_or_capure());
    }

    #[test]
    fn test_move_stack() {
        let m = Move::new(WHITE,
                          2,
                          MOVE_NORMAL,
                          PAWN,
                          E2,
                          E4,
                          NO_PIECE,
                          NO_ENPASSANT_FILE,
                          CastlingRights::new(),
                          0);
        let mut s = MoveStack::new();
        assert!(s.remove_best_move().is_none());
        s.save();
        s.push_move(m);
        assert_eq!(s.remove_best_move().unwrap(), m);
        assert!(s.remove_best_move().is_none());
        s.restore();
        assert!(s.remove_best_move().is_none());
        s.push_move(m);
        s.push_move(m);
        s.save();
        s.push_move(m);
        s.restore();
        assert_eq!(s.remove_best_move().unwrap(), m);
        assert_eq!(s.remove_best_move().unwrap(), m);
        assert!(s.remove_best_move().is_none());
    }
}
