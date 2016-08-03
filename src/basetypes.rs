//! Defines the most basic types and constants like the pieces,
//! colors, squares, files, and ranks.
//!
//! This module also defines few simple functions.


/// `WHITE` or `BLACK`.
pub type Color = usize; // 0 or 1

/// `KING`, `QUEEN`, `ROOK`, `BISHOP`, `KINGHT`, `PAWN`, `NO_PIECE`,
/// or `UNKNOWN_PIECE`.
pub type PieceType = usize;  // from 0 to 5

/// From 0 to 7 (0 is rank 1, 7 is rank 8).
pub type Rank = usize;  // from 0 to 7

/// From 0 to 7 (0 is file A, 7 is file H).
pub type File = usize;  // from 0 to 7

/// From 0 to 63 (0 is A1, 63 is H8).
pub type Square = usize;  // from 0 to 63

/// Number of searched positions.
pub type NodeCount = u64;

/// `QUEENSIDE` of `KINGSIDE`.
pub type CastlingSide = usize;

/// A set of squares on the chessboard.
///
/// `u64` bit-sets called *bitboards* (BB) can be used to represent a
/// set of squares on the chess board. For example, the set of squares
/// which are occupied by white rooks in the beginning of the game is:
/// `1 << A1 || 1 << H1`.
pub type Bitboard = u64;


/// Evaluation value in centipawns.
///
/// Positive values mean that the position is favorable for the side
/// to move. Negative values mean the position is favorable for the
/// other side (not to move). A value of `0` means that the chances
/// are equal. For example: a value of `100` might mean that the side
/// to move is a pawn ahead.
///
/// Values over `19999` and under `-19999` designate a certain
/// win/loss. The constant `VALUE_UNKNOWN` equals to `-32768`, and has
/// the special meaning of "unknown value".
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
pub const UNKNOWN_PIECE: PieceType = 7;

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
pub const FILE_A: File = 0;
pub const FILE_B: File = 1;
pub const FILE_C: File = 2;
pub const FILE_D: File = 3;
pub const FILE_E: File = 4;
pub const FILE_F: File = 5;
pub const FILE_G: File = 6;
pub const FILE_H: File = 7;

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

// Value
pub const VALUE_UNKNOWN: Value = ::std::i16::MIN;

// Castling side
pub const QUEENSIDE: CastlingSide = 0;
pub const KINGSIDE: CastlingSide = 1;

// Bitboards.
pub const BB_EMPTY_SET: Bitboard = 0;
pub const BB_UNIVERSAL_SET: Bitboard = 0xffffffffffffffff;


/// Returns the square on given file and rank.
#[inline]
pub fn square(file: File, rank: Rank) -> Square {
    assert!(file < 8);
    assert!(rank < 8);
    rank * 8 + file
}

/// Returns the rank of a given square.
#[inline(always)]
pub fn rank(square: Square) -> Rank {
    assert!(square <= 63);
    square >> 3
}

/// Returns the file of a given square.
#[inline(always)]
pub fn file(square: Square) -> File {
    assert!(square <= 63);
    square % 8
}

/// Returns the algebraic notation for a given square.
pub fn notation(square: Square) -> &'static str {
    lazy_static! {
        static ref NOTATION: Vec<String> = (0..64).map(|i| format!("{}{}",
            ["a", "b", "c", "d", "e", "f", "g", "h"][file(i)],
            ["1", "2", "3", "4", "5", "6", "7", "8"][rank(i)])
        ).collect();
    }
    NOTATION[square].as_str()
}
