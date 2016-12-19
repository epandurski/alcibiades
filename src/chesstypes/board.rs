//! Defines how the chess board is represented in memory.

use std::fmt;
use utils::parse_fen;
use super::*;


/// `WHITE` or `BLACK`.
pub type Color = usize;

pub const WHITE: Color = 0;
pub const BLACK: Color = 1;


/// `KING`, `QUEEN`, `ROOK`, `BISHOP`, `KINGHT`, `PAWN` or `NO_PIECE`.
pub type PieceType = usize;

pub const KING: PieceType = 0;
pub const QUEEN: PieceType = 1;
pub const ROOK: PieceType = 2;
pub const BISHOP: PieceType = 3;
pub const KNIGHT: PieceType = 4;
pub const PAWN: PieceType = 5;
pub const NO_PIECE: PieceType = 6;


/// From 0 to 7 (0 is rank 1, 7 is rank 8).
pub type Rank = usize;


/// From 0 to 7 (0 is file A, 7 is file H).
pub type File = usize;


/// From 0 to 63 (0 is A1, 1 is B1, .. , 62 is G8, 63 is H8).
pub type Square = usize;


/// A set of squares on the chessboard.
///
/// `u64` bit-sets called *bitboards* can be used to represent a set
/// of squares on the chessboard. For example, the set of squares that
/// are occupied by white rooks in the beginning of the game is: `1 <<
/// A1 | 1 << H1`. `0` represents the empty set, `0xffffffffffffffff`
/// represents the set of all 64 squares on the board.
pub type Bitboard = u64;


/// Describes how pieces are placed on the board.
#[derive(Clone, Copy, Debug)]
pub struct PiecesPlacement {
    /// An array of occupation bitboards indexed by piece type.  For
    /// example, `pieces_placement.piece_type[PAWN]` gives the set of
    /// all pawns on the board (white and black).
    pub piece_type: [Bitboard; 6],

    /// An array of occupation bitboards indexed by color.  For
    /// example, `pieces_placement.color[WHITE]` gives the set of all
    /// white pieces and pawns on the board.
    pub color: [Bitboard; 2],
}

impl fmt::Display for PiecesPlacement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        for rank in (0..8).rev() {
            s.push('\n');
            for file in 0..8 {
                let square = square(file, rank);
                let bb = 1 << square;
                let piece = match bb {
                    x if x & self.piece_type[KING] != 0 => 'k',
                    x if x & self.piece_type[QUEEN] != 0 => 'q',
                    x if x & self.piece_type[ROOK] != 0 => 'r',
                    x if x & self.piece_type[BISHOP] != 0 => 'b',
                    x if x & self.piece_type[KNIGHT] != 0 => 'n',
                    x if x & self.piece_type[PAWN] != 0 => 'p',
                    _ => '.',
                };
                if bb & self.color[WHITE] != 0 {
                    s.push(piece.to_uppercase().next().unwrap());
                } else {
                    s.push(piece);
                }
            }
        }
        writeln!(f, "{}", s)
    }
}


/// Holds a chess position.
#[derive(Clone)]
pub struct Board {
    /// The placement of the pieces on the board.
    pub pieces: PiecesPlacement,

    /// The side to move.
    pub to_move: Color,

    /// The castling rights for both players.
    pub castling_rights: CastlingRights,

    /// If the previous move was a double pawn push, contains pushed
    /// pawn's file (a value between 0 and 7). Otherwise contains `8`.
    pub enpassant_file: usize,

    /// The set of all occupied squares on the board.
    ///
    /// Always equals `self.pieces.color[WHITE] |
    /// self.pieces.color[BLACK]`. Deserves a field on its own because
    /// it is very frequently needed.
    pub occupied: Bitboard,
}

impl Board {
    /// Creates a new instance from Forsyth–Edwards Notation (FEN).
    pub fn from_fen(fen: &str) -> Result<Board, NotationError> {
        parse_fen(fen).map(|x| x.0)
    }
}


/// Returns the square on given file and rank.
#[inline]
pub fn square(file: File, rank: Rank) -> Square {
    debug_assert!(file < 8);
    debug_assert!(rank < 8);
    rank * 8 + file
}

/// Returns the rank of a given square.
#[inline(always)]
pub fn rank(square: Square) -> Rank {
    debug_assert!(square <= 63);
    square >> 3
}

/// Returns the file of a given square.
#[inline(always)]
pub fn file(square: Square) -> File {
    debug_assert!(square <= 63);
    square % 8
}
