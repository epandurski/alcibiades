//! Defines how the chess board is represented in memory.

use std::fmt;
use squares::square;
use notation::parse_fen;


/// `WHITE` or `BLACK`.
pub type Color = usize;

pub const WHITE: Color = 0;
pub const BLACK: Color = 1;


/// `KING`, `QUEEN`, `ROOK`, `BISHOP`, `KINGHT`, `PAWN` or `PIECE_NONE`.
pub type PieceType = usize;

pub const KING: PieceType = 0;
pub const QUEEN: PieceType = 1;
pub const ROOK: PieceType = 2;
pub const BISHOP: PieceType = 3;
pub const KNIGHT: PieceType = 4;
pub const PAWN: PieceType = 5;
pub const PIECE_NONE: PieceType = 6;


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


/// Describes how the pieces are placed on the board.
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


/// `QUEENSIDE` or `KINGSIDE`.
pub type CastlingSide = usize;

pub const QUEENSIDE: CastlingSide = 0;
pub const KINGSIDE: CastlingSide = 1;


/// Holds information about which player can castle on which side.
///
/// The castling rights are held in a `usize` value. The lowest 4 bits
/// of the value contain the whole needed information. It is laid out
/// in the following way:
///
/// ```text
///  usize                    3   2   1   0
///  +----------------------+---+---+---+---+
///  |                      |   |   |   |   |
///  |    Unused (zeros)    |Castling flags |
///  |                      |   |   |   |   |
///  +----------------------+---+---+---+---+
///
///  bit 0 -- if set, white can castle on queen-side;
///  bit 1 -- if set, white can castle on king-side;
///  bit 2 -- if set, black can castle on queen-side;
///  bit 3 -- if set, black can castle on king-side.
/// ```
#[derive(Clone, Copy, Debug)]
pub struct CastlingRights(usize);

impl CastlingRights {
    /// Creates a new instance.
    ///
    /// The least significant 4 bits of `value` are used as a raw
    /// value for the new instance.
    #[inline(always)]
    pub fn new(value: usize) -> CastlingRights {
        CastlingRights(value & 0b1111)
    }

    /// Returns the contained raw value (between 0 and 15).
    #[inline(always)]
    pub fn value(&self) -> usize {
        self.0
    }

    /// Grants a given player the right to castle on a given side.
    ///
    /// This method returns `true` if the player did not have the
    /// right to castle on the given side before this method was
    /// called, and `false` otherwise.
    pub fn grant(&mut self, player: Color, side: CastlingSide) -> bool {
        assert!(player <= 1);
        assert!(side <= 1);
        let rights_before = self.0;
        let granted = 1 << (player << 1) << side;
        self.0 |= granted;

        granted & !rights_before != 0
    }

    /// Updates the castling rights after played move.
    ///
    /// `orig_square` and `dest_square` describe the played move.
    #[inline(always)]
    pub fn update(&mut self, orig_square: Square, dest_square: Square) {
        debug_assert!(orig_square <= 63);
        debug_assert!(dest_square <= 63);
        const WQ: usize = (1 << (WHITE << 1) << QUEENSIDE);
        const WK: usize = (1 << (WHITE << 1) << KINGSIDE);
        const W: usize = WQ | WK;
        const BQ: usize = (1 << (BLACK << 1) << QUEENSIDE);
        const BK: usize = (1 << (BLACK << 1) << KINGSIDE);
        const B: usize = BQ | BK;

        // On each move, the value of `CASTLING_RELATION` for the
        // origin and destination squares should be AND-ed with the
        // castling rights value, to derive the updated castling
        // rights.
        const CASTLING_RELATION: [usize; 64] = [
            !WQ, !0, !0, !0, !W, !0, !0, !WK,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !0,  !0, !0, !0, !0, !0, !0, !0,
            !BQ, !0, !0, !0, !B, !0, !0, !BK
        ];
        self.0 &= CASTLING_RELATION[orig_square] & CASTLING_RELATION[dest_square];
    }

    /// Returns if a given player has the rights to castle on a given
    /// side.
    #[inline(always)]
    pub fn can_castle(&self, player: Color, side: CastlingSide) -> bool {
        debug_assert!(player <= 1);
        debug_assert!(side <= 1);
        (1 << (player << 1) << side) & self.0 != 0
    }
}

impl fmt::Display for CastlingRights {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut value = self.value();
        for s in ["Q", "K", "q", "k"].iter() {
            if value & 1 == 1 {
                try!(f.write_str(s));
            }
            value >>= 1;
        }
        Ok(())
    }
}


/// Represents an illegal position error.
pub struct IllegalBoard;


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
    pub fn from_fen(fen: &str) -> Result<Board, IllegalBoard> {
        parse_fen(fen).map(|x| x.0)
    }
}
