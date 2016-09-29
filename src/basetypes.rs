//! Defines the most basic types and constants like the pieces,
//! colors, squares, files, and ranks.
//!
//! This module also defines few simple functions.


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

pub const RANK_1: Rank = 0;
pub const RANK_2: Rank = 1;
pub const RANK_3: Rank = 2;
pub const RANK_4: Rank = 3;
pub const RANK_5: Rank = 4;
pub const RANK_6: Rank = 5;
pub const RANK_7: Rank = 6;
pub const RANK_8: Rank = 7;



/// From 0 to 7 (0 is file A, 7 is file H).
pub type File = usize;

pub const FILE_A: File = 0;
pub const FILE_B: File = 1;
pub const FILE_C: File = 2;
pub const FILE_D: File = 3;
pub const FILE_E: File = 4;
pub const FILE_F: File = 5;
pub const FILE_G: File = 6;
pub const FILE_H: File = 7;



/// From 0 to 63 (0 is A1, 63 is H8).
pub type Square = usize;

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



/// Number of searched positions.
pub type NodeCount = u64;



/// A set of squares on the chessboard.
///
/// `u64` bit-sets called *bitboards* can be used to represent a set
/// of squares on the chessboard. For example, the set of squares that
/// are occupied by white rooks in the beginning of the game is: `1 <<
/// A1 | 1 << H1`. `0` represents the empty set, `0xffffffffffffffff`
/// represents the set of all 64 squares on the board.
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

pub const VALUE_UNKNOWN: Value = ::std::i16::MIN;



/// `QUEENSIDE` or `KINGSIDE`.
pub type CastlingSide = usize; // 0 or 1

pub const QUEENSIDE: CastlingSide = 0;
pub const KINGSIDE: CastlingSide = 1;



/// Describes how pieces are placed on the board.
#[derive(Clone, Copy)]
pub struct PiecesPlacement {
    /// An array of occupation bitboards indexed by piece type.  For
    /// example, `piece_placement.piece_type[PAWN]` gives the set of
    /// all pawns on the board (white and black).
    pub piece_type: [Bitboard; 6],

    /// An array of occupation bitboards indexed by color.  For
    /// example, `piece_placement.color[WHITE]` gives the set of all
    /// white pieces and pawns on the board.
    pub color: [Bitboard; 2],
}

impl PiecesPlacement {
    /// Returns a human-readable representation of the placement of
    /// pieces.
    pub fn pretty_string(&self) -> String {
        let mut s = String::new();
        for rank in (0..8).rev() {
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
            s.push('\n');
        }
        s
    }
}



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
#[derive(Clone, Copy)]
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
    /// This method returns `true` if the player had the right to
    /// castle on the given side before this method was called, and
    /// `false` otherwise.
    pub fn grant(&mut self, player: Color, side: CastlingSide) -> bool {
        if player > 1 || side > 1 {
            panic!("invalid arguments");
        }
        let before = self.0;
        let mask = 1 << (player << 1) << side;
        self.0 |= mask;
        !before & mask == 0
    }

    /// Updates the castling rights after played move.
    ///
    /// `orig_square` and `dest_square` describe the played move.
    #[inline]
    pub fn update(&mut self, orig_square: Square, dest_square: Square) {
        assert!(orig_square <= 63);
        assert!(dest_square <= 63);
        
        const WQ: usize = 1 << 0;
        const WK: usize = 1 << 1;
        const WB: usize = WQ | WK;
        const BQ: usize = 1 << 2;
        const BK: usize = 1 << 3;
        const BB: usize = BQ | BK;
        
        // On each move, the value of `CASTLING_RELATION` for the
        // origin and destination squares should be AND-ed with the
        // castling rights value, to derive the updated castling
        // rights.
        const CASTLING_RELATION: [usize; 64] = [
            !WQ, !0,  !0,  !0,  !WB, !0,  !0,  !WK,
            !0,  !0,  !0,  !0,  !0,  !0,  !0,  !0,
            !0,  !0,  !0,  !0,  !0,  !0,  !0,  !0,
            !0,  !0,  !0,  !0,  !0,  !0,  !0,  !0,
            !0,  !0,  !0,  !0,  !0,  !0,  !0,  !0,
            !0,  !0,  !0,  !0,  !0,  !0,  !0,  !0,
            !0,  !0,  !0,  !0,  !0,  !0,  !0,  !0,
            !BQ, !0,  !0,  !0,  !BB, !0,  !0,  !BK,
        ];
        self.0 &= unsafe {
            // AND-ing with anything can not corrupt the instance, so
            // we are safe even if `orig_square` and `dest_square` are
            // out of bounds.
            *CASTLING_RELATION.get_unchecked(orig_square) &
            *CASTLING_RELATION.get_unchecked(dest_square)
        };
    }

    /// Returns if a given player has the rights to castle on a given
    /// side.
    #[inline]
    pub fn can_castle(&self, player: Color, side: CastlingSide) -> bool {
        assert!(player <= 1);
        assert!(side <= 1);
        (1 << (player << 1) << side) & self.0 != 0
    }

    /// Returns a bitboard with potential castling obstacles.
    /// 
    /// Returns a bitboard with the set of squares that should be
    /// vacant in order for the specified (`player`, `side`) castling
    /// move to be eventually possible. If `player` does not have the
    /// rights to castle on `side`, this method will return
    /// `BB_UNIVERSAL_SET`.
    #[inline]
    pub fn obstacles(&self, player: Color, side: CastlingSide) -> Bitboard {
        const OBSTACLES: [[Bitboard; 2]; 2] = [[1 << B1 | 1 << C1 | 1 << D1, 1 << F1 | 1 << G1],
                                               [1 << B8 | 1 << C8 | 1 << D8, 1 << F8 | 1 << G8]];
        if self.can_castle(player, side) {
            OBSTACLES[player][side]
        } else {
            // Castling is not allowed, therefore every piece on every
            // square on the board can be considered an obstacle.
            !0
        }
    }
}


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
