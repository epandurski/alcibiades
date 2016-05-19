#![allow(dead_code)]
#![allow(unused_variables)]

pub mod board_geometry;
pub mod castling_rights;
pub mod chess_move;
pub mod board;

// use basetypes::*;
use notation;
use self::board::Board;


pub struct Position {
    board: Board,
    halfmove_clock: u32,
    fullmove_number: u32, /* move_stack
                           * move_history (including fullmove_number?)
                           * ply
                           * hply?
                           * various hash tables
                           * first_move_index[usize; MAX_PLY]
                           * undo_move data stack */
}

impl Position {
    // A FEN (Forsyth–Edwards Notation) string defines a particular
    // position using only the ASCII character set.
    //
    // A FEN string contains six fields separated by a space. The fields are:
    //
    // 1) Piece placement (from white's perspective). Each rank is described, starting
    //    with rank 8 and ending with rank 1. Within each rank, the contents of each
    //    square are described from file A through file H. Following the Standard
    //    Algebraic Notation (SAN), each piece is identified by a single letter taken
    //    from the standard English names. White pieces are designated using upper-case
    //    letters ("PNBRQK") whilst Black uses lowercase ("pnbrqk"). Blank squares are
    //    noted using digits 1 through 8 (the number of blank squares), and "/"
    //    separates ranks.
    //
    // 2) Active color. "w" means white moves next, "b" means black.
    //
    // 3) Castling availability. If neither side can castle, this is "-". Otherwise,
    //    this has one or more letters: "K" (White can castle kingside), "Q" (White
    //    can castle queenside), "k" (Black can castle kingside), and/or "q" (Black
    //    can castle queenside).
    //
    // 4) En passant target square (in algebraic notation). If there's no en passant
    //    target square, this is "-". If a pawn has just made a 2-square move, this
    //    is the position "behind" the pawn. This is recorded regardless of whether
    //    there is a pawn in position to make an en passant capture.
    //
    // 5) Halfmove clock. This is the number of halfmoves since the last pawn advance
    //    or capture. This is used to determine if a draw can be claimed under the
    //    fifty-move rule.
    //
    // 6) Fullmove number. The number of the full move. It starts at 1, and is
    //    incremented after Black's move.
    //
    pub fn from_fen(fen: &str) -> Result<Position, notation::ParseError> {
        let parts: Vec<_> = fen.split_whitespace().collect();

        if parts.len() == 6 {
            let p = Position {
                board: try!(Board::create(&try!(notation::parse_fen_piece_placement(parts[0])),
                                          try!(notation::parse_fen_enpassant_square(parts[3])),
                                          try!(notation::parse_fen_castling_rights(parts[2])),
                                          try!(notation::parse_fen_active_color(parts[1])))
                                .map_err(|e| notation::ParseError)), /* TODO: Should it be other error? */
                halfmove_clock: try!(parts[4].parse::<u32>().map_err(|e| notation::ParseError)),
                fullmove_number: try!(parts[5].parse::<u32>().map_err(|e| notation::ParseError)),
            };
            Ok(p)
        } else {
            Err(notation::ParseError)
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fen_parsing() {
        assert!(Position::from_fen("nbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr1/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBN b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR/ b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNRR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP01PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP91PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP*1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 * 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 *")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b - e3 0 1")
                    .is_ok());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_ok());
        assert!(Position::from_fen("8/8/8/8/8/8/8/8 w - - 0 1").is_err());
        assert!(Position::from_fen("8/8/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/8/8/8/7K w - - 0 1").is_ok());
        assert!(Position::from_fen("k7/8/8/8/8/8/8/6KK w - - 0 1").is_err());
        assert!(Position::from_fen("k7/pppppppp/p7/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/8/7P/PPPPPPPP/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/pppppppp/8/8/8/8/PPPPPPPP/7K w - - 0 1").is_ok());
        assert!(Position::from_fen("k7/1P6/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/1B6/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/1N6/8/8/8/8/8/7K w - - 0 1").is_ok());
        assert!(Position::from_fen("k3P3/8/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k3p3/8/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/8/8/8/pP5K w - - 0 1").is_err());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1").is_ok());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K2B w KQkq - 0 1").is_err());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K3 w KQkq - 0 1").is_err());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K3 w KQkq - 0 1").is_err());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K3 w Qkq - 0 1").is_ok());
        assert!(Position::from_fen("r2k3r/8/8/8/8/8/8/R3K3 w Qkq - 0 1").is_err());
        assert!(Position::from_fen("r2k3r/8/8/8/8/8/8/R3K3 w Qk - 0 1").is_err());
        assert!(Position::from_fen("r2k3r/8/8/8/8/8/8/R3K3 w Q - 0 1").is_ok());
        assert!(Position::from_fen("k7/8/8/8/7P/8/8/7K w - h3 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/7P/8/8/7K b - h3 0 1").is_ok());
        assert!(Position::from_fen("k7/8/8/7P/8/8/8/7K b - h4 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/7P/7P/8/7K b - h3 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/7P/8/7P/7K b - h3 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/6P1/7P/8/7K b - h3 0 1").is_err());
        assert!(Position::from_fen("8/8/8/6k1/7P/8/8/7K b - h3 0 1").is_ok());
        assert!(Position::from_fen("8/8/8/6k1/7P/8/8/6RK b - h3 0 1").is_err());
        assert!(Position::from_fen("8/8/8/6k1/3P4/8/8/2B4K b - d3 0 1").is_ok());
        assert!(Position::from_fen("8/8/8/6k1/7P/4B3/8/7K b - h3 0 1").is_err());
    }
}
