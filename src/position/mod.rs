#![allow(dead_code)]
#![allow(unused_variables)]

pub mod board_geometry;
pub mod board;

use basetypes::*;
use notation;
use self::board::Board;
    
    
pub struct Position {
    board: Board,
    to_move: Color,
    castling_rights: CastlingRights,
    en_passant_square: Option<Square>,
    halfmove_clock: u32,
    fullmove_number: u32,
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
            Ok(Position {
                board: try!(notation::parse_fen_piece_placement(parts[0])),
                to_move: try!(notation::parse_fen_active_color(parts[1])),
                castling_rights: try!(notation::parse_fen_castling_rights(parts[2])),
                en_passant_square: try!(notation::parse_fen_enpassant_square(parts[3])),
                halfmove_clock: try!(parts[4].parse::<u32>().map_err(|e| notation::ParseError)),
                fullmove_number: try!(parts[5].parse::<u32>().map_err(|e| notation::ParseError)),
            })
        } else {
            Err(notation::ParseError)
        }
    }


    // Perform basic position sanity checks.
    // fn is_position_possible(&self) -> bool {

    // }

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
    }
}
