//! Implements Forsyth–Edwards Notation parsing.

use regex::Regex;
use board::*;
use files::*;
use ranks::*;


/// Parses Forsyth–Edwards Notation (FEN).
///
/// Returns a tuple with the following elements: `0`) a board
/// instance, `1`) halfmove clock, `2`) fullmove number.
///
/// # Forsyth–Edwards Notation
///
/// A FEN string defines a particular position using only the ASCII
/// character set. A FEN string contains six fields separated by a
/// space. The fields are:
///
/// 1. Piece placement (from white's perspective). Each rank is
///    described, starting with rank 8 and ending with rank 1. Within
///    each rank, the contents of each square are described from file A
///    through file H. Following the Standard Algebraic Notation (SAN),
///    each piece is identified by a single letter taken from the
///    standard English names. White pieces are designated using
///    upper-case letters ("PNBRQK") whilst Black uses lowercase
///    ("pnbrqk"). Blank squares are noted using digits 1 through 8
///    (the number of blank squares), and "/" separates ranks.
///
/// 2. Active color. "w" means white moves next, "b" means black.
///
/// 3. Castling availability. If neither side can castle, this is
///    "-". Otherwise, this has one or more letters: "K" (White can
///    castle kingside), "Q" (White can castle queenside), "k" (Black
///    can castle kingside), and/or "q" (Black can castle queenside).
///
/// 4. En-passant target square (in algebraic notation). If there's no
///    en-passant target square, this is "-". If a pawn has just made
///    a 2-square move, this is the position "behind" the pawn. This
///    is recorded regardless of whether there is a pawn in position
///    to make an en-passant capture.
///
/// 5. Halfmove clock. This is the number of halfmoves since the last
///    pawn advance or capture. This is used to determine if a draw can
///    be claimed under the fifty-move rule.
///
/// 6. Fullmove number. The number of the full move. It starts at 1,
///    and is incremented after Black's move.
///
/// ## Example:
/// The starting position: `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1`
pub fn parse_fen(s: &str) -> Result<(Board, u8, u16), IllegalBoard> {
    let fileds: Vec<_> = s.split_whitespace().collect();
    if fileds.len() == 6 {
        let pieces = try!(parse_fen_piece_placement(fileds[0]));
        let to_move = try!(parse_fen_active_color(fileds[1]));
        let castling_rights = try!(parse_fen_castling_rights(fileds[2]));
        let enpassant_file = if let Some(x) = try!(parse_fen_enpassant_square(fileds[3])) {
            match to_move {
                WHITE if Board::rank(x) == RANK_6 => Board::file(x),
                BLACK if Board::rank(x) == RANK_3 => Board::file(x),
                _ => return Err(IllegalBoard),
            }
        } else {
            8
        };
        let halfmove_clock = try!(fileds[4].parse::<u8>().map_err(|_| IllegalBoard));
        let fullmove_number = try!(fileds[5].parse::<u16>().map_err(|_| IllegalBoard));
        if let 1...9000 = fullmove_number {
            return Ok((Board {
                occupied: pieces.color[WHITE] | pieces.color[BLACK],
                pieces: pieces,
                to_move: to_move,
                castling_rights: castling_rights,
                enpassant_file: enpassant_file,
            },
                       halfmove_clock,
                       fullmove_number));
        }
    }
    Err(IllegalBoard)
}


/// Parses square's algebraic notation (lowercase only).
pub fn parse_square(s: &str) -> Result<Square, IllegalBoard> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[a-h][1-8]$").unwrap();
    }
    if RE.is_match(s) {
        let mut chars = s.chars();
        let file = (chars.next().unwrap().to_digit(18).unwrap() - 10) as usize;
        let rank = (chars.next().unwrap().to_digit(9).unwrap() - 1) as usize;
        Ok(Board::square(file, rank))
    } else {
        Err(IllegalBoard)
    }
}


fn parse_fen_piece_placement(s: &str) -> Result<PiecesPlacement, IllegalBoard> {
    // These are the possible productions in the grammar.
    enum Token {
        Piece(Color, PieceType),
        EmptySquares(u32),
        Separator,
    }

    // FEN describes the board starting from A8 and going toward H1.
    let mut file = FILE_A;
    let mut rank = RANK_8;

    // We start with an empty board.
    let mut pieces = PiecesPlacement {
        piece_type: [0u64; 6],
        color: [0u64; 2],
    };

    // Then we read `s` character by character, updating `pieces`.
    for c in s.chars() {
        let token = match c {
            'K' => Token::Piece(WHITE, KING),
            'Q' => Token::Piece(WHITE, QUEEN),
            'R' => Token::Piece(WHITE, ROOK),
            'B' => Token::Piece(WHITE, BISHOP),
            'N' => Token::Piece(WHITE, KNIGHT),
            'P' => Token::Piece(WHITE, PAWN),
            'k' => Token::Piece(BLACK, KING),
            'q' => Token::Piece(BLACK, QUEEN),
            'r' => Token::Piece(BLACK, ROOK),
            'b' => Token::Piece(BLACK, BISHOP),
            'n' => Token::Piece(BLACK, KNIGHT),
            'p' => Token::Piece(BLACK, PAWN),
            n @ '1'...'8' => Token::EmptySquares(n.to_digit(9).unwrap()),
            '/' => Token::Separator,
            _ => return Err(IllegalBoard),
        };
        match token {
            Token::Piece(color, piece_type) => {
                if file > 7 {
                    return Err(IllegalBoard);
                }
                let mask = 1 << Board::square(file, rank);
                pieces.piece_type[piece_type] |= mask;
                pieces.color[color] |= mask;
                file += 1;
            }
            Token::EmptySquares(n) => {
                file += n as usize;
                if file > 8 {
                    return Err(IllegalBoard);
                }
            }
            Token::Separator => {
                if file == 8 && rank > 0 {
                    file = 0;
                    rank -= 1;
                } else {
                    return Err(IllegalBoard);
                }
            }
        }
    }

    // Make sure that all squares were initialized.
    if file != 8 || rank != 0 {
        return Err(IllegalBoard);
    }

    Ok(pieces)
}


fn parse_fen_active_color(s: &str) -> Result<Color, IllegalBoard> {
    match s {
        "w" => Ok(WHITE),
        "b" => Ok(BLACK),
        _ => Err(IllegalBoard),
    }
}


fn parse_fen_castling_rights(s: &str) -> Result<CastlingRights, IllegalBoard> {
    let mut rights = CastlingRights::new(0);
    if s != "-" {
        for c in s.chars() {
            let (color, side) = match c {
                'K' => (WHITE, KINGSIDE),
                'Q' => (WHITE, QUEENSIDE),
                'k' => (BLACK, KINGSIDE),
                'q' => (BLACK, QUEENSIDE),
                _ => return Err(IllegalBoard),
            };
            if !rights.grant(color, side) {
                return Err(IllegalBoard);
            }
        }
    }
    Ok(rights)
}


fn parse_fen_enpassant_square(s: &str) -> Result<Option<Square>, IllegalBoard> {
    if s == "-" {
        Ok(None)
    } else {
        parse_square(s).map(|x| Some(x))
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_fen_string() {
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_ok());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_err());
        assert!(parse_fen("nbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_err());
        assert!(parse_fen("rnbqkbnr1/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_err());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBN b KQkq e3 0 1").is_err());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR/ b KQkq e3 0 1").is_err());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNRR b KQkq e3 0 1").is_err());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP01PPP/RNBQKBNR b KQkq e3 0 1").is_err());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP91PPP/RNBQKBNR b KQkq e3 0 1").is_err());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP*1PPP/RNBQKBNR b KQkq e3 0 1").is_err());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 * 1").is_err());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 *").is_err());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b - e3 0 1").is_ok());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_ok());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b Kkq e3 0 1").is_ok());
        assert!(parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b kq - 0 1").is_ok());
        assert!(parse_fen("k7/8/8/8/8/8/8/7K w - - 0 1").is_ok());
        assert!(parse_fen("k7/pppppppp/8/8/8/8/PPPPPPPP/7K w - - 0 1").is_ok());
        assert!(parse_fen("k7/8/8/8/7P/8/8/7K w - h3 0 1").is_err());
        assert!(parse_fen("k7/8/8/7P/8/8/8/7K b - h4 0 1").is_err());
        assert!(parse_fen("8/8/8/6k1/7P/8/8/6RK b - h3 0 1").is_ok());
        assert!(parse_fen("8/8/8/6k1/7P/8/8/7K b - h3 0 0").is_err());
    }
}
