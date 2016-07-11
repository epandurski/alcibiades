//! Implements Forsyth–Edwards notation (FEN) parsing.

use regex::Regex;
use basetypes::*;
use castling_rights::*;


/// Represents a parse error.
pub struct ParseError;


/// Pieces placement desctiption.
pub struct PiecesPlacement {
    /// An array of occupation bitboards indexed by piece type.
    pub piece_type: [u64; 6],

    /// An array of occupation bitboards indexed by color.
    pub color: [u64; 2],
}


/// Parses a FEN string.
///
/// A FEN (Forsyth–Edwards Notation) string defines a particular
/// position using only the ASCII character set. A FEN string
/// contains six fields separated by a space. The fields are:
///
/// 1) Piece placement (from white's perspective). Each rank is
///    described, starting with rank 8 and ending with rank 1. Within
///    each rank, the contents of each square are described from file A
///    through file H. Following the Standard Algebraic Notation (SAN),
///    each piece is identified by a single letter taken from the
///    standard English names. White pieces are designated using
///    upper-case letters ("PNBRQK") whilst Black uses lowercase
///    ("pnbrqk"). Blank squares are noted using digits 1 through 8
///    (the number of blank squares), and "/" separates ranks.
///
/// 2) Active color. "w" means white moves next, "b" means black.
///
/// 3) Castling availability. If neither side can castle, this is
///    "-". Otherwise, this has one or more letters: "K" (White can
///    castle kingside), "Q" (White can castle queenside), "k" (Black
///    can castle kingside), and/or "q" (Black can castle queenside).
///
/// 4) En passant target square (in algebraic notation). If there's no
///    en passant target square, this is "-". If a pawn has just made a
///    2-square move, this is the position "behind" the pawn. This is
///    recorded regardless of whether there is a pawn in position to
///    make an en passant capture.
///
/// 5) Halfmove clock. This is the number of halfmoves since the last
///    pawn advance or capture. This is used to determine if a draw can
///    be claimed under the fifty-move rule.
///
/// 6) Fullmove number. The number of the full move. It starts at 1,
///    and is incremented after Black's move.
pub fn parse_fen
    (s: &str)
     -> Result<(PiecesPlacement, Color, CastlingRights, Option<Square>, u8, u16), ParseError> {

    let fileds: Vec<_> = s.split_whitespace().collect();
    if fileds.len() == 6 {
        Ok((try!(parse_fen_piece_placement(fileds[0])),
            try!(parse_fen_active_color(fileds[1])),
            try!(parse_fen_castling_rights(fileds[2])),
            try!(parse_fen_enpassant_square(fileds[3])),
            try!(fileds[4].parse::<u8>().map_err(|_| ParseError)),
            match try!(fileds[5].parse::<u16>().map_err(|_| ParseError)) {
            0 => return Err(ParseError),
            x => x,
        }))
    } else {
        Err(ParseError)
    }
}


// Parses a square in algebraic notation.
fn parse_square(s: &str) -> Result<Square, ParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[a-h][1-8]$").unwrap();
    }

    if RE.is_match(s) {
        let mut chars = s.chars();
        let file = (chars.next().unwrap().to_digit(18).unwrap() - 10) as File;
        let rank = (chars.next().unwrap().to_digit(9).unwrap() - 1) as Rank;
        Ok(square(file, rank))
    } else {
        Err(ParseError)
    }
}


fn parse_fen_piece_placement(s: &str) -> Result<PiecesPlacement, ParseError> {

    // We start with an empty board. FEN describes the board
    // starting at A8 and going toward H1.
    let mut piece_type = [0u64; 6];
    let mut color = [0u64; 2];
    let mut file = 0;
    let mut rank = 7;

    // These are the possible productions in the grammar.
    enum Token {
        Piece(Color, PieceType),
        EmptySquares(u32),
        Separator,
    }

    for c in s.chars() {

        // Parse the next character.
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
            _ => return Err(ParseError),
        };

        // Update the board accordting to the token.
        match token {
            Token::Piece(_color, _piece) => {
                if file > 7 {
                    return Err(ParseError);
                }
                let mask = 1 << square(file, rank);
                piece_type[_piece] |= mask;
                color[_color] |= mask;
                file += 1;
            }
            Token::EmptySquares(n) => {
                file += n as File;
                if file > 8 {
                    return Err(ParseError);
                }
            }
            Token::Separator => {
                if file == 8 && rank > 0 {
                    file = 0;
                    rank -= 1;
                } else {
                    return Err(ParseError);
                }
            }
        }
    }

    // Ensure the piece placement field had the right length.
    if file == 8 && rank == 0 {
        Ok(PiecesPlacement {
            piece_type: piece_type,
            color: color,
        })
    } else {
        Err(ParseError)
    }
}


fn parse_fen_active_color(s: &str) -> Result<Color, ParseError> {
    match s {
        "w" => Ok(WHITE),
        "b" => Ok(BLACK),
        _ => Err(ParseError),
    }
}


fn parse_fen_castling_rights(s: &str) -> Result<CastlingRights, ParseError> {

    // We start with no caltling allowed.
    let mut rights = CastlingRights::new();

    // Then we parse the content and update the castling rights.
    if s != "-" {
        for c in s.chars() {
            match c {
                'K' => {
                    if rights.grant(WHITE, KINGSIDE) {
                        return Err(ParseError);
                    }
                }
                'Q' => {
                    if rights.grant(WHITE, QUEENSIDE) {
                        return Err(ParseError);
                    }
                }
                'k' => {
                    if rights.grant(BLACK, KINGSIDE) {
                        return Err(ParseError);
                    }
                }
                'q' => {
                    if rights.grant(BLACK, QUEENSIDE) {
                        return Err(ParseError);
                    }
                }
                _ => {
                    return Err(ParseError);
                }
            };
        }
    }

    // Successfully parsed.
    Ok(rights)
}


fn parse_fen_enpassant_square(s: &str) -> Result<Option<Square>, ParseError> {
    match s {
        "-" => Ok(None),
        _ => Ok(Some(try!(parse_square(s).map_err(|_| ParseError)))),
    }
}
