use basetypes::*;
use position::castling_rights::*;


pub struct ParseError;


pub type Result<T> = ::std::result::Result<T, ParseError>;


pub struct PiecesPlacement {
    pub piece_type: [u64; 6],
    pub color: [u64; 2],
}


// Parses a square in algebraic notation.
pub fn parse_square(s: &str) -> Result<Square> {
    use regex::Regex;
    let re = Regex::new(r"^[a-h][1-8]$").unwrap();  // TODO: Do this at compile time!

    if re.is_match(s) {
        let mut chars = s.chars();
        let file = (chars.next().unwrap().to_digit(18).unwrap() - 10) as File;
        let rank = (chars.next().unwrap().to_digit(9).unwrap() - 1) as Rank;
        Ok(square(file, rank))
    } else {
        Err(ParseError)
    }
}


// Parse FEN piece placement field.
pub fn parse_fen_piece_placement(s: &str) -> Result<PiecesPlacement> {

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
        Ok(PiecesPlacement { piece_type: piece_type, color: color })
    } else {
        Err(ParseError)
    }
}


// Parse FEN active color field.
pub fn parse_fen_active_color(s: &str) -> Result<Color> {
    match s {
        "w" => Ok(WHITE),
        "b" => Ok(BLACK),
        _ => Err(ParseError),
    }
}


// Parses FEN castling rights field.
pub fn parse_fen_castling_rights(s: &str) -> Result<CastlingRights> {

    // We start with no caltling allowed.
    let mut rights = CastlingRights::new();

    // Then we parse the content and update the castling rights.
    if s != "-" {
        for c in s.chars() {
            match c {
                'K' => {
                    if rights.grant(CASTLE_WHITE_KINGSIDE) == 0 {
                        return Err(ParseError);
                    }
                }
                'Q' => {
                    if rights.grant(CASTLE_WHITE_QUEENSIDE) == 0 {
                        return Err(ParseError);
                    }
                }
                'k' => {
                    if rights.grant(CASTLE_BLACK_KINGSIDE) == 0 {
                        return Err(ParseError);
                    }
                }
                'q' => {
                    if rights.grant(CASTLE_BLACK_QUEENSIDE) == 0 {
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


// Parse FEN en-passant square field.
pub fn parse_fen_enpassant_square(s: &str) -> Result<Option<Square>> {
    match s {
        "-" => Ok(None),
        _ => Ok(Some(try!(parse_square(s).map_err(|_| ParseError)))),
    }
}

