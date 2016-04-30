#![allow(dead_code)]
#![allow(unused_variables)]

use basetypes::*;

pub struct Position {
    board: Board,
    to_move: Color,
    castling_rights: CastlingRights,
    en_passant_square: Option<Square>,
    halfmove_clock: u32,
    fullmove_number: u32,
}

pub struct ParseError;

pub type Result<T> = ::std::result::Result<T, ParseError>;

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
    pub fn from_fen(fen: &str) -> Result<Position> {
        let parts: Vec<_> = fen.split_whitespace().collect();

        if parts.len() == 6 {
            Ok(Position {
                board: try!(Position::parse_fen_piece_placement(parts[0])),
                to_move: try!(Position::parse_fen_active_color(parts[1])),
                castling_rights: try!(Position::parse_fen_castling_rights(parts[2])),
                en_passant_square: try!(Position::parse_fen_enpassant_square(parts[3])),
                halfmove_clock: try!(parts[4].parse::<u32>().map_err(|e| ParseError)),
                fullmove_number: try!(parts[5].parse::<u32>().map_err(|e| ParseError)),
            })
        } else {
            Err(ParseError)
        }
    }


    // Perform basic position sanity checks.
    // fn is_position_possible(&self) -> bool {

    // }


    // Parses a square in algebraic notation.
    fn parse_square_notation(s: &str) -> Result<Square> {
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
    fn parse_fen_piece_placement(s: &str) -> Result<Board> {

        // We start with an empty board. FEN describes the board
        // starting at A8 and going toward H1.
        let mut board = Board {
            piece_type: [0u64; 6],
            color: [0u64; 2],
        };
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
                Token::Piece(color, piece_type) => {
                    if file > 7 {
                        return Err(ParseError);
                    }
                    let mask = 1 << square(file, rank);
                    board.piece_type[piece_type] |= mask;
                    board.color[color] |= mask;
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
            Ok(board)
        } else {
            Err(ParseError)
        }
    }


    // Parse FEN active color field.
    fn parse_fen_active_color(s: &str) -> Result<Color> {
        match s {
            "w" => Ok(WHITE),
            "b" => Ok(BLACK),
            _ => Err(ParseError),
        }
    }


    // Parses FEN castling rights field.
    fn parse_fen_castling_rights(s: &str) -> Result<CastlingRights> {

        // We start with no caltling allowed.
        let mut castling_rights = [(false, false); 2];

        // Then we parse the content and update the castling rights.
        if s != "-" {
            for c in s.chars() {
                match c {
                    'K' => {
                        if !castling_rights[WHITE].0 {
                            castling_rights[WHITE].0 = true;
                        } else {
                            return Err(ParseError);
                        }
                    }
                    'Q' => {
                        if !castling_rights[WHITE].1 {
                            castling_rights[WHITE].1 = true;
                        } else {
                            return Err(ParseError);
                        }
                    }
                    'k' => {
                        if !castling_rights[BLACK].0 {
                            castling_rights[BLACK].0 = true;
                        } else {
                            return Err(ParseError);
                        }
                    }
                    'q' => {
                        if !castling_rights[BLACK].1 {
                            castling_rights[BLACK].1 = true;
                        } else {
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
        Ok(castling_rights)
    }


    // Parse FEN en-passant square field.
    fn parse_fen_enpassant_square(s: &str) -> Result<Option<Square>> {
        match s {
            "-" => Ok(None),
            _ => Ok(Some(try!(Position::parse_square_notation(s).map_err(|e| ParseError)))),
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
    }
}
