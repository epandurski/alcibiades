#![allow(dead_code)]
#![allow(unused_variables)]

use basetypes::*;

struct Bitboard {
    piece_type_bb: [u64; 6],
    color_bb: [u64; 2],
}

pub struct Board {
    bitboard: Bitboard,
    to_move: Color,
    castling_rights: CastlingRights,
    en_passant_square: Option<Square>,
    halfmove_clock: u32,
    fullmove_number: u32,
}

pub struct ParseError;

pub type Result<T> = ::std::result::Result<T, ParseError>;

impl Board {
    pub fn from_fen(fen: &str) -> Result<Board> {
        let parts: Vec<_> = fen.split_whitespace().collect();

        if parts.len() == 6 {
            Ok(Board {
                bitboard: try!(Board::parse_fen_piece_placement(parts[0])),
                to_move: try!(Board::parse_fen_active_color(parts[1])),
                castling_rights: try!(Board::parse_fen_castling_rights(parts[2])),
                en_passant_square: try!(Board::parse_fen_enpassant_square(parts[3])),
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
    fn parse_fen_piece_placement(s: &str) -> Result<Bitboard> {

        // We start with an empty bitboard. FEN describes the board
        // starting at A8 and going toward H1.
        let mut bitboard = Bitboard {
            piece_type_bb: [0u64; 6],
            color_bb: [0u64; 2],
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

            // Update the bitboard accordting to the token.
            match token {
                Token::Piece(color, piece_type) => {
                    if file > 7 {
                        return Err(ParseError);
                    }
                    let mask = 1 << square(file, rank);
                    bitboard.piece_type_bb[piece_type] |= mask;
                    bitboard.color_bb[color] |= mask;
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
            Ok(bitboard)
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
            _ => Ok(Some(try!(Board::parse_square_notation(s).map_err(|e| ParseError)))),
        }
    }
}

pub type AttackArray = [[u64; 64]; 5];  // for example
                                   // attack_set[Pieces::Queen][0]
                                   // gives the attack set for a queen
                                   // at A1.


pub fn generate_attack_and_blockers_arrays() -> (AttackArray, AttackArray) {

    fn square2grid_index(i: Square) -> usize {
        ((i / 8) * 10 + (i % 8) + 21)
    }

    // Generate a 10x12 grid -- 0xff is the "out of board" marker.
    let mut grid = [0xffu8; 120];
    for i in 0..64 {
        grid[square2grid_index(i)] = i as u8;
    }

    // 0: Queen, 1: Rook, 3: Bishop, 3: Knight, 4: King.
    static PIECE_LONGRANGE: [bool; 5] = [true, true, true, false, false];
    static PIECE_DELTAS: [[i8; 8]; 5] = [[-11, -10, -9, -1, 1, 9, 10, 11],
                                         [0, -10, 0, -1, 1, 0, 10, 0],
                                         [-11, 0, -9, 0, 0, 9, 0, 11],
                                         [-21, -19, -12, -8, 8, 12, 19, 21],
                                         [-11, -10, -9, -1, 1, 9, 10, 11]];

    // At the end those arrays will hold the attack bitsets, and the
    // "blockers and beyond" bitsets for each piece on each possible
    // square.
    let mut attack_array = [[0u64; 64]; 5];
    let mut blockers_array = [[0u64; 64]; 5];

    for piece_type in 0..5 {
        for square in 0..64 {
            let mut attack = 0u64;
            let mut blockers = 0u64;
            for move_direction in 0..8 {
                let delta = PIECE_DELTAS[piece_type][move_direction];
                if delta != 0 {
                    let mut last_mask = 0u64;
                    let mut curr_grid_index = square2grid_index(square);
                    loop {
                        curr_grid_index = (curr_grid_index as i8 + delta) as usize;
                        let curr_square = grid[curr_grid_index];
                        if curr_square != 0xff {
                            last_mask = 1 << curr_square;
                            attack |= last_mask;
                            blockers |= last_mask;
                            if PIECE_LONGRANGE[piece_type] {
                                continue;
                            }
                        }
                        blockers &= !last_mask;
                        break;
                    }
                }
            }
            attack_array[piece_type][square] = attack;
            blockers_array[piece_type][square] = blockers;
        }
    }

    (attack_array, blockers_array)
}

// Forsyth–Edwards Notation


// The StateInfo struct stores information needed to restore a Position
// object to its previous state when we retract a move. Whenever a move
// is made on the board (by calling Position::do_move), a StateInfo
// object must be passed as a parameter.

// struct StateInfo {
//   Key pawnKey, materialKey;
//   Value npMaterial[COLOR_NB];
//   int castlingRights, rule50, pliesFromNull;
//   Score psq;
//   Square epSquare;

//   Key key;
//   Bitboard checkersBB;
//   PieceType capturedType;
//   StateInfo* previous;
// };



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fen_parsing() {
        assert!(Board::from_fen("nbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr1/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBN b KQkq e3 0 1")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR/ b KQkq e3 0 1")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNRR b KQkq e3 0 1")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP01PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP91PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP*1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 * 1")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 *")
                    .is_err());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b - e3 0 1")
                    .is_ok());
        assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_ok());
    }

    #[test]
    fn test_attack_sets() {
        use basetypes::*;
        let (att_sets, bl_sets) = generate_attack_and_blockers_arrays();
        assert_eq!(att_sets[KING][0], 0b11 << 8 | 0b10);
        assert_eq!(bl_sets[KING][0], 0);
        assert_eq!(att_sets[ROOK][0],
                   0b11111110 | 1 << 8 | 1 << 16 | 1 << 24 | 1 << 32 | 1 << 40 | 1 << 48 | 1 << 56);
        assert_eq!(bl_sets[ROOK][0],
                   0b01111110 | 1 << 8 | 1 << 16 | 1 << 24 | 1 << 32 | 1 << 40 | 1 << 48 | 0 << 56);
    }

}
