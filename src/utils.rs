#![allow(dead_code)]
#![allow(unused_variables)]

use std::num::Wrapping;

pub type Color = usize;
pub type File = usize;
pub type Rank = usize;
pub type Square = usize;
pub type PieceType = usize;
pub type Bitboard = [[u64; 6]; 2];
pub type CastlingRights = [(bool, bool); 2];  // (King-side, Queen-side)

// Useful square-sets
const UNIVERSAL_SET: u64 = 0xffffffffffffffff;
const EMPTY_SET: u64 = 0;

// Color
const WHITE: Color = 0;
const BLACK: Color = 1;

// Piece types
const KING: PieceType = 0;
const QUEEN: PieceType = 1;
const ROOK: PieceType = 2;
const BISHOP: PieceType = 3;
const KNIGHT: PieceType = 4;
const PAWN: PieceType = 5;

fn square(file: File, rank: Rank) -> Square {
    assert!(file < 8);
    assert!(rank < 8);
    rank * 8 + file
}

pub fn ls1b(x: u64) -> u64 {
    (x as i64 & (Wrapping(0) - Wrapping(x as i64)).0) as u64
}

pub fn clear_ls1b(x: &mut u64) {
    *x &= (Wrapping(*x) - Wrapping(1)).0;
}

pub fn above_ls1b_mask(x: u64) -> u64 {
    assert!(x != 0);
    (x as i64 ^ (Wrapping(0) - Wrapping(x as i64)).0) as u64
}

pub fn below_lsb1_mask_including(x: u64) -> u64 {
    assert!(x != 0);
    x ^ (x - 1)
}

pub fn below_lsb1_mask(x: u64) -> u64 {
    assert!(x != 0);
    !x & (x - 1)
}

pub fn smeared_ls1b_up(x: u64) -> u64 {
    assert!(x != 0);
    ((x as i64) | (Wrapping(0) - Wrapping(x as i64)).0) as u64
}

pub fn smeared_ls1b_down(x: u64) -> u64 {
    assert!(x != 0);
    x | (x - 1)
}

pub fn bitscan_forward(b: u64) -> Square {
    const DEBRUIJN64: Wrapping<u64> = Wrapping(0x03f79d71b4cb0a89);
    const INDEX64: [Square; 64] = [0, 1, 48, 2, 57, 49, 28, 3, 61, 58, 50, 42, 38, 29, 17, 4, 62,
                                   55, 59, 36, 53, 51, 43, 22, 45, 39, 33, 30, 24, 18, 12, 5, 63,
                                   47, 56, 27, 60, 41, 37, 16, 54, 35, 52, 21, 44, 32, 23, 11, 46,
                                   26, 40, 15, 34, 20, 31, 10, 25, 14, 19, 9, 13, 8, 7, 6];
    assert!(b != 0);
    INDEX64[((Wrapping(ls1b(b)) * DEBRUIJN64).0 >> 58) as usize]
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
        let mut bitboard = [[0u64; 6]; 2];
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
                    bitboard[color][piece_type] |= 1 << square(file, rank);
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

    // 0: King, 1: Queen, 2: Rook, 3: Bishop, 4: Knight.
    static PIECE_LONGRANGE: [bool; 5] = [false, true, true, true, false];
    static PIECE_DELTAS: [[i8; 8]; 5] = [[-11, -10, -9, -1, 1, 9, 10, 11],
                                         [-11, -10, -9, -1, 1, 9, 10, 11],
                                         [0, -10, 0, -1, 1, 0, 10, 0],
                                         [-11, 0, -9, 0, 0, 9, 0, 11],
                                         [-21, -19, -12, -8, 8, 12, 19, 21]];

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
