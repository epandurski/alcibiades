#![allow(dead_code)]
#![allow(unused_variables)]

// enum Color {
//     White = 0,
//     Black = 1,
// }

// pub enum Pieces {
//     King = 0,
//     Queen = 1,
//     Rook = 2,
//     Bishop = 3,
//     Knight = 4,
//     Pawn = 5,
// }

struct PieceSets {
    pawns: u64,
    knights: u64,
    bishops: u64,
    rooks: u64,
    queens: u64,
    kings: u64,
}

const EMPTY_SET: u64 = 0;
const UNIVERSAL_SET: u64 = 0xffffffffffffffff;

// Color
const WHITE: usize = 0;
const BLACK: usize = 1;

// PieceType
const KING: usize = 0;
const QUEEN: usize = 1;
const ROOK: usize = 2;
const BISHOP: usize = 3;
const KNIGHT: usize = 4;
const PAWN: usize = 5;

pub type Color = usize;
pub type Square = u8;
pub type PieceType = usize;
pub type OccupationArray = [u64; 6];
pub type Bitboard = [OccupationArray; 2];
pub type CastlingRights = [(bool, bool); 2];  // (King-side, Queen-side)

fn square(file: u8, rank: u8) -> Square {
    assert!(file < 8);
    assert!(rank < 8);
    rank * 8 + file
}

pub struct Board {
    bitboard: Bitboard,
    to_move: Color,
    castling_rights: CastlingRights,
    pub en_passant_square: Option<Square>,
    halfmove_clock: u32,
    fullmove_number: u32,
}

impl Board {
    
    pub fn from_fen(fen: &str) -> Option<Board> {
        let parts: Vec<&str> = fen.split_whitespace().collect();
        if parts.len() != 6 {
            return None;
        }

        let bitboard = match Board::parse_fen_piece_placement_description(parts[0]) {
            Some(b) => b, 
            None => {
                return None;
            }
        };

        let to_move = match parts[1] {
            "w" => WHITE,
            "b" => BLACK,
            _ => {
                return None;
            }
        };

        Some(Board {
            bitboard: bitboard,
            to_move: to_move,
            castling_rights: Board::parse_fen_castling_rights(parts[2]),
            en_passant_square: Board::parse_square_notation(parts[3]),
            halfmove_clock: parts[4].parse::<u32>().unwrap_or(0),
            fullmove_number: parts[5].parse::<u32>().unwrap_or(1),
        })
    }


    // This function parses a square in algebraic notation.
    fn parse_square_notation(s: &str) -> Option<Square> {
        use regex::Regex;
        let re = Regex::new(r"^[a-h][1-8]$").unwrap();  // TODO: Do this at compile time!

        if re.is_match(s) {
            let mut chars = s.chars();
            let file = chars.next().unwrap().to_digit(18).unwrap() - 10;
            let rank = chars.next().unwrap().to_digit(9).unwrap() - 1;
            Some(square(file as u8, rank as u8))
        } else {
            None
        }
    }


    // This function parses FEN piece placement descriptions.
    fn parse_fen_piece_placement_description(ppd: &str) -> Option<Bitboard> {

        // We start with an empty board.
        let mut bitboard = [[0u64; 6]; 2];

        // FEN describes the board starting at A8 and going toward H1.
        let mut file = 0u8;
        let mut rank = 7u8;
        let mut error_detected = false;

        enum Token {
            Piece(Color, PieceType),
            EmptySquares(u8),
            Separator,
            Error,
        }

        for c in ppd.chars() {

            // Parse the token
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
                n @ '1'...'8' => Token::EmptySquares(n.to_digit(9).unwrap() as u8),
                '/' => Token::Separator,
                _ => Token::Error,
            };

            // Update the bitboard
            match token {
                Token::Piece(color, piece_type) => {
                    if file > 7 {
                        error_detected = true;
                        break;
                    }
                    bitboard[color][piece_type] |= 1 << square(file, rank);
                    file += 1;
                }
                Token::EmptySquares(n) => {
                    file += n;
                    if file > 8 {
                        error_detected = true;
                        break;
                    }
                }
                Token::Separator => {
                    if file == 8 && rank > 0 {
                        file = 0;
                        rank -= 1;
                    } else {
                        error_detected = true;
                        break;
                    }
                }
                Token::Error => {
                    error_detected = true;
                    break;
                }
            }
        }

        // Ensure everything is OK.
        if !error_detected && file == 8 && rank == 0 {
            Some(bitboard)
        } else {
            None
        }
    }


    // This function parses FEN castling.
    fn parse_fen_castling_rights(cr: &str) -> CastlingRights {

        const NO_CASTLING: CastlingRights = [(false, false); 2];

        // We start with an no caltling allowed.
        let mut castling_rights = NO_CASTLING;

        for c in cr.chars() {

            // Parse a character and update castling rights.
            //
            // We do not allow the same character to occur more than
            // once, even though it is not ambiguous, because that is
            // probably due to an error.
            match c {
                'K' => {
                    if !castling_rights[WHITE].0 {
                        castling_rights[WHITE].0 = true;
                    } else {
                        return NO_CASTLING;
                    }
                }
                'Q' => {
                    if !castling_rights[WHITE].1 {
                        castling_rights[WHITE].1 = true;
                    } else {
                        return NO_CASTLING;
                    }
                }
                'k' => {
                    if !castling_rights[BLACK].0 {
                        castling_rights[BLACK].0 = true;
                    } else {
                        return NO_CASTLING;
                    }
                }
                'q' => {
                    if !castling_rights[BLACK].1 {
                        castling_rights[BLACK].1 = true;
                    } else {
                        return NO_CASTLING;
                    }
                }
                _ => {
                    return NO_CASTLING;
                }
            };
        }

        castling_rights
    }
}

pub type AttackArray = [[u64; 64]; 5];  // for example
                                   // attack_set[Pieces::Queen][0]
                                   // gives the attack set for a queen
                                   // at A1.


#[allow(dead_code)]
pub fn generate_attack_and_blockers_arrays() -> (AttackArray, AttackArray) {

    fn square2grid_index(i: usize) -> usize {
        ((i / 8) * 10 + (i % 8) + 21)
    }

    // Generate a 10x12 grid -- 0xff is the "out of board" marker.
    let mut grid = [0xffu8; 120];
    for i in 0..64 {
        grid[square2grid_index(i)] = i as Square;
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
