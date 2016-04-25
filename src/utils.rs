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

fn square(file: u8, rank: u8) -> Square {
    assert!(file < 8);
    assert!(rank < 8);
    rank * 8 + file
}

pub struct Board {
    bitboard: Bitboard,
    to_move: Color,
    castling: [(bool, bool); 2], // (King-side, Queen-side)
    pub en_passant_square: Option<Square>,
    halfmove_clock: u32,
    fullmove_number: u32,
}

impl Board {
    pub fn from_fen(fen: &str) -> Option<Board> {
        use regex::Regex;

        let parts: Vec<&str> = fen.split_whitespace().collect();
        if parts.len() != 6 {
            return None;
        }
        // let pieces_str = parts[0];
        // let active_str = parts[1];
        // let castling_str = parts[2];
        // let en_passant_str = parts[3];
        // let halfmove_str = parts[4];
        // let fullmove_str = parts[5];

        // let en_passant_str = parts[3];
        // let mut en_passant_square = None;

        let re = Regex::new(r"^[a-h][1-8]$").unwrap();

        // FEN's piece placement character. 
        enum Token {
            Piece(Color, PieceType),
            EmptySquares(u8),
            Separator,
            Error,
        }

        // We start with an empty board.
        let mut bitboard = [[0u64; 6]; 2];  
        
        // FEN describes the board starting at A8 and going to H1.        
        let mut file = 0u8;
        let mut rank = 7u8;
        
        for c in parts[0].chars() {
            
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
                        return None;
                    }
                    bitboard[color][piece_type] |= 1 << square(file, rank);
                    file += 1;
                }
                Token::EmptySquares(n) => {
                    file += n;
                    if file > 8 {
                        return None;
                    }
                }
                Token::Separator => {
                    if file == 8 && rank > 0 {
                        file = 0;
                        rank -= 1;
                    } else {
                        return None;
                    }
                }
                Token::Error => {
                    return None;
                }
            }
        }
        
        // Ensure pieces placement description has the right length.
        if file != 8 || rank != 0 {
            return None;
        }

        let en_passant_square = if re.is_match(parts[3]) {
            let mut chars = parts[3].chars();
            let file = chars.next().unwrap().to_digit(18).unwrap() - 10;
            let rank = chars.next().unwrap().to_digit(9).unwrap() - 1;
            Some((rank * 8 + file) as Square)
        } else {
            None
        };
        let halfmove_clock = parts[4].parse::<u32>().unwrap_or(0);
        let fullmove_number = parts[5].parse::<u32>().unwrap_or(1);

        Some(Board {
            bitboard: [[0u64; 6]; 2],
            to_move: WHITE,
            castling: [(false, false); 2],
            en_passant_square: en_passant_square,
            halfmove_clock: halfmove_clock,
            fullmove_number: fullmove_number,
        })
    }
    
    // fn parse_fen_piece_placement_description(&str) -> Bitboard {
        
    // }
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
