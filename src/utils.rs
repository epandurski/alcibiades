#![allow(dead_code)]
#![allow(unused_variables)]

enum Color {
    White = 0,
    Black = 1,
}

pub enum Pieces {
    King = 0,
    Queen = 1,
    Rook = 2,
    Bishop = 3,
    Knight = 4,
    Pawn = 5,
}

struct PieceSets {
    pawns: u64,
    knights: u64,
    bishops: u64,
    rooks: u64,
    queens: u64,
    kings: u64,
}

// const EMPTY_SET: u64 = 0;
// const UNIVERSAL_SET: u64 = 0xffffffffffffffff;

type Square = u8;
type PieceType = usize;
type OccupationArray = [u64; 6];

pub struct Board {
    bitboard: [OccupationArray; 2],
    to_move: Color,
    castling: [(bool, bool); 2], // (King-side, Queen-side)
    en_passant_square: Option<Square>,
    halfmove_clock: u32,
    fullmove_number: u32,
}

impl Board {
    pub fn from_fen(fen: &str) -> Option<Board> {
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
        let halfmove_clock = parts[4].parse::<u32>().unwrap_or(0);
        let fullmove_number = parts[5].parse::<u32>().unwrap_or(1);
        None
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
