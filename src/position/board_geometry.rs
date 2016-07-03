//! Generates look-up tables and implements look-up methods.

use rand::{Rng, SeedableRng};
use rand::isaac::Isaac64Rng;
use basetypes::*;
use bitsets::*;
use castling_rights::*;


/// Returns a reference to a properly initialized `BoardGeometry`
/// object.
///
/// The object is created and initialized only during the first
/// call. All next calls will return a reference to the same
/// object. This is done in a thread-safe manner.
pub fn board_geometry() -> &'static BoardGeometry {
    use std::sync::{Once, ONCE_INIT};
    static INIT_GEOMETRY: Once = ONCE_INIT;
    static mut geometry: Option<BoardGeometry> = None;
    unsafe {
        INIT_GEOMETRY.call_once(|| {
            geometry = Some(BoardGeometry::new());
        });
        match geometry {
            Some(ref x) => x,
            None => panic!("board geometry not initialized"),
        }
    }
}


/// A collection of look-up tables and look-up methods.
///
/// `BoardGeometry` contains tables and methods that are needed for
/// implementing move generation and various other board-related
/// problems.
pub struct BoardGeometry {
    grid: [u8; 120],
    piece_grid_deltas: [[i8; 8]; 5],
    piece_longrange: [bool; 5],

    /// Holds attack bitboards for each piece on each possible square.
    /// 
    /// # Examples
    ///
    /// ```text
    /// g.attacks[QUEEN][D4]        g.attacks[KNIGHT][D4] 
    /// . . . 1 . . . 1             . . . . . . . .       
    /// 1 . . 1 . . 1 .             . . . . . . . .       
    /// . 1 . 1 . 1 . .             . . 1 . 1 . . .       
    /// . . 1 1 1 . . .             . 1 . . . 1 . .       
    /// 1 1 1 Q 1 1 1 1             . . . N . . . .       
    /// . . 1 1 1 . . .             . 1 . . . 1 . .       
    /// . 1 . 1 . 1 . .             . . 1 . 1 . . .       
    /// 1 . . 1 . . 1 .             . . . . . . . .       
    /// ```
    attacks: [[u64; 64]; 5],

    /// Holds "blockers and beyond" bitboards for each piece on each
    /// possible square.
    ///
    /// # Examples:
    ///
    /// ```text
    /// g.blockers_and_beyond[KNIGHT][D4]  g.blockers_and_beyond[QUEEN][D4]
    /// . . . . . . . .                    . . . . . . . .
    /// . . . . . . . .                    . . . 1 . . 1 .
    /// . . . . . . . .                    . 1 . 1 . 1 . .
    /// . . . . . . . .                    . . 1 1 1 . . .
    /// . . . N . . . .                    . 1 1 Q 1 1 1 .
    /// . . . . . . . .                    . . 1 1 1 . . .
    /// . . . . . . . .                    . 1 . 1 . 1 . .
    /// . . . . . . . .                    . . . . . . . .
    /// ```
    blockers_and_beyond: [[u64; 64]; 5],

    /// Holds bitsets that describe all squares lying at the line
    /// determined by two squares.
    ///
    /// # Examples
    ///
    /// ```text
    /// g.squares_at_line[B2][F6]
    /// . . . . . . . 1
    /// . . . . . . 1 .
    /// . . . . . 1 . .
    /// . . . . 1 . . .
    /// . . . 1 . . . .
    /// . . 1 . . . . .
    /// . 1 . . . . . .
    /// 1 . . . . . . .
    /// ```
    pub squares_at_line: [[u64; 64]; 64],

    /// Holds bitboards that describe all squares lying between two
    /// squares including the two squares themselves.
    ///
    /// # Examples
    ///
    /// ```text
    /// g.squares_between_including[B2][F6]
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . 1 . .
    /// . . . . 1 . . .
    /// . . . 1 . . . .
    /// . . 1 . . . . .
    /// . 1 . . . . . .
    /// . . . . . . . .
    /// ```
    pub squares_between_including: [[u64; 64]; 64],

    /// Holds bitboards that describe all squares hidden behind a
    /// blocker from the attacker's position.
    ///
    /// # Examples:
    /// 
    /// ```text
    /// g.squares_behind_blocker[B2][F6]
    /// . . . . . . . 1
    /// . . . . . . 1 .
    /// . . . . . B . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . A . . . . . .
    /// . . . . . . . .
    /// ```
    pub squares_behind_blocker: [[u64; 64]; 64],

    /// Used in calculating the Zobrist hash function.
    pub zobrist_pieces: [[[u64; 64]; 6]; 2],

    /// Used in calculating the Zobrist hash function.
    pub zobrist_to_move: u64,
    
    /// Used in calculating the Zobrist hash function.
    pub zobrist_en_passant: [u64; 16],

    /// Used in calculating the Zobrist hash function.
    pub zobrist_castling: [u64; 16],

    /// Derived from `zobrist_pieces`. Contains the constants with
    /// which the Zobrist hash value should be XOR-ed to reflect the
    /// movement of the rook during castling.
    pub zobrist_castling_rook_move: [[u64; 2]; 2],
}


impl BoardGeometry {
    /// Creates and initializes a new instance.
    pub fn new() -> BoardGeometry {
        // We use 10x12 grid (8x8 with guarding markers, 2 at top and
        // bottom, 1 at the sides), so that we can detect out-of-board
        // movements. Each cell in the grid contains the corresponding
        // square number (from 0 to 63) or 0xff (the guarding marker).
        let mut grid = [0xffu8; 120];
        for i in 0..64 {
            grid[grid_index_from_square(i)] = i as u8;
        }

        // "piece_deltas" represent the change in the grid-index when
        // sliding a particular piece by one square in a particular
        // direction. We are not concerned with pawns here.
        let mut piece_grid_deltas = [[0i8; 8]; 5];
        piece_grid_deltas[QUEEN] = [-11, -10, -9, -1, 1, 9, 10, 11];
        piece_grid_deltas[ROOK] = [0, -10, 0, -1, 1, 0, 10, 0];
        piece_grid_deltas[BISHOP] = [-11, 0, -9, 0, 0, 9, 0, 11];
        piece_grid_deltas[KNIGHT] = [-21, -19, -12, -8, 8, 12, 19, 21];
        piece_grid_deltas[KING] = [-11, -10, -9, -1, 1, 9, 10, 11];

        // All pieces except knights and kings are long-range (They
        // can slide by more than one square). We are not concerned
        // with pawns here.
        let mut piece_longrange = [true; 5];
        piece_longrange[KNIGHT] = false;
        piece_longrange[KING] = false;

        let mut bg = BoardGeometry {
            grid: grid,
            piece_grid_deltas: piece_grid_deltas,
            piece_longrange: piece_longrange,
            attacks: [[0; 64]; 5],
            blockers_and_beyond: [[0; 64]; 5],
            squares_at_line: [[0; 64]; 64],
            squares_between_including: [[0; 64]; 64],
            squares_behind_blocker: [[0; 64]; 64],
            zobrist_pieces: [[[0; 64]; 6]; 2],
            zobrist_castling: [0; 16],
            zobrist_castling_rook_move: [[0; 2]; 2],
            zobrist_en_passant: [0; 16],
            zobrist_to_move: 0,
        };

        bg.fill_attack_and_blockers_and_beyond_arrays();
        bg.fill_squares_between_including_and_squares_behind_blocker_arrays();
        bg.fill_squares_at_line_array();
        bg.fill_zobrist_arrays();
        bg
    }

    /// Returns the set of squares that are attacked by a piece (not a
    /// pawn).
    ///
    /// This function returns the set of squares that are attacked by
    /// a piece of type `piece` from the square `from_square`, on a
    /// board which is occupied with other pieces according to the
    /// `occupied` bitboard.
    #[inline]
    pub fn piece_attacks_from(&self, occupied: u64, piece: PieceType, from_square: Square) -> u64 {
        assert!(piece < PAWN);
        assert!(from_square <= 63);
        unsafe {
            let behind: &[u64; 64] = self.squares_behind_blocker.get_unchecked(from_square);
            let mut attacks = *self.attacks.get_unchecked(piece).get_unchecked(from_square);
            let mut blockers = occupied &
                               *self.blockers_and_beyond
                                    .get_unchecked(piece)
                                    .get_unchecked(from_square);
            while blockers != EMPTY_SET {
                attacks &= !*behind.get_unchecked(bitscan_forward_and_reset(&mut blockers));
            }
            attacks
        }
    }

    fn grid_index(&self, i: Square) -> usize {
        grid_index_from_square(i)
    }

    fn fill_attack_and_blockers_and_beyond_arrays(&mut self) {
        for piece in 0..5 {
            for square in 0..64 {
                let mut attack = 0u64;
                let mut blockers = 0u64;
                for move_direction in 0..8 {
                    let delta = self.piece_grid_deltas[piece][move_direction];
                    if delta != 0 {
                        let mut last_mask = 0u64;
                        let mut curr_grid_index = self.grid_index(square);
                        loop {
                            curr_grid_index = (curr_grid_index as i8 + delta) as usize;
                            let curr_square = self.grid[curr_grid_index] as Square;
                            if curr_square != 0xff {
                                last_mask = 1 << curr_square;
                                attack |= last_mask;
                                blockers |= last_mask;
                                if self.piece_longrange[piece] {
                                    continue;
                                }
                            }
                            blockers &= !last_mask;
                            break;
                        }
                    }
                }
                self.attacks[piece][square] = attack;
                self.blockers_and_beyond[piece][square] = blockers;
            }
        }
    }

    fn fill_squares_between_including_and_squares_behind_blocker_arrays(&mut self) {
        for attacker in 0..64 {
            for blocker in 0..64 {
                // Try to find a grid-index increment (delta) that
                // will generate all squares at the line. If the
                // attacker and the blocker happens not to lie at a
                // straight line, then and we simply proceed to the
                // next attacker/blocker pair.
                let rank_diff = rank(blocker) as i8 - rank(attacker) as i8;
                let file_diff = file(blocker) as i8 - file(attacker) as i8;
                let delta = match (rank_diff, file_diff) {
                    (0, 0) => continue,
                    (0, f) => f.signum(),
                    (r, 0) => 10 * r.signum(),
                    (r, f) if r == f => 10 * r.signum() + r.signum(),
                    (r, f) if r == -f => 10 * r.signum() - r.signum(),
                    _ => continue,
                };

                // Starting from the attacker's square update
                // "squares_between_including" until the blocker's
                // square is encountered, then switch to updating
                // "squares_behind_blocker" until the end of the board
                // is reached.
                let mut squares_between_including = 0u64;
                let mut squares_behind_blocker = 0u64;
                let mut curr_grid_index = self.grid_index(attacker);
                let mut blocker_encountered = false;
                loop {
                    let curr_square = self.grid[curr_grid_index] as Square;
                    match curr_square {
                        0xff => {
                            break;
                        }
                        x if x == blocker => {
                            squares_between_including |= 1 << curr_square;
                            blocker_encountered = true;
                        }
                        _ => {
                            if blocker_encountered {
                                squares_behind_blocker |= 1 << curr_square;
                            } else {
                                squares_between_including |= 1 << curr_square;
                            }
                        }
                    }
                    curr_grid_index = (curr_grid_index as i8 + delta) as usize;
                }
                assert!(blocker_encountered);
                self.squares_between_including[attacker][blocker] = squares_between_including;
                self.squares_behind_blocker[attacker][blocker] = squares_behind_blocker;
            }
        }
    }

    fn fill_squares_at_line_array(&mut self) {
        for a in 0..64 {
            for b in 0..64 {
                self.squares_at_line[a][b] = self.squares_between_including[a][b] |
                                             self.squares_behind_blocker[a][b] |
                                             self.squares_behind_blocker[b][a];
            }
        }
    }

    fn fill_zobrist_arrays(&mut self) {
        let seed: &[_] = &[1, 2, 3, 4];
        let mut rng: Isaac64Rng = SeedableRng::from_seed(seed);
        for color in 0..2 {
            for piece in 0..6 {
                for square in 0..64 {
                    self.zobrist_pieces[color][piece][square] = rng.gen();
                }
            }
        }
        for value in 0..16 {
            self.zobrist_castling[value] = rng.gen();
        }
        self.zobrist_castling_rook_move[WHITE][QUEENSIDE] = self.zobrist_pieces[WHITE][ROOK][A1] ^
                                                            self.zobrist_pieces[WHITE][ROOK][D1];
        self.zobrist_castling_rook_move[WHITE][KINGSIDE] = self.zobrist_pieces[WHITE][ROOK][H1] ^
                                                           self.zobrist_pieces[WHITE][ROOK][F1];
        self.zobrist_castling_rook_move[BLACK][QUEENSIDE] = self.zobrist_pieces[BLACK][ROOK][A8] ^
                                                            self.zobrist_pieces[BLACK][ROOK][D8];
        self.zobrist_castling_rook_move[BLACK][KINGSIDE] = self.zobrist_pieces[BLACK][ROOK][H8] ^
                                                           self.zobrist_pieces[BLACK][ROOK][F8];
        for file in 0..8 {
            self.zobrist_en_passant[file] = rng.gen();
        }
        self.zobrist_to_move = rng.gen();
    }
}


#[inline(always)]
fn grid_index_from_square(i: Square) -> usize {
    ((i / 8) * 10 + (i % 8) + 21)
}


#[cfg(test)]
mod tests {
    use super::*;
    use basetypes::*;

    #[test]
    fn test_attack_sets() {
        let g = BoardGeometry::new();
        assert_eq!(g.attacks[KING][A1], 0b11 << 8 | 0b10);
        assert_eq!(g.blockers_and_beyond[KING][A1], 0);
        assert_eq!(g.attacks[ROOK][A1],
                   0b11111110 | 1 << 8 | 1 << 16 | 1 << 24 | 1 << 32 | 1 << 40 | 1 << 48 | 1 << 56);
        assert_eq!(g.blockers_and_beyond[ROOK][A1],
                   0b01111110 | 1 << 8 | 1 << 16 | 1 << 24 | 1 << 32 | 1 << 40 | 1 << 48 | 0 << 56);
        assert_eq!(g.attacks[KING][D4], g.attacks[KING][E4] >> 1);
        assert_eq!(g.attacks[KING][D4], g.attacks[KING][D5] >> 8);
        assert_eq!(g.attacks[KNIGHT][D4], g.attacks[KNIGHT][D5] >> 8);
        assert_eq!(g.attacks[KNIGHT][D4] & g.attacks[KING][D5],
                   1 << C6 | 1 << E6);
        assert_eq!(g.attacks[ROOK][D4] | g.attacks[BISHOP][D4],
                   g.attacks[QUEEN][D4]);
        assert_eq!(g.attacks[ROOK][D4] & g.attacks[BISHOP][D4], 0);
        assert_eq!(g.attacks[KING][D4] & g.attacks[QUEEN][D4],
                   g.attacks[KING][D4]);
        assert_eq!(g.attacks[BISHOP][E1] & g.attacks[KNIGHT][H1],
                   1 << F2 | 1 << G3);
    }

    #[test]
    fn test_line_sets() {
        let g = BoardGeometry::new();
        assert_eq!(g.squares_at_line[B1][G1], 0b11111111);
        assert_eq!(g.squares_at_line[G8][B8], 0b11111111 << 56);
        assert_eq!(g.squares_between_including[B1][G1], 0b01111110);
        assert_eq!(g.squares_between_including[G8][B8], 0b01111110 << 56);
        assert_eq!(g.squares_behind_blocker[B1][G1], 1 << H1);
        assert_eq!(g.squares_behind_blocker[G8][B8], 1 << A8);
        assert_eq!(g.squares_behind_blocker[A1][G7], 1 << H8);
        assert_eq!(g.squares_behind_blocker[H1][B7], 1 << A8);
        assert_eq!(g.squares_behind_blocker[B7][G2], 1 << H1);
        assert_eq!(g.squares_behind_blocker[G7][B2], 1 << A1);
        assert_eq!(g.squares_behind_blocker[D7][D7], 0);
        assert_eq!(g.squares_behind_blocker[D7][F8], 0);
        assert_eq!(g.squares_between_including[A1][A4] | g.squares_behind_blocker[A1][A4],
                   g.squares_at_line[A1][A4]);
    }
}
