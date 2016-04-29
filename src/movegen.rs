use basetypes::*;

pub struct BoardGeometry {
    pub grid: [u8; 120],
    pub piece_deltas: [[i8; 8]; 5],
    pub piece_longrange: [bool; 5],
    pub attacks: [[u64; 64]; 5],
    pub blockers_and_beyond: [[u64; 64]; 5],
}

impl BoardGeometry {
    pub fn new() -> BoardGeometry {
        // We use 10x12 grid (8x8 with guarding markers, 2 at top and
        // bottom, 1 at the sides), so that we can detect out-of-board
        // movements. Each cell in the grid contains the corresponding
        // square number (from 0 to 63) or 0xff (the guarding marker).
        let mut grid = [0xffu8; 120];
        for i in 0..64 {
            grid[BoardGeometry::grid_index_from_square(i)] = i as u8;
        }

        // "piece_deltas" represent the change in the grid-index when
        // sliding a particular piece by one square in a particular
        // direction. We are not concerned with pawns here.
        let mut piece_deltas = [[0i8; 8]; 5];
        piece_deltas[QUEEN] = [-11, -10, -9, -1, 1, 9, 10, 11];
        piece_deltas[ROOK] = [0, -10, 0, -1, 1, 0, 10, 0];
        piece_deltas[BISHOP] = [-11, 0, -9, 0, 0, 9, 0, 11];
        piece_deltas[KNIGHT] = [-21, -19, -12, -8, 8, 12, 19, 21];
        piece_deltas[KING] = [-11, -10, -9, -1, 1, 9, 10, 11];

        // All pieces except knights and kings are long-range (They
        // can slide by more than one square). We are not concerned
        // with pawns here.
        let mut piece_longrange = [true; 5];
        piece_longrange[KNIGHT] = false;
        piece_longrange[KING] = false;

        let mut g = BoardGeometry {
            grid: grid,
            piece_deltas: piece_deltas,
            piece_longrange: piece_longrange,
            attacks: [[0u64; 64]; 5],
            blockers_and_beyond: [[0u64; 64]; 5],
        };

        // "attacks" and "blockers_and_beyond" fileds hold attack and
        // blockers bitsets for each piece on each possible square.
        // For example:
        //
        // g.attacks[QUEEN][D4]  g.blockers_and_beyond[QUEEN][D4]
        // . . . 1 . . . 1       . . . . . . . .
        // 1 . . 1 . . 1 .       . . . 1 . . 1 .
        // . 1 . 1 . 1 . .       . 1 . 1 . 1 . .
        // . . 1 1 1 . . .       . . 1 1 1 . . .
        // 1 1 1 Q 1 1 1 1       . 1 1 Q 1 1 1 .
        // . . 1 1 1 . . .       . . 1 1 1 . . .
        // . 1 . 1 . 1 . .       . 1 . 1 . 1 . .
        // 1 . . 1 . . 1 .       . . . . . . . .
        // 
        // g.attacks[KNIGHT][D4] g.blockers_and_beyond[KNIGHT][D4]
        // . . . . . . . .       . . . . . . . .
        // . . . . . . . .       . . . . . . . .
        // . . 1 . 1 . . .       . . . . . . . .
        // . 1 . . . 1 . .       . . . . . . . .
        // . . . N . . . .       . . . N . . . .
        // . 1 . . . 1 . .       . . . . . . . .
        // . . 1 . 1 . . .       . . . . . . . .
        // . . . . . . . .       . . . . . . . .
        g.fill_attack_and_blockers_and_beyond_arrays();

        g
    }

    pub fn grid_index(&self, i: Square) -> usize {
        Self::grid_index_from_square(i)
    }

    #[inline(always)]
    fn grid_index_from_square(i: Square) -> usize {
        ((i / 8) * 10 + (i % 8) + 21)
    }

    fn fill_attack_and_blockers_and_beyond_arrays(&mut self) {
        for piece_type in 0..5 {
            for square in 0..64 {
                let mut attack = 0u64;
                let mut blockers = 0u64;
                for move_direction in 0..8 {
                    let delta = self.piece_deltas[piece_type][move_direction];
                    if delta != 0 {
                        let mut last_mask = 0u64;
                        let mut curr_grid_index = self.grid_index(square);
                        loop {
                            curr_grid_index = (curr_grid_index as i8 + delta) as usize;
                            let curr_square = self.grid[curr_grid_index];
                            if curr_square != 0xff {
                                last_mask = 1 << curr_square;
                                attack |= last_mask;
                                blockers |= last_mask;
                                if self.piece_longrange[piece_type] {
                                    continue;
                                }
                            }
                            blockers &= !last_mask;
                            break;
                        }
                    }
                }
                self.attacks[piece_type][square] = attack;
                self.blockers_and_beyond[piece_type][square] = blockers;
            }
        }
    }
}

// Attack (or blockers and beyond) array for all sliding pieces.
pub type SlidersArray = [[u64; 64]; 5];


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


pub type LinesArray = [[u64; 64]; 64];

// pub fn generate_behind_and_between_arrays() -> LinesArray {

// }


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_attack_sets() {
        use basetypes::*;
        let g = BoardGeometry::new();
        let (att_sets, bl_sets) = (g.attacks, g.blockers_and_beyond);
        assert_eq!(att_sets[KING][0], 0b11 << 8 | 0b10);
        assert_eq!(bl_sets[KING][0], 0);
        assert_eq!(att_sets[ROOK][0],
                   0b11111110 | 1 << 8 | 1 << 16 | 1 << 24 | 1 << 32 | 1 << 40 | 1 << 48 | 1 << 56);
        assert_eq!(bl_sets[ROOK][0],
                   0b01111110 | 1 << 8 | 1 << 16 | 1 << 24 | 1 << 32 | 1 << 40 | 1 << 48 | 0 << 56);
    }

}
