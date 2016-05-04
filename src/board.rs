use basetypes::*;
use bitsets::*;

pub type PawnMoveType = usize;

pub const PAWN_PUSH: PawnMoveType = 0;
pub const PAWN_DOUBLE_PUSH: PawnMoveType = 1;
pub const PAWN_QUEENSIDE_CAPTURE: PawnMoveType = 2;
pub const PAWN_KINGSIDE_CAPTURE: PawnMoveType = 3;

pub static PAWN_MOVE_QUIET: [u64; 4] = [UNIVERSAL_SET, UNIVERSAL_SET, EMPTY_SET, EMPTY_SET];
pub static PAWN_MOVE_SHIFTS: [[i8; 4]; 2] = [[8, 16, 7, 9], [-8, -16, -9, -7]];
pub static PAWN_MOVE_CANDIDATES: [u64; 4] = [!(BB_RANK_1 | BB_RANK_8),
                                             BB_RANK_2 | BB_RANK_7,
                                             !(BB_FILE_A | BB_RANK_1 | BB_RANK_8),
                                             !(BB_FILE_H | BB_RANK_1 | BB_RANK_8)];

pub const PAWN_PROMOTION_RANK: [u64; 2] = [BB_RANK_8, BB_RANK_1];



pub struct Board {
    geometry: &'static BoardGeometry,
    pub piece_type: [u64; 6],
    pub color: [u64; 2],
    pub occupied: u64,
}

impl Board {
    // Create a new board instance.
    pub fn new(piece_type: &[u64; 6], color: &[u64; 2]) -> Board {
        assert!(piece_type.into_iter().fold(0, |acc, x| acc | x) == color[WHITE] | color[BLACK]);
        assert!(piece_type[PAWN] & PAWN_PROMOTION_RANK[WHITE] == 0);
        assert!(piece_type[PAWN] & PAWN_PROMOTION_RANK[BLACK] == 0);
        Board {
            geometry: board_geometry(),
            piece_type: *piece_type,
            color: *color,
            occupied: color[WHITE] | color[BLACK],
        }
    }

    // Return the set of squares that are attacked by a piece (not a
    // pawn) of type "piece" from the square "square".
    #[inline]
    pub fn piece_attacks_from(&self, square: Square, piece: PieceType) -> u64 {
        piece_attacks_from(self.geometry, self.occupied, square, piece)
    }

    // Return the set of squares that have on them pieces (or pawns)
    // of color "us" that attack the square "square" directly (no
    // x-rays).
    pub fn attacks_to(&self, square: Square, us: Color) -> u64 {
        let occupied_by_us = self.color[us];
        let mut attacks = EMPTY_SET;
        attacks |= self.piece_attacks_from(square, ROOK) & occupied_by_us &
                   (self.piece_type[ROOK] | self.piece_type[QUEEN]);
        attacks |= self.piece_attacks_from(square, BISHOP) & occupied_by_us &
                   (self.piece_type[BISHOP] | self.piece_type[QUEEN]);
        attacks |= self.piece_attacks_from(square, KNIGHT) & occupied_by_us &
                   self.piece_type[KNIGHT];
        attacks |= self.piece_attacks_from(square, KING) & occupied_by_us & self.piece_type[KING];
        attacks |= gen_shift(1 << square, -PAWN_MOVE_SHIFTS[us][PAWN_KINGSIDE_CAPTURE]) &
                   occupied_by_us & self.piece_type[PAWN] & !BB_FILE_H;
        attacks |= gen_shift(1 << square, -PAWN_MOVE_SHIFTS[us][PAWN_QUEENSIDE_CAPTURE]) &
                   occupied_by_us & self.piece_type[PAWN] & !BB_FILE_A;
        attacks
    }

    // A Static Exchange Evaluation (SEE) examines the consequence of
    // a series of exchanges on a single square after a given move,
    // and calculates the likely evaluation change (material) to be
    // lost or gained, Donald Michie coined the term swap-off value. A
    // positive static exchange indicates a "winning" move. For
    // example, PxQ will always be a win, since the Pawn side can
    // choose to stop the exchange after its Pawn is recaptured, and
    // still be ahead.
    //
    // The impemented algorithm creates a swap-list of best case
    // material gains by traversing a square attacked/defended by set
    // in least valuable piece order from pawn, knight, bishop, rook,
    // queen until king, with alternating sides. The swap-list, an
    // unary tree since there are no branches but just a series of
    // captures, is negamaxed for a final static exchange evaluation.
    //
    // The returned value is the material that is expected to be
    // gained in the exchange by the attacking side
    // ("attacking_color"), when capturing the "target_piece" on the
    // "target_square". The "from_square" specifies the square from
    // which the "attacking_piece" makes the capture.
    pub fn calc_see(&self,
                    from_square: Square,
                    mut attacking_piece: PieceType,
                    mut attacking_color: Color,
                    to_square: Square,
                    target_piece: PieceType)
                    -> Value {
        use std::mem::uninitialized;
        use std::cmp::max;
        static VALUE: [Value; 6] = [10000, 975, 500, 325, 325, 100];

        // "may_xray" pieces may block x-ray attacks from other
        // pieces, so we must consider adding new attackers/defenders
        // every time a "may_xray"-piece makes a capture.
        let may_xray = self.piece_type[PAWN] | self.piece_type[BISHOP] | self.piece_type[ROOK] |
                       self.piece_type[QUEEN];
        let mut depth = 0;
        let mut occupied = self.occupied;
        let mut attackers_and_defenders = self.attacks_to(to_square, WHITE) |
                                          self.attacks_to(to_square, BLACK);
        let mut from_square_bb = 1 << from_square;
        unsafe {
            let mut gain: [Value; 33] = uninitialized();
            gain[depth] = VALUE[target_piece];
            while from_square_bb != EMPTY_SET {
                depth += 1;  // next depth
                attacking_color ^= 1;  // next side
                gain[depth] = VALUE[attacking_piece] - gain[depth - 1];  // speculative store, if defended
                if max(-gain[depth - 1], gain[depth]) < 0 {
                    break;  // pruning does not influence the outcome
                }
                attackers_and_defenders ^= from_square_bb;
                occupied ^= from_square_bb;
                if from_square_bb & may_xray != EMPTY_SET {
                    attackers_and_defenders |= self.consider_xrays(occupied,
                                                                   to_square,
                                                                   bitscan_forward(from_square_bb));
                }
                assert_eq!(occupied | attackers_and_defenders, occupied);
                // Find the next piece in the exchange:
                let next_attack =
                    self.get_least_valuable_piece_in_a_set(attackers_and_defenders &
                                                           self.color[attacking_color]);
                attacking_piece = next_attack.0;
                from_square_bb = next_attack.1;
            }
            depth -= 1;  // discard the speculative store
            while depth > 0 {
                gain[depth - 1] = -max(-gain[depth - 1], gain[depth]);
                depth -= 1;
            }
            gain[0]
        }
    }

    // Generate all legal moves in the current board position.
    //
    // "checkers" should represent all pieces that give
    // check. "pinned" should represent all pinned pieces (and
    // pawns). "side" is the side to move. "castling_rights" gives the
    // current castling rights. "en_passant_bb" is a bitboard that
    // contains 1 for the passing square (if there is one).
    // "move_stack" gives a slice to the global array of moves.
    //
    // Returns the number of moves that have been generated.
    pub fn generate_moves(&self,
                          checkers: u64,
                          pinned: u64,
                          side: Color,
                          castling_rights: CastlingRights,
                          en_passant_bb: u64,
                          move_stack: &mut [MoveAndMoveScore])
                          -> usize {
        0
    }

    #[inline]
    fn consider_xrays(&self, occupied: u64, target_square: Square, xrayed_square: Square) -> u64 {
        let candidates = occupied &
                         self.geometry.squares_behind_blocker[target_square][xrayed_square];
        let diag_attackers = piece_attacks_from(&self.geometry, candidates, target_square, BISHOP) &
                             (self.piece_type[QUEEN] | self.piece_type[BISHOP]);
        let line_attackers = piece_attacks_from(&self.geometry, candidates, target_square, ROOK) &
                             (self.piece_type[QUEEN] | self.piece_type[ROOK]);
        assert_eq!(diag_attackers & line_attackers, EMPTY_SET);
        assert_eq!(ls1b(candidates & diag_attackers),
                   candidates & diag_attackers);
        assert_eq!(ls1b(candidates & line_attackers),
                   candidates & line_attackers);
        candidates & (diag_attackers | line_attackers)
    }

    #[inline]
    fn get_least_valuable_piece_in_a_set(&self, set: u64) -> (PieceType, u64) {
        for p in (0..6).rev() {
            let piece_subset = self.piece_type[p] & set;
            if piece_subset != EMPTY_SET {
                return (p, ls1b(piece_subset));
            }
        }
        (NO_PIECE, EMPTY_SET)
    }

    #[inline]
    fn piece_legal_dest_squares(&self,
                                from_square: Square,
                                piece: PieceType,
                                color: Color,
                                pinned: Option<Square>)
                                -> u64 {
        assert!(piece != PAWN);
        let pseudo_legal = self.piece_attacks_from(from_square, piece) & !self.color[color];
        match pinned {
            Some(king_square) => {
                pseudo_legal & self.geometry.squares_at_line[from_square][king_square]
            }
            None => pseudo_legal,
        }
    }

    #[inline]
    pub fn pawn_dest_sets(&self, pawns: u64, us: Color, en_passant_bb: u64) -> [u64; 4] {
        use std::mem::uninitialized;
        let shifts = &PAWN_MOVE_SHIFTS[us];
        let not_occupied_by_us = !self.color[us];
        let capture_targets = self.color[1 ^ us] | en_passant_bb;
        unsafe {
            let mut dest_sets: [u64; 4] = uninitialized();
            for move_type in 0..4 {
                dest_sets[move_type] = gen_shift(pawns & PAWN_MOVE_CANDIDATES[move_type],
                                                 shifts[move_type]) &
                                       not_occupied_by_us &
                                       (capture_targets ^ PAWN_MOVE_QUIET[move_type]);
            }
            // A double-push is legal only if a single-push is legal too.
            dest_sets[PAWN_DOUBLE_PUSH] &= gen_shift(dest_sets[PAWN_PUSH], shifts[PAWN_PUSH]);
            dest_sets
        }
    }
}

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

pub struct BoardGeometry {
    grid: [u8; 120],
    piece_grid_deltas: [[i8; 8]; 5],
    piece_longrange: [bool; 5],
    pub attacks: [[u64; 64]; 5],
    pub blockers_and_beyond: [[u64; 64]; 5],
    pub squares_at_line: [[u64; 64]; 64],
    pub squares_between_including: [[u64; 64]; 64],
    pub squares_behind_blocker: [[u64; 64]; 64],
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
            attacks: [[0u64; 64]; 5],
            blockers_and_beyond: [[0u64; 64]; 5],
            squares_at_line: [[0u64; 64]; 64],
            squares_between_including: [[0u64; 64]; 64],
            squares_behind_blocker: [[0u64; 64]; 64],
        };

        // "attacks" and "blockers_and_beyond" fields hold attack and
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
        bg.fill_attack_and_blockers_and_beyond_arrays();

        // The "squares_behind_blocker" field holds bitsets that
        // describe all squares hidden behind a blocker from the
        // attacker's position. For example:
        //
        // g.squares_behind_blocker[B2][F6]
        // . . . . . . . 1
        // . . . . . . 1 .
        // . . . . . B . .
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . . . .
        // . A . . . . . .
        // . . . . . . . .
        //
        // The "squares_between_including" field holds bitsets that
        // describe all squares between an attacker an a blocker
        // including the attacker's and blocker's fields
        // themselves. For example:
        //
        // g.squares_between_including[B2][F6]
        // . . . . . . . .
        // . . . . . . . .
        // . . . . . 1 . .
        // . . . . 1 . . .
        // . . . 1 . . . .
        // . . 1 . . . . .
        // . 1 . . . . . .
        // . . . . . . . .
        bg.fill_squares_between_including_and_squares_behind_blocker_arrays();

        // The "squares_at_line" field holds bitsets that describe all
        // squares lying at the line determined by the attacker and
        // the blocker. For example:
        //
        // g.squares_at_line[B2][F6]
        // . . . . . . . 1
        // . . . . . . 1 .
        // . . . . . 1 . .
        // . . . . 1 . . .
        // . . . 1 . . . .
        // . . 1 . . . . .
        // . 1 . . . . . .
        // 1 . . . . . . .
        bg.fill_squares_at_line_array();

        bg
    }

    fn grid_index(&self, i: Square) -> usize {
        Self::grid_index_from_square(i)
    }

    #[inline(always)]
    fn grid_index_from_square(i: Square) -> usize {
        ((i / 8) * 10 + (i % 8) + 21)
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
}


// Return the set of squares that are attacked by a piece (not a pawn)
// of type "piece" from the square "square", on a board which is
// occupied with other pieces according to the "occupied"
// bit-set. "geometry" supplies the look-up tables needed to perform
// the calculation.
#[inline]
pub fn piece_attacks_from(geometry: &BoardGeometry,
                          occupied: u64,
                          square: Square,
                          piece: PieceType)
                          -> u64 {
    assert!(piece != PAWN);
    let mut attacks = geometry.attacks[piece][square];
    let mut blockers = occupied & geometry.blockers_and_beyond[piece][square];
    while blockers != EMPTY_SET {
        let blocker_square = bitscan_and_clear(&mut blockers);
        attacks &= !geometry.squares_behind_blocker[square][blocker_square];
    }
    attacks
}


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
    fn test_attack_sets() {
        use basetypes::*;
        let g = board_geometry();
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
        use basetypes::*;
        let g = board_geometry();
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

    #[test]
    fn test_attacks_from() {
        use basetypes::*;
        let mut piece_type = [0u64; 6];
        let mut color = [0u64; 2];
        piece_type[PAWN] |= 1 << D4;
        piece_type[PAWN] |= 1 << G7;
        color[WHITE] = piece_type[PAWN];
        let b = Board::new(&piece_type, &color);
        assert_eq!(b.piece_attacks_from(A1, BISHOP),
                   1 << B2 | 1 << C3 | 1 << D4);
        assert_eq!(b.piece_attacks_from(A1, BISHOP),
                   1 << B2 | 1 << C3 | 1 << D4);
        assert_eq!(b.piece_attacks_from(A1, KNIGHT), 1 << B3 | 1 << C2);
    }

    #[test]
    fn test_attacks_to() {
        use basetypes::*;
        let mut piece_type = [0u64; 6];
        let mut color = [0u64; 2];
        piece_type[PAWN] |= 1 << D3;
        color[WHITE] |= 1 << D3;
        piece_type[PAWN] |= 1 << H5;
        color[WHITE] |= 1 << H5;
        piece_type[KNIGHT] |= 1 << G3;
        color[WHITE] |= 1 << G3;
        piece_type[BISHOP] |= 1 << B1;
        color[WHITE] |= 1 << B1;
        piece_type[QUEEN] |= 1 << H1;
        color[WHITE] |= 1 << H1;
        piece_type[KING] |= 1 << D5;
        color[WHITE] |= 1 << D5;
        piece_type[PAWN] |= 1 << H2;
        color[BLACK] |= 1 << H2;
        piece_type[PAWN] |= 1 << F5;
        color[BLACK] |= 1 << F5;
        piece_type[ROOK] |= 1 << A4;
        color[BLACK] |= 1 << A4;
        piece_type[QUEEN] |= 1 << E3;
        color[BLACK] |= 1 << E3;
        piece_type[KING] |= 1 << F4;
        color[BLACK] |= 1 << F4;
        let b = Board::new(&piece_type, &color);
        assert_eq!(b.attacks_to(E4, WHITE),
                   1 << D3 | 1 << G3 | 1 << D5 | 1 << H1);
        assert_eq!(b.attacks_to(E4, BLACK),
                   1 << E3 | 1 << F4 | 1 << F5 | 1 << A4);
        assert_eq!(b.attacks_to(G6, BLACK), 0);
        assert_eq!(b.attacks_to(G6, WHITE), 1 << H5);
        assert_eq!(b.attacks_to(C2, WHITE), 1 << B1);
        assert_eq!(b.attacks_to(F4, WHITE), 0);
        assert_eq!(b.attacks_to(F4, BLACK), 1 << A4 | 1 << E3);
        assert_eq!(b.attacks_to(F5, BLACK), 1 << F4);
        assert_eq!(b.attacks_to(A6, WHITE), 0);
        assert_eq!(b.attacks_to(G1, BLACK), 1 << H2 | 1 << E3);
        assert_eq!(b.attacks_to(A1, BLACK), 1 << A4);
    }

    #[test]
    fn test_piece_type_constants_constraints() {
        use basetypes::*;
        assert_eq!(KING, 0);
        assert_eq!(QUEEN, 1);
        assert_eq!(ROOK, 2);
        assert_eq!(BISHOP, 3);
        assert_eq!(KNIGHT, 4);
        assert_eq!(PAWN, 5);
    }

    #[test]
    fn test_static_exchange_evaluation() {
        use basetypes::*;
        let mut piece_type = [0u64; 6];
        let mut color = [0u64; 2];
        piece_type[KING] |= 1 << A3;
        color[BLACK] |= 1 << A3;
        piece_type[QUEEN] |= 1 << E5;
        color[BLACK] |= 1 << E5;
        piece_type[ROOK] |= 1 << F8;
        color[BLACK] |= 1 << F8;
        piece_type[BISHOP] |= 1 << D2;
        color[BLACK] |= 1 << D2;
        piece_type[PAWN] |= 1 << G5;
        color[BLACK] |= 1 << G5;
        piece_type[KING] |= 1 << A1;
        color[WHITE] |= 1 << A1;
        piece_type[PAWN] |= 1 << A2;
        color[WHITE] |= 1 << A2;
        piece_type[PAWN] |= 1 << E3;
        color[WHITE] |= 1 << E3;
        piece_type[PAWN] |= 1 << G3;
        color[WHITE] |= 1 << G3;
        piece_type[PAWN] |= 1 << D4;
        color[WHITE] |= 1 << D4;
        piece_type[BISHOP] |= 1 << H2;
        color[WHITE] |= 1 << H2;
        piece_type[ROOK] |= 1 << F1;
        color[WHITE] |= 1 << F1;
        piece_type[ROOK] |= 1 << F2;
        color[WHITE] |= 1 << F2;
        let b = Board::new(&piece_type, &color);
        assert_eq!(b.calc_see(E5, QUEEN, BLACK, E3, PAWN), 100);
        assert_eq!(b.calc_see(E5, QUEEN, BLACK, D4, PAWN), -875);
        assert_eq!(b.calc_see(G3, PAWN, WHITE, F4, PAWN), 100);
        assert_eq!(b.calc_see(A3, KING, BLACK, A2, PAWN), -9900);
    }

    #[test]
    fn test_move_scores() {
        use basetypes::*;
        let mut ms = MoveScore::new(PAWN, QUEEN);
        assert_eq!(ms.attacking_piece(), PAWN);
        assert_eq!(ms.target_piece(), QUEEN);
        assert!(ms > MoveScore::new(KNIGHT, QUEEN));
        assert!(ms > MoveScore::new(PAWN, ROOK));
        assert_eq!(ms, MoveScore::new(PAWN, QUEEN));
        let ms2 = ms;
        assert_eq!(ms, ms2);
        ms.set_bit(6);
        assert!(ms > ms2);
        assert_eq!(ms.attacking_piece(), PAWN);
        assert_eq!(ms.target_piece(), QUEEN);
        ms.clear_bit(6);
        assert_eq!(ms, ms2);
    }

    #[test]
    fn test_pawn_dest_sets() {
        use basetypes::*;
        let mut piece_type = [0u64; 6];
        let mut color = [0u64; 2];
        piece_type[PAWN] |= 1 << E7;
        color[WHITE] |= 1 << E7;
        piece_type[PAWN] |= 1 << H2;
        color[WHITE] |= 1 << H2;
        piece_type[PAWN] |= 1 << G4;
        color[WHITE] |= 1 << G4;
        piece_type[PAWN] |= 1 << G5;
        color[WHITE] |= 1 << G5;
        piece_type[PAWN] |= 1 << F6;
        color[WHITE] |= 1 << F6;
        piece_type[PAWN] |= 1 << F7;
        color[BLACK] |= 1 << F7;
        piece_type[PAWN] |= 1 << G7;
        color[BLACK] |= 1 << G7;
        piece_type[PAWN] |= 1 << H5;
        color[BLACK] |= 1 << H5;
        piece_type[QUEEN] |= 1 << D8;
        color[BLACK] |= 1 << D8;
        let b = Board::new(&piece_type, &color);
        let ds = b.pawn_dest_sets(b.piece_type[PAWN] & b.color[WHITE], WHITE, 1 << H6);
        assert_eq!(ds[PAWN_PUSH], 1 << H3 | 1 << G6 | 1 << E8);
        assert_eq!(ds[PAWN_DOUBLE_PUSH], 1 << H4);
        assert_eq!(ds[PAWN_KINGSIDE_CAPTURE], 1 << H5 | 1 << G7 | 1 << H6);
        assert_eq!(ds[PAWN_QUEENSIDE_CAPTURE], 1 << D8);
        let ds = b.pawn_dest_sets(b.piece_type[PAWN] & b.color[BLACK], BLACK, 0);
        assert_eq!(ds[PAWN_PUSH], 1 << H4 | 1 << G6);
        assert_eq!(ds[PAWN_DOUBLE_PUSH], 0);
        assert_eq!(ds[PAWN_KINGSIDE_CAPTURE], 0);
        assert_eq!(ds[PAWN_QUEENSIDE_CAPTURE], 1 << G4 | 1 << F6);
    }

}
