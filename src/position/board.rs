use basetypes::*;
use bitsets::*;
use super::board_geometry::BoardGeometry;
use super::chess_move::{Move, MoveStack};

type PawnMoveType = usize;

// Pawn move types
const PAWN_PUSH: PawnMoveType = 0;
const PAWN_DOUBLE_PUSH: PawnMoveType = 1;
const PAWN_QUEENSIDE_CAPTURE: PawnMoveType = 2;
const PAWN_KINGSIDE_CAPTURE: PawnMoveType = 3;

// Pawn move tables
static PAWN_MOVE_SHIFTS: [[isize; 4]; 2] = [[8, 16, 7, 9], [-8, -16, -9, -7]];
const PAWN_MOVE_QUIET: [u64; 4] = [UNIVERSAL_SET, UNIVERSAL_SET, EMPTY_SET, EMPTY_SET];
const PAWN_MOVE_CANDIDATES: [u64; 4] = [!(BB_RANK_1 | BB_RANK_8),
                                        BB_RANK_2 | BB_RANK_7,
                                        !(BB_FILE_A | BB_RANK_1 | BB_RANK_8),
                                        !(BB_FILE_H | BB_RANK_1 | BB_RANK_8)];

// Pawn useful constants
const PAWN_PROMOTION_RANKS: u64 = BB_RANK_1 | BB_RANK_8;


pub struct PiecesPlacement {
    pub piece_type: [u64; 6],
    pub color: [u64; 2],
}


pub struct IllegalBoard;


pub struct Board {
    pub geometry: &'static BoardGeometry,
    pub piece_type: [u64; 6],
    pub color: [u64; 2],
    pub en_passant_file: File,
    pub castling: CastlingRights,
    pub to_move: Color,
}

impl Board {
    // Create a new board instance.
    pub fn create(placement: &PiecesPlacement,
                  en_passant_square: Option<Square>,
                  castling: CastlingRights,
                  to_move: Color)
                  -> Result<Board, IllegalBoard> {
        let en_passant_rank = match to_move {
            WHITE => RANK_6,
            BLACK => RANK_3,
            _ => return Err(IllegalBoard),
        };
        let en_passant_file = match en_passant_square {
            None => NO_ENPASSANT_FILE,
            Some(x) if x <= 63 && rank(x) == en_passant_rank => file(x),
            _ => return Err(IllegalBoard),
        };
        let b = Board {
            geometry: board_geometry(),
            piece_type: placement.piece_type,
            color: placement.color,
            en_passant_file: en_passant_file,
            castling: castling,
            to_move: to_move,
        };
        if b.is_legal() {
            Ok(b)
        } else {
            Err(IllegalBoard)
        }

    }

    pub fn do_move(&mut self, m: Move) -> bool {
        let us = self.to_move;
        let them = 1 ^ us;
        let move_type = m.move_type();
        let orig_square = m.orig_square();
        let dest_square = m.dest_square();
        let aux_data = m.aux_data();
        let castling_data = m.castling_data();
        let piece = m.piece();
        let captured_piece = m.captured_piece();
        assert!(us <= 1);
        assert!(piece < NO_PIECE);
        assert!(captured_piece <= NO_PIECE);

        if piece == KING && self.attacks_to(them, dest_square) != EMPTY_SET {
            return false;  // the king is in check -- illegal move
        }

        let not_orig_bb = !(1 << orig_square);
        let dest_bb = 1 << dest_square;
        let piece_type_array = &mut self.piece_type;
        let color_array = &mut self.color;

        // empty the origin square
        unsafe {
            *piece_type_array.get_unchecked_mut(piece) &= not_orig_bb;
            *color_array.get_unchecked_mut(us) &= not_orig_bb;
        }

        // remove the captured piece (and update the en-passant file)
        self.en_passant_file = NO_ENPASSANT_FILE;
        if captured_piece < NO_PIECE {
            let not_captured_bb = if move_type == MOVE_ENPASSANT {
                self.en_passant_file = file(dest_square);
                !gen_shift(dest_bb, PAWN_MOVE_SHIFTS[them][PAWN_PUSH])
            } else {
                !dest_bb
            };
            unsafe {
                *piece_type_array.get_unchecked_mut(captured_piece) &= not_captured_bb;
                *color_array.get_unchecked_mut(them) &= not_captured_bb;
            }
        }

        // occupy the destination square (and shift the rook if the move is castling)
        let dest_piece = if move_type == MOVE_PROMOTION {
            Move::piece_from_aux_data(aux_data)
        } else {
            piece
        };
        unsafe {
            *piece_type_array.get_unchecked_mut(dest_piece) |= dest_bb;
            *color_array.get_unchecked_mut(us) |= dest_bb;
        }
        if move_type == MOVE_CASTLING {
            let side = if dest_square > orig_square {
                KINGSIDE
            } else {
                QUEENSIDE
            };
            let mask = self.castling.rook_xor_mask(us, side);
            unsafe {
                *piece_type_array.get_unchecked_mut(ROOK) ^= mask;
                *color_array.get_unchecked_mut(us) ^= mask;
            }
        }

        // update castling rights
        self.castling.0 &= unsafe {
            *self.geometry.castling_relation.get_unchecked(orig_square) &
            *self.geometry.castling_relation.get_unchecked(dest_square)
        };

        // change the side to move
        self.to_move = them;
        true
    }

    // Return the set of squares that have on them pieces (or pawns)
    // of color "us" that attack the square "square" directly (no
    // x-rays).
    pub fn attacks_to(&self, us: Color, square: Square) -> u64 {
        attacks_to(self.geometry,
                   &self.piece_type,
                   &self.color,
                   self.color[WHITE] | self.color[BLACK],
                   us,
                   square)
    }

    // Analyzes the board and decides if it is a legal board.
    //
    // In addition to the obviously wrong boards (that for example
    // declare some pieces having no or more than one color), there
    // are many chess boards that are impossible to create from the
    // starting chess position. Here we are interested to detect and
    // guard against only those of the cases that have a chance of
    // disturbing some of our explicit and unavoidably, implicit
    // presumptions about what a chess position is when writing the
    // code.
    //
    // Invalid boards: 1. having more or less than 1 king from each
    // color; 2. having more than 8 pawns of a color; 3. having more
    // than 16 pieces (and pawns) of one color; 4. having the side not
    // to move in check; 5. having pawns on ranks 1 or 8; 6. having
    // castling rights when the king or the corresponding rook is not
    // on its initial square; 7. having an en-passant square that is
    // not on 3/6-th rank, or not having a pawn of corresponding color
    // before, and an empty square on it and behind it; 8. having an
    // en-passant square while the wrong side is to move; 9. having an
    // en-passant square while the king is in check not from the
    // passing pawn and not from a checker that was discovered by the
    // passing pawn.
    pub fn is_legal(&self) -> bool {
        if self.to_move > 1 {
            return false;
        }
        let us = self.to_move;
        let en_passant_bb = match self.en_passant_file {
            NO_ENPASSANT_FILE => EMPTY_SET,
            x if x <= 7 => {
                match us {
                    WHITE => 1 << x << 40,
                    BLACK => 1 << x << 16,
                    _ => panic!("invalid color to move"),
                }
            }
            _ => return false,
        };
        let occupied = self.piece_type.into_iter().fold(0, |acc, x| {
            if acc & x == 0 {
                acc | x
            } else {
                UNIVERSAL_SET
            }
        });  // returns "UNIVERSAL_SET" if "self.piece_type" is messed up

        let them = 1 ^ us;
        let o_us = self.color[us];
        let o_them = self.color[them];
        let our_king_bb = self.piece_type[KING] & o_us;
        let their_king_bb = self.piece_type[KING] & o_them;
        let pawns = self.piece_type[PAWN];

        occupied != UNIVERSAL_SET && occupied == o_us | o_them && o_us & o_them == 0 &&
        pop_count(our_king_bb) == 1 && pop_count(their_king_bb) == 1 &&
        pop_count(pawns & o_us) <= 8 &&
        pop_count(pawns & o_them) <= 8 && pop_count(o_us) <= 16 &&
        pop_count(o_them) <= 16 &&
        self.attacks_to(us, bitscan_forward(their_king_bb)) == 0 &&
        pawns & PAWN_PROMOTION_RANKS == 0 &&
        (!self.castling.can_castle(WHITE, QUEENSIDE) ||
         (self.piece_type[ROOK] & self.color[WHITE] & 1 << A1 != 0) &&
         (self.piece_type[KING] & self.color[WHITE] & 1 << E1 != 0)) &&
        (!self.castling.can_castle(WHITE, KINGSIDE) ||
         (self.piece_type[ROOK] & self.color[WHITE] & 1 << H1 != 0) &&
         (self.piece_type[KING] & self.color[WHITE] & 1 << E1 != 0)) &&
        (!self.castling.can_castle(BLACK, QUEENSIDE) ||
         (self.piece_type[ROOK] & self.color[BLACK] & 1 << A8 != 0) &&
         (self.piece_type[KING] & self.color[BLACK] & 1 << E8 != 0)) &&
        (!self.castling.can_castle(BLACK, KINGSIDE) ||
         (self.piece_type[ROOK] & self.color[BLACK] & 1 << H8 != 0) &&
         (self.piece_type[KING] & self.color[BLACK] & 1 << E8 != 0)) &&
        (en_passant_bb == EMPTY_SET ||
         {
            let dest_square_bb = gen_shift(en_passant_bb, PAWN_MOVE_SHIFTS[them][PAWN_PUSH]);
            let orig_square_bb = gen_shift(en_passant_bb, -PAWN_MOVE_SHIFTS[them][PAWN_PUSH]);
            let our_king_square = bitscan_forward(our_king_bb);
            let checkers = self.attacks_to(them, our_king_square);
            ([BB_RANK_6, BB_RANK_3][us] & en_passant_bb != 0) &&
            (dest_square_bb & pawns & o_them != 0) &&
            (en_passant_bb & !occupied != 0) && (orig_square_bb & !occupied != 0) &&
            (checkers == EMPTY_SET || checkers == dest_square_bb ||
             (pop_count(checkers) == 1 &&
              self.geometry.squares_between_including[our_king_square][bitscan_forward(checkers)] &
              orig_square_bb != 0))
        })
    }

    // Generate pseudo-legal moves in the current board position.
    //
    // It is guaranteed that all legal moves will be found. It is also
    // guaranteed, that all generated moves with pieces other than the
    // king are legal. *It is possible that some of the king's moves
    // are illegal because the destination square is under
    // check*. This is because verifying that all king destination
    // squares are not under attack is quite expensive, and therefore
    // we hope that the alpha-beta pruning will eliminate the need for
    // this verification at all.
    //
    // "us" is the side to move. "king_square" should be the moving
    // side king's square. "checkers" should represent all pieces that
    // give check. "pinned" should represent all pinned pieces (and
    // pawns). "castling" gives the current castling
    // rights. "en_passant_bb" is a bitboard that contains 1 for the
    // passing square (if there is one). "move_stack" is the global
    // moves stack.
    //
    // Returns the number of moves that have been generated.
    pub fn generate_pseudolegal_moves(&self,
                                      us: Color,
                                      king_square: Square,
                                      checkers: u64,
                                      pinned: u64,
                                      en_passant_bb: u64,
                                      castling: CastlingRights,
                                      move_stack: &mut MoveStack)
                                      -> usize {
        assert!(us <= 1);
        assert!(king_square <= 63);
        let mut counter = 0;
        let geometry = self.geometry;
        let piece_type_array = &self.piece_type;
        let color_array = &self.color;
        let occupied = color_array[WHITE] | color_array[BLACK];
        let occupied_by_us = unsafe { *color_array.get_unchecked(us) };
        let occupied_by_them = unsafe { *color_array.get_unchecked(1 ^ us) };
        let not_occupied_by_us = !occupied_by_us;
        let en_passant_file = match ls1b(en_passant_bb) {
            0 => NO_ENPASSANT_FILE,
            x => file(bitscan_1bit(x)),   
        };
        let pin_lines: &[u64; 64] = unsafe { geometry.squares_at_line.get_unchecked(king_square) };

        // When in check, for every move except king's moves, the only
        // legal destination squares are those lying on the line
        // between the checker and the king. Also, no piece can move
        // to a square that is occupied by a friendly piece.
        let legal_dests = not_occupied_by_us &
                          match ls1b(checkers) {
            0 => {
                // Not in check -- every move destination may be
                // considered "covering".
                UNIVERSAL_SET
            }
            x if x == checkers => {
                // Single check -- calculate the check covering
                // destination subset (the squares between the king
                // and the checker). Notice that we must OR with "x"
                // itself, because knights give check not lying on a
                // line with the king.
                x |
                unsafe {
                    *geometry.squares_between_including
                             .get_unchecked(king_square)
                             .get_unchecked(bitscan_1bit(x))
                }
            }
            _ => {
                // Double check -- no covering moves.
                EMPTY_SET
            }
        };

        if legal_dests != EMPTY_SET {
            // This block is not executed when the king is in double
            // check.

            // Find all queen, rook, bishop, and knight moves.
            for piece in QUEEN..PAWN {
                let mut bb = piece_type_array[piece] & occupied_by_us;
                while bb != EMPTY_SET {
                    let piece_bb = ls1b(bb);
                    bb ^= piece_bb;
                    let from_square = bitscan_1bit(piece_bb);
                    let piece_legal_dests = match piece_bb & pinned {
                        0 => legal_dests,
                        _ => unsafe { legal_dests & *pin_lines.get_unchecked(from_square) },
                    };
                    counter += write_piece_moves_to_stack(geometry,
                                                          piece_type_array,
                                                          occupied,
                                                          en_passant_file,
                                                          castling,
                                                          us,
                                                          piece,
                                                          from_square,
                                                          piece_legal_dests,
                                                          move_stack);
                }
            }

            // When in check, en-passant capture is a legal evasion
            // move only when the checking piece is the passing pawn
            // itself. To determine if the checker is the passing
            // pawn, or if there is a discovered check we take
            // advantage of the fact that if the checker itself is the
            // only square on the check-line, then we can not have a
            // discovered check.
            let pawn_legal_dests = match legal_dests == checkers {
                false => legal_dests,
                true => legal_dests | en_passant_bb,
            };

            // Find all free pawn moves at once.
            let all_pawns = piece_type_array[PAWN] & occupied_by_us;
            let mut pinned_pawns = all_pawns & pinned;
            let free_pawns = all_pawns ^ pinned_pawns;
            if free_pawns != EMPTY_SET {
                counter += write_pawn_moves_to_stack(geometry,
                                                     piece_type_array,
                                                     occupied,
                                                     occupied_by_us,
                                                     occupied_by_them,
                                                     en_passant_file,
                                                     castling,
                                                     us,
                                                     free_pawns,
                                                     en_passant_bb,
                                                     pawn_legal_dests,
                                                     move_stack);
            }

            // Find pinned pawn moves pawn by pawn.
            while pinned_pawns != EMPTY_SET {
                let pawn_bb = ls1b(pinned_pawns);
                pinned_pawns ^= pawn_bb;
                let pin_line = unsafe { *pin_lines.get_unchecked(bitscan_1bit(pawn_bb)) };

                // TODO: When working with a single pawn (pawn_bb),
                // this procedure probably could be optimized. Not
                // clear if it worth it, though.
                counter += write_pawn_moves_to_stack(geometry,
                                                     piece_type_array,
                                                     occupied,
                                                     occupied_by_us,
                                                     occupied_by_them,
                                                     en_passant_file,
                                                     castling,
                                                     us,
                                                     pawn_bb,
                                                     en_passant_bb,
                                                     pin_line & pawn_legal_dests,
                                                     move_stack);
            }
        }

        // Find all king moves (pseudo-legal, possibly moving into
        // check).
        //
        // This is executed even when the king is in double check.
        counter += write_castling_moves_to_stack(geometry,
                                                 piece_type_array,
                                                 color_array,
                                                 occupied,
                                                 en_passant_file,
                                                 castling,
                                                 us,
                                                 king_square,
                                                 checkers,
                                                 move_stack);
        counter += write_piece_moves_to_stack(geometry,
                                              piece_type_array,
                                              occupied,
                                              en_passant_file,
                                              castling,
                                              us,
                                              KING,
                                              king_square,
                                              not_occupied_by_us,
                                              move_stack);
        counter
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
                    mut attacking_color: Color,
                    from_square: Square,
                    mut attacking_piece: PieceType,
                    to_square: Square,
                    target_piece: PieceType)
                    -> Value {

        // TODO: This method (and the functions it calls) does a lot
        // of array access and therefore, lots of array boundary
        // check. Also I expect this code to be crucial for the
        // performance. Therefore we probably have to switch to
        // unchecked array indexing.

        use std::mem::uninitialized;
        use std::cmp::max;
        static VALUE: [Value; 6] = [10000, 975, 500, 325, 325, 100];

        let geometry = self.geometry;
        let piece_type_array = &self.piece_type;
        let color_array = &self.color;
        let mut occupied = color_array[WHITE] | color_array[BLACK];
        let mut depth = 0;
        let mut attackers_and_defenders = attacks_to(geometry,
                                                     piece_type_array,
                                                     color_array,
                                                     occupied,
                                                     WHITE,
                                                     to_square) |
                                          attacks_to(geometry,
                                                     piece_type_array,
                                                     color_array,
                                                     occupied,
                                                     BLACK,
                                                     to_square);
        let mut from_square_bb = 1 << from_square;

        // "may_xray" pieces may block x-ray attacks from other
        // pieces, so we must consider adding new attackers/defenders
        // every time a "may_xray"-piece makes a capture.
        let may_xray = piece_type_array[PAWN] | piece_type_array[BISHOP] | piece_type_array[ROOK] |
                       piece_type_array[QUEEN];
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
                    attackers_and_defenders |= consider_xrays(geometry,
                                                              piece_type_array,
                                                              occupied,
                                                              to_square,
                                                              bitscan_forward(from_square_bb));
                }
                assert_eq!(occupied | attackers_and_defenders, occupied);

                // find the next piece in the exchange
                let next_attack = get_least_valuable_piece_in_a_set(piece_type_array,
                                                                    attackers_and_defenders &
                                                                    color_array[attacking_color]);
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
}


// Return a reference to a properly initialized BoardGeometry
// object. The object is created and initialized only during the first
// call. All next calls will return a reference to the same
// object. This is done in a thread-safe manner.
fn board_geometry() -> &'static BoardGeometry {
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


// Return the set of squares that have on them pieces (or pawns)
// of color "us" that attack the square "square" directly (no
// x-rays).
#[inline]
fn attacks_to(geometry: &BoardGeometry,
              piece_type_array: &[u64; 6],
              color_array: &[u64; 2],
              occupied: u64,
              us: Color,
              square: Square)
              -> u64 {
    assert!(us <= 1);

    // This code is performance critical, so we do everything without
    // array boundary checks.
    unsafe {
        let occupied_by_us = *color_array.get_unchecked(us);
        let shifts: &[isize; 4] = PAWN_MOVE_SHIFTS.get_unchecked(us);
        let square_bb = 1 << square;
        let pawns = piece_type_array[PAWN];
        let queens = piece_type_array[QUEEN];
        (piece_attacks_from(geometry, occupied, ROOK, square) & occupied_by_us &
         (piece_type_array[ROOK] | queens)) |
        (piece_attacks_from(geometry, occupied, BISHOP, square) & occupied_by_us &
         (piece_type_array[BISHOP] | queens)) |
        (piece_attacks_from(geometry, occupied, KNIGHT, square) & occupied_by_us &
         piece_type_array[KNIGHT]) |
        (piece_attacks_from(geometry, occupied, KING, square) & occupied_by_us &
         piece_type_array[KING]) |
        (gen_shift(square_bb, -shifts[PAWN_KINGSIDE_CAPTURE]) & occupied_by_us & pawns &
         !(BB_FILE_H | BB_RANK_1 | BB_RANK_8)) |
        (gen_shift(square_bb, -shifts[PAWN_QUEENSIDE_CAPTURE]) & occupied_by_us & pawns &
         !(BB_FILE_A | BB_RANK_1 | BB_RANK_8))
    }
}


// Return the set of squares that are attacked by a piece (not a pawn)
// of type "piece" from the square "square", on a board which is
// occupied with other pieces according to the "occupied"
// bit-set. "geometry" supplies the look-up tables needed to perform
// the calculation.
#[inline(always)]
pub fn piece_attacks_from(geometry: &BoardGeometry,
                          occupied: u64,
                          piece: PieceType,
                          square: Square)
                          -> u64 {
    assert!(piece < PAWN);
    assert!(square <= 63);

    // This code is extremely performance critical, so we must do
    // everything without array boundary checks.
    unsafe {
        let behind: &[u64; 64] = geometry.squares_behind_blocker.get_unchecked(square);
        let mut attacks = *geometry.attacks.get_unchecked(piece).get_unchecked(square);
        let mut blockers = occupied &
                           *geometry.blockers_and_beyond
                                    .get_unchecked(piece)
                                    .get_unchecked(square);
        while blockers != EMPTY_SET {
            attacks &= !*behind.get_unchecked(bitscan_forward_and_reset(&mut blockers));
        }
        attacks
    }
}


// This is a helper function for
// Board::generate_pseudolegal_moves(). It really does not do anything
// other than scanning the destination set, and for each move
// destination it figures out what piece is captured (if any), and
// writes a new move and its score to the move stack.
#[inline(always)]
fn write_piece_moves_to_stack(geometry: &BoardGeometry,
                              piece_type_array: &[u64; 6],
                              occupied: u64,
                              en_passant_file: File,
                              castling: CastlingRights,
                              us: Color,
                              piece: PieceType,
                              from_square: Square,
                              legal_dests: u64,
                              move_stack: &mut MoveStack)
                              -> usize {
    let mut counter = 0;
    let mut dest_set = piece_attacks_from(geometry, occupied, piece, from_square) & legal_dests;
    while dest_set != EMPTY_SET {
        let dest_bb = ls1b(dest_set);
        dest_set ^= dest_bb;
        let dest_square = bitscan_1bit(dest_bb);
        let captured_piece = get_piece_type_at(piece_type_array, occupied, dest_bb);
        move_stack.push(Move::new(us,
                                  0,
                                  MOVE_NORMAL,
                                  piece,
                                  from_square,
                                  dest_square,
                                  captured_piece,
                                  en_passant_file,
                                  castling,
                                  0));
        counter += 1;
    }
    counter
}


// Return the piece type at the square represented by the bit-set
// "square_bb", on a board which is occupied with other pieces
// according to the "piece_type_array" array and "occupied" bit-set
// and.
#[inline(always)]
fn get_piece_type_at(piece_type_array: &[u64; 6], occupied: u64, square_bb: u64) -> PieceType {
    assert!(square_bb != EMPTY_SET);
    assert_eq!(square_bb, ls1b(square_bb));
    match square_bb & occupied {
        EMPTY_SET => NO_PIECE,
        x if x & piece_type_array[PAWN] != 0 => PAWN,
        x if x & piece_type_array[KNIGHT] != 0 => KNIGHT,
        x if x & piece_type_array[BISHOP] != 0 => BISHOP,
        x if x & piece_type_array[ROOK] != 0 => ROOK,
        x if x & piece_type_array[QUEEN] != 0 => QUEEN,
        x if x & piece_type_array[KING] != 0 => KING,
        _ => panic!("invalid board"),
    }
}


// This is a helper function for Board::generate_pseudolegal_moves().
//
// It generates candidate pawn destination sets, then performs an
// intersection between those sets and the set of legal
// destinations. After that it scans the resulting sets, and for each
// destination figures out what piece is captured (if any), and writes
// a new move and its score to the move stack. It also recognizes and
// discards the very rare case of pseudo-legal en-passant capture that
// leaves discovered check on the 4/5-th rank.
#[inline(always)]
fn write_pawn_moves_to_stack(geometry: &BoardGeometry,
                             piece_type_array: &[u64; 6],
                             occupied: u64,
                             occupied_by_us: u64,
                             occupied_by_them: u64,
                             en_passant_file: File,
                             castling: CastlingRights,
                             us: Color,
                             pawns: u64,
                             en_passant_bb: u64,
                             legal_dests: u64,
                             move_stack: &mut MoveStack)
                             -> usize {
    assert!(us <= 1);
    let mut counter = 0;
    let shifts: &[isize; 4] = unsafe { PAWN_MOVE_SHIFTS.get_unchecked(us) };

    // Generate candidate pawn destination sets.
    let mut dest_sets = pawn_dest_sets(occupied_by_us,
                                       occupied_by_them,
                                       shifts,
                                       pawns,
                                       en_passant_bb);

    // Make sure all destination squares in all sets are legal.
    dest_sets[PAWN_PUSH] &= legal_dests;
    dest_sets[PAWN_DOUBLE_PUSH] &= legal_dests;
    dest_sets[PAWN_QUEENSIDE_CAPTURE] &= legal_dests;
    dest_sets[PAWN_KINGSIDE_CAPTURE] &= legal_dests;

    // Scan each destination set (push, double-push, queen-side
    // capture, king-side capture). For each move calculate the "to"
    // and "from" sqares, and determinne the move type (en-passant
    // capture, pawn promotion, or a normal move).
    for move_type in 0..4 {
        let s = &mut dest_sets[move_type];
        while *s != EMPTY_SET {
            let pawn_bb = ls1b(*s);
            *s ^= pawn_bb;
            let dest_square = bitscan_1bit(pawn_bb);
            let orig_square = (dest_square as isize - shifts[move_type]) as Square;
            let captured_piece = get_piece_type_at(piece_type_array, occupied, pawn_bb);
            match pawn_bb {
                // en-passant capture
                x if x == en_passant_bb => {
                    let king_bb = piece_type_array[KING] & occupied_by_us;
                    if king_bb & [BB_RANK_5, BB_RANK_4][us] == 0 ||
                       en_passant_special_check_ok(geometry,
                                                   piece_type_array,
                                                   occupied,
                                                   occupied_by_them,
                                                   us,
                                                   bitscan_1bit(king_bb),
                                                   orig_square,
                                                   dest_square) {
                        counter += 1;
                        move_stack.push(Move::new(us,
                                                  0,
                                                  MOVE_ENPASSANT,
                                                  PAWN,
                                                  orig_square,
                                                  dest_square,
                                                  PAWN,
                                                  en_passant_file,
                                                  castling,
                                                  0));
                    }
                }
                // pawn promotion
                x if x & PAWN_PROMOTION_RANKS != 0 => {
                    for p in 0..4 {
                        counter += 1;
                        move_stack.push(Move::new(us,
                                                  if Move::piece_from_aux_data(p) == QUEEN {
                                                      1
                                                  } else {
                                                      0
                                                  },
                                                  MOVE_PROMOTION,
                                                  PAWN,
                                                  orig_square,
                                                  dest_square,
                                                  captured_piece,
                                                  en_passant_file,
                                                  castling,
                                                  p));
                    }
                }
                // normal pawn move (push or plain capture)
                _ => {
                    counter += 1;
                    move_stack.push(Move::new(us,
                                              0,
                                              MOVE_NORMAL,
                                              PAWN,
                                              orig_square,
                                              dest_square,
                                              captured_piece,
                                              en_passant_file,
                                              castling,
                                              0));
                }
            }
        }
    }
    counter
}


// This is a helper function for "write_pawn_moves_to_stack()". It
// generates array with 4 pawn destination sets.
//
// We differentiate 4 types of pawn moves: single push, double push,
// queen-side capture (capturing toward queen side), and king-side
// capture (capturing toward king side). The benefit of this
// separation is that knowing the destination square and the pawn move
// type (the index in the destination sets array) is enough to recover
// the origin square.
//
// The function returns an array of 4 bit-sets (1 for each pawn move
// type), describing all pseudo-legal destination
// squares. (Pseudo-legal means that we may still leave the king under
// check.)
#[inline(always)]
fn pawn_dest_sets(occupied_by_us: u64,
                  occupied_by_them: u64,
                  shifts: &[isize; 4],
                  pawns: u64,
                  en_passant_bb: u64)
                  -> [u64; 4] {
    use std::mem::uninitialized;
    let not_occupied_by_us = !occupied_by_us;
    let capture_targets = occupied_by_them | en_passant_bb;
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


// This is a helper function for "write_pawn_moves_to_stack()".
//
// It tests for the special case when an en-passant capture discovers
// check on 4/5-th rank. This is the very rare occasion when the two
// pawns participating in en-passant capture, disappearing in one
// move, discover an unexpected check along the horizontal (rank 4 of
// 5).
#[inline]
fn en_passant_special_check_ok(geometry: &BoardGeometry,
                               piece_type_array: &[u64; 6],
                               occupied: u64,
                               occupied_by_them: u64,
                               us: Color,
                               king_square: Square,
                               orig_square: Square,
                               dest_square: Square)
                               -> bool {
    let the_two_pawns = 1 << orig_square |
                        gen_shift(1, dest_square as isize - PAWN_MOVE_SHIFTS[us][PAWN_PUSH]);
    let occupied = occupied & !the_two_pawns;
    let occupied_by_them = occupied_by_them & !the_two_pawns;
    let checkers = piece_attacks_from(geometry, occupied, ROOK, king_square) & occupied_by_them &
                   (piece_type_array[ROOK] | piece_type_array[QUEEN]);
    checkers == EMPTY_SET
}


// This is a helper function for
// Board::generate_pseudolegal_moves(). It figures out if castling on
// each side is pseudo-legal and if it is, writes a new move and its
// score to the move stack.
#[inline(always)]
fn write_castling_moves_to_stack(geometry: &BoardGeometry,
                                 piece_type_array: &[u64; 6],
                                 color_array: &[u64; 2],
                                 occupied: u64,
                                 en_passant_file: File,
                                 castling: CastlingRights,
                                 us: Color,
                                 king_square: Square,
                                 checkers: u64,
                                 move_stack: &mut MoveStack)
                                 -> usize {
    const FINAL_SQUARES: [[Square; 2]; 2] = [[C1, C8], [G1, G8]];
    const PASSING_SQUARES: [[Square; 2]; 2] = [[D1, D8], [F1, F8]];
    assert!(us <= 1);
    let mut counter = 0;

    // can not castle if in check
    if checkers == EMPTY_SET {
        let them = 1 ^ us;

        // try queen-side and king-side castling
        for side in 0..2 {

            // ensure squares between the king and the rook are empty
            if castling.obstacles(us, side) & occupied == 0 {

                // ensure king's passing square is not attacked (this
                // is a quite expensive check).
                //
                // TODO: This check is probably too expensive to do
                // here. We probably have to move this check in the
                // "do_move()" method of "Position" class.
                if attacks_to(geometry,
                              piece_type_array,
                              color_array,
                              occupied,
                              them,
                              unsafe { *PASSING_SQUARES[side].get_unchecked(us) }) ==
                   0 {

                    // it seems castling is legal unless king's final
                    // square is attacked, but we do not care about
                    // that, because this will be verified later.
                    counter += 1;
                    move_stack.push(Move::new(us,
                                              0,
                                              MOVE_CASTLING,
                                              KING,
                                              king_square,
                                              unsafe { *FINAL_SQUARES[side].get_unchecked(us) },
                                              NO_PIECE,
                                              en_passant_file,
                                              castling,
                                              0));
                }
            }
        }
    }
    counter
}


// Return a bit-set describing all pieces that can attack
// "target_square" once "xrayed_square" becomes vacant.
//
// This is a helper function for the static exchange evaluation
// (Board::calc_see)
#[inline(always)]
fn consider_xrays(geometry: &BoardGeometry,
                  piece_type_array: &[u64; 6],
                  occupied: u64,
                  target_square: Square,
                  xrayed_square: Square)
                  -> u64 {
    let candidates = occupied & geometry.squares_behind_blocker[target_square][xrayed_square];
    let diag_attackers = piece_attacks_from(geometry, candidates, BISHOP, target_square) &
                         (piece_type_array[QUEEN] | piece_type_array[BISHOP]);
    let line_attackers = piece_attacks_from(geometry, candidates, ROOK, target_square) &
                         (piece_type_array[QUEEN] | piece_type_array[ROOK]);
    assert_eq!(diag_attackers & line_attackers, EMPTY_SET);
    assert_eq!(ls1b(candidates & diag_attackers),
               candidates & diag_attackers);
    assert_eq!(ls1b(candidates & line_attackers),
               candidates & line_attackers);
    candidates & (diag_attackers | line_attackers)
}


// Return the least valuble piece in the subset "set".
//
// This is a helper function for the static exchange evaluation
// (Board::calc_see)
#[inline(always)]
fn get_least_valuable_piece_in_a_set(piece_type_array: &[u64; 6], set: u64) -> (PieceType, u64) {
    for p in (0..6).rev() {
        let piece_subset = piece_type_array[p] & set;
        if piece_subset != EMPTY_SET {
            return (p, ls1b(piece_subset));
        }
    }
    (NO_PIECE, EMPTY_SET)
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
    use super::board_geometry;
    use position::chess_move::MoveStack;
    use notation::parse_fen_piece_placement as fen;

    #[test]
    fn test_attacks_from() {
        use basetypes::*;
        let b = Board::create(&fen("k7/8/8/8/3P4/8/8/7K").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        let g = board_geometry();
        assert_eq!(piece_attacks_from(g, b.color[WHITE] | b.color[BLACK], BISHOP, A1),
                   1 << B2 | 1 << C3 | 1 << D4);
        assert_eq!(piece_attacks_from(g, b.color[WHITE] | b.color[BLACK], BISHOP, A1),
                   1 << B2 | 1 << C3 | 1 << D4);
        assert_eq!(piece_attacks_from(g, b.color[WHITE] | b.color[BLACK], KNIGHT, A1),
                   1 << B3 | 1 << C2);
    }

    #[test]
    fn test_attacks_to() {
        use basetypes::*;
        let b = Board::create(&fen("8/8/8/3K1p1P/r4k2/3Pq1N1/7p/1B5Q").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.attacks_to(WHITE, E4),
                   1 << D3 | 1 << G3 | 1 << D5 | 1 << H1);
        assert_eq!(b.attacks_to(BLACK, E4),
                   1 << E3 | 1 << F4 | 1 << F5 | 1 << A4);
        assert_eq!(b.attacks_to(BLACK, G6), 0);
        assert_eq!(b.attacks_to(WHITE, G6), 1 << H5);
        assert_eq!(b.attacks_to(WHITE, C2), 1 << B1);
        assert_eq!(b.attacks_to(WHITE, F4), 0);
        assert_eq!(b.attacks_to(BLACK, F4), 1 << A4 | 1 << E3);
        assert_eq!(b.attacks_to(BLACK, F5), 1 << F4);
        assert_eq!(b.attacks_to(WHITE, A6), 0);
        assert_eq!(b.attacks_to(BLACK, G1), 1 << H2 | 1 << E3);
        assert_eq!(b.attacks_to(BLACK, A1), 1 << A4);
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
        let b = Board::create(&fen("5r2/8/8/4q1p1/3P4/k3P1P1/P2b1R1B/K4R2").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.calc_see(BLACK, E5, QUEEN, E3, PAWN), 100);
        assert_eq!(b.calc_see(BLACK, E5, QUEEN, D4, PAWN), -875);
        assert_eq!(b.calc_see(WHITE, G3, PAWN, F4, PAWN), 100);
        assert_eq!(b.calc_see(BLACK, A3, KING, A2, PAWN), -9900);
    }

    #[test]
    fn test_pawn_dest_sets() {
        use basetypes::*;
        use super::pawn_dest_sets;
        use super::PAWN_MOVE_SHIFTS;
        let b = Board::create(&fen("k2q4/4Ppp1/5P2/6Pp/6P1/8/7P/7K").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        let ds = pawn_dest_sets(b.color[WHITE],
                                b.color[BLACK],
                                &PAWN_MOVE_SHIFTS[WHITE],
                                b.piece_type[PAWN] & b.color[WHITE],
                                1 << H6);
        assert_eq!(ds[0], 1 << H3 | 1 << G6 | 1 << E8);
        assert_eq!(ds[1], 1 << H4);
        assert_eq!(ds[3], 1 << H5 | 1 << G7 | 1 << H6);
        assert_eq!(ds[2], 1 << D8);
        let ds = pawn_dest_sets(b.color[BLACK],
                                b.color[WHITE],
                                &PAWN_MOVE_SHIFTS[BLACK],
                                b.piece_type[PAWN] & b.color[BLACK],
                                0);
        assert_eq!(ds[0], 1 << H4 | 1 << G6);
        assert_eq!(ds[1], 0);
        assert_eq!(ds[3], 0);
        assert_eq!(ds[2], 1 << G4 | 1 << F6);
    }

    #[test]
    fn test_move_generation_1() {
        use basetypes::*;

        let b = Board::create(&fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/4K3").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();

        assert_eq!(b.generate_pseudolegal_moves(WHITE,
                                                E1,
                                                1 << E3,
                                                1 << D2,
                                                0,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   5);
        let b = Board::create(&fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/6K1").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(WHITE,
                                                G1,
                                                1 << E3,
                                                0,
                                                0,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   7);
        let b = Board::create(&fen("8/8/6NK/2pP4/3PR3/2b1q3/3P4/7k").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(WHITE,
                                                H6,
                                                1 << E3,
                                                0,
                                                0,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   8);
        let b = Board::create(&fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/7K").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(WHITE,
                                                H1,
                                                0,
                                                0,
                                                0,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   22);
        let b = Board::create(&fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/7K").ok().unwrap(),
                              Some(C6),
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(WHITE,
                                                H1,
                                                0,
                                                0,
                                                1 << C6,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   23);
        let b = Board::create(&fen("K7/8/6N1/2pP4/3PR3/2b1q3/3P4/7k").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              BLACK)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(BLACK,
                                                H1,
                                                0,
                                                0,
                                                0,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   25);
        let b = Board::create(&fen("K7/8/6N1/2pP4/3PR2k/2b1q3/3P4/8").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              BLACK)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(BLACK,
                                                H4,
                                                1 << E4 | 1 << G6,
                                                0,
                                                0,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   5);
    }

    #[test]
    fn test_move_generation_2() {
        use basetypes::*;
        let b = Board::create(&fen("8/8/8/7k/5pP1/8/8/5R1K").ok().unwrap(),
                              Some(G3),
                              CastlingRights::new(),
                              BLACK)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(BLACK,
                                                H5,
                                                1 << G4,
                                                0,
                                                1 << G3,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   6);

        let b = Board::create(&fen("8/8/8/5k2/5pP1/8/8/5R1K").ok().unwrap(),
                              Some(G3),
                              CastlingRights::new(),
                              BLACK)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(BLACK,
                                                F5,
                                                1 << G4,
                                                1 << F4,
                                                1 << G3,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   7);

        let b = Board::create(&fen("8/8/8/8/5pP1/7k/8/5B1K").ok().unwrap(),
                              Some(G3),
                              CastlingRights::new(),
                              BLACK)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(BLACK,
                                                H3,
                                                1 << F1,
                                                0,
                                                1 << G3,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   5);
    }

    #[test]
    fn test_move_generation_3() {
        use basetypes::*;
        let b = Board::create(&fen("8/8/8/8/4RpPk/8/8/7K").ok().unwrap(),
                              Some(G3),
                              CastlingRights::new(),
                              BLACK)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(BLACK,
                                                H4,
                                                0,
                                                0,
                                                1 << G3,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   6);
    }

    #[test]
    fn test_move_generation_4() {
        use basetypes::*;
        let b = Board::create(&fen("8/8/8/8/3QPpPk/8/8/7K").ok().unwrap(),
                              Some(G3),
                              CastlingRights::new(),
                              BLACK)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(BLACK,
                                                H4,
                                                0,
                                                0,
                                                1 << G3,
                                                CastlingRights::new(),
                                                &mut MoveStack::new()),
                   7);
    }

    #[test]
    fn test_move_generation_5() {
        use basetypes::*;

        let b = Board::create(&fen("rn2k2r/8/8/8/8/8/8/R3K2R").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        let mut cr = CastlingRights::new();
        assert_eq!(b.generate_pseudolegal_moves(WHITE, E1, 0, 0, 0, cr, &mut MoveStack::new()),
                   19 + 5);
        cr.set_with_mask(CASTLE_WHITE_KINGSIDE);
        assert_eq!(b.generate_pseudolegal_moves(WHITE, E1, 0, 0, 0, cr, &mut MoveStack::new()),
                   19 + 6);
        cr.set_with_mask(CASTLE_WHITE_QUEENSIDE);
        assert_eq!(b.generate_pseudolegal_moves(WHITE, E1, 0, 0, 0, cr, &mut MoveStack::new()),
                   19 + 7);
        assert_eq!(b.generate_pseudolegal_moves(BLACK, E8, 0, 0, 0, cr, &mut MoveStack::new()),
                   19 + 5);
        cr.set_with_mask(CASTLE_BLACK_KINGSIDE);
        assert_eq!(b.generate_pseudolegal_moves(BLACK, E8, 0, 0, 0, cr, &mut MoveStack::new()),
                   19 + 6);

        let b = Board::create(&fen("4k3/8/8/8/8/5n2/8/R3K2R").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(WHITE,
                                                E1,
                                                1 << F3,
                                                0,
                                                0,
                                                cr,
                                                &mut MoveStack::new()),
                   5);

        let b = Board::create(&fen("4k3/8/8/8/8/6n1/8/R3K2R").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(WHITE, E1, 0, 0, 0, cr, &mut MoveStack::new()),
                   19 + 6);
        let b = Board::create(&fen("4k3/8/8/8/8/4n3/8/R3K2R").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(WHITE, E1, 0, 0, 0, cr, &mut MoveStack::new()),
                   19 + 5);

        let b = Board::create(&fen("4k3/8/1b6/8/8/8/8/R3K2R").ok().unwrap(),
                              None,
                              CastlingRights::new(),
                              WHITE)
                    .ok()
                    .unwrap();
        assert_eq!(b.generate_pseudolegal_moves(WHITE, E1, 0, 0, 0, cr, &mut MoveStack::new()),
                   19 + 7);
    }
}
