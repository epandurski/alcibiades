/// Implements `StdMoveGenerator`.

use std::mem::uninitialized;
use std::cell::Cell;
use std::cmp::max;
use uci::{SetOption, OptionDescription};
use board::*;
use squares::*;
use moves::*;
use value::*;
use evaluator::Evaluator;
use move_generator::MoveGenerator;
use bitsets::*;
use utils::{BoardGeometry, ZobristArrays};


/// Implements the `MoveGenerator` trait.
#[derive(Clone)]
pub struct StdMoveGenerator<T: Evaluator<Board>> {
    geometry: &'static BoardGeometry,
    zobrist: &'static ZobristArrays,
    board: Board,
    evaluator: T,

    /// Lazily calculated bitboard of all checkers -- `BB_ALL` if not
    /// calculated yet.
    checkers: Cell<Bitboard>,
}


impl<T: Evaluator<Board>> MoveGenerator<Board> for StdMoveGenerator<T> {
    type Evaluator = T;

    fn from_board(board: Board) -> Result<Self, IllegalBoard> {
        let mut gen = StdMoveGenerator {
            geometry: BoardGeometry::get(),
            zobrist: ZobristArrays::get(),
            board: board,
            evaluator: unsafe { uninitialized() },
            checkers: Cell::new(BB_ALL),
        };
        if gen.is_legal() {
            gen.evaluator = T::new(gen.board());
            Ok(gen)
        } else {
            Err(IllegalBoard)
        }
    }

    fn hash(&self) -> u64 {
        let mut hash = 0;
        for color in 0..2 {
            for piece in 0..6 {
                let mut bb = self.board.pieces.color[color] & self.board.pieces.piece_type[piece];
                while bb != 0 {
                    let square = bsf_reset(&mut bb);
                    hash ^= self.zobrist.pieces[color][piece][square];
                }
            }
        }
        hash ^= self.zobrist.castling_rights[self.board.castling_rights.value()];
        hash ^= self.zobrist.enpassant_file[self.board.enpassant_file];
        if self.board.to_move == BLACK {
            hash ^= self.zobrist.to_move;
        }
        hash
    }

    #[inline]
    fn board(&self) -> &Board {
        &self.board
    }

    fn attacks_to(&self, square: Square) -> Bitboard {
        assert!(square <= 63);
        unsafe {
            (self.geometry.attacks_from_unsafe(ROOK, square, self.board.occupied) &
             (self.board.pieces.piece_type[ROOK] | self.board.pieces.piece_type[QUEEN])) |
            (self.geometry.attacks_from_unsafe(BISHOP, square, self.board.occupied) &
             (self.board.pieces.piece_type[BISHOP] | self.board.pieces.piece_type[QUEEN])) |
            (self.geometry.attacks_from_unsafe(KNIGHT, square, self.board.occupied) &
             self.board.pieces.piece_type[KNIGHT]) |
            (self.geometry.attacks_from_unsafe(KING, square, self.board.occupied) &
             self.board.pieces.piece_type[KING]) |
            ((*self.geometry.pawn_attacks[WHITE].get_unchecked(square) &
              self.board.pieces.color[BLACK]) |
             (*self.geometry.pawn_attacks[BLACK].get_unchecked(square) &
              self.board.pieces.color[WHITE])) & self.board.pieces.piece_type[PAWN]
        }
    }

    #[inline]
    fn is_check(&self) -> bool {
        self.checkers() != 0
    }

    #[inline]
    fn evaluator(&self) -> &Self::Evaluator {
        &self.evaluator
    }

    /// Generates all legal moves, possibly including some
    /// pseudo-legal moves too.
    ///
    /// The moves are added to `moves`. All generated moves with
    /// pieces other than the king will be legal. Some of the
    /// generated king's moves may be illegal because the destination
    /// square is under attack.
    ///
    /// The initial move score for all generated moves is `0`.
    ///
    /// **Note:** A pseudo-legal move is a move that is otherwise
    /// legal, except it might leave the king in check.
    fn generate_all<U: AddMove>(&self, moves: &mut U) {
        let (king_square, checkers) = self.king_square_and_checkers();
        let occupied_by_us = unsafe { *self.board.pieces.color.get_unchecked(self.board.to_move) };
        let legal_dests = !occupied_by_us &
                          match lsb(checkers) {
            0 =>
                // Not in check -- every move destination may be
                // considered "covering".
                BB_ALL,
            x if x == checkers =>
                // Single check -- calculate the check covering
                // destination subset (the squares between the king
                // and the checker). Notice that we must OR with "x"
                // itself, because knights give check not lying on a
                // line with the king.
                x |
                unsafe {
                    *self.geometry
                         .squares_between_including
                         .get_unchecked(king_square)
                         .get_unchecked(bsf(x))
                },
            _ =>
                // Double check -- no covering moves.
                0,
        };

        if legal_dests != 0 {
            let pinned = self.find_pinned(king_square);

            // Generate queen, rook, bishop, and knight moves.
            for piece in QUEEN..PAWN {
                let mut bb = self.board.pieces.piece_type[piece] & occupied_by_us;
                while bb != 0 {
                    let orig_square = bsf_reset(&mut bb);
                    let piece_legal_dests = if 1 << orig_square & pinned == 0 {
                        legal_dests
                    } else {
                        // The piece is pinned -- reduce the set of
                        // legal destination to the squares on the
                        // line of the pin.
                        legal_dests & self.geometry.squares_at_line[king_square][orig_square]
                    };
                    self.add_piece_moves(piece, orig_square, piece_legal_dests, moves);
                }
            }

            // Generate pawn moves.
            {
                let our_pawns = self.board.pieces.piece_type[PAWN] & occupied_by_us;
                let mut pinned_pawns = our_pawns & pinned;
                let pawn_legal_dests = if checkers & self.board.pieces.piece_type[PAWN] == 0 {
                    legal_dests
                } else {
                    // We are in check from a pawn, therefore an
                    // en-passant capture is legal too.
                    legal_dests | self.enpassant_bb()
                };

                // Generate all moves with not-pinned pawns.
                self.add_pawn_moves(our_pawns ^ pinned_pawns, pawn_legal_dests, false, moves);

                // Generate pinned pawns' moves pawn by pawn, reducing
                // the set of legal destination for each pinned pawn
                // to the squares on the line of the pin.
                while pinned_pawns != 0 {
                    let pawn_square = bsf_reset(&mut pinned_pawns);
                    let pawn_legal_dests = pawn_legal_dests &
                                           self.geometry.squares_at_line[king_square][pawn_square];
                    self.add_pawn_moves(1 << pawn_square, pawn_legal_dests, false, moves);
                }
            }
        }

        // Generate king moves (pseudo-legal, possibly moving into check).
        for side in 0..2 {
            if self.can_castle(king_square, side) {
                moves.add_move(Move::new(MOVE_CASTLING,
                                         king_square,
                                         [[C1, C8], [G1, G8]][side][self.board.to_move],
                                         0,
                                         PIECE_NONE,
                                         KING,
                                         self.board.castling_rights,
                                         self.board.enpassant_file,
                                         0));
            }
        }
        self.add_piece_moves(KING, king_square, !occupied_by_us, moves);
    }

    /// Generates moves for the quiescence search.
    ///
    /// The moves are added to `moves`. This method always generates a
    /// **subset** of the moves generated by `generate_all`:
    ///
    /// * If the king is in check, all legal moves are included.
    ///
    /// * Captures and pawn promotions to queen are always included.
    ///
    /// * If `generate_checks` is `true`, moves that give check are
    ///   included too. Discovered checks and checks given by castling
    ///   are omitted for speed.
    fn generate_forcing<U: AddMove>(&self, generate_checks: bool, moves: &mut U) {
        let (king_square, checkers) = self.king_square_and_checkers();
        if checkers != 0 {
            return self.generate_all(moves);
        }
        let pinned = self.find_pinned(king_square);
        let occupied_by_us = unsafe { *self.board.pieces.color.get_unchecked(self.board.to_move) };
        let occupied_by_them = self.board.occupied ^ occupied_by_us;
        let enpassant_bb = self.enpassant_bb();
        let pawn_dests;

        // Generate queen, rook, bishop, and knight moves.
        if generate_checks {
            let their_king_square = bsf(self.board.pieces.piece_type[KING] & occupied_by_them);
            unsafe {
                pawn_dests = occupied_by_them | enpassant_bb | BB_PAWN_PROMOTION_RANKS |
                             *self.geometry
                                  .pawn_attacks
                                  .get_unchecked(1 ^ self.board.to_move)
                                  .get_unchecked(their_king_square);
                for piece in QUEEN..PAWN {
                    let mut bb = self.board.pieces.piece_type[piece] & occupied_by_us;
                    while bb != 0 {
                        let orig_square = bsf_reset(&mut bb);
                        let checking_squares = !occupied_by_us &
                                               self.geometry
                                                   .attacks_from_unsafe(piece,
                                                                        their_king_square,
                                                                        self.board.occupied);
                        let mut dests = occupied_by_them | checking_squares;
                        if 1 << orig_square & pinned != 0 {
                            dests &= self.geometry.squares_at_line[king_square][orig_square];
                        }
                        self.add_piece_moves(piece, orig_square, dests, moves);
                    }
                }
            }
        } else {
            pawn_dests = occupied_by_them | enpassant_bb | BB_PAWN_PROMOTION_RANKS;
            for piece in QUEEN..PAWN {
                let mut bb = self.board.pieces.piece_type[piece] & occupied_by_us;
                while bb != 0 {
                    let orig_square = bsf_reset(&mut bb);
                    let mut dests = occupied_by_them;
                    if 1 << orig_square & pinned != 0 {
                        dests &= self.geometry.squares_at_line[king_square][orig_square];
                    }
                    self.add_piece_moves(piece, orig_square, dests, moves);
                }
            }
        }

        // Generate pawn moves.
        {
            let our_pawns = self.board.pieces.piece_type[PAWN] & occupied_by_us;
            let mut pinned_pawns = our_pawns & pinned;
            self.add_pawn_moves(our_pawns ^ pinned_pawns, pawn_dests, true, moves);
            while pinned_pawns != 0 {
                let pawn_square = bsf_reset(&mut pinned_pawns);
                let pawn_dests = pawn_dests &
                                 self.geometry.squares_at_line[king_square][pawn_square];
                self.add_pawn_moves(1 << pawn_square, pawn_dests, true, moves);
            }
        }

        // Generate king moves.
        self.add_piece_moves(KING, king_square, occupied_by_them, moves);
    }

    fn try_move_digest(&self, move_digest: MoveDigest) -> Option<Move> {
        // We will use `generated_move` to assert that our result is correct.
        let mut generated_move = unsafe { uninitialized() };

        // The purpose of `try_move_digest` is to check if a move is
        // pseudo-legal, without spending time to generate all
        // pseudo-legal moves. Therefore, if we did not care about
        // performace, the whole complex logic of this method could be
        // substituted with the next few lines:
        if cfg!(debug_assertions) {
            generated_move = None;
            let mut move_stack = Vec::new();
            self.generate_all(&mut move_stack);
            while let Some(m) = move_stack.pop() {
                if m.digest() == move_digest {
                    generated_move = Some(m);
                    break;
                }
            }
        }

        if move_digest == MoveDigest::invalid() {
            debug_assert!(generated_move.is_none());
            return None;
        }

        let move_type = move_digest.move_type();
        let orig_square = move_digest.orig_square();
        let dest_square = move_digest.dest_square();
        let promoted_piece_code = move_digest.aux_data();
        let (king_square, checkers) = self.king_square_and_checkers();

        if move_type == MOVE_CASTLING {
            let side = if dest_square < orig_square {
                QUEENSIDE
            } else {
                KINGSIDE
            };
            if !self.can_castle(king_square, side) || orig_square != king_square ||
               dest_square != [[C1, C8], [G1, G8]][side][self.board.to_move] ||
               promoted_piece_code != 0 {
                debug_assert!(generated_move.is_none());
                return None;
            }
            let m = Move::new(MOVE_CASTLING,
                              orig_square,
                              dest_square,
                              0,
                              PIECE_NONE,
                              KING,
                              self.board.castling_rights,
                              self.board.enpassant_file,
                              0);
            debug_assert_eq!(generated_move, Some(m));
            return Some(m);
        }

        let occupied_by_us = unsafe { *self.board.pieces.color.get_unchecked(self.board.to_move) };
        let orig_square_bb = occupied_by_us & (1 << orig_square);
        let dest_square_bb = 1 << dest_square;
        let mut captured_piece = self.get_piece_type_at(dest_square);

        // Figure out what is the type of the moved piece.
        let piece;
        'pieces: loop {
            for i in KING..PIECE_NONE {
                if orig_square_bb & self.board.pieces.piece_type[i] != 0 {
                    piece = i;
                    break 'pieces;
                }
            }
            debug_assert!(generated_move.is_none());
            return None;
        }
        debug_assert!(piece <= PAWN);

        // Initialize the pseudo-legal destinations set -- we will
        // continue to shrink (or expand) this set as we go.
        let mut pseudo_legal_dests = !occupied_by_us;

        if piece != KING {
            // Verify if the king is in check.
            pseudo_legal_dests &= match lsb(checkers) {
                0 => BB_ALL,
                x if x == checkers => {
                    x |
                    unsafe {
                        *self.geometry
                             .squares_between_including
                             .get_unchecked(king_square)
                             .get_unchecked(bsf(x))
                    }
                }
                _ => {
                    debug_assert!(generated_move.is_none());
                    return None;
                } 
            };

            // Verify if the moved piece is pinned.
            if orig_square_bb & self.find_pinned(king_square) != 0 {
                pseudo_legal_dests &= self.geometry.squares_at_line[king_square][orig_square]
            }
        };

        if piece == PAWN {
            let enpassant_bb = self.enpassant_bb();
            if checkers & self.board.pieces.piece_type[PAWN] != 0 {
                // We are in check from a pawn, therefore the
                // en-passant capture is legal too.
                pseudo_legal_dests |= enpassant_bb;
            }

            unsafe {
                let mut dest_sets: [Bitboard; 4] = uninitialized();
                calc_pawn_dest_sets(self.board.to_move,
                                    occupied_by_us,
                                    *self.board.pieces.color.get_unchecked(1 ^ self.board.to_move),
                                    enpassant_bb,
                                    orig_square_bb,
                                    &mut dest_sets);
                pseudo_legal_dests &= dest_sets[0] | dest_sets[1] | dest_sets[2] | dest_sets[3];
            }
            if pseudo_legal_dests & dest_square_bb == 0 {
                debug_assert!(generated_move.is_none());
                return None;
            }

            match dest_square_bb {
                x if x == enpassant_bb => {
                    if move_type != MOVE_ENPASSANT ||
                       !self.enpassant_special_check_is_ok(orig_square, dest_square) ||
                       promoted_piece_code != 0 {
                        debug_assert!(generated_move.is_none());
                        return None;
                    }
                    captured_piece = PAWN;
                }
                x if x & BB_PAWN_PROMOTION_RANKS != 0 => {
                    if move_type != MOVE_PROMOTION {
                        debug_assert!(generated_move.is_none());
                        return None;
                    }
                }
                _ => {
                    if move_type != MOVE_NORMAL || promoted_piece_code != 0 {
                        debug_assert!(generated_move.is_none());
                        return None;
                    }
                }
            }

        } else {
            // This is not a pawn move, nor a castling move.
            pseudo_legal_dests &= unsafe {
                self.geometry
                    .attacks_from_unsafe(piece, orig_square, self.board.occupied)
            };
            if move_type != MOVE_NORMAL || pseudo_legal_dests & dest_square_bb == 0 ||
               promoted_piece_code != 0 {
                debug_assert!(generated_move.is_none());
                return None;
            }
        }

        let m = Move::new(move_type,
                          orig_square,
                          dest_square,
                          promoted_piece_code,
                          captured_piece,
                          piece,
                          self.board.castling_rights,
                          self.board.enpassant_file,
                          0);
        debug_assert_eq!(generated_move, Some(m));
        Some(m)
    }

    #[inline]
    fn null_move(&self) -> Move {
        let king_square = self.king_square();
        Move::new(MOVE_NORMAL,
                  king_square,
                  king_square,
                  0,
                  PIECE_NONE,
                  KING,
                  self.board.castling_rights,
                  self.board.enpassant_file,
                  0)
    }

    fn do_move(&mut self, m: Move) -> Option<u64> {
        let mut old_hash: u64 = unsafe { uninitialized() };
        let mut h = 0;
        let us = self.board.to_move;
        let them = 1 ^ us;
        let move_type = m.move_type();
        let orig_square = m.orig_square();
        let dest_square = m.dest_square();
        let dest_square_bb = 1 << dest_square;
        let played_piece = m.played_piece();
        let captured_piece = m.captured_piece();

        if cfg!(debug_assertions) {
            // Assert that `m` could be generated by `null_move` or
            // `generate_all`.
            assert!({
                m.is_null() && played_piece == KING
            } ||
                    {
                let mut m1 = m;
                let mut m2 = self.try_move_digest(m.digest()).unwrap();
                m1.set_score(0);
                m2.set_score(0);
                m1 == m2
            });

            // Initialize `old_hash`, which will be used to assert
            // that the returned value (`h`) is calculated correctly.
            old_hash = self.hash();
        }

        // Verify if the move will leave the king in check. (We are
        // certain that all the moves that we generate with pieces
        // other than the king do not leave the king in check.)
        if played_piece == KING {
            if orig_square != dest_square {
                if self.king_would_be_in_check(orig_square, dest_square) {
                    return None;  // the king is in check -- illegal move
                }
            } else {
                if self.is_check() {
                    return None;  // invalid "null move"
                }
            }
        }

        // Tell the evaluator that a move will be played.
        self.evaluator.will_do_move(&self.board, m);

        // Move the rook if the move is castling.
        if move_type == MOVE_CASTLING {
            let side = if dest_square > orig_square {
                KINGSIDE
            } else {
                QUEENSIDE
            };
            let mask = BB_CASTLING_ROOK_MOVEMENT[us][side];
            self.board.pieces.piece_type[ROOK] ^= mask;
            self.board.pieces.color[us] ^= mask;
            h ^= self.zobrist.pieces[us][ROOK][CASTLING_ROOK_MOVEMENT[us][side].0] ^
                 self.zobrist.pieces[us][ROOK][CASTLING_ROOK_MOVEMENT[us][side].1];
        }

        // empty the origin square
        let not_orig_square_bb = !(1 << orig_square);
        self.board.pieces.piece_type[played_piece] &= not_orig_square_bb;
        self.board.pieces.color[us] &= not_orig_square_bb;
        h ^= self.zobrist.pieces[us][played_piece][orig_square];

        // Remove the captured piece (if any).
        if captured_piece < PIECE_NONE {
            let not_captured_bb = if move_type == MOVE_ENPASSANT {
                let captured_pawn_square =
                    (dest_square as isize + PAWN_MOVE_SHIFTS[them][PAWN_PUSH]) as Square;
                h ^= self.zobrist.pieces[them][captured_piece][captured_pawn_square];
                !(1 << captured_pawn_square)
            } else {
                h ^= self.zobrist.pieces[them][captured_piece][dest_square];
                !dest_square_bb
            };
            self.board.pieces.piece_type[captured_piece] &= not_captured_bb;
            self.board.pieces.color[them] &= not_captured_bb;
        }

        // Occupy the destination square.
        let dest_piece = if move_type == MOVE_PROMOTION {
            Move::piece_from_aux_data(m.aux_data())
        } else {
            played_piece
        };
        self.board.pieces.piece_type[dest_piece] |= dest_square_bb;
        self.board.pieces.color[us] |= dest_square_bb;
        h ^= self.zobrist.pieces[us][dest_piece][dest_square];

        unsafe {
            // Update castling rights (null moves do not affect castling).
            if orig_square != dest_square {
                h ^= *self.zobrist
                          .castling_rights
                          .get_unchecked(self.board.castling_rights.value());
                self.board.castling_rights.update(orig_square, dest_square);
                h ^= *self.zobrist
                          .castling_rights
                          .get_unchecked(self.board.castling_rights.value());
            }

            // Update the en-passant file.
            h ^= *self.zobrist.enpassant_file.get_unchecked(self.board.enpassant_file);
            let is_double_push = dest_square as isize - orig_square as isize == [16, -16][us] &&
                                 played_piece == PAWN;
            self.board.enpassant_file = if is_double_push {
                let file = Board::file(dest_square);
                h ^= *self.zobrist.enpassant_file.get_unchecked(file);
                file
            } else {
                8
            };
        }

        // Change the side to move.
        self.board.to_move = them;
        h ^= self.zobrist.to_move;

        // Update the auxiliary fields.
        self.board.occupied = self.board.pieces.color[WHITE] | self.board.pieces.color[BLACK];
        self.checkers.set(BB_ALL);

        // Tell the evaluator that a move was played.
        self.evaluator.done_move(&self.board, m);

        debug_assert!(self.is_legal());
        debug_assert_eq!(old_hash ^ h, self.hash());
        Some(h)
    }

    fn undo_move(&mut self, m: Move) {
        // In this method we basically do the same things that we do
        // in `do_move`, but in reverse.

        let them = self.board.to_move;
        let us = 1 ^ them;
        let move_type = m.move_type();
        let orig_square = m.orig_square();
        let dest_square = m.dest_square();
        let dest_square_bb = 1 << dest_square;
        let played_piece = m.played_piece();
        let captured_piece = m.captured_piece();
        debug_assert!(m.enpassant_file() <= 8);

        // Tell the evaluator that a move will be taken back.
        self.evaluator.will_undo_move(&self.board, m);

        // Change the side to move.
        self.board.to_move = us;

        // Restore the en-passant file.
        self.board.enpassant_file = m.enpassant_file();

        // Restore castling rights.
        self.board.castling_rights = m.castling_rights();

        // Empty the destination square.
        let dest_piece = if move_type == MOVE_PROMOTION {
            Move::piece_from_aux_data(m.aux_data())
        } else {
            played_piece
        };
        self.board.pieces.piece_type[dest_piece] &= !dest_square_bb;
        self.board.pieces.color[us] &= !dest_square_bb;

        // Put back the captured piece (if any).
        if captured_piece < PIECE_NONE {
            let captured_piece_bb = if move_type == MOVE_ENPASSANT {
                gen_shift(dest_square_bb, PAWN_MOVE_SHIFTS[them][PAWN_PUSH])
            } else {
                dest_square_bb
            };
            self.board.pieces.piece_type[captured_piece] |= captured_piece_bb;
            self.board.pieces.color[them] |= captured_piece_bb;
        }

        // Restore the piece on the origin square.
        let orig_square_bb = 1 << orig_square;
        self.board.pieces.piece_type[played_piece] |= orig_square_bb;
        self.board.pieces.color[us] |= orig_square_bb;

        // Move the rook back if the move is castling.
        if move_type == MOVE_CASTLING {
            let side = if dest_square > orig_square {
                KINGSIDE
            } else {
                QUEENSIDE
            };
            let mask = BB_CASTLING_ROOK_MOVEMENT[us][side];
            self.board.pieces.piece_type[ROOK] ^= mask;
            self.board.pieces.color[us] ^= mask;
        }

        // Update the auxiliary fields.
        self.board.occupied = self.board.pieces.color[WHITE] | self.board.pieces.color[BLACK];
        self.checkers.set(BB_ALL);

        // Tell the evaluator that a move was taken back.
        self.evaluator.undone_move(&self.board, m);

        debug_assert!(self.is_legal());
    }

    fn evaluate_move(&self, m: Move) -> Value {
        debug_assert!(m.played_piece() < PIECE_NONE);
        debug_assert!(m.captured_piece() <= PIECE_NONE);
        const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];

        unsafe {
            let mut piece = m.played_piece();
            let captured_piece = m.captured_piece();

            // Try not to waste CPU cycles when the played piece is
            // less valuable than the captured piece.
            if piece > captured_piece {
                return *PIECE_VALUES.get_unchecked(captured_piece);
            }

            // This is the square on which all the action takes place.
            let exchange_square = m.dest_square();

            let color: &[Bitboard; 2] = &self.board().pieces.color;
            let piece_type: &[Bitboard; 6] = &self.board().pieces.piece_type;
            let file_sliders = piece_type[QUEEN] | piece_type[ROOK];
            let diag_sliders = piece_type[QUEEN] | piece_type[BISHOP];
            let geometry = BoardGeometry::get();
            let behind_blocker: &[Bitboard; 64] = geometry.squares_behind_blocker
                                                          .get_unchecked(exchange_square);

            // These variables (along with `piece`) will be updated on each capture:
            let mut us = self.board().to_move;
            let mut depth = 0;
            let mut orig_square_bb = 1 << m.orig_square();
            let mut attackers_and_defenders = self.attacks_to(exchange_square);

            // The `gain` array will hold the total material gained at
            // each `depth`, from the viewpoint of the side that made the
            // last capture (`us`).
            let mut gain: [Value; 34] = uninitialized();
            gain[0] = if m.move_type() == MOVE_PROMOTION {
                piece = Move::piece_from_aux_data(m.aux_data());
                PIECE_VALUES[captured_piece] + PIECE_VALUES[piece] - PIECE_VALUES[PAWN]
            } else {
                *PIECE_VALUES.get_unchecked(captured_piece)
            };

            // Examine the possible exchanges, fill the `gain` array.
            'exchange: while orig_square_bb != 0 {
                let current_gain = *gain.get_unchecked(depth);

                // Store a speculative value that will be used if the
                // captured piece happens to be defended.
                let speculative_gain: &mut Value = gain.get_unchecked_mut(depth + 1);
                *speculative_gain = *PIECE_VALUES.get_unchecked(piece) - current_gain;

                if max(-current_gain, *speculative_gain) < 0 {
                    // The side that made the last capture wins even if
                    // the captured piece happens to be defended. So, we
                    // stop here to save precious CPU cycles. Note that
                    // here we may happen to return an incorrect SEE
                    // value, but the sign will be correct, which is by
                    // far the most important information.
                    break;
                }

                // Register that capturing piece's origin square is now vacant.
                attackers_and_defenders &= !orig_square_bb;

                // Consider adding new attackers/defenders, now that
                // capturing piece's origin square is vacant.
                let behind = self.board().occupied &
                             *behind_blocker.get_unchecked(bsf(orig_square_bb));
                if behind & (file_sliders | diag_sliders) != 0 && piece != KING {
                    attackers_and_defenders |=
                        match behind & file_sliders &
                              geometry.attacks_from_unsafe(ROOK, exchange_square, behind) {
                            0 => {
                                // Not a file slider, possibly a diagonal slider.
                                behind & diag_sliders &
                                geometry.attacks_from_unsafe(BISHOP, exchange_square, behind)
                            }
                            bb => {
                                // A file slider.
                                bb
                            }
                        };
                }

                // Change the side to move.
                us ^= 1;

                // Find the next piece to enter the exchange. (The least
                // valuable piece belonging to the side to move.)
                let candidates = attackers_and_defenders & *color.get_unchecked(us);
                if candidates != 0 {
                    for p in (KING..PIECE_NONE).rev() {
                        let bb = candidates & piece_type[p];
                        if bb != 0 {
                            depth += 1;
                            piece = p;
                            orig_square_bb = lsb(bb);
                            continue 'exchange;
                        }
                    }
                }
                break 'exchange;
            }

            // Negamax the `gain` array for the final static exchange
            // evaluation. (The `gain` array actually represents an unary
            // tree, at each node of which the player can either continue
            // the exchange or back off.)
            while depth > 0 {
                *gain.get_unchecked_mut(depth - 1) = -max(-*gain.get_unchecked(depth - 1),
                                                          *gain.get_unchecked(depth));
                depth -= 1;
            }
            gain[0]
        }
    }
}


impl<T: Evaluator<Board>> SetOption for StdMoveGenerator<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        T::options()
    }

    fn set_option(name: &str, value: &str) {
        T::set_option(name, value)
    }
}


impl<T: Evaluator<Board>> StdMoveGenerator<T> {
    /// A helper method for `create`. It analyzes the position on the
    /// board and decides if it is legal.
    ///
    /// In addition to the obviously messed up `Board` instances (that
    /// for example declare some pieces having no or more than one
    /// color), there are many chess positions that are impossible to
    /// create by normal play. Here we are interested only to guard
    /// against those of the cases that can invalidate our assumptions
    /// about what a "normal" chess position is when writing the code.
    ///
    /// Illegal positions:
    ///
    /// 1. having more or less than 1 king from each color;
    /// 2. having more than 8 pawns of a color;
    /// 3. having more than 16 pieces (and pawns) of one color;
    /// 4. having the side not to move in check;
    /// 5. having pawns on ranks 1 or 8;
    /// 6. having castling rights when the king or the corresponding
    ///    rook is not on its initial square;
    /// 7. having an en-passant square that is not having a pawn of
    ///    corresponding color before, and an empty square on it and
    ///    behind it;
    /// 8. having an en-passant square while the king of the side to
    ///    move would be in check if the passing pawn is moved back to
    ///    its original position.
    fn is_legal(&self) -> bool {
        if self.board.to_move > 1 || self.board.enpassant_file > 8 {
            return false;
        }
        let us = self.board.to_move;
        let them = 1 ^ us;
        let enpassant_bb = self.enpassant_bb();
        let color: &[Bitboard; 2] = &self.board.pieces.color;
        let piece_type: &[Bitboard; 6] = &self.board.pieces.piece_type;

        // Verify if `piece_type` is messed up. `occupied` becomes
        // `BB_ALL` in this case.
        let occupied = piece_type.into_iter().fold(0, |acc, x| {
            if acc & x == 0 {
                acc | x
            } else {
                BB_ALL
            }
        });

        (occupied != BB_ALL) &&
        (occupied == color[us] | color[them] && color[us] & color[them] == 0) &&
        (pop_count(piece_type[KING] & color[us]) == 1 &&
         pop_count(piece_type[KING] & color[them]) == 1) &&
        (pop_count(piece_type[PAWN] & color[us]) <= 8 &&
         pop_count(piece_type[PAWN] & color[them]) <= 8) &&
        (pop_count(color[us]) <= 16 && pop_count(color[them]) <= 16) &&
        (color[us] & self.attacks_to(bsf(piece_type[KING] & color[them])) == 0) &&
        (piece_type[PAWN] & BB_PAWN_PROMOTION_RANKS == 0) &&
        ((!self.board.castling_rights.can_castle(WHITE, QUEENSIDE) ||
          (piece_type[ROOK] & color[WHITE] & 1 << A1 != 0) &&
          (piece_type[KING] & color[WHITE] & 1 << E1 != 0)) &&
         (!self.board.castling_rights.can_castle(WHITE, KINGSIDE) ||
          (piece_type[ROOK] & color[WHITE] & 1 << H1 != 0) &&
          (piece_type[KING] & color[WHITE] & 1 << E1 != 0)) &&
         (!self.board.castling_rights.can_castle(BLACK, QUEENSIDE) ||
          (piece_type[ROOK] & color[BLACK] & 1 << A8 != 0) &&
          (piece_type[KING] & color[BLACK] & 1 << E8 != 0)) &&
         (!self.board.castling_rights.can_castle(BLACK, KINGSIDE) ||
          (piece_type[ROOK] & color[BLACK] & 1 << H8 != 0) &&
          (piece_type[KING] & color[BLACK] & 1 << E8 != 0))) &&
        (enpassant_bb == 0 ||
         {
            let dest_square_bb = gen_shift(enpassant_bb, PAWN_MOVE_SHIFTS[them][PAWN_PUSH]);
            let orig_square_bb = gen_shift(enpassant_bb, -PAWN_MOVE_SHIFTS[them][PAWN_PUSH]);
            let our_king_square = bsf(piece_type[KING] & color[us]);
            (dest_square_bb & piece_type[PAWN] & color[them] != 0) &&
            (enpassant_bb & !occupied != 0) && (orig_square_bb & !occupied != 0) &&
            {
                let mask = orig_square_bb | dest_square_bb;
                let pawns = piece_type[PAWN] ^ mask;
                let occupied = occupied ^ mask;
                let occupied_by_them = color[them] ^ mask;
                0 ==
                occupied_by_them &
                ((self.geometry.attacks_from(ROOK, our_king_square, occupied) &
                  (piece_type[ROOK] | piece_type[QUEEN])) |
                 (self.geometry.attacks_from(BISHOP, our_king_square, occupied) &
                  (piece_type[BISHOP] | piece_type[QUEEN])) |
                 (self.geometry.attacks_from(KNIGHT, our_king_square, occupied) &
                  piece_type[KNIGHT]) |
                 (self.geometry.pawn_attacks[us][our_king_square] & pawns))
            }
        }) &&
        {
            assert!(self.checkers.get() == BB_ALL ||
                    self.checkers.get() ==
                    color[them] & self.attacks_to(bsf(piece_type[KING] & color[us])));
            self.board.occupied == occupied
        }
    }

    /// A helper method. It finds all squares attacked by `piece` from
    /// square `orig_square`, and for each square that is within the
    /// `legal_dests` set adds a new move to `moves`. `piece` must not
    /// be `PAWN`.
    #[inline]
    fn add_piece_moves<U: AddMove>(&self,
                                   piece: PieceType,
                                   orig_square: Square,
                                   legal_dests: Bitboard,
                                   moves: &mut U) {
        debug_assert!(piece < PAWN);
        debug_assert!(orig_square <= 63);
        debug_assert!(legal_dests & self.board.pieces.color[self.board.to_move] == 0);

        let mut piece_legal_dests = legal_dests &
                                    unsafe {
            self.geometry
                .attacks_from_unsafe(piece, orig_square, self.board.occupied)
        };
        while piece_legal_dests != 0 {
            let dest_square = bsf_reset(&mut piece_legal_dests);
            let captured_piece = self.get_piece_type_at(dest_square);
            moves.add_move(Move::new(MOVE_NORMAL,
                                     orig_square,
                                     dest_square,
                                     0,
                                     captured_piece,
                                     piece,
                                     self.board.castling_rights,
                                     self.board.enpassant_file,
                                     0));
        }
    }

    /// A helper method. It adds all pseudo-legal moves by the set of
    /// pawns given by `pawns` to `moves`, ensuring that all
    /// destination squares are within the `legal_dests` set. When
    /// `only_queen_promotions` is `true`, promotions to pieces other
    /// that queen will not be added to `moves`.
    fn add_pawn_moves<U: AddMove>(&self,
                                  pawns: Bitboard,
                                  legal_dests: Bitboard,
                                  only_queen_promotions: bool,
                                  moves: &mut U) {
        debug_assert!(pawns & !self.board.pieces.piece_type[PAWN] == 0);
        debug_assert!(pawns & !self.board.pieces.color[self.board.to_move] == 0);

        let mut dest_sets: [Bitboard; 4];
        let enpassant_bb = self.enpassant_bb();
        let shifts = unsafe {
            dest_sets = uninitialized();
            calc_pawn_dest_sets(self.board.to_move,
                                *self.board.pieces.color.get_unchecked(self.board.to_move),
                                *self.board.pieces.color.get_unchecked(1 ^ self.board.to_move),
                                enpassant_bb,
                                pawns,
                                &mut dest_sets)
        };

        // Process each pawn move sub-type (push, double push, west
        // capture, east capture).
        for i in 0..4 {
            let mut pawn_legal_dests = dest_sets[i] & legal_dests;

            // For each legal destination, determine the move type
            // (en-passant capture, pawn promotion, normal move), and
            // push the move to `moves`.
            while pawn_legal_dests != 0 {
                let dest_square = bsf_reset(&mut pawn_legal_dests);
                let orig_square = (dest_square as isize - shifts[i]) as Square;
                let captured_piece = self.get_piece_type_at(dest_square);
                match 1 << dest_square {

                    // en-passant capture
                    x if x == enpassant_bb => {
                        if self.enpassant_special_check_is_ok(orig_square, dest_square) {
                            moves.add_move(Move::new(MOVE_ENPASSANT,
                                                     orig_square,
                                                     dest_square,
                                                     0,
                                                     PAWN,
                                                     PAWN,
                                                     self.board.castling_rights,
                                                     self.board.enpassant_file,
                                                     0));
                        }
                    }

                    // pawn promotion
                    x if x & BB_PAWN_PROMOTION_RANKS != 0 => {
                        for p in 0..4 {
                            moves.add_move(Move::new(MOVE_PROMOTION,
                                                     orig_square,
                                                     dest_square,
                                                     p,
                                                     captured_piece,
                                                     PAWN,
                                                     self.board.castling_rights,
                                                     self.board.enpassant_file,
                                                     0));
                            if only_queen_promotions {
                                break;
                            }
                        }
                    }

                    // normal pawn move
                    _ => {
                        moves.add_move(Move::new(MOVE_NORMAL,
                                                 orig_square,
                                                 dest_square,
                                                 0,
                                                 captured_piece,
                                                 PAWN,
                                                 self.board.castling_rights,
                                                 self.board.enpassant_file,
                                                 0));
                    }
                }
            }
        }
    }

    /// A helper method. It returns all pinned pieces belonging to the
    /// side to move. `king_square` should the side to move's king
    /// square.
    fn find_pinned(&self, king_square: Square) -> Bitboard {
        let mut pinned = 0;
        unsafe {
            let g: &BoardGeometry = &self.geometry;
            let occupied_by_them = *self.board.pieces.color.get_unchecked(1 ^ self.board.to_move);

            // To find the potential pinners, we "remove" all our
            // pieces from the board, and then verify if a bishop or a
            // rook placed on our king's square can attack any enemy
            // bishops, rooks, or queens.
            let file_sliders = self.board.pieces.piece_type[QUEEN] |
                               self.board.pieces.piece_type[ROOK];
            let diag_sliders = self.board.pieces.piece_type[QUEEN] |
                               self.board.pieces.piece_type[BISHOP];
            let mut pinners = occupied_by_them &
                              (file_sliders &
                               g.attacks_from_unsafe(ROOK, king_square, occupied_by_them) |
                               diag_sliders &
                               g.attacks_from_unsafe(BISHOP, king_square, occupied_by_them));

            // Then, for each potential pinner we verify if there is
            // exactly one defender between our king and the pinner.
            if pinners != 0 {
                let defenders = *self.board.pieces.color.get_unchecked(self.board.to_move) &
                                !(1 << king_square);
                loop {
                    let pinner_square = bsf_reset(&mut pinners);
                    let bb = defenders &
                             *g.squares_between_including
                               .get_unchecked(king_square)
                               .get_unchecked(pinner_square);
                    if lsb(bb) == bb {
                        pinned |= bb;
                    }
                    if pinners == 0 {
                        break;
                    }
                }
            }
        }
        pinned
    }

    /// A helper method. It returns the square that the king of the
    /// side to move occupies.
    #[inline]
    fn king_square(&self) -> Square {
        bsf(self.board.pieces.piece_type[KING] &
            unsafe { *self.board.pieces.color.get_unchecked(self.board.to_move) })
    }

    /// Returns a bitboard with all enemy pieces and pawns that attack
    /// the king of the side to move.
    ///
    /// The calculated bitboard is temporarily stored in move
    /// generator's instance. In case another call to `checkers` is
    /// made before any move was done/undone, `checkers` will return
    /// the stored bitboard instead of re-calculating it, thus saving
    /// time.
    #[inline]
    fn checkers(&self) -> Bitboard {
        if self.checkers.get() == BB_ALL {
            self.checkers
                .set(self.attacks_to(self.king_square()) &
                     unsafe { *self.board.pieces.color.get_unchecked(1 ^ self.board.to_move) });
        }
        self.checkers.get()
    }

    /// A helper method. It returns the square that the king of the
    /// side to move occupies, and its checker. Needed only for
    /// performance reasons.
    #[inline]
    fn king_square_and_checkers(&self) -> (Square, Bitboard) {
        let king_square = self.king_square();
        if self.checkers.get() == BB_ALL {
            self.checkers
                .set(self.attacks_to(king_square) &
                     unsafe { *self.board.pieces.color.get_unchecked(1 ^ self.board.to_move) });
        }
        (king_square, self.checkers.get())
    }

    /// A helper method. It returns if the king of the side to move
    /// would be in check if moved from `orig_square` to
    /// `dest_square`.
    fn king_would_be_in_check(&self, orig_square: Square, dest_square: Square) -> bool {
        debug_assert_eq!(orig_square, self.king_square());
        debug_assert!(dest_square <= 63);
        let occupied = self.board.occupied & !(1 << orig_square);
        unsafe {
            *self.board.pieces.color.get_unchecked(1 ^ self.board.to_move) &
            ((self.geometry.attacks_from_unsafe(ROOK, dest_square, occupied) &
              (self.board.pieces.piece_type[ROOK] | self.board.pieces.piece_type[QUEEN])) |
             (self.geometry.attacks_from_unsafe(BISHOP, dest_square, occupied) &
              (self.board.pieces.piece_type[BISHOP] | self.board.pieces.piece_type[QUEEN])) |
             (self.geometry.attacks_from_unsafe(KNIGHT, dest_square, occupied) &
              self.board.pieces.piece_type[KNIGHT]) |
             (self.geometry.attacks_from_unsafe(KING, dest_square, occupied) &
              self.board.pieces.piece_type[KING]) |
             (*self.geometry
                   .pawn_attacks
                   .get_unchecked(self.board.to_move)
                   .get_unchecked(dest_square) &
              self.board.pieces.piece_type[PAWN])) != 0
        }
    }

    /// A helper method. It returns if castling on the given `side` is
    /// pseudo-legal. `king_square` should be the square that the king
    /// of the side to move occupies.
    #[inline]
    fn can_castle(&self, king_square: Square, side: CastlingSide) -> bool {
        debug_assert_eq!(king_square, self.king_square());
        const BETWEEN: [[Bitboard; 2]; 2] = [[1 << B1 | 1 << C1 | 1 << D1, 1 << F1 | 1 << G1],
                                             [1 << B8 | 1 << C8 | 1 << D8, 1 << F8 | 1 << G8]];
        unsafe {
            self.board.castling_rights.can_castle(self.board.to_move, side) &&
            (self.board.occupied &
             *BETWEEN.get_unchecked(self.board.to_move).get_unchecked(side) == 0) &&
            !self.is_check() &&
            !self.king_would_be_in_check(king_square,
                                         *[[D1, F1], [D8, F8]]
                                              .get_unchecked(self.board.to_move)
                                              .get_unchecked(side))
        }
    }

    /// A helper method. It returns a bitboard representing the
    /// en-passant target square if there is one.
    #[inline]
    fn enpassant_bb(&self) -> Bitboard {
        if self.board.enpassant_file >= 8 {
            0
        } else {
            unsafe {
                *[1 << A6, 1 << A3].get_unchecked(self.board.to_move) << self.board.enpassant_file
            }
        }
    }

    /// A helper method. It tests for the rare occasion when the two
    /// pawns participating in the en-passant capture, disappearing
    /// from the 4/5-th rank in one move, discover a check along this
    /// rank. `orig_square` and `dist_square` are the origin and
    /// destination squares of the capturing pawn.
    fn enpassant_special_check_is_ok(&self, orig_square: Square, dest_square: Square) -> bool {
        let king_square = self.king_square();
        if Board::rank(king_square) == Board::rank(orig_square) {
            let pawn1_bb = 1 << orig_square;
            let pawn2_bb = gen_shift(1 << dest_square,
                                     -PAWN_MOVE_SHIFTS[self.board.to_move][PAWN_PUSH]);
            let occupied = self.board.occupied & !(pawn1_bb | pawn2_bb);
            return 0 ==
                   self.geometry.attacks_from(ROOK, king_square, occupied) &
                   self.board.pieces.color[1 ^ self.board.to_move] &
                   (self.board.pieces.piece_type[ROOK] | self.board.pieces.piece_type[QUEEN]);
        }
        true
    }

    /// A helper method. It returns the type of the piece at `square`.
    #[inline(always)]
    fn get_piece_type_at(&self, square: Square) -> PieceType {
        debug_assert!(square <= 63);
        let bb = 1 << square & self.board.occupied;
        if bb == 0 {
            return PIECE_NONE;
        }
        for i in (QUEEN..PIECE_NONE).rev() {
            if bb & self.board.pieces.piece_type[i] != 0 {
                return i;
            }
        }
        debug_assert!(bb & self.board.pieces.piece_type[KING] != 0);
        return KING;
    }
}


/// Pawn move sub-type -- a single push.
const PAWN_PUSH: usize = 0;

/// Pawn move sub-type -- a double push.
const PAWN_DOUBLE_PUSH: usize = 1;

/// Pawn move sub-type -- a capture toward the queen-side.
#[allow(dead_code)]
const PAWN_WEST_CAPTURE: usize = 2;

/// Pawn move sub-type -- a capture toward the king-side.
#[allow(dead_code)]
const PAWN_EAST_CAPTURE: usize = 3;


/// Constants used for the generation of pawn moves (by bit shifting)
/// -- one for each color and pawn move sub-type.
///
/// Example: The bitboard for a white pawn on "e2" is `1 << E2`. If
/// the pawn is pushed one square forward, the updated bitboard would
/// be: `gen_shift(1 << E2, PAWN_MOVE_SHIFTS[WHITE][PAWN_PUSH])`
static PAWN_MOVE_SHIFTS: [[isize; 4]; 2] = [[8, 16, 7, 9], [-8, -16, -9, -7]];


/// The squares on rank 1 and rank 8.
const BB_PAWN_PROMOTION_RANKS: Bitboard = BB_RANK_1 | BB_RANK_8;

/// The origin and destination squares of the castling rook.
const CASTLING_ROOK_MOVEMENT: [[(Square, Square); 2]; 2] = [[(A1, D1), (H1, F1)],
                                                            [(A8, D8), (H8, F8)]];

/// Bitboards for the origin and destination squares of the castling rook.
const BB_CASTLING_ROOK_MOVEMENT: [[Bitboard; 2]; 2] = [[1 << A1 | 1 << D1, 1 << H1 | 1 << F1],
                                                       [1 << A8 | 1 << D8, 1 << H8 | 1 << F8]];


/// A helper function. It calculates the pseudo-legal destinations
/// for a given set of `pawns`, and writes them to the supplied
/// `dest_sets` array.
///
/// `dest_sets` is indexed by the sub-type of the pawn move: 0) push,
/// 1) double push, 2) west capture, 3) east capture. The benefit of
/// this separation is that knowing the destination square and the
/// pawn move sub-type (the index in the `dest_sets` array) is enough
/// to recover the origin square.
#[inline]
fn calc_pawn_dest_sets(us: Color,
                       occupied_by_us: Bitboard,
                       occupied_by_them: Bitboard,
                       enpassant_bb: Bitboard,
                       pawns: Bitboard,
                       dest_sets: &mut [Bitboard; 4])
                       -> &'static [isize; 4] {
    debug_assert!(us <= 1);
    debug_assert!(pawns & !occupied_by_us == 0);
    debug_assert!(occupied_by_us & occupied_by_them == 0);
    debug_assert!(gen_shift(enpassant_bb, -PAWN_MOVE_SHIFTS[us][PAWN_PUSH]) & !occupied_by_them ==
                  0);
    const PUSHING_TARGETS: [Bitboard; 4] = [BB_ALL, BB_ALL, 0, 0];
    const LEGITIMATE_ORIGINS: [Bitboard; 4] = [!(BB_RANK_1 | BB_RANK_8),
                                               BB_RANK_2 | BB_RANK_7,
                                               !(BB_FILE_A | BB_RANK_1 | BB_RANK_8),
                                               !(BB_FILE_H | BB_RANK_1 | BB_RANK_8)];
    let shifts: &[isize; 4] = unsafe { PAWN_MOVE_SHIFTS.get_unchecked(us) };
    let capture_targets = occupied_by_them | enpassant_bb;
    for i in 0..4 {
        dest_sets[i] = gen_shift(pawns & LEGITIMATE_ORIGINS[i], shifts[i]) &
                       (capture_targets ^ PUSHING_TARGETS[i]) &
                       !occupied_by_us;
    }

    // Double pushes are trickier -- for a double push to be
    // pseudo-legal, a single push must be pseudo-legal too.
    dest_sets[PAWN_DOUBLE_PUSH] &= gen_shift(dest_sets[PAWN_PUSH], shifts[PAWN_PUSH]);

    // For convenience, return a reference to the pawn shift array for `us`.
    shifts
}


#[cfg(test)]
mod tests {
    use board::*;
    use squares::*;
    use utils::MoveStack;
    use move_generator::*;
    use evaluator::*;
    use stock::{StdMoveGenerator, SimpleEvaluator};

    impl<E: Evaluator<Board>> StdMoveGenerator<E> {
        fn from_fen(fen: &str) -> Result<StdMoveGenerator<E>, IllegalBoard> {
            StdMoveGenerator::from_board(try!(Board::from_fen(fen)))
        }
    }
    type P = StdMoveGenerator<SimpleEvaluator>;

    #[test]
    fn attacks_to() {
        let b = P::from_fen("8/8/8/3K1p1P/r4k2/3Pq1N1/7p/1B5Q w - - 0 1").ok().unwrap();
        let white = b.board().pieces.color[WHITE];
        let black = b.board().pieces.color[BLACK];

        assert_eq!(white & b.attacks_to(E4),
                   1 << D3 | 1 << G3 | 1 << D5 | 1 << H1);
        assert_eq!(black & b.attacks_to(E4),
                   1 << E3 | 1 << F4 | 1 << F5 | 1 << A4);
        assert_eq!(black & b.attacks_to(G6), 0);
        assert_eq!(white & b.attacks_to(G6), 1 << H5);
        assert_eq!(white & b.attacks_to(C2), 1 << B1);
        assert_eq!(white & b.attacks_to(F4), 0);
        assert_eq!(black & b.attacks_to(F4), 1 << A4 | 1 << E3);
        assert_eq!(black & b.attacks_to(F5), 1 << F4);
        assert_eq!(white & b.attacks_to(A6), 0);
        assert_eq!(black & b.attacks_to(G1), 1 << H2 | 1 << E3);
        assert_eq!(black & b.attacks_to(A1), 1 << A4);
    }

    #[test]
    fn piece_type_constants() {
        assert_eq!(KING, 0);
        assert_eq!(QUEEN, 1);
        assert_eq!(ROOK, 2);
        assert_eq!(BISHOP, 3);
        assert_eq!(KNIGHT, 4);
        assert_eq!(PAWN, 5);
    }

    #[test]
    fn pawn_dest_sets() {
        let mut s = MoveStack::new();

        let b = P::from_fen("k2q4/4Ppp1/5P2/6Pp/6P1/8/7P/7K w - h6 0 1").ok().unwrap();
        b.generate_all(&mut s);
        let mut pawn_dests = 0u64;
        while let Some(m) = s.pop() {
            if m.played_piece() == PAWN {
                pawn_dests |= 1 << m.dest_square();
            }
        }
        assert_eq!(pawn_dests,
                   1 << H3 | 1 << H4 | 1 << G6 | 1 << E8 | 1 << H5 | 1 << G7 | 1 << H6 | 1 << D8);

        let b = P::from_fen("k2q4/4Ppp1/5P2/6Pp/6P1/8/7P/7K b - - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        let mut pawn_dests = 0u64;
        while let Some(m) = s.pop() {
            if m.played_piece() == PAWN {
                pawn_dests |= 1 << m.dest_square();
            }
        }
        assert_eq!(pawn_dests, 1 << H4 | 1 << G6 | 1 << G4 | 1 << F6);
    }

    #[test]
    fn move_generation() {
        let mut s = MoveStack::new();

        let b = P::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/4K3 w - - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 5);
        s.clear_all();

        let b = P::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/6K1 w - - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 7);
        s.clear_all();

        let b = P::from_fen("8/8/6NK/2pP4/3PR3/2b1q3/3P4/7k w - - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 8);
        s.clear_all();

        let b = P::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/7K w - - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 22);
        s.clear_all();

        let b = P::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/7K w - c6 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 23);
        s.clear_all();

        let b = P::from_fen("K7/8/6N1/2pP4/3PR3/2b1q3/3P4/7k b - - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 25);
        s.clear_all();

        let b = P::from_fen("K7/8/6N1/2pP4/3PR2k/2b1q3/3P4/8 b - - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 5);
        s.clear_all();

        assert!(P::from_fen("8/8/7k/8/4pP2/8/3B4/7K b - f3 0 1").is_err());
        assert!(P::from_fen("8/8/8/8/4pP2/8/3B4/7K b - f3 0 1").is_err());
        assert!(P::from_fen("8/8/8/4k3/4pP2/8/3B4/7K b - f3 0 1").is_ok());

        let b = P::from_fen("8/8/8/7k/5pP1/8/8/5R1K b - g3 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 6);
        s.clear_all();

        let b = P::from_fen("8/8/8/5k2/5pP1/8/8/5R1K b - g3 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 7);
        s.clear_all();

        let b = P::from_fen("8/8/8/8/4pP1k/8/8/4B2K b - f3 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 5);
        s.clear_all();

        let b = P::from_fen("8/8/8/8/4RpPk/8/8/7K b - g3 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 6);
        s.clear_all();

        let b = P::from_fen("8/8/8/8/3QPpPk/8/8/7K b - g3 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 7);
        s.clear();

        let b = P::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R w - - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 19 + 5);
        s.clear_all();

        let b = P::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R w K - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 19 + 6);
        s.clear_all();

        let b = P::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 19 + 7);
        s.clear_all();

        let b = P::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R b KQ - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 19 + 5);
        s.clear_all();

        let b = P::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R b KQk - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 19 + 6);
        s.clear_all();

        let b = P::from_fen("4k3/8/8/8/8/5n2/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 5);
        s.clear_all();

        let mut b = P::from_fen("4k3/8/8/8/8/6n1/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        let mut count = 0;
        while let Some(m) = s.pop() {
            if b.do_move(m).is_some() {
                count += 1;
                b.undo_move(m);
            }
        }
        assert_eq!(count, 19 + 4);

        let b = P::from_fen("4k3/8/8/8/8/4n3/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 19 + 5);
        s.clear_all();

        let b = P::from_fen("4k3/8/8/8/8/4n3/8/R3K2R w - - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 19 + 5);
        s.clear_all();

        let b = P::from_fen("4k3/8/1b6/8/8/8/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        assert_eq!(s.list().len(), 19 + 7);
        s.clear_all();
    }

    #[test]
    fn do_undo_move() {
        let mut s = MoveStack::new();

        let mut b = P::from_fen("b3k2r/6P1/8/5pP1/8/8/6P1/R3K2R w kKQ f6 0 1").ok().unwrap();
        b.generate_all(&mut s);
        let count = s.list().len();
        while let Some(m) = s.pop() {
            if let Some(h) = b.do_move(m) {
                assert!(h != 0);
                b.undo_move(m);
                let mut s2 = MoveStack::new();
                b.generate_all(&mut s2);
                assert_eq!(count, s2.list().len());
            }
        }
        assert_eq!(s.list().len(), 0);

        let mut b = P::from_fen("b3k2r/6P1/8/5pP1/8/8/8/R3K2R b kKQ - 0 1").ok().unwrap();
        b.generate_all(&mut s);
        let count = s.list().len();
        while let Some(m) = s.pop() {
            if let Some(h) = b.do_move(m) {
                assert!(h != 0);
                b.undo_move(m);
                let mut s2 = MoveStack::new();
                b.generate_all(&mut s2);
                assert_eq!(count, s2.list().len());
            }
        }
        assert_eq!(s.list().len(), 0);
    }

    #[test]
    fn find_pinned() {
        let b = P::from_fen("k2r4/3r4/3N4/5n2/qp1K2Pq/8/3PPR2/6b1 w - - 0 1").ok().unwrap();
        assert_eq!(b.find_pinned(b.king_square()), 1 << F2 | 1 << D6 | 1 << G4);
    }

    #[test]
    fn generate_forcing() {
        let mut s = MoveStack::new();

        let b = P::from_fen("k6r/P7/8/6p1/6pP/8/8/7K b - h3 0 1").ok().unwrap();
        b.generate_forcing(false, &mut s);
        assert_eq!(s.list().len(), 4);
        s.clear_all();

        let b = P::from_fen("k7/8/8/4Pp2/4K3/8/8/8 w - f6 0 1").ok().unwrap();
        b.generate_forcing(false, &mut s);
        assert_eq!(s.list().len(), 8);
        s.clear_all();

        let b = P::from_fen("k7/8/8/4Pb2/4K3/8/8/8 w - - 0 1").ok().unwrap();
        b.generate_forcing(false, &mut s);
        assert_eq!(s.list().len(), 7);
        s.clear_all();
    }

    #[test]
    fn null_move() {
        let mut s = MoveStack::new();

        let mut b = P::from_fen("k7/8/8/5Pp1/8/8/8/4K2R w K g6 0 1").ok().unwrap();
        b.generate_all(&mut s);
        let count = s.list().len();
        s.clear_all();
        let m = b.null_move();
        assert!(b.do_move(m).is_some());
        b.undo_move(m);
        b.generate_all(&mut s);
        assert_eq!(count, s.list().len());
        s.clear_all();

        let mut b = P::from_fen("k7/4r3/8/8/8/8/8/4K3 w - - 0 1").ok().unwrap();
        let m = b.null_move();
        assert!(b.do_move(m).is_none());
    }

    #[test]
    fn move_into_check_bug() {
        let mut s = MoveStack::new();
        let fen = "rnbq1bn1/pppP3k/8/3P2B1/2B5/5N2/PPPN1PP1/2K4R b - - 0 1";
        let mut b = P::from_fen(fen).ok().unwrap();
        b.generate_all(&mut s);
        let m = s.pop().unwrap();
        b.do_move(m);
        assert!(b.is_legal());
    }

    #[test]
    fn try_move_digest() {
        use std::mem::transmute;
        use evaluator::*;
        fn try_all<E: Evaluator<Board>>(b: &StdMoveGenerator<E>, stack: &MoveStack) {
            let mut i = 0u16;
            loop {
                if let Some(m) = b.try_move_digest(unsafe { transmute(i) }) {
                    assert!(stack.list().iter().find(|x| **x == m).is_some());
                }
                if i == 0xffff {
                    break;
                } else {
                    i += 1;
                }
            }
        }
        let mut s = MoveStack::new();

        s.clear_all();
        let fen = "rnbqk2r/p1p1pppp/8/8/2Pp4/5NP1/pP1PPPBP/RNBQK2R b KQkq c3 0 1";
        let b = P::from_fen(fen).ok().unwrap();
        b.generate_all(&mut s);
        try_all(&b, &s);

        s.clear_all();
        let fen = "rnbqk2r/p1p1pppp/8/8/Q1Pp4/5NP1/pP1PPPBP/RNB1K2R b KQkq - 0 1";
        let b = P::from_fen(fen).ok().unwrap();
        b.generate_all(&mut s);
        try_all(&b, &s);

        s.clear_all();
        let fen = "rnbqk2r/p1p1pppp/3N4/8/Q1Pp4/6P1/pP1PPPBP/RNB1K2R b KQkq - 0 1";
        let b = P::from_fen(fen).ok().unwrap();
        b.generate_all(&mut s);
        try_all(&b, &s);

        s.clear_all();
        let fen = "rnbq3r/p1p1pppp/8/3k4/2Pp4/5NP1/pP1PPPBP/RNBQK2R b KQ c3 0 1";
        let b = P::from_fen(fen).ok().unwrap();
        b.generate_all(&mut s);
        try_all(&b, &s);

        s.clear_all();
        let fen = "rn1qk2r/p1pbpppp/8/8/Q1Pp4/5NP1/pP1PPPBP/RNB1K2R b KQkq - 0 1";
        let b = P::from_fen(fen).ok().unwrap();
        b.generate_all(&mut s);
        try_all(&b, &s);

        s.clear_all();
        let fen = "8/8/8/8/4RpPk/8/8/7K b - g3 0 1";
        let b = P::from_fen(fen).ok().unwrap();
        b.generate_all(&mut s);
        try_all(&b, &s);

        s.clear_all();
        let fen = "8/8/8/8/5pPk/8/8/7K b - g3 0 1";
        let b = P::from_fen(fen).ok().unwrap();
        b.generate_all(&mut s);
        try_all(&b, &s);
    }

    #[test]
    fn perft() {
        use utils::perft;
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let mut b = P::from_fen(fen).ok().unwrap();
        assert_eq!(perft(&mut b, 0), 1);
        assert_eq!(perft(&mut b, 1), 20);
        assert_eq!(perft(&mut b, 2), 400);
        assert_eq!(perft(&mut b, 3), 8_902);

        let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
        let mut b = P::from_fen(fen).ok().unwrap();
        assert_eq!(perft(&mut b, 1), 48);
        assert_eq!(perft(&mut b, 2), 2_039);
        assert_eq!(perft(&mut b, 3), 97_862);

        let fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
        let mut b = P::from_fen(fen).ok().unwrap();
        assert_eq!(perft(&mut b, 1), 14);
        assert_eq!(perft(&mut b, 2), 191);
        assert_eq!(perft(&mut b, 3), 2_812);
        assert_eq!(perft(&mut b, 4), 43_238);

        let fen = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
        let mut b = P::from_fen(fen).ok().unwrap();
        assert_eq!(perft(&mut b, 1), 6);
        assert_eq!(perft(&mut b, 2), 264);
        assert_eq!(perft(&mut b, 3), 9467);

        let fen = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
        let mut b = P::from_fen(fen).ok().unwrap();
        assert_eq!(perft(&mut b, 1), 44);
        assert_eq!(perft(&mut b, 2), 1_486);
        assert_eq!(perft(&mut b, 3), 62_379);

        let fen = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";
        let mut b = P::from_fen(fen).ok().unwrap();
        assert_eq!(perft(&mut b, 1), 46);
        assert_eq!(perft(&mut b, 2), 2_079);
        assert_eq!(perft(&mut b, 3), 89_890);
    }
}
