//! Implements the internal chess board and the move generation logic.

use std::mem::uninitialized;
use std::cell::Cell;
use basetypes::*;
use moves::*;
use notation::parse_fen;
use position::bitsets::*;
use position::IllegalPosition;
use position::tables::{BoardGeometry, ZobristArrays};


/// Holds the current position and can determine which moves are
/// legal.
///
/// In a nutshell, `Board` can generate all possible moves in the
/// current position, play a selected move, and take it back. It can
/// also play a "null move" which can be used to selectively prune the
/// search tree. `Board` does not try to be clever. In particular, it
/// is completely unaware of repeating positions, rule-50, chess
/// strategy or tactics.
#[derive(Clone)]
pub struct Board {
    geometry: &'static BoardGeometry,
    zobrist: &'static ZobristArrays,

    /// The placement of the pieces on the board.
    pieces: PiecesPlacement,

    /// The side to move.
    to_move: Color,

    /// The castling rights for both players.
    castling: CastlingRights,

    /// The file on which an en-passant pawn capture is
    /// possible. Values between 8 and 15 indicate that en-passant
    /// capture is not possible.
    en_passant_file: usize,

    /// This will always be equal to `self.pieces.color[WHITE] |
    /// self.pieces.color[BLACK]`
    _occupied: Bitboard,

    /// The square on which the king of the side to move is
    /// placed. Lazily calculated, >= 64 if not calculated yet.
    _king_square: Cell<Square>,

    /// Lazily calculated bitboard of all checkers --
    /// `BB_UNIVERSAL_SET` if not calculated yet.
    _checkers: Cell<Bitboard>,

    /// Lazily calculated bitboard of all pinned pieces and pawns --
    /// `BB_UNIVERSAL_SET` if not calculated yet.
    _pinned: Cell<Bitboard>,
}


impl Board {
    /// Creates a new board instance.
    ///
    /// This function makes expensive verification to make sure that
    /// the resulting new board is legal.
    pub fn create(pieces_placement: &PiecesPlacement,
                  to_move: Color,
                  castling: CastlingRights,
                  en_passant_square: Option<Square>)
                  -> Result<Board, IllegalPosition> {

        let en_passant_rank = match to_move {
            WHITE => RANK_6,
            BLACK => RANK_3,
            _ => return Err(IllegalPosition),
        };
        let en_passant_file = match en_passant_square {
            None => NO_ENPASSANT_FILE,
            Some(x) if x <= 63 && rank(x) == en_passant_rank => file(x),
            _ => return Err(IllegalPosition),
        };
        let b = Board {
            geometry: BoardGeometry::get(),
            zobrist: ZobristArrays::get(),
            pieces: *pieces_placement,
            to_move: to_move,
            castling: castling,
            en_passant_file: en_passant_file,
            _occupied: pieces_placement.color[WHITE] | pieces_placement.color[BLACK],
            _king_square: Cell::new(64),
            _checkers: Cell::new(BB_UNIVERSAL_SET),
            _pinned: Cell::new(BB_UNIVERSAL_SET),
        };

        if b.is_legal() {
            Ok(b)
        } else {
            Err(IllegalPosition)
        }
    }

    /// Creates a new board instance from a FEN string.
    ///
    /// A FEN (Forsyth–Edwards Notation) string defines a particular
    /// position using only the ASCII character set. This function
    /// makes expensive verification to make sure that the resulting
    /// new board is legal.
    pub fn from_fen(fen: &str) -> Result<Board, IllegalPosition> {
        let (ref placement, to_move, castling, en_passant_square, _, _) =
            try!(parse_fen(fen).map_err(|_| IllegalPosition));
        Board::create(placement, to_move, castling, en_passant_square)
    }

    /// Returns a reference to a properly initialized `BoardGeometry`
    /// object.
    #[inline(always)]
    pub fn geometry(&self) -> &BoardGeometry {
        self.geometry
    }

    /// Returns a reference to a properly initialized `ZobristArrays`
    /// object.
    #[inline(always)]
    pub fn zobrist(&self) -> &ZobristArrays {
        self.zobrist
    }

    /// Returns a description of the placement of the pieces on the
    /// board.
    #[inline(always)]
    pub fn pieces(&self) -> &PiecesPlacement {
        &self.pieces
    }

    /// Returns the side to move.
    #[inline(always)]
    pub fn to_move(&self) -> Color {
        self.to_move
    }

    /// Returns the castling rights.
    #[inline(always)]
    pub fn castling(&self) -> CastlingRights {
        self.castling
    }

    /// Returns the file on which an en-passant pawn capture is
    /// possible.
    #[inline(always)]
    pub fn en_passant_file(&self) -> Option<File> {
        if self.en_passant_file < 8 {
            Some(self.en_passant_file)
        } else {
            None
        }
    }

    /// Returns a bitboard of all occupied squares.
    #[inline(always)]
    pub fn occupied(&self) -> Bitboard {
        self._occupied
    }

    /// Returns the bitboard of all checkers that are attacking the
    /// king.
    ///
    /// The bitboard of all checkers is calculated the first time it
    /// is needed and is saved to the `_checkers` filed, in case it is
    /// needed again. If there is a saved value already, the call to
    /// `checkers` is practically free.
    #[inline]
    pub fn checkers(&self) -> Bitboard {
        if self._checkers.get() == BB_UNIVERSAL_SET {
            self._checkers.set(self.attacks_to(1 ^ self.to_move, self.king_square()));
        }
        self._checkers.get()
    }

    /// Returns the bitboard of all pinned pieces and pawns of the
    /// color of the side to move.
    ///
    /// The bitboard of all pinned pieces and pawns is calculated the
    /// first time it is needed and is saved to the `_pinned` filed,
    /// in case it is needed again. If there is a saved value already,
    /// the call to `pinned` is practically free.
    #[inline]
    pub fn pinned(&self) -> Bitboard {
        if self._pinned.get() == BB_UNIVERSAL_SET {
            self._pinned.set(self.find_pinned());
        }
        self._pinned.get()
    }

    /// Returns a bitboard of all pieces and pawns of color `us` that
    /// attack `square`.
    pub fn attacks_to(&self, us: Color, square: Square) -> Bitboard {
        let occupied_by_us = self.pieces.color[us];
        if square > 63 {
            // We call "piece_attacks_from()" here many times, which for
            // performance reasons do not do array boundary checks. Since
            // "Board::attacks_to()" is a public function, we have to
            // guarantee memory safety for all its users.
            panic!("invalid square");
        }
        let square_bb = 1 << square;
        unsafe {
            let shifts: &[isize; 4] = PAWN_MOVE_SHIFTS.get_unchecked(us);

            (self.geometry.piece_attacks_from(ROOK, square, self.occupied()) & occupied_by_us &
             (self.pieces.piece_type[ROOK] | self.pieces.piece_type[QUEEN])) |
            (self.geometry.piece_attacks_from(BISHOP, square, self.occupied()) & occupied_by_us &
             (self.pieces.piece_type[BISHOP] | self.pieces.piece_type[QUEEN])) |
            (self.geometry.piece_attacks_from(KNIGHT, square, self.occupied()) & occupied_by_us &
             self.pieces.piece_type[KNIGHT]) |
            (self.geometry.piece_attacks_from(KING, square, self.occupied()) & occupied_by_us &
             self.pieces.piece_type[KING]) |
            (gen_shift(square_bb, -shifts[PAWN_EAST_CAPTURE]) & occupied_by_us &
             self.pieces.piece_type[PAWN] & !(BB_FILE_H | BB_RANK_1 | BB_RANK_8)) |
            (gen_shift(square_bb, -shifts[PAWN_WEST_CAPTURE]) & occupied_by_us &
             self.pieces.piece_type[PAWN] & !(BB_FILE_A | BB_RANK_1 | BB_RANK_8))
        }
    }

    /// Generates pseudo-legal moves.
    ///
    /// A pseudo-legal move is a move that is otherwise legal, except
    /// it might leave the king in check. Every legal move is a
    /// pseudo-legal move, but not every pseudo-legal move is legal.
    /// The generated moves will be pushed to `move_stack`. When `all`
    /// is `true`, all pseudo-legal moves will be generated. When
    /// `all` is `false`, only captures, pawn promotions to queen, and
    /// check evasions will be generated.
    pub fn generate_moves(&self, all: bool, move_stack: &mut MoveStack) {
        // All generated moves with pieces other than the king will be
        // legal. It is possible that some of the king's moves are
        // illegal because the destination square is under check, or
        // when castling, king's passing square is attacked. This is
        // so because verifying that these squares are not under
        // attack is quite expensive, and therefore we hope that the
        // alpha-beta pruning will eliminate the need for this
        // verification at all.

        assert!(self.is_legal());
        let king_square = self.king_square();
        let checkers = self.checkers();
        let occupied_by_us = unsafe { *self.pieces.color.get_unchecked(self.to_move) };
        let occupied_by_them = self.occupied() ^ occupied_by_us;
        let generate_all_moves = all || checkers != 0;
        assert!(king_square <= 63);

        // When in check, for every move except king's moves, the only
        // legal destination squares are those lying on the line
        // between the checker and the king. Also, no piece can move
        // to a square that is occupied by a friendly piece.
        let legal_dests = !occupied_by_us &
                          match ls1b(checkers) {
            0 =>
                // Not in check -- every move destination may be
                // considered "covering".
                BB_UNIVERSAL_SET,

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
                         .get_unchecked(bitscan_1bit(x))
                },

            _ =>
                // Double check -- no covering moves.
                BB_EMPTY_SET,
        };

        if legal_dests != BB_EMPTY_SET {
            // This block is not executed when the king is in double
            // check.

            let pinned = self.pinned();
            let pin_lines = unsafe { self.geometry.squares_at_line.get_unchecked(king_square) };
            let en_passant_bb = self.en_passant_bb();

            // Find queen, rook, bishop, and knight moves.
            {
                // Reduce the set of legal destinations when searching
                // only for captures, pawn promotions to queen, and
                // check evasions.
                let legal_dests = if generate_all_moves {
                    legal_dests
                } else {
                    assert_eq!(legal_dests, !occupied_by_us);
                    occupied_by_them
                };

                for piece in QUEEN..PAWN {
                    let mut bb = unsafe { *self.pieces.piece_type.get_unchecked(piece) } &
                                 occupied_by_us;
                    while bb != BB_EMPTY_SET {
                        let orig_square = bitscan_forward_and_reset(&mut bb);
                        let piece_legal_dests = if 1 << orig_square & pinned == 0 {
                            legal_dests
                        } else {
                            // If the piece is pinned, reduce the set
                            // of legal destination to the squares on
                            // the line of the pin.
                            unsafe { legal_dests & *pin_lines.get_unchecked(orig_square) }
                        };
                        self.push_piece_moves_to_stack(piece,
                                                       orig_square,
                                                       piece_legal_dests,
                                                       move_stack);
                    }
                }
            }

            // Find pawn moves.
            {
                // Reduce the set of legal destinations when searching
                // only for captures, pawn promotions to queen, and
                // check evasions.
                let legal_dests = if generate_all_moves {
                    legal_dests
                } else {
                    assert_eq!(legal_dests, !occupied_by_us);
                    legal_dests & (occupied_by_them | en_passant_bb | BB_PAWN_PROMOTION_RANKS)
                };

                // When in check, the en-passant capture can be a
                // legal evasion move, but only when the checking
                // piece is the passing pawn itself.
                let pawn_legal_dests = if checkers & self.pieces.piece_type[PAWN] == 0 {
                    legal_dests
                } else {
                    legal_dests | en_passant_bb
                };

                // Find all free pawn moves at once.
                let all_pawns = self.pieces.piece_type[PAWN] & occupied_by_us;
                let mut pinned_pawns = all_pawns & pinned;
                let free_pawns = all_pawns ^ pinned_pawns;
                if free_pawns != BB_EMPTY_SET {
                    self.push_pawn_moves_to_stack(free_pawns,
                                                  en_passant_bb,
                                                  pawn_legal_dests,
                                                  !generate_all_moves,
                                                  move_stack);
                }

                // Find pinned pawn moves pawn by pawn, reducing the
                // set of legal destination for each pawn to the
                // squares on the line of the pin.
                while pinned_pawns != BB_EMPTY_SET {
                    let pawn_square = bitscan_forward_and_reset(&mut pinned_pawns);
                    let pin_line = unsafe { *pin_lines.get_unchecked(pawn_square) };
                    self.push_pawn_moves_to_stack(1 << pawn_square,
                                                  en_passant_bb,
                                                  pin_line & pawn_legal_dests,
                                                  !generate_all_moves,
                                                  move_stack);
                }
            }
        }

        // Find king moves (pseudo-legal, possibly moving into check
        // or passing through an attacked square when castling). This
        // is executed even when the king is in double check.
        {
            // Reduce the set of destinations when searching only for
            // captures, pawn promotions to queen, and check evasions.
            let king_dests = if generate_all_moves {
                self.push_castling_moves_to_stack(move_stack);
                !occupied_by_us
            } else {
                occupied_by_them
            };

            self.push_piece_moves_to_stack(KING, king_square, king_dests, move_stack);
        }
    }

    /// Returns a null move.
    ///
    /// "Null move" is a pseudo-move that changes nothing on the board
    /// except the side to move. It is sometimes useful to include a
    /// speculative null move in the search tree so as to achieve more
    /// aggressive pruning.
    #[inline]
    pub fn null_move(&self) -> Move {
        let king_square = self.king_square();
        assert!(king_square <= 63);
        Move::new(self.to_move,
                  MOVE_NORMAL,
                  KING,
                  king_square,
                  king_square,
                  NO_PIECE,
                  self.en_passant_file,
                  self.castling,
                  0)
    }

    /// Checks if `move_digest` represents a pseudo-legal move.
    ///
    /// If a move `m` exists that would be generated by
    /// `generate_moves` if called for the current position on the
    /// board, and for that move `m.digest() == move_digest`, this
    /// method will return `Some(m)`. Otherwise it will return
    /// `None`. This is useful when playing moves from the
    /// transposition table, without calling `generate_moves`.
    pub fn try_move_digest(&self, move_digest: MoveDigest) -> Option<Move> {
        // We could easily call `generate_moves` here and verify if
        // some of the generated moves has the right digest, but this
        // would be much slower. The whole purpose of this method is
        // to be able to check if a move is pseudo-legal *without*
        // generating all moves.

        if move_digest == 0 {
            return None;
        }
        let move_type = get_move_type(move_digest);
        let orig_square = get_orig_square(move_digest);
        let dest_square = get_dest_square(move_digest);
        let king_square = self.king_square();
        let checkers = self.checkers();
        assert!(self.to_move <= 1);
        assert!(move_type <= 3);
        assert!(orig_square <= 63);
        assert!(dest_square <= 63);

        if move_type == MOVE_CASTLING {
            let side = if dest_square < orig_square {
                QUEENSIDE
            } else {
                KINGSIDE
            };
            if checkers != 0 || self.castling_obstacles(side) != 0 || orig_square != king_square ||
               dest_square != [[C1, C8], [G1, G8]][side][self.to_move] {
                return None;
            }
            return Some(Move::new(self.to_move,
                                  MOVE_CASTLING,
                                  KING,
                                  orig_square,
                                  dest_square,
                                  NO_PIECE,
                                  self.en_passant_file,
                                  self.castling,
                                  0));
        }

        // Figure out what is the moved piece.
        let occupied_by_us = unsafe { *self.pieces.color.get_unchecked(self.to_move) };
        let orig_square_bb = occupied_by_us & (1 << orig_square);
        let dest_square_bb = 1 << dest_square;
        let piece;
        'pieces: loop {
            for i in (KING..NO_PIECE).rev() {
                if orig_square_bb & unsafe { *self.pieces.piece_type.get_unchecked(i) } != 0 {
                    piece = i;
                    break 'pieces;
                }
            }
            return None;
        }
        assert!(piece <= PAWN);

        // We will shrink the pseudo-legal destinations set as we go.
        let mut pseudo_legal_dests = !occupied_by_us;

        if piece != KING {
            pseudo_legal_dests &= match ls1b(checkers) {
                0 => BB_UNIVERSAL_SET,
                x if x == checkers => {
                    // We are in check.
                    x |
                    unsafe {
                        *self.geometry
                             .squares_between_including
                             .get_unchecked(king_square)
                             .get_unchecked(bitscan_1bit(x))
                    }
                }
                // We are in double check.
                _ => return None,
            };
            if orig_square_bb & self.pinned() != 0 {
                // The piece is pinned.
                pseudo_legal_dests &= unsafe {
                    *self.geometry
                         .squares_at_line
                         .get_unchecked(king_square)
                         .get_unchecked(orig_square)
                }
            }
        };

        // These are good initial guesses.
        let mut promoted_piece_code = 0;
        let mut captured_piece = self.get_piece_type_at(dest_square_bb);

        if piece == PAWN {
            let en_passant_bb = self.en_passant_bb();
            if checkers & self.pieces.piece_type[PAWN] != 0 {
                // If we are in check, and the checking piece is the
                // passing pawn, the en-passant capture is a legal
                // check evasion.
                pseudo_legal_dests |= en_passant_bb;
            }
            let mut dest_sets: [Bitboard; 4] = unsafe { uninitialized() };
            self.calc_pawn_dest_sets(orig_square_bb, en_passant_bb, &mut dest_sets);
            pseudo_legal_dests &= dest_sets[PAWN_PUSH] | dest_sets[PAWN_DOUBLE_PUSH] |
                                  dest_sets[PAWN_WEST_CAPTURE] |
                                  dest_sets[PAWN_EAST_CAPTURE];
            if pseudo_legal_dests & dest_square_bb == 0 {
                return None;
            }
            match dest_square_bb {
                x if x == en_passant_bb => {
                    if move_type != MOVE_ENPASSANT ||
                       !self.en_passant_special_check_ok(orig_square, dest_square) {
                        return None;
                    }
                    captured_piece = PAWN;
                }
                x if x & BB_PAWN_PROMOTION_RANKS != 0 => {
                    if move_type != MOVE_PROMOTION {
                        return None;
                    }
                    promoted_piece_code = get_aux_data(move_digest);
                }
                _ => {
                    if move_type != MOVE_NORMAL {
                        return None;
                    }
                }
            }
        } else {
            pseudo_legal_dests &= unsafe {
                self.geometry.piece_attacks_from(piece, orig_square, self.occupied())
            };
            if move_type != MOVE_NORMAL || pseudo_legal_dests & dest_square_bb == 0 {
                return None;
            }
        }

        Some(Move::new(self.to_move,
                       move_type,
                       piece,
                       orig_square,
                       dest_square,
                       captured_piece,
                       self.en_passant_file,
                       self.castling,
                       promoted_piece_code))
    }

    /// Plays a move on the board.
    ///
    /// It verifies if the move is legal. If the move is legal, the
    /// board is updated and an `u64` value is returned, which should
    /// be XOR-ed with the old board's hash value to obtain the new
    /// board's hash value. If the move is illegal, `None` is returned
    /// without updating the board. The move passed to this method
    /// **must** have been generated by `generate_moves`,
    /// `try_move_digest`, or `null_move` methods for the current
    /// position on the board.
    ///
    /// Moves generated by the `null_move` method are exceptions. For
    /// them `do_move(m)` will return `None` if and only if the king
    /// is in check.
    pub fn do_move(&mut self, m: Move) -> Option<u64> {
        let us = self.to_move;
        let them = 1 ^ us;
        let move_type = m.move_type();
        let orig_square = m.orig_square();
        let dest_square = m.dest_square();
        let piece = m.piece();
        let captured_piece = m.captured_piece();
        let mut hash = 0;
        assert!(us <= 1);
        assert!(piece < NO_PIECE);
        assert!(move_type <= 3);
        assert!(orig_square <= 63);
        assert!(dest_square <= 63);
        assert!({
            // Check if `m` was generated by `null_move`.
            m.is_null()
        } ||
                {
            // Check if `m` was generated by `try_move_digest` or
            // `generate_moves`.
            let mut m1 = m;
            let mut m2 = self.try_move_digest(m.digest()).unwrap();
            m1.set_score(0);
            m2.set_score(0);
            m1 == m2
        });

        if piece >= NO_PIECE {
            // Since "Board::do_move()" is a public function, we have
            // to guarantee memory safety for all its users.
            panic!("invalid piece");
        }

        unsafe {
            // Verify if the move will leave the king in check.
            if piece == KING {
                if orig_square != dest_square {
                    if self.king_would_be_in_check(dest_square) {
                        return None;  // the king is in check -- illegal move
                    }
                } else {
                    if self.checkers() != 0 {
                        return None;  // invalid "null move"
                    }
                }
            }

            // Move the rook if the move is castling.
            if move_type == MOVE_CASTLING {
                if self.king_would_be_in_check((orig_square + dest_square) >> 1) {
                    return None;  // king's passing square is attacked -- illegal move
                }

                let side = if dest_square > orig_square {
                    KINGSIDE
                } else {
                    QUEENSIDE
                };
                let mask = CASTLING_ROOK_MASK[us][side];
                self.pieces.piece_type[ROOK] ^= mask;
                self.pieces.color[us] ^= mask;
                hash ^= self.zobrist._castling_rook_movement[us][side];
            }

            let not_orig_bb = !(1 << orig_square);
            let dest_bb = 1 << dest_square;

            // empty the origin square
            *self.pieces.piece_type.get_unchecked_mut(piece) &= not_orig_bb;
            *self.pieces.color.get_unchecked_mut(us) &= not_orig_bb;
            hash ^= *self.zobrist
                         .pieces
                         .get_unchecked(us)
                         .get_unchecked(piece)
                         .get_unchecked(orig_square);

            // Remove the captured piece (if any).
            if captured_piece < NO_PIECE {
                let not_captured_bb = if move_type == MOVE_ENPASSANT {
                    let shift = PAWN_MOVE_SHIFTS.get_unchecked(them)[PAWN_PUSH];
                    let captured_pawn_square = (dest_square as isize + shift) as Square;
                    hash ^= *self.zobrist
                                 .pieces
                                 .get_unchecked(them)
                                 .get_unchecked(captured_piece)
                                 .get_unchecked(captured_pawn_square);
                    !(1 << captured_pawn_square)
                } else {
                    hash ^= *self.zobrist
                                 .pieces
                                 .get_unchecked(them)
                                 .get_unchecked(captured_piece)
                                 .get_unchecked(dest_square);
                    !dest_bb
                };
                *self.pieces.piece_type.get_unchecked_mut(captured_piece) &= not_captured_bb;
                *self.pieces.color.get_unchecked_mut(them) &= not_captured_bb;
            }

            // Occupy the destination square.
            let dest_piece = if move_type == MOVE_PROMOTION {
                Move::piece_from_aux_data(m.aux_data())
            } else {
                piece
            };
            *self.pieces.piece_type.get_unchecked_mut(dest_piece) |= dest_bb;
            *self.pieces.color.get_unchecked_mut(us) |= dest_bb;
            hash ^= *self.zobrist
                         .pieces
                         .get_unchecked(us)
                         .get_unchecked(dest_piece)
                         .get_unchecked(dest_square);

            // Update castling rights (null moves do not affect castling).
            if orig_square != dest_square {
                hash ^= *self.zobrist.castling.get_unchecked(self.castling.value());
                self.castling.update(orig_square, dest_square);
                hash ^= *self.zobrist.castling.get_unchecked(self.castling.value());
            }

            // Update the en-passant file.
            hash ^= *self.zobrist.en_passant_file.get_unchecked(self.en_passant_file);
            self.en_passant_file = if piece == PAWN {
                match dest_square as isize - orig_square as isize {
                    16 | -16 => {
                        let file = file(dest_square);
                        hash ^= *self.zobrist.en_passant_file.get_unchecked(file);
                        file
                    }
                    _ => NO_ENPASSANT_FILE,
                }
            } else {
                NO_ENPASSANT_FILE
            };

            // Change the side to move.
            self.to_move = them;
            hash ^= self.zobrist.to_move;

            // Update "_occupied", "_checkers", "_pinned", and
            // "_king_square".
            self._occupied = self.pieces.color[WHITE] | self.pieces.color[BLACK];
            self._king_square.set(64);
            self._checkers.set(BB_UNIVERSAL_SET);
            self._pinned.set(BB_UNIVERSAL_SET);
        }

        assert!(self.is_legal());
        Some(hash)
    }

    /// Takes back a previously played move.
    ///
    /// The move passed to this method **must** be the last move passed
    /// to `do_move`.
    pub fn undo_move(&mut self, m: Move) {
        let them = self.to_move;
        let us = 1 ^ them;
        let move_type = m.move_type();
        let orig_square = m.orig_square();
        let dest_square = m.dest_square();
        let aux_data = m.aux_data();
        let piece = m.piece();
        let captured_piece = m.captured_piece();
        assert!(them <= 1);
        assert!(piece < NO_PIECE);
        assert!(move_type <= 3);
        assert!(orig_square <= 63);
        assert!(dest_square <= 63);
        assert!(aux_data <= 3);
        assert!(m.en_passant_file() <= NO_ENPASSANT_FILE);

        if piece >= NO_PIECE {
            // Since "Board::undo_move()" is a public function, we
            // have to guarantee memory safety for all its users.
            panic!("invalid piece");
        }

        let orig_bb = 1 << orig_square;
        let not_dest_bb = !(1 << dest_square);

        unsafe {
            // Change the side to move.
            self.to_move = us;

            // Restore the en-passant file.
            self.en_passant_file = m.en_passant_file();

            // Restore castling rights.
            self.castling = m.castling();

            // Empty the destination square.
            let dest_piece = if move_type == MOVE_PROMOTION {
                Move::piece_from_aux_data(aux_data)
            } else {
                piece
            };
            *self.pieces.piece_type.get_unchecked_mut(dest_piece) &= not_dest_bb;
            *self.pieces.color.get_unchecked_mut(us) &= not_dest_bb;

            // Put back the captured piece (if any).
            if captured_piece < NO_PIECE {
                let captured_bb = if move_type == MOVE_ENPASSANT {
                    let shift = PAWN_MOVE_SHIFTS.get_unchecked(them)[PAWN_PUSH];
                    let captured_pawn_square = (dest_square as isize + shift) as Square;
                    1 << captured_pawn_square
                } else {
                    !not_dest_bb
                };
                *self.pieces.piece_type.get_unchecked_mut(captured_piece) |= captured_bb;
                *self.pieces.color.get_unchecked_mut(them) |= captured_bb;
            }

            // Restore the piece on the origin square.
            *self.pieces.piece_type.get_unchecked_mut(piece) |= orig_bb;
            *self.pieces.color.get_unchecked_mut(us) |= orig_bb;

            // Move the rook back if the move is castling.
            if move_type == MOVE_CASTLING {
                let side = if dest_square > orig_square {
                    KINGSIDE
                } else {
                    QUEENSIDE
                };
                let mask = *CASTLING_ROOK_MASK.get_unchecked(us).get_unchecked(side);
                self.pieces.piece_type[ROOK] ^= mask;
                *self.pieces.color.get_unchecked_mut(us) ^= mask;
            }

            // Update "_occupied", "_checkers", "_pinned", and
            // "_king_square".
            self._occupied = self.pieces.color[WHITE] | self.pieces.color[BLACK];
            self._king_square.set(64);
            self._checkers.set(BB_UNIVERSAL_SET);
            self._pinned.set(BB_UNIVERSAL_SET);
        }

        assert!(self.is_legal());
    }

    /// Calculates and returns the Zobrist hash value for the board.
    ///
    /// This is a relatively expensive operation.
    ///
    /// Zobrist hashing is a technique to transform a board position
    /// into a number of a fixed length, with an equal distribution
    /// over all possible numbers, invented by Albert Zobrist. The key
    /// property of this method is that two similar positions generate
    /// entirely different hash numbers.
    pub fn calc_hash(&self) -> u64 {
        let mut hash = 0;
        for color in 0..2 {
            for piece in 0..6 {
                let mut bb = self.pieces.color[color] & self.pieces.piece_type[piece];
                while bb != BB_EMPTY_SET {
                    let square = bitscan_forward_and_reset(&mut bb);
                    hash ^= self.zobrist.pieces[color][piece][square];
                }
            }
        }
        hash ^= self.zobrist.castling[self.castling.value()];
        hash ^= self.zobrist.en_passant_file[self.en_passant_file];
        if self.to_move == BLACK {
            hash ^= self.zobrist.to_move;
        }
        hash
    }

    /// A helper method for `create`. It analyzes the board and
    /// decides if it is a legal board.
    ///
    /// In addition to the obviously wrong boards (that for example
    /// declare some pieces having no or more than one color), there
    /// are many chess boards that are impossible to create from the
    /// starting chess position. Here we are interested to detect and
    /// guard against only those of the cases that have a chance of
    /// disturbing some of our explicit and unavoidably, implicit
    /// presumptions about what a chess position is when writing the
    /// code.
    ///
    /// Invalid boards: 1. having more or less than 1 king from each
    /// color; 2. having more than 8 pawns of a color; 3. having more
    /// than 16 pieces (and pawns) of one color; 4. having the side
    /// not to move in check; 5. having pawns on ranks 1 or 8;
    /// 6. having castling rights when the king or the corresponding
    /// rook is not on its initial square; 7. having an en-passant
    /// square that is not having a pawn of corresponding color
    /// before, and an empty square on it and behind it; 8. having an
    /// en-passant square while the king would be in check if the
    /// passing pawn is moved back to its original position.
    fn is_legal(&self) -> bool {
        if self.to_move > 1 || self.en_passant_file > NO_ENPASSANT_FILE {
            return false;
        }
        let us = self.to_move;
        let en_passant_bb = self.en_passant_bb();
        let occupied = self.pieces.piece_type.into_iter().fold(0, |acc, x| {
            if acc & x == 0 {
                acc | x
            } else {
                BB_UNIVERSAL_SET
            }
        });  // Returns `UNIVERSAL_SET` if `self.pieces.piece_type` is messed up.

        let them = 1 ^ us;
        let o_us = self.pieces.color[us];
        let o_them = self.pieces.color[them];
        let our_king_bb = self.pieces.piece_type[KING] & o_us;
        let their_king_bb = self.pieces.piece_type[KING] & o_them;
        let pawns = self.pieces.piece_type[PAWN];

        occupied != BB_UNIVERSAL_SET && occupied == o_us | o_them && o_us & o_them == 0 &&
        pop_count(our_king_bb) == 1 && pop_count(their_king_bb) == 1 &&
        pop_count(pawns & o_us) <= 8 &&
        pop_count(pawns & o_them) <= 8 && pop_count(o_us) <= 16 &&
        pop_count(o_them) <= 16 &&
        self.attacks_to(us, bitscan_forward(their_king_bb)) == 0 &&
        pawns & BB_PAWN_PROMOTION_RANKS == 0 &&
        (!self.castling.can_castle(WHITE, QUEENSIDE) ||
         (self.pieces.piece_type[ROOK] & self.pieces.color[WHITE] & 1 << A1 != 0) &&
         (self.pieces.piece_type[KING] & self.pieces.color[WHITE] & 1 << E1 != 0)) &&
        (!self.castling.can_castle(WHITE, KINGSIDE) ||
         (self.pieces.piece_type[ROOK] & self.pieces.color[WHITE] & 1 << H1 != 0) &&
         (self.pieces.piece_type[KING] & self.pieces.color[WHITE] & 1 << E1 != 0)) &&
        (!self.castling.can_castle(BLACK, QUEENSIDE) ||
         (self.pieces.piece_type[ROOK] & self.pieces.color[BLACK] & 1 << A8 != 0) &&
         (self.pieces.piece_type[KING] & self.pieces.color[BLACK] & 1 << E8 != 0)) &&
        (!self.castling.can_castle(BLACK, KINGSIDE) ||
         (self.pieces.piece_type[ROOK] & self.pieces.color[BLACK] & 1 << H8 != 0) &&
         (self.pieces.piece_type[KING] & self.pieces.color[BLACK] & 1 << E8 != 0)) &&
        (en_passant_bb == BB_EMPTY_SET ||
         {
            let shifts: &[isize; 4] = &PAWN_MOVE_SHIFTS[them];
            let dest_square_bb = gen_shift(en_passant_bb, shifts[PAWN_PUSH]);
            let orig_square_bb = gen_shift(en_passant_bb, -shifts[PAWN_PUSH]);
            let our_king_square = bitscan_forward(our_king_bb);
            (dest_square_bb & pawns & o_them != 0) && (en_passant_bb & !occupied != 0) &&
            (orig_square_bb & !occupied != 0) &&
            unsafe {
                let mask = orig_square_bb | dest_square_bb;
                let pawns = pawns ^ mask;
                let o_them = o_them ^ mask;
                let occupied = occupied ^ mask;
                0 ==
                (self.geometry.piece_attacks_from(ROOK, our_king_square, occupied) & o_them &
                 (self.pieces.piece_type[ROOK] | self.pieces.piece_type[QUEEN])) |
                (self.geometry.piece_attacks_from(BISHOP, our_king_square, occupied) & o_them &
                 (self.pieces.piece_type[BISHOP] | self.pieces.piece_type[QUEEN])) |
                (self.geometry.piece_attacks_from(KNIGHT, our_king_square, occupied) & o_them &
                 self.pieces.piece_type[KNIGHT]) |
                (gen_shift(our_king_bb, -shifts[PAWN_EAST_CAPTURE]) & o_them & pawns & !BB_FILE_H) |
                (gen_shift(our_king_bb, -shifts[PAWN_WEST_CAPTURE]) & o_them & pawns & !BB_FILE_A)
            }
        }) &&
        {
            assert_eq!(self._occupied, occupied);
            assert!(self._checkers.get() == BB_UNIVERSAL_SET ||
                    self._checkers.get() == self.attacks_to(them, bitscan_1bit(our_king_bb)));
            assert!(self._pinned.get() == BB_UNIVERSAL_SET ||
                    self._pinned.get() == self.find_pinned());
            assert!(self._king_square.get() > 63 ||
                    self._king_square.get() == bitscan_1bit(our_king_bb));
            true
        }
    }

    /// A helper method for `push_piece_moves_to_stack` and
    /// `try_move_digest`. It calculates the pseudo-legal destination
    /// squares for each pawn in `pawns` and stores them in the
    /// `dest_sets` array.
    ///
    /// `dest_sets` is indexed by the type of the pawn move: push,
    /// double push, west-capture (capturing toward queen-side), and
    /// east-capture (capturing toward king-side). The benefit of this
    /// separation is that knowing the destination square and the pawn
    /// move type (the index in the `dest_sets` array) is enough to
    /// recover the origin square.
    #[inline]
    fn calc_pawn_dest_sets(&self,
                           pawns: Bitboard,
                           en_passant_bb: Bitboard,
                           dest_sets: &mut [Bitboard; 4]) {
        const PAWN_MOVE_QUIET: [Bitboard; 4] = [BB_UNIVERSAL_SET,
                                                BB_UNIVERSAL_SET,
                                                BB_EMPTY_SET,
                                                BB_EMPTY_SET];
        const PAWN_MOVE_CANDIDATES: [Bitboard; 4] = [!(BB_RANK_1 | BB_RANK_8),
                                                     BB_RANK_2 | BB_RANK_7,
                                                     !(BB_FILE_A | BB_RANK_1 | BB_RANK_8),
                                                     !(BB_FILE_H | BB_RANK_1 | BB_RANK_8)];
        unsafe {
            let shifts: &[isize; 4] = PAWN_MOVE_SHIFTS.get_unchecked(self.to_move);
            let not_occupied_by_us = !*self.pieces.color.get_unchecked(self.to_move);
            let capture_targets = *self.pieces.color.get_unchecked(1 ^ self.to_move) |
                                  en_passant_bb;
            for i in 0..4 {
                *dest_sets.get_unchecked_mut(i) =
                    gen_shift(pawns & *PAWN_MOVE_CANDIDATES.get_unchecked(i),
                              *shifts.get_unchecked(i)) &
                    (capture_targets ^ *PAWN_MOVE_QUIET.get_unchecked(i)) &
                    not_occupied_by_us;
            }
            dest_sets[PAWN_DOUBLE_PUSH] &= gen_shift(dest_sets[PAWN_PUSH], shifts[PAWN_PUSH]);
        }
    }

    /// A helper method for `generate_moves`. It finds all squares
    /// attacked by `piece` from square `orig_square`, and for each
    /// square that is within the `legal_dests` set pushes a new move
    /// to `move_stack`. `piece` must not be a pawn.
    #[inline]
    fn push_piece_moves_to_stack(&self,
                                 piece: PieceType,
                                 orig_square: Square,
                                 legal_dests: Bitboard,
                                 move_stack: &mut MoveStack) {
        assert!(piece < PAWN);
        assert!(orig_square <= 63);
        let mut dest_set = unsafe {
            self.geometry.piece_attacks_from(piece, orig_square, self.occupied())
        } & legal_dests;
        while dest_set != BB_EMPTY_SET {
            let dest_square = bitscan_forward_and_reset(&mut dest_set);
            let captured_piece = self.get_piece_type_at(1 << dest_square);
            move_stack.push(Move::new(self.to_move,
                                      MOVE_NORMAL,
                                      piece,
                                      orig_square,
                                      dest_square,
                                      captured_piece,
                                      self.en_passant_file,
                                      self.castling,
                                      0));
        }
    }

    /// A helper method for `generate_moves()`. It finds all
    /// pseudo-legal moves by the set of pawns given by `pawns`,
    /// making sure that all destination squares are within the
    /// `legal_dests` set. Then it pushes the moves to `move_stack`.
    #[inline]
    fn push_pawn_moves_to_stack(&self,
                                pawns: Bitboard,
                                en_passant_bb: Bitboard,
                                legal_dests: Bitboard,
                                only_queen_promotions: bool,
                                move_stack: &mut MoveStack) {
        let mut dest_sets: [Bitboard; 4] = unsafe { uninitialized() };
        self.calc_pawn_dest_sets(pawns, en_passant_bb, &mut dest_sets);

        // Make sure all destination squares in all sets are legal.
        dest_sets[PAWN_DOUBLE_PUSH] &= legal_dests;
        dest_sets[PAWN_PUSH] &= legal_dests;
        dest_sets[PAWN_WEST_CAPTURE] &= legal_dests;
        dest_sets[PAWN_EAST_CAPTURE] &= legal_dests;

        // Scan each destination set (push, double push, west capture,
        // east capture). For each move calculate the origin and
        // destination squares, and determine the move type
        // (en-passant capture, pawn promotion, or a normal move).
        let shifts: &[isize; 4] = unsafe { PAWN_MOVE_SHIFTS.get_unchecked(self.to_move) };
        for i in 0..4 {
            let s = unsafe { dest_sets.get_unchecked_mut(i) };
            while *s != BB_EMPTY_SET {
                let dest_square = bitscan_forward_and_reset(s);
                let dest_square_bb = 1 << dest_square;
                let orig_square = (dest_square as isize -
                                   unsafe {
                    *shifts.get_unchecked(i)
                }) as Square;
                let captured_piece = self.get_piece_type_at(dest_square_bb);
                match dest_square_bb {

                    // en-passant capture
                    x if x == en_passant_bb => {
                        if self.en_passant_special_check_ok(orig_square, dest_square) {
                            move_stack.push(Move::new(self.to_move,
                                                      MOVE_ENPASSANT,
                                                      PAWN,
                                                      orig_square,
                                                      dest_square,
                                                      PAWN,
                                                      self.en_passant_file,
                                                      self.castling,
                                                      0));
                        }
                    }

                    // pawn promotion
                    x if x & BB_PAWN_PROMOTION_RANKS != 0 => {
                        for p in 0..4 {
                            move_stack.push(Move::new(self.to_move,
                                                      MOVE_PROMOTION,
                                                      PAWN,
                                                      orig_square,
                                                      dest_square,
                                                      captured_piece,
                                                      self.en_passant_file,
                                                      self.castling,
                                                      p));
                            if only_queen_promotions {
                                break;
                            }
                        }
                    }

                    // normal pawn move (push or plain capture)
                    _ => {
                        move_stack.push(Move::new(self.to_move,
                                                  MOVE_NORMAL,
                                                  PAWN,
                                                  orig_square,
                                                  dest_square,
                                                  captured_piece,
                                                  self.en_passant_file,
                                                  self.castling,
                                                  0));
                    }
                }
            }
        }
    }

    /// A helper method for `generate_moves`. It figures out which
    /// castling moves are pseudo-legal and pushes them to
    /// `move_stack`.
    #[inline(always)]
    fn push_castling_moves_to_stack(&self, move_stack: &mut MoveStack) {
        if self.checkers() == BB_EMPTY_SET {
            for side in 0..2 {
                if self.castling_obstacles(side) == 0 {
                    // It seems castling is legal unless king's
                    // passing or final squares are attacked, but
                    // we do not care about that, because this
                    // will be verified in "do_move()".
                    move_stack.push(Move::new(self.to_move,
                                              MOVE_CASTLING,
                                              KING,
                                              self.king_square(),
                                              unsafe {
                                                  *[[C1, C8], [G1, G8]]
                                                       .get_unchecked(side)
                                                       .get_unchecked(self.to_move)
                                              },
                                              NO_PIECE,
                                              self.en_passant_file,
                                              self.castling,
                                              0));
                }
            }
        }
    }

    /// A helper method for `generate_moves`. It returns all pinned
    /// pieces belonging to the side to move.
    #[inline(always)]
    fn find_pinned(&self) -> Bitboard {
        let king_square = self.king_square();
        let occupied_by_them = unsafe { *self.pieces.color.get_unchecked(1 ^ self.to_move) };
        assert!(king_square <= 63);

        // To find all potential pinners, we remove all our pieces
        // from the board, and all enemy pieces that can not slide in
        // the particular manner (diagonally or straight). Then we
        // calculate what enemy pieces a bishop or a rook placed on
        // our king's square can attack. The attacked enemy pieces are
        // the potential pinners.
        let diag_sliders = occupied_by_them &
                           (self.pieces.piece_type[QUEEN] | self.pieces.piece_type[BISHOP]);
        let straight_sliders = occupied_by_them &
                               (self.pieces.piece_type[QUEEN] | self.pieces.piece_type[ROOK]);
        let mut pinners = unsafe {
            diag_sliders & self.geometry.piece_attacks_from(BISHOP, king_square, diag_sliders) |
            straight_sliders & self.geometry.piece_attacks_from(ROOK, king_square, straight_sliders)
        };

        if pinners == BB_EMPTY_SET {
            BB_EMPTY_SET
        } else {
            let occupied_by_us = unsafe { *self.pieces.color.get_unchecked(self.to_move) };
            let between_king_square_and = unsafe {
                self.geometry
                    .squares_between_including
                    .get_unchecked(king_square)
            };
            let blockers = occupied_by_us & !(1 << king_square) | (occupied_by_them & !pinners);
            let mut pinned_or_discovered_checkers = BB_EMPTY_SET;

            // Scan all potential pinners and see if there is one and only
            // one piece between the pinner and our king.
            while pinners != BB_EMPTY_SET {
                let pinner_square = bitscan_forward_and_reset(&mut pinners);
                let blockers_group = unsafe {
                    between_king_square_and.get_unchecked(pinner_square)
                } & blockers;
                if ls1b(blockers_group) == blockers_group {
                    // A group of blockers consisting of only one
                    // piece is either a pinned piece of ours or
                    // enemy's discovered checker.
                    pinned_or_discovered_checkers |= blockers_group;
                }
            }
            pinned_or_discovered_checkers & occupied_by_us
        }
    }

    /// A helper method for `generate_moves`. It returns a bitboard
    /// representing the en-passant target square if there is one.
    #[inline]
    fn en_passant_bb(&self) -> Bitboard {
        assert!(self.en_passant_file <= NO_ENPASSANT_FILE);
        if self.en_passant_file >= NO_ENPASSANT_FILE {
            0
        } else if self.to_move == WHITE {
            1 << self.en_passant_file << 40
        } else {
            1 << self.en_passant_file << 16
        }
    }

    /// A helper method. It returns the square that the king of the
    /// side to move occupies. The value is lazily calculated and
    /// saved for future use.
    #[inline]
    fn king_square(&self) -> Square {
        if self._king_square.get() > 63 {
            self._king_square
                .set(bitscan_1bit(self.pieces.piece_type[KING] &
                                  unsafe { *self.pieces.color.get_unchecked(self.to_move) }));
        }
        self._king_square.get()
    }

    /// A helper method for `do_move`. It returns if the king of the
    /// side to move would be in check if moved to `square`.
    #[inline]
    fn king_would_be_in_check(&self, square: Square) -> bool {
        let them = 1 ^ self.to_move;
        let occupied = self.occupied() & !(1 << self.king_square());
        assert!(them <= 1);
        assert!(square <= 63);
        unsafe {
            let occupied_by_them = *self.pieces.color.get_unchecked(them);

            (self.geometry.piece_attacks_from(ROOK, square, occupied) & occupied_by_them &
             (self.pieces.piece_type[ROOK] | self.pieces.piece_type[QUEEN])) !=
            BB_EMPTY_SET ||
            (self.geometry.piece_attacks_from(BISHOP, square, occupied) & occupied_by_them &
             (self.pieces.piece_type[BISHOP] | self.pieces.piece_type[QUEEN])) !=
            BB_EMPTY_SET ||
            (self.geometry.piece_attacks_from(KNIGHT, square, occupied) & occupied_by_them &
             self.pieces.piece_type[KNIGHT]) != BB_EMPTY_SET ||
            (self.geometry.piece_attacks_from(KING, square, occupied) & occupied_by_them &
             self.pieces.piece_type[KING]) != BB_EMPTY_SET ||
            {
                let shifts: &[isize; 4] = PAWN_MOVE_SHIFTS.get_unchecked(them);
                let square_bb = 1 << square;

                (gen_shift(square_bb, -shifts[PAWN_EAST_CAPTURE]) & occupied_by_them &
                 self.pieces.piece_type[PAWN] &
                 !(BB_FILE_H | BB_RANK_1 | BB_RANK_8)) != BB_EMPTY_SET ||
                (gen_shift(square_bb, -shifts[PAWN_WEST_CAPTURE]) & occupied_by_them &
                 self.pieces.piece_type[PAWN] &
                 !(BB_FILE_A | BB_RANK_1 | BB_RANK_8)) != BB_EMPTY_SET
            }
        }
    }

    /// A helper method. It returns the type of the piece at the
    /// square represented by the bitboard `square_bb`.
    #[inline(always)]
    fn get_piece_type_at(&self, square_bb: Bitboard) -> PieceType {
        assert!(square_bb != BB_EMPTY_SET);
        assert_eq!(square_bb, ls1b(square_bb));
        let bb = square_bb & self.occupied();
        if bb == 0 {
            return NO_PIECE;
        }
        for i in (KING..NO_PIECE).rev() {
            if bb & unsafe { *self.pieces.piece_type.get_unchecked(i) } != 0 {
                return i;
            }
        }
        panic!("invalid board");
    }

    /// A helper method for `push_pawn_moves_to_stack`. It tests for
    /// the special case when an en-passant capture discovers check on
    /// 4/5-th rank.
    ///
    /// This method tests for the very rare occasion when the two
    /// pawns participating in en-passant capture, disappearing in one
    /// move, discover an unexpected check along the horizontal (rank
    /// 4 of 5). `orig_square` and `dist_square` are the origin square
    /// and the destination square of the capturing pawn.
    fn en_passant_special_check_ok(&self, orig_square: Square, dest_square: Square) -> bool {
        let king_square = self.king_square();
        if (1 << king_square) & [BB_RANK_5, BB_RANK_4][self.to_move] == 0 {
            // The king is not on the 4/5-th rank -- we are done.
            true
        } else {
            // The king is on the 4/5-th rank -- we have more work to do.
            let the_two_pawns = 1 << orig_square |
                                gen_shift(1,
                                          dest_square as isize -
                                          PAWN_MOVE_SHIFTS[self.to_move][PAWN_PUSH]);
            let occupied = self.occupied() & !the_two_pawns;
            let occupied_by_them = self.pieces.color[1 ^ self.to_move] & !the_two_pawns;
            let checkers = unsafe {
                self.geometry.piece_attacks_from(ROOK, king_square, occupied)
            } & occupied_by_them &
                           (self.pieces.piece_type[ROOK] | self.pieces.piece_type[QUEEN]);
            checkers == BB_EMPTY_SET
        }
    }

    /// A helper method. It returns a bitboard with the set of pieces
    /// between the king and the castling rook.
    #[inline]
    fn castling_obstacles(&self, side: CastlingSide) -> Bitboard {
        assert!(side <= 1);
        const BETWEEN: [[Bitboard; 2]; 2] = [[1 << B1 | 1 << C1 | 1 << D1, 1 << F1 | 1 << G1],
                                             [1 << B8 | 1 << C8 | 1 << D8, 1 << F8 | 1 << G8]];
        if self.castling.can_castle(self.to_move, side) {
            self.occupied() & unsafe { *BETWEEN.get_unchecked(self.to_move).get_unchecked(side) }
        } else {
            // Castling is not possible, therefore every piece on
            // every square on the board can be considered an
            // obstacle.
            BB_UNIVERSAL_SET
        }
    }
}


// Pawn move types.
const PAWN_PUSH: usize = 0;
const PAWN_DOUBLE_PUSH: usize = 1;
const PAWN_WEST_CAPTURE: usize = 2;
const PAWN_EAST_CAPTURE: usize = 3;


/// Pawn move shifts (one for each color and pawn move type).
static PAWN_MOVE_SHIFTS: [[isize; 4]; 2] = [[8, 16, 7, 9], [-8, -16, -9, -7]];


/// Indicates no passing pawn file.
const NO_ENPASSANT_FILE: usize = 8;


/// Bitboards that describe how the castling rook moves during the
/// castling move.
const CASTLING_ROOK_MASK: [[Bitboard; 2]; 2] = [[1 << A1 | 1 << D1, 1 << H1 | 1 << F1],
                                                [1 << A8 | 1 << D8, 1 << H8 | 1 << F8]];


#[cfg(test)]
mod tests {
    use super::*;
    use basetypes::*;
    use moves::*;

    #[test]
    fn test_attacks_from() {
        use position::tables::*;
        let b = Board::from_fen("k7/8/8/8/3P4/8/8/7K w - - 0 1").ok().unwrap();
        let g = BoardGeometry::get();
        unsafe {
            assert_eq!(g.piece_attacks_from(BISHOP,
                                            A1,
                                            b.pieces.color[WHITE] | b.pieces.color[BLACK]),
                       1 << B2 | 1 << C3 | 1 << D4);
            assert_eq!(g.piece_attacks_from(BISHOP,
                                            A1,
                                            b.pieces.color[WHITE] | b.pieces.color[BLACK]),
                       1 << B2 | 1 << C3 | 1 << D4);
            assert_eq!(g.piece_attacks_from(KNIGHT,
                                            A1,
                                            b.pieces.color[WHITE] | b.pieces.color[BLACK]),
                       1 << B3 | 1 << C2);
        }
    }

    #[test]
    fn test_attacks_to() {
        let b = Board::from_fen("8/8/8/3K1p1P/r4k2/3Pq1N1/7p/1B5Q w - - 0 1").ok().unwrap();
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
        assert_eq!(KING, 0);
        assert_eq!(QUEEN, 1);
        assert_eq!(ROOK, 2);
        assert_eq!(BISHOP, 3);
        assert_eq!(KNIGHT, 4);
        assert_eq!(PAWN, 5);
    }

    #[test]
    fn test_pawn_dest_sets() {
        let mut stack = MoveStack::new();

        let b = Board::from_fen("k2q4/4Ppp1/5P2/6Pp/6P1/8/7P/7K w - h6 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        let mut pawn_dests = 0u64;
        while let Some(m) = stack.pop() {
            if m.piece() == PAWN {
                pawn_dests |= 1 << m.dest_square();
            }
        }
        assert_eq!(pawn_dests,
                   1 << H3 | 1 << H4 | 1 << G6 | 1 << E8 | 1 << H5 | 1 << G7 | 1 << H6 | 1 << D8);

        let b = Board::from_fen("k2q4/4Ppp1/5P2/6Pp/6P1/8/7P/7K b - - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        let mut pawn_dests = 0u64;
        while let Some(m) = stack.pop() {
            if m.piece() == PAWN {
                pawn_dests |= 1 << m.dest_square();
            }
        }
        assert_eq!(pawn_dests, 1 << H4 | 1 << G6 | 1 << G4 | 1 << F6);
    }

    #[test]
    fn test_move_generation_1() {
        let mut stack = MoveStack::new();

        let b = Board::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/4K3 w - - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 5);
        stack.clear();

        let b = Board::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/6K1 w - - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 7);
        stack.clear();

        let b = Board::from_fen("8/8/6NK/2pP4/3PR3/2b1q3/3P4/7k w - - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 8);
        stack.clear();

        let b = Board::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/7K w - - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 22);
        stack.clear();

        let b = Board::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/7K w - c6 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 23);
        stack.clear();

        let b = Board::from_fen("K7/8/6N1/2pP4/3PR3/2b1q3/3P4/7k b - - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 25);
        stack.clear();

        let b = Board::from_fen("K7/8/6N1/2pP4/3PR2k/2b1q3/3P4/8 b - - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 5);
        stack.clear();
    }

    #[test]
    fn test_move_generation_2() {
        let mut stack = MoveStack::new();

        assert!(Board::from_fen("8/8/7k/8/4pP2/8/3B4/7K b - f3 0 1").is_err());
        assert!(Board::from_fen("8/8/8/8/4pP2/8/3B4/7K b - f3 0 1").is_err());
        assert!(Board::from_fen("8/8/8/4k3/4pP2/8/3B4/7K b - f3 0 1").is_ok());

        let b = Board::from_fen("8/8/8/7k/5pP1/8/8/5R1K b - g3 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 6);
        stack.clear();

        let b = Board::from_fen("8/8/8/5k2/5pP1/8/8/5R1K b - g3 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 7);
        stack.clear();

        let b = Board::from_fen("8/8/8/8/4pP1k/8/8/4B2K b - f3 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 5);
        stack.clear();
    }

    #[test]
    fn test_move_generation_3() {
        let mut stack = MoveStack::new();

        let b = Board::from_fen("8/8/8/8/4RpPk/8/8/7K b - g3 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 6);
        stack.clear();
    }

    #[test]
    fn test_move_generation_4() {
        let mut stack = MoveStack::new();

        let b = Board::from_fen("8/8/8/8/3QPpPk/8/8/7K b - g3 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 7);
        stack.clear();
    }

    #[test]
    fn test_move_generation_5() {
        let mut stack = MoveStack::new();

        let b = Board::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R w - - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 19 + 5);
        stack.clear();

        let b = Board::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R w K - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 19 + 6);
        stack.clear();

        let b = Board::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 19 + 7);
        stack.clear();

        let b = Board::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R b KQ - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 19 + 5);
        stack.clear();

        let b = Board::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R b KQk - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 19 + 6);
        stack.clear();

        let b = Board::from_fen("4k3/8/8/8/8/5n2/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 5);
        stack.clear();

        let mut b = Board::from_fen("4k3/8/8/8/8/6n1/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        let mut count = 0;
        while let Some(m) = stack.pop() {
            if b.do_move(m).is_some() {
                count += 1;
                b.undo_move(m);
            }
        }
        assert_eq!(count, 19 + 4);

        let b = Board::from_fen("4k3/8/8/8/8/4n3/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 19 + 7);
        stack.clear();

        let b = Board::from_fen("4k3/8/8/8/8/4n3/8/R3K2R w - - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 19 + 5);
        stack.clear();

        let b = Board::from_fen("4k3/8/1b6/8/8/8/8/R3K2R w KQ - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.count(), 19 + 7);
        stack.clear();
    }

    #[test]
    fn test_do_undo_move() {
        let mut stack = MoveStack::new();

        let mut b = Board::from_fen("b3k2r/6P1/8/5pP1/8/8/6P1/R3K2R w kKQ f6 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        let count = stack.count();
        while let Some(m) = stack.pop() {
            if let Some(h) = b.do_move(m) {
                assert!(h != 0);
                b.undo_move(m);
                let mut other_stack = MoveStack::new();
                b.generate_moves(true, &mut other_stack);
                assert_eq!(count, other_stack.count());
            }
        }
        assert_eq!(stack.count(), 0);
        let mut b = Board::from_fen("b3k2r/6P1/8/5pP1/8/8/8/R3K2R b kKQ - 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        let count = stack.count();
        while let Some(m) = stack.pop() {
            if b.do_move(m).is_some() {
                b.undo_move(m);
                let mut other_stack = MoveStack::new();
                b.generate_moves(true, &mut other_stack);
                assert_eq!(count, other_stack.count());
            }
        }
    }

    #[test]
    fn test_find_pinned() {
        use basetypes::*;
        let b = Board::from_fen("k2r4/3r4/3N4/5n2/qp1K2Pq/8/3PPR2/6b1 w - - 0 1").ok().unwrap();
        assert_eq!(b.find_pinned(), 1 << F2 | 1 << D6 | 1 << G4);
    }

    #[test]
    fn test_generate_only_captures() {
        let mut stack = MoveStack::new();

        let b = Board::from_fen("k6r/P7/8/6p1/6pP/8/8/7K b - h3 0 1").ok().unwrap();
        b.generate_moves(false, &mut stack);
        assert_eq!(stack.count(), 4);
        stack.clear();

        let b = Board::from_fen("k7/8/8/4Pp2/4K3/8/8/8 w - f6 0 1").ok().unwrap();
        b.generate_moves(false, &mut stack);
        assert_eq!(stack.count(), 8);
        stack.clear();

        let b = Board::from_fen("k7/8/8/4Pb2/4K3/8/8/8 w - - 0 1").ok().unwrap();
        b.generate_moves(false, &mut stack);
        assert_eq!(stack.count(), 7);
        stack.clear();
    }

    #[test]
    fn test_null_move() {
        let mut stack = MoveStack::new();

        let mut b = Board::from_fen("k7/8/8/5Pp1/8/8/8/4K2R w K g6 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        let count = stack.count();
        stack.clear();
        let m = b.null_move();
        assert!(b.do_move(m).is_some());
        b.undo_move(m);
        b.generate_moves(true, &mut stack);
        assert_eq!(count, stack.count());
        stack.clear();

        let mut b = Board::from_fen("k7/4r3/8/8/8/8/8/4K3 w - - 0 1").ok().unwrap();
        let m = b.null_move();
        assert!(b.do_move(m).is_none());
    }

    #[test]
    fn test_move_into_check_bug() {
        let mut stack = MoveStack::new();

        let mut b = Board::from_fen("rnbq1bn1/pppP3k/8/3P2B1/2B5/5N2/PPPN1PP1/2K4R b - - 0 1")
                        .ok()
                        .unwrap();
        b.generate_moves(true, &mut stack);
        let m = stack.pop().unwrap();
        b.do_move(m);
        assert!(b.is_legal());
    }

    #[test]
    fn test_try_move_digest() {
        fn try_all(b: &Board, stack: &MoveStack) {
            let mut i = 0;
            loop {
                if let Some(m) = b.try_move_digest(i) {
                    assert!(stack.iter().find(|x| **x == m).is_some());
                }
                if i == 0xffff {
                    break;
                } else {
                    i += 1;
                }
            }
        }

        let mut stack = MoveStack::new();
        let b = Board::from_fen("rnbqk2r/p1p1pppp/8/8/2Pp4/5NP1/pP1PPPBP/RNBQK2R b KQkq c3 0 \
                                     1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear();
        let b = Board::from_fen("rnbqk2r/p1p1pppp/8/8/Q1Pp4/5NP1/pP1PPPBP/RNB1K2R b KQkq - 0 \
                                 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear();
        let b = Board::from_fen("rnbqk2r/p1p1pppp/3N4/8/Q1Pp4/6P1/pP1PPPBP/RNB1K2R b KQkq - 0 \
                                 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear();
        let b = Board::from_fen("rnbq3r/p1p1pppp/8/3k4/2Pp4/5NP1/pP1PPPBP/RNBQK2R b KQ c3 0 \
                                     1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear();
        let b = Board::from_fen("rn1qk2r/p1pbpppp/8/8/Q1Pp4/5NP1/pP1PPPBP/RNB1K2R b KQkq - 0 \
                                 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear();
        let b = Board::from_fen("8/8/8/8/4RpPk/8/8/7K b - g3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear();
        let b = Board::from_fen("8/8/8/8/5pPk/8/8/7K b - g3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);
    }
}
