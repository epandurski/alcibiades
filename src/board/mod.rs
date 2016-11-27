//! Facilities for implementing static position evaluation.
//!
//! # Static position evaluation
//!
//! An evaluation function is used to heuristically determine the
//! relative value of a position, i.e. the chances of winning. If we
//! could see to the end of the game in every line, the evaluation
//! would only have values of "loss", "draw", and "win". In practice,
//! however, we do not know the exact value of a position, so we must
//! make an approximation. Beginning chess players learn to do this
//! starting with the value of the pieces themselves. Computer
//! evaluation functions also use the value of the material as the
//! most significant aspect and then add other considerations.
//!
//! Static evaluation is an evaluation that considers only the static
//! material and positional properties of the current position,
//! without analyzing any tactical variations. Therefore, if the
//! position has pending tactical threats, the static evaluation will
//! be grossly incorrect.
//!
//! Writing a new static evaluator is as simple as defining a type
//! that implements the `BoardEvaluator` trait. Then you pass that as
//! a type parameter to `Position`.
//!
//! # Writing your own move generator
//!
//! The generation of moves is at the heart of every chess
//! engine. `Position` implements a very fast move generator, and
//! also: quiescence search, static exchange evaluation, move legality
//! check, hashing. Re-writing those things is a lot of work. Still,
//! if you decide to do this, you should write your own implementation
//! of the `SearchNode` trait.
mod notation;
mod rules;
pub mod tables;
pub mod bitsets;
pub mod evaluators;

use std::mem::uninitialized;
use std::cell::Cell;
use chesstypes::*;
use chesstypes::moves::{get_aux_data, get_dest_square, get_orig_square, get_move_type};
use search::MoveStack;
use uci::SetOption;
use board::notation::parse_fen;
use board::bitsets::*;
use board::tables::{BoardGeometry, ZobristArrays};
pub use board::rules::Position;


/// A trait used to statically evaluate positions.
pub trait BoardEvaluator: Clone + Send + SetOption {
    /// Creates a new instance and binds it to a given position.
    ///
    /// When a new instance is created, it is bound to a particular
    /// chess position (given by the `board` parameter). And for a
    /// moment, this is the only position that can be correctly
    /// evaluated. The instance then can be re-bound to the next (or
    /// the previous) position in the line of play by issuing calls to
    /// `will_do_move` and `done_move` methods (or respectively,
    /// `will_undo_move` and `undone_move` methods) .
    fn new(board: &Board<Self>) -> Self;

    /// Evaluates the the position to which the instance is bound.
    ///
    /// `board` points to the position to which the instance is bound.
    /// `halfmove_clock` gives the number of half-moves since the last
    /// piece capture or pawn advance.
    ///
    /// The returned value must be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`.
    fn evaluate(&self, board: &Board<Self>, halfmove_clock: u8) -> Value;

    /// Returns whether the position is zugzwangy.
    ///
    /// In many endgame positions there is a relatively high
    /// probability of zugzwang occurring. For such positions, this
    /// method returns `true`.
    fn is_zugzwangy(&self) -> bool;

    /// Updates evaluator's state to keep up with a move that will be
    /// played.
    ///
    /// `board` points to the position to which the instance is bound.
    ///
    /// `m` is a legal move, or (if not in check) a "null move".
    #[inline]
    #[allow(unused_variables)]
    fn will_do_move(&mut self, board: &Board<Self>, m: Move) {}

    /// Updates evaluator's state to keep up with a move that was
    /// played.
    ///
    /// `board` points to the new position to which the instance is
    /// bound.
    #[inline]
    #[allow(unused_variables)]
    fn done_move(&mut self, board: &Board<Self>, m: Move) {}

    /// Updates evaluator's state to keep up with a move that will be
    /// taken back.
    ///
    /// `board` points to the position to which the instance is bound.
    #[inline]
    #[allow(unused_variables)]
    fn will_undo_move(&mut self, board: &Board<Self>, m: Move) {}

    /// Updates evaluator's state in accordance with a move that was
    /// taken back.
    ///
    /// `board` points to the new position to which the instance is
    /// bound.
    #[inline]
    #[allow(unused_variables)]
    fn undone_move(&mut self, board: &Board<Self>, m: Move) {}
}


/// Holds a chess position.
#[derive(Clone)]
pub struct Board<E: BoardEvaluator> {
    geometry: &'static BoardGeometry,
    zobrist: &'static ZobristArrays,
    evaluator: E,

    /// The placement of the pieces on the board.
    pieces: PiecesPlacement,

    /// The side to move.
    to_move: Color,

    /// The castling rights for both players.
    castling_rights: CastlingRights,

    /// If the previous move was a double pawn push, contains pushed
    /// pawn's file (a value between 0 and 7). Otherwise contains `8`.
    en_passant_file: usize,

    /// This will always be equal to `self.pieces.color[WHITE] |
    /// self.pieces.color[BLACK]`
    _occupied: Bitboard,

    /// Lazily calculated bitboard of all checkers --
    /// `BB_UNIVERSAL_SET` if not calculated yet.
    _checkers: Cell<Bitboard>,
}


// In a nutshell: `Board` can generate all possible moves in the
// current position, play a selected move, and take it back. It can
// also play a "null move" which can be used to selectively prune the
// search tree. `Board` does not try to be too clever. In particular,
// it is completely unaware of repeating positions and
// rule-50. Although `Board` is able to statically evaluate the
// position and decide if it is zugzwangy, it delegates this to
// `BoardEvaluator`.
//
// Note that many of the implemented methods are private -- they are
// used solely by the module `board::rules`.
impl<E: BoardEvaluator> Board<E> {
    /// Creates a new instance.
    ///
    /// Verifies that the position is legal.
    fn from_raw_parts(pieces: &PiecesPlacement,
                      to_move: Color,
                      castling_rights: CastlingRights,
                      en_passant_square: Option<Square>)
                      -> Result<Board<E>, String> {
        let en_passant_rank = match to_move {
            WHITE => RANK_6,
            BLACK => RANK_3,
            _ => return Err(format!("illegal position")),
        };
        let en_passant_file = match en_passant_square {
            None => 8,
            Some(x) if x <= 63 && rank(x) == en_passant_rank => file(x),
            _ => return Err(format!("illegal position")),
        };
        let mut b = Board {
            geometry: BoardGeometry::get(),
            zobrist: ZobristArrays::get(),
            evaluator: unsafe { uninitialized() },
            pieces: *pieces,
            to_move: to_move,
            castling_rights: castling_rights,
            en_passant_file: en_passant_file,
            _occupied: pieces.color[WHITE] | pieces.color[BLACK],
            _checkers: Cell::new(BB_UNIVERSAL_SET),
        };
        if !b.is_legal() {
            return Err(format!("illegal position"));
        }
        b.evaluator = E::new(&b);
        Ok(b)
    }

    /// Creates a new instance from a Forsyth–Edwards Notation (FEN)
    /// string.
    ///
    /// Verifies that the position is legal.
    pub fn from_fen(fen: &str) -> Result<Board<E>, String> {
        let parts = try!(parse_fen(fen).map_err(|_| fen));
        let (ref placement, to_move, castling, en_passant_square, _, _) = parts;
        Board::from_raw_parts(placement, to_move, castling, en_passant_square)
            .map_err(|_| fen.to_string())
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
    pub fn castling_rights(&self) -> CastlingRights {
        self.castling_rights
    }

    /// If the previous move was a double pawn push, returns double
    /// pushed pawn's file.
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

    /// Returns a bitboard of all pieces and pawns of color `us` that
    /// attack `square`.
    pub fn attacks_to(&self, us: Color, square: Square) -> Bitboard {
        debug_assert!(square <= 63);
        let occupied_by_us = self.pieces.color[us];
        let square_bb = 1 << square;
        let shifts: &[isize; 4] = &PAWN_MOVE_SHIFTS[us];

        (self.geometry.attacks_from(ROOK, square, self.occupied()) & occupied_by_us &
         (self.pieces.piece_type[ROOK] | self.pieces.piece_type[QUEEN])) |
        (self.geometry.attacks_from(BISHOP, square, self.occupied()) & occupied_by_us &
         (self.pieces.piece_type[BISHOP] | self.pieces.piece_type[QUEEN])) |
        (self.geometry.attacks_from(KNIGHT, square, self.occupied()) & occupied_by_us &
         self.pieces.piece_type[KNIGHT]) |
        (self.geometry.attacks_from(KING, square, self.occupied()) & occupied_by_us &
         self.pieces.piece_type[KING]) |
        (gen_shift(square_bb, -shifts[PAWN_EAST_CAPTURE]) & occupied_by_us &
         self.pieces.piece_type[PAWN] & !(BB_FILE_H | BB_RANK_1 | BB_RANK_8)) |
        (gen_shift(square_bb, -shifts[PAWN_WEST_CAPTURE]) & occupied_by_us &
         self.pieces.piece_type[PAWN] & !(BB_FILE_A | BB_RANK_1 | BB_RANK_8))
    }

    /// Returns a bitboard of all enemy pieces and pawns that attack
    /// the king.
    #[inline]
    pub fn checkers(&self) -> Bitboard {
        if self._checkers.get() == BB_UNIVERSAL_SET {
            // The result is saved, in case it is needed again.
            self._checkers.set(self.attacks_to(1 ^ self.to_move, self.king_square()));
        }
        debug_assert_eq!(self._checkers.get(),
                         self.attacks_to(1 ^ self.to_move, self.king_square()));
        self._checkers.get()
    }

    /// Statically evaluates the position.
    ///
    /// `halfmove_clock` gives the number of half-moves since the last
    /// piece capture or pawn advance.
    ///
    /// The returned value will be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`.
    #[inline]
    pub fn evaluate(&self, halfmove_clock: u8) -> Value {
        unsafe {
            let board_ptr: *const Board<E> = self;
            let v = self.evaluator.evaluate(board_ptr.as_ref().unwrap(), halfmove_clock);
            debug_assert!(VALUE_EVAL_MIN <= v && v <= VALUE_EVAL_MAX);
            v
        }
    }

    /// Returns whether the position is zugzwangy.
    ///
    /// In many endgame positions there is a relatively high
    /// probability of zugzwang occurring. For such positions, this
    /// method returns `true`.
    #[inline]
    pub fn is_zugzwangy(&self) -> bool {
        self.evaluator.is_zugzwangy()
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
    fn generate_moves(&self, all: bool, move_stack: &mut MoveStack) {
        // All generated moves with pieces other than the king will be
        // legal. It is possible that some of the king's moves are
        // illegal because the destination square is under
        // attack. This is so because verifying that this square is
        // not under attack is quite expensive, and therefore we hope
        // that the alpha-beta pruning will eliminate the need for
        // this verification.

        let king_square = self.king_square();
        let checkers = self.checkers();
        let occupied_by_us = self.pieces.color[self.to_move];
        let occupied_by_them = self.occupied() ^ occupied_by_us;
        let generate_all_moves = all || checkers != 0;

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
                x | self.geometry.squares_between_including[king_square][bitscan_1bit(x)],
            _ =>
                // Double check -- no covering moves.
                0,
        };

        if legal_dests != 0 {
            // This block is not executed when the king is in double
            // check.

            let pinned = self.find_pinned();
            let our_pawns = self.pieces.piece_type[PAWN] & occupied_by_us;
            let mut pinned_pawns = our_pawns & pinned;
            let free_pawns = our_pawns ^ pinned_pawns;
            let en_passant_bb = self.en_passant_bb();

            // Generate queen, rook, bishop, and knight moves.
            {
                let piece_legal_dests = if generate_all_moves {
                    legal_dests
                } else {
                    debug_assert_eq!(legal_dests, !occupied_by_us);
                    occupied_by_them
                };

                for piece in QUEEN..PAWN {
                    let mut bb = self.pieces.piece_type[piece] & occupied_by_us;
                    while bb != 0 {
                        let orig_square = bitscan_forward_and_reset(&mut bb);
                        let piece_legal_dests = if 1 << orig_square & pinned == 0 {
                            piece_legal_dests
                        } else {
                            // The piece is pinned -- reduce the set
                            // of legal destination to the squares on
                            // the line of the pin.
                            piece_legal_dests &
                            self.geometry.squares_at_line[king_square][orig_square]
                        };
                        self.push_piece_moves_to_stack(piece,
                                                       orig_square,
                                                       piece_legal_dests,
                                                       move_stack);
                    }
                }
            }

            // Generate pawn moves.
            {
                let pawn_legal_dests = if generate_all_moves {
                    if checkers & self.pieces.piece_type[PAWN] == 0 {
                        legal_dests
                    } else {
                        // We are in check from a pawn, therefore the
                        // en-passant capture is legal too.
                        legal_dests | en_passant_bb
                    }
                } else {
                    debug_assert_eq!(checkers, 0);
                    debug_assert_eq!(legal_dests, !occupied_by_us);
                    legal_dests & (occupied_by_them | en_passant_bb | BB_PAWN_PROMOTION_RANKS)
                };

                // Generate all free pawn moves at once.
                if free_pawns != 0 {
                    self.push_pawn_moves_to_stack(free_pawns,
                                                  pawn_legal_dests,
                                                  !generate_all_moves,
                                                  move_stack);
                }

                // Generate pinned pawn moves pawn by pawn, reducing
                // the set of legal destination for each pawn to the
                // squares on the line of the pin.
                while pinned_pawns != 0 {
                    let pawn_square = bitscan_forward_and_reset(&mut pinned_pawns);
                    let pawn_legal_dests = pawn_legal_dests &
                                           self.geometry.squares_at_line[king_square][pawn_square];
                    self.push_pawn_moves_to_stack(1 << pawn_square,
                                                  pawn_legal_dests,
                                                  !generate_all_moves,
                                                  move_stack);
                }
            }
        }

        // Generate king moves (pseudo-legal, possibly moving into
        // check). This is executed even when the king is in double
        // check.
        let king_dests = if generate_all_moves {
            for side in 0..2 {
                if self.can_castle(side) {
                    move_stack.push(Move::new(MOVE_CASTLING,
                                              king_square,
                                              [[C1, C8], [G1, G8]][side][self.to_move],
                                              0,
                                              NO_PIECE,
                                              KING,
                                              self.castling_rights,
                                              self.en_passant_file,
                                              0));
                }
            }
            !occupied_by_us
        } else {
            occupied_by_them
        };
        self.push_piece_moves_to_stack(KING, king_square, king_dests, move_stack);
    }

    /// Returns a null move.
    ///
    /// "Null move" is a pseudo-move that changes nothing on the board
    /// except the side to move. It is sometimes useful to include a
    /// speculative null move in the search tree so as to achieve more
    /// aggressive pruning.
    #[inline]
    fn null_move(&self) -> Move {
        // Null moves are represented as king's moves for which the
        // destination square equals the origin square.
        let king_square = self.king_square();
        Move::new(MOVE_NORMAL,
                  king_square,
                  king_square,
                  0,
                  NO_PIECE,
                  KING,
                  self.castling_rights,
                  self.en_passant_file,
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
            let mut move_stack = MoveStack::new();
            self.generate_moves(true, &mut move_stack);
            while let Some(m) = move_stack.pop() {
                if m.digest() == move_digest {
                    generated_move = Some(m);
                    break;
                }
            }
        }

        if move_digest == 0 {
            debug_assert!(generated_move.is_none());
            return None;
        }

        let move_type = get_move_type(move_digest);
        let orig_square = get_orig_square(move_digest);
        let dest_square = get_dest_square(move_digest);
        let promoted_piece_code = get_aux_data(move_digest);
        let king_square = self.king_square();
        let checkers = self.checkers();

        if move_type == MOVE_CASTLING {
            let side = if dest_square < orig_square {
                QUEENSIDE
            } else {
                KINGSIDE
            };
            if !self.can_castle(side) || orig_square != king_square ||
               dest_square != [[C1, C8], [G1, G8]][side][self.to_move] ||
               promoted_piece_code != 0 {
                debug_assert!(generated_move.is_none());
                return None;
            }
            let m = Move::new(MOVE_CASTLING,
                              orig_square,
                              dest_square,
                              0,
                              NO_PIECE,
                              KING,
                              self.castling_rights,
                              self.en_passant_file,
                              0);
            debug_assert_eq!(generated_move, Some(m));
            return Some(m);
        }

        let occupied_by_us = self.pieces.color[self.to_move];
        let orig_square_bb = occupied_by_us & (1 << orig_square);
        let dest_square_bb = 1 << dest_square;
        let mut captured_piece = self.get_piece_type_at(dest_square);
        let move_score;

        // Figure out what is the type of the moved piece.
        let piece;
        'pieces: loop {
            for i in KING..NO_PIECE {
                if orig_square_bb & self.pieces.piece_type[i] != 0 {
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
            pseudo_legal_dests &= match ls1b(checkers) {
                0 => BB_UNIVERSAL_SET,
                x if x == checkers => {
                    x | self.geometry.squares_between_including[king_square][bitscan_1bit(x)]
                }
                _ => {
                    debug_assert!(generated_move.is_none());
                    return None;
                } 
            };

            // Verify if the moved piece is pinned.
            if orig_square_bb & self.find_pinned() != 0 {
                pseudo_legal_dests &= self.geometry.squares_at_line[king_square][orig_square]
            }
        };

        if piece == PAWN {
            let en_passant_bb = self.en_passant_bb();
            if checkers & self.pieces.piece_type[PAWN] != 0 {
                // We are in check from a pawn, therefore the
                // en-passant capture is legal too.
                pseudo_legal_dests |= en_passant_bb;
            }

            let mut dest_sets: [Bitboard; 4] = unsafe { uninitialized() };
            calc_pawn_dest_sets(self.to_move,
                                occupied_by_us,
                                self.pieces.color[1 ^ self.to_move],
                                en_passant_bb,
                                orig_square_bb,
                                &mut dest_sets);
            pseudo_legal_dests &= dest_sets[PAWN_PUSH] | dest_sets[PAWN_DOUBLE_PUSH] |
                                  dest_sets[PAWN_WEST_CAPTURE] |
                                  dest_sets[PAWN_EAST_CAPTURE];
            if pseudo_legal_dests & dest_square_bb == 0 {
                debug_assert!(generated_move.is_none());
                return None;
            }

            match dest_square_bb {
                x if x == en_passant_bb => {
                    if move_type != MOVE_ENPASSANT ||
                       !self.en_passant_special_check_is_ok(orig_square, dest_square) ||
                       promoted_piece_code != 0 {
                        debug_assert!(generated_move.is_none());
                        return None;
                    }
                    move_score = MOVE_SCORE_MAX;
                    captured_piece = PAWN;
                }
                x if x & BB_PAWN_PROMOTION_RANKS != 0 => {
                    if move_type != MOVE_PROMOTION {
                        debug_assert!(generated_move.is_none());
                        return None;
                    }
                    move_score = if promoted_piece_code == 0 {
                        MOVE_SCORE_MAX
                    } else {
                        0
                    }
                }
                _ => {
                    if move_type != MOVE_NORMAL || promoted_piece_code != 0 {
                        debug_assert!(generated_move.is_none());
                        return None;
                    }
                    move_score = if captured_piece < NO_PIECE {
                        MOVE_SCORE_MAX
                    } else {
                        0
                    }
                }
            }

        } else {
            // This is not a pawn move, nor a castling move.
            pseudo_legal_dests &= self.geometry.attacks_from(piece, orig_square, self.occupied());
            if move_type != MOVE_NORMAL || pseudo_legal_dests & dest_square_bb == 0 ||
               promoted_piece_code != 0 {
                debug_assert!(generated_move.is_none());
                return None;
            }
            move_score = if captured_piece < NO_PIECE {
                MOVE_SCORE_MAX
            } else {
                0
            }
        }

        let m = Move::new(move_type,
                          orig_square,
                          dest_square,
                          promoted_piece_code,
                          captured_piece,
                          piece,
                          self.castling_rights,
                          self.en_passant_file,
                          move_score);
        debug_assert_eq!(generated_move, Some(m));
        Some(m)
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
    fn do_move(&mut self, m: Move) -> Option<u64> {
        let us = self.to_move;
        let them = 1 ^ us;
        let move_type = m.move_type();
        let orig_square = m.orig_square();
        let dest_square = m.dest_square();
        let dest_square_bb = 1 << dest_square;
        let played_piece = m.played_piece();
        let captured_piece = m.captured_piece();
        let mut h = 0;
        let mut old_hash: u64 = unsafe { uninitialized() };

        if cfg!(debug_assertions) {
            // Assert that `m` was generated by `null_move`,
            // `try_move_digest`, or `generate_moves`.
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
            old_hash = self.calc_hash();
        }

        // Verify if the move will leave the king in check. (We are
        // certain that all the moves that we generate with pieces
        // other than the king do not leave the king in check.)
        if played_piece == KING {
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

        // Tell the evaluator that a move will be played.
        unsafe {
            let board_ptr: *const Board<E> = self;
            self.evaluator.will_do_move(board_ptr.as_ref().unwrap(), m);
        }

        // Move the rook if the move is castling.
        if move_type == MOVE_CASTLING {
            let side = if dest_square > orig_square {
                KINGSIDE
            } else {
                QUEENSIDE
            };
            let mask = BB_CASTLING_ROOK_MOVEMENT[us][side];
            self.pieces.piece_type[ROOK] ^= mask;
            self.pieces.color[us] ^= mask;
            h ^= self.zobrist.pieces[us][ROOK][CASTLING_ROOK_MOVEMENT[us][side].0] ^
                 self.zobrist.pieces[us][ROOK][CASTLING_ROOK_MOVEMENT[us][side].1];
        }

        // empty the origin square
        let not_orig_square_bb = !(1 << orig_square);
        self.pieces.piece_type[played_piece] &= not_orig_square_bb;
        self.pieces.color[us] &= not_orig_square_bb;
        h ^= self.zobrist.pieces[us][played_piece][orig_square];

        // Remove the captured piece (if any).
        if captured_piece < NO_PIECE {
            let not_captured_bb = if move_type == MOVE_ENPASSANT {
                let captured_pawn_square =
                    (dest_square as isize + PAWN_MOVE_SHIFTS[them][PAWN_PUSH]) as Square;
                h ^= self.zobrist.pieces[them][captured_piece][captured_pawn_square];
                !(1 << captured_pawn_square)
            } else {
                h ^= self.zobrist.pieces[them][captured_piece][dest_square];
                !dest_square_bb
            };
            self.pieces.piece_type[captured_piece] &= not_captured_bb;
            self.pieces.color[them] &= not_captured_bb;
        }

        // Occupy the destination square.
        let dest_piece = if move_type == MOVE_PROMOTION {
            Move::piece_from_aux_data(m.aux_data())
        } else {
            played_piece
        };
        self.pieces.piece_type[dest_piece] |= dest_square_bb;
        self.pieces.color[us] |= dest_square_bb;
        h ^= self.zobrist.pieces[us][dest_piece][dest_square];

        // Update castling rights (null moves do not affect castling).
        if orig_square != dest_square {
            h ^= self.zobrist.castling_rights[self.castling_rights.value()];
            self.castling_rights.update(orig_square, dest_square);
            h ^= self.zobrist.castling_rights[self.castling_rights.value()];
        }

        // Update the en-passant file.
        h ^= self.zobrist.en_passant_file[self.en_passant_file];
        self.en_passant_file = if played_piece == PAWN &&
                                  dest_square as isize - orig_square as isize ==
                                  PAWN_MOVE_SHIFTS[us][PAWN_DOUBLE_PUSH] {
            let file = file(dest_square);
            h ^= self.zobrist.en_passant_file[file];
            file
        } else {
            8
        };

        // Change the side to move.
        self.to_move = them;
        h ^= self.zobrist.to_move;

        // Update the auxiliary fields.
        self._occupied = self.pieces.color[WHITE] | self.pieces.color[BLACK];
        self._checkers.set(BB_UNIVERSAL_SET);

        // Tell the evaluator that a move was played.
        unsafe {
            let board_ptr: *const Board<E> = self;
            self.evaluator.done_move(board_ptr.as_ref().unwrap(), m);
        }

        debug_assert!(self.is_legal());
        debug_assert_eq!(old_hash ^ h, self.calc_hash());
        Some(h)
    }

    /// Takes back a previously played move.
    ///
    /// The move passed to this method **must** be the last move passed
    /// to `do_move`.
    fn undo_move(&mut self, m: Move) {
        // In this method we basically do the same things that we do
        // in `do_move`, but in reverse.

        let them = self.to_move;
        let us = 1 ^ them;
        let move_type = m.move_type();
        let orig_square = m.orig_square();
        let dest_square = m.dest_square();
        let dest_square_bb = 1 << dest_square;
        let played_piece = m.played_piece();
        let captured_piece = m.captured_piece();
        debug_assert!(m.en_passant_file() <= 8);

        // Tell the evaluator that a move will be taken back.
        unsafe {
            let board_ptr: *const Board<E> = self;
            self.evaluator.will_undo_move(board_ptr.as_ref().unwrap(), m);
        }

        // Change the side to move.
        self.to_move = us;

        // Restore the en-passant file.
        self.en_passant_file = m.en_passant_file();

        // Restore castling rights.
        self.castling_rights = m.castling_rights();

        // Empty the destination square.
        let dest_piece = if move_type == MOVE_PROMOTION {
            Move::piece_from_aux_data(m.aux_data())
        } else {
            played_piece
        };
        self.pieces.piece_type[dest_piece] &= !dest_square_bb;
        self.pieces.color[us] &= !dest_square_bb;

        // Put back the captured piece (if any).
        if captured_piece < NO_PIECE {
            let captured_piece_bb = if move_type == MOVE_ENPASSANT {
                gen_shift(dest_square_bb, PAWN_MOVE_SHIFTS[them][PAWN_PUSH])
            } else {
                dest_square_bb
            };
            self.pieces.piece_type[captured_piece] |= captured_piece_bb;
            self.pieces.color[them] |= captured_piece_bb;
        }

        // Restore the piece on the origin square.
        let orig_square_bb = 1 << orig_square;
        self.pieces.piece_type[played_piece] |= orig_square_bb;
        self.pieces.color[us] |= orig_square_bb;

        // Move the rook back if the move is castling.
        if move_type == MOVE_CASTLING {
            let side = if dest_square > orig_square {
                KINGSIDE
            } else {
                QUEENSIDE
            };
            let mask = BB_CASTLING_ROOK_MOVEMENT[us][side];
            self.pieces.piece_type[ROOK] ^= mask;
            self.pieces.color[us] ^= mask;
        }

        // Update the auxiliary fields.
        self._occupied = self.pieces.color[WHITE] | self.pieces.color[BLACK];
        self._checkers.set(BB_UNIVERSAL_SET);

        // Tell the evaluator that a move was taken back.
        unsafe {
            let board_ptr: *const Board<E> = self;
            self.evaluator.undone_move(board_ptr.as_ref().unwrap(), m);
        }

        debug_assert!(self.is_legal());
    }

    /// Calculates and returns the Zobrist hash value for the board.
    ///
    /// Zobrist hashing is a technique to transform a board position
    /// into a number of a fixed length, with an equal distribution
    /// over all possible numbers, invented by Albert Zobrist. The key
    /// property of this method is that two similar positions generate
    /// entirely different hash numbers.
    fn calc_hash(&self) -> u64 {
        let mut hash = 0;
        for color in 0..2 {
            for piece in 0..6 {
                let mut bb = self.pieces.color[color] & self.pieces.piece_type[piece];
                while bb != 0 {
                    let square = bitscan_forward_and_reset(&mut bb);
                    hash ^= self.zobrist.pieces[color][piece][square];
                }
            }
        }
        hash ^= self.zobrist.castling_rights[self.castling_rights.value()];
        hash ^= self.zobrist.en_passant_file[self.en_passant_file];
        if self.to_move == BLACK {
            hash ^= self.zobrist.to_move;
        }
        hash
    }

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
        if self.to_move > 1 || self.en_passant_file > 8 {
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
        });  // `occupied` becomes `UNIVERSAL_SET` if `self.pieces.piece_type` is messed up.

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
        (!self.castling_rights.can_castle(WHITE, QUEENSIDE) ||
         (self.pieces.piece_type[ROOK] & self.pieces.color[WHITE] & 1 << A1 != 0) &&
         (self.pieces.piece_type[KING] & self.pieces.color[WHITE] & 1 << E1 != 0)) &&
        (!self.castling_rights.can_castle(WHITE, KINGSIDE) ||
         (self.pieces.piece_type[ROOK] & self.pieces.color[WHITE] & 1 << H1 != 0) &&
         (self.pieces.piece_type[KING] & self.pieces.color[WHITE] & 1 << E1 != 0)) &&
        (!self.castling_rights.can_castle(BLACK, QUEENSIDE) ||
         (self.pieces.piece_type[ROOK] & self.pieces.color[BLACK] & 1 << A8 != 0) &&
         (self.pieces.piece_type[KING] & self.pieces.color[BLACK] & 1 << E8 != 0)) &&
        (!self.castling_rights.can_castle(BLACK, KINGSIDE) ||
         (self.pieces.piece_type[ROOK] & self.pieces.color[BLACK] & 1 << H8 != 0) &&
         (self.pieces.piece_type[KING] & self.pieces.color[BLACK] & 1 << E8 != 0)) &&
        (en_passant_bb == 0 ||
         {
            let shifts: &[isize; 4] = &PAWN_MOVE_SHIFTS[them];
            let dest_square_bb = gen_shift(en_passant_bb, shifts[PAWN_PUSH]);
            let orig_square_bb = gen_shift(en_passant_bb, -shifts[PAWN_PUSH]);
            let our_king_square = bitscan_forward(our_king_bb);
            (dest_square_bb & pawns & o_them != 0) && (en_passant_bb & !occupied != 0) &&
            (orig_square_bb & !occupied != 0) &&
            {
                let mask = orig_square_bb | dest_square_bb;
                let pawns = pawns ^ mask;
                let o_them = o_them ^ mask;
                let occupied = occupied ^ mask;
                0 ==
                (self.geometry.attacks_from(ROOK, our_king_square, occupied) & o_them &
                 (self.pieces.piece_type[ROOK] | self.pieces.piece_type[QUEEN])) |
                (self.geometry.attacks_from(BISHOP, our_king_square, occupied) & o_them &
                 (self.pieces.piece_type[BISHOP] | self.pieces.piece_type[QUEEN])) |
                (self.geometry.attacks_from(KNIGHT, our_king_square, occupied) & o_them &
                 self.pieces.piece_type[KNIGHT]) |
                (gen_shift(our_king_bb, -shifts[PAWN_EAST_CAPTURE]) & o_them & pawns & !BB_FILE_H) |
                (gen_shift(our_king_bb, -shifts[PAWN_WEST_CAPTURE]) & o_them & pawns & !BB_FILE_A)
            }
        }) &&
        {
            assert_eq!(self._occupied, occupied);
            assert!(self._checkers.get() == BB_UNIVERSAL_SET ||
                    self._checkers.get() == self.attacks_to(them, bitscan_1bit(our_king_bb)));
            true
        }
    }

    /// A helper method for `generate_moves`. It finds all squares
    /// attacked by `piece` from square `orig_square`, and for each
    /// square that is within the `legal_dests` set pushes a new move
    /// to `move_stack`. `piece` must not be `PAWN`.
    #[inline(always)]
    fn push_piece_moves_to_stack(&self,
                                 piece: PieceType,
                                 orig_square: Square,
                                 legal_dests: Bitboard,
                                 move_stack: &mut MoveStack) {
        debug_assert!(piece < PAWN);
        debug_assert!(orig_square <= 63);
        debug_assert!(legal_dests & self.pieces.color[self.to_move] == 0);

        let mut piece_legal_dests = legal_dests &
                                    self.geometry.attacks_from(piece, orig_square, self.occupied());
        while piece_legal_dests != 0 {
            let dest_square = bitscan_forward_and_reset(&mut piece_legal_dests);
            let captured_piece = self.get_piece_type_at(dest_square);
            let move_score = if captured_piece < NO_PIECE {
                MOVE_SCORE_MAX
            } else {
                0
            };
            move_stack.push(Move::new(MOVE_NORMAL,
                                      orig_square,
                                      dest_square,
                                      0,
                                      captured_piece,
                                      piece,
                                      self.castling_rights,
                                      self.en_passant_file,
                                      move_score));
        }
    }

    /// A helper method for `generate_moves`. It pushes all
    /// pseudo-legal moves by the set of pawns given by `pawns` to
    /// `move_stack`, ensuring that all destination squares are within
    /// the `legal_dests` set. When `only_queen_promotions` is `true`,
    /// promotions to pieces other that queen will not be pushed to
    /// `move_stack`.
    fn push_pawn_moves_to_stack(&self,
                                pawns: Bitboard,
                                legal_dests: Bitboard,
                                only_queen_promotions: bool,
                                move_stack: &mut MoveStack) {
        debug_assert!(pawns & !self.pieces.piece_type[PAWN] == 0);
        debug_assert!(pawns & !self.pieces.color[self.to_move] == 0);
        debug_assert!(legal_dests & self.pieces.color[self.to_move] == 0);

        let mut dest_sets: [Bitboard; 4] = unsafe { uninitialized() };
        let en_passant_bb = self.en_passant_bb();
        calc_pawn_dest_sets(self.to_move,
                            self.pieces.color[self.to_move],
                            self.pieces.color[1 ^ self.to_move],
                            en_passant_bb,
                            pawns,
                            &mut dest_sets);

        // Process each pawn move sub-type (push, double push, west
        // capture, east capture).
        let shifts: &[isize; 4] = &PAWN_MOVE_SHIFTS[self.to_move];
        for i in 0..4 {
            let mut pawn_legal_dests = dest_sets[i] & legal_dests;

            // For each legal destination, determine the move type
            // (en-passant capture, pawn promotion, normal move), and
            // push the move to `move_stack`.
            while pawn_legal_dests != 0 {
                let dest_square = bitscan_forward_and_reset(&mut pawn_legal_dests);
                let orig_square = (dest_square as isize - shifts[i]) as Square;
                let captured_piece = self.get_piece_type_at(dest_square);
                match 1 << dest_square {

                    // en-passant capture
                    x if x == en_passant_bb => {
                        if self.en_passant_special_check_is_ok(orig_square, dest_square) {
                            move_stack.push(Move::new(MOVE_ENPASSANT,
                                                      orig_square,
                                                      dest_square,
                                                      0,
                                                      PAWN,
                                                      PAWN,
                                                      self.castling_rights,
                                                      self.en_passant_file,
                                                      MOVE_SCORE_MAX));
                        }
                    }

                    // pawn promotion
                    x if x & BB_PAWN_PROMOTION_RANKS != 0 => {
                        for p in 0..4 {
                            let move_score = if p == 0 {
                                MOVE_SCORE_MAX
                            } else {
                                0
                            };
                            move_stack.push(Move::new(MOVE_PROMOTION,
                                                      orig_square,
                                                      dest_square,
                                                      p,
                                                      captured_piece,
                                                      PAWN,
                                                      self.castling_rights,
                                                      self.en_passant_file,
                                                      move_score));
                            if only_queen_promotions {
                                break;
                            }
                        }
                    }

                    // normal pawn move
                    _ => {
                        let move_score = if captured_piece < NO_PIECE {
                            MOVE_SCORE_MAX
                        } else {
                            0
                        };
                        move_stack.push(Move::new(MOVE_NORMAL,
                                                  orig_square,
                                                  dest_square,
                                                  0,
                                                  captured_piece,
                                                  PAWN,
                                                  self.castling_rights,
                                                  self.en_passant_file,
                                                  move_score));
                    }
                }
            }
        }
    }

    /// A helper method. It returns all pinned pieces belonging to the
    /// side to move.
    fn find_pinned(&self) -> Bitboard {
        let mut pinned = 0;
        let king_square = self.king_square();
        let occupied_by_them = self.pieces.color[1 ^ self.to_move];

        // To find the pinners, we "remove" all our pieces from the
        // board, and then verify if a bishop or a rook placed on our
        // king's square can attack any enemy bishops, rooks, or
        // queens.
        let mut pinners = (self.geometry.attacks_from(ROOK, king_square, occupied_by_them) &
                           (self.pieces.piece_type[QUEEN] | self.pieces.piece_type[ROOK]) &
                           occupied_by_them) |
                          (self.geometry.attacks_from(BISHOP, king_square, occupied_by_them) &
                           (self.pieces.piece_type[QUEEN] | self.pieces.piece_type[BISHOP]) &
                           occupied_by_them);

        // Then, for each pinner we verify if there is exactly one
        // defender between our king and the pinner.
        if pinners != 0 {
            let defenders = self.pieces.color[self.to_move] & !(1 << king_square);
            let between_our_king_and: &[Bitboard; 64] =
                &self.geometry.squares_between_including[king_square];
            while pinners != 0 {
                let pinner_square = bitscan_forward_and_reset(&mut pinners);
                let bb = defenders & between_our_king_and[pinner_square];
                if ls1b(bb) == bb {
                    pinned |= bb;
                }
            }
        }
        pinned
    }

    /// A helper method. It returns a bitboard representing the
    /// en-passant target square if there is one.
    #[inline(always)]
    fn en_passant_bb(&self) -> Bitboard {
        if self.en_passant_file >= 8 {
            0
        } else {
            [1 << A6, 1 << A3][self.to_move] << self.en_passant_file
        }
    }

    /// A helper method. It returns the square that the king of the
    /// side to move occupies.
    #[inline(always)]
    fn king_square(&self) -> Square {
        bitscan_1bit(self.pieces.piece_type[KING] & self.pieces.color[self.to_move])
    }

    /// A helper method. It returns if the king of the side to move
    /// would be in check if moved to `square`.
    fn king_would_be_in_check(&self, square: Square) -> bool {
        debug_assert!(square <= 63);
        let them = 1 ^ self.to_move;
        let occupied = self.occupied() & !(1 << self.king_square());
        let occupied_by_them = self.pieces.color[them];

        (self.geometry.attacks_from(ROOK, square, occupied) & occupied_by_them &
         (self.pieces.piece_type[ROOK] | self.pieces.piece_type[QUEEN])) != 0 ||
        (self.geometry.attacks_from(BISHOP, square, occupied) & occupied_by_them &
         (self.pieces.piece_type[BISHOP] | self.pieces.piece_type[QUEEN])) != 0 ||
        (self.geometry.attacks_from(KNIGHT, square, occupied) & occupied_by_them &
         self.pieces.piece_type[KNIGHT]) != 0 ||
        (self.geometry.attacks_from(KING, square, occupied) & occupied_by_them &
         self.pieces.piece_type[KING]) != 0 ||
        {
            let shifts: &[isize; 4] = &PAWN_MOVE_SHIFTS[them];
            let square_bb = 1 << square;
            gen_shift(square_bb, -shifts[PAWN_EAST_CAPTURE]) & occupied_by_them &
            self.pieces.piece_type[PAWN] & !(BB_FILE_H | BB_RANK_1 | BB_RANK_8) != 0 ||
            gen_shift(square_bb, -shifts[PAWN_WEST_CAPTURE]) & occupied_by_them &
            self.pieces.piece_type[PAWN] & !(BB_FILE_A | BB_RANK_1 | BB_RANK_8) != 0
        }
    }

    /// A helper method. It returns the type of the piece at `square`.
    #[inline(always)]
    fn get_piece_type_at(&self, square: Square) -> PieceType {
        debug_assert!(square <= 63);
        let bb = 1 << square & self.occupied();
        if bb == 0 {
            return NO_PIECE;
        }
        for i in (KING..NO_PIECE).rev() {
            if bb & self.pieces.piece_type[i] != 0 {
                return i;
            }
        }
        panic!("invalid board");
    }

    /// A helper method. It tests for the rare occasion when the two
    /// pawns participating in the en-passant capture, disappearing
    /// from the 4/5-th rank in one move, discover a check along this
    /// rank. `orig_square` and `dist_square` are the origin and
    /// destination squares of the capturing pawn.
    fn en_passant_special_check_is_ok(&self, orig_square: Square, dest_square: Square) -> bool {
        let king_square = self.king_square();
        if rank(king_square) == rank(orig_square) {
            let pawn1_bb = 1 << orig_square;
            let pawn2_bb = gen_shift(1 << dest_square, -PAWN_MOVE_SHIFTS[self.to_move][PAWN_PUSH]);
            let occupied = self.occupied() & !(pawn1_bb | pawn2_bb);
            return 0 ==
                   self.geometry.attacks_from(ROOK, king_square, occupied) &
                   self.pieces.color[1 ^ self.to_move] &
                   (self.pieces.piece_type[ROOK] | self.pieces.piece_type[QUEEN]);
        }
        true
    }

    /// A helper method. It returns if castling on a given `side` is
    /// pseudo-legal.
    #[inline(always)]
    fn can_castle(&self, side: CastlingSide) -> bool {
        const BETWEEN: [[Bitboard; 2]; 2] = [[1 << B1 | 1 << C1 | 1 << D1, 1 << F1 | 1 << G1],
                                             [1 << B8 | 1 << C8 | 1 << D8, 1 << F8 | 1 << G8]];
        self.castling_rights.can_castle(self.to_move, side) &&
        self.occupied() & BETWEEN[self.to_move][side] == 0 && self.checkers() == 0 &&
        !self.king_would_be_in_check([[D1, F1], [D8, F8]][self.to_move][side])
    }
}


/// Pawn move sub-type -- a single push.
const PAWN_PUSH: usize = 0;

/// Pawn move sub-type -- a double push.
const PAWN_DOUBLE_PUSH: usize = 1;

/// Pawn move sub-type -- a capture toward the queen-side.
const PAWN_WEST_CAPTURE: usize = 2;

/// Pawn move sub-type -- a capture toward the king-side.
const PAWN_EAST_CAPTURE: usize = 3;


/// Constants used for the generation of pawn moves (by bit shifting)
/// -- one for each color and pawn move sub-type.
///
/// Example: The bitboard for a white pawn on "e2" is `1 << E2`. If
/// the pawn is pushed one square forward, the updated bitboard would
/// be: `gen_shift(1 << E2, PAWN_MOVE_SHIFTS[WHITE][PAWN_PUSH])`
static PAWN_MOVE_SHIFTS: [[isize; 4]; 2] = [[8, 16, 7, 9], [-8, -16, -9, -7]];


/// The highest possible move score.
const MOVE_SCORE_MAX: u32 = ::std::u32::MAX;


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
/// `dest_sets` is indexed by the sub-type of the pawn move: push,
/// double push, west capture, east capture. The benefit of this
/// separation is that knowing the destination square and the pawn
/// move sub-type (the index in the `dest_sets` array) is enough
/// to recover the origin square.
#[inline(always)]
fn calc_pawn_dest_sets(us: Color,
                       occupied_by_us: Bitboard,
                       occupied_by_them: Bitboard,
                       en_passant_bb: Bitboard,
                       pawns: Bitboard,
                       dest_sets: &mut [Bitboard; 4]) {
    debug_assert!(pawns & !occupied_by_us == 0);
    debug_assert!(occupied_by_us & occupied_by_them == 0);
    debug_assert!(gen_shift(en_passant_bb, -PAWN_MOVE_SHIFTS[us][PAWN_PUSH]) &
                  !occupied_by_them == 0);
    const NOT_CAPTURING: [Bitboard; 4] = [BB_UNIVERSAL_SET, // push
                                          BB_UNIVERSAL_SET, // double push
                                          0, // west capture
                                          0]; // east capture
    const PROPER_ORIGIN: [Bitboard; 4] = [!(BB_RANK_1 | BB_RANK_8),
                                          BB_RANK_2 | BB_RANK_7,
                                          !(BB_FILE_A | BB_RANK_1 | BB_RANK_8),
                                          !(BB_FILE_H | BB_RANK_1 | BB_RANK_8)];
    let shifts: &[isize; 4] = &PAWN_MOVE_SHIFTS[us];
    let capture_targets = occupied_by_them | en_passant_bb;
    for i in 0..4 {
        dest_sets[i] = gen_shift(pawns & PROPER_ORIGIN[i], shifts[i]) &
                       (capture_targets ^ NOT_CAPTURING[i]) &
                       !occupied_by_us;
    }

    // Double pushes are trickier -- for a double push to be
    // pseudo-legal, a single push must be pseudo-legal too.
    dest_sets[PAWN_DOUBLE_PUSH] &= gen_shift(dest_sets[PAWN_PUSH], shifts[PAWN_PUSH]);
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::evaluators::RandomEvaluator;
    use chesstypes::*;
    use search::MoveStack;

    #[test]
    fn test_attacks_from() {
        use board::tables::*;
        let b = Board::<RandomEvaluator>::from_fen("k7/8/8/8/3P4/8/8/7K w - - 0 1").ok().unwrap();
        let g = BoardGeometry::get();
        assert_eq!(g.attacks_from(BISHOP, A1, b.pieces.color[WHITE] | b.pieces.color[BLACK]),
                   1 << B2 | 1 << C3 | 1 << D4);
        assert_eq!(g.attacks_from(BISHOP, A1, b.pieces.color[WHITE] | b.pieces.color[BLACK]),
                   1 << B2 | 1 << C3 | 1 << D4);
        assert_eq!(g.attacks_from(KNIGHT, A1, b.pieces.color[WHITE] | b.pieces.color[BLACK]),
                   1 << B3 | 1 << C2);
    }

    #[test]
    fn test_attacks_to() {
        let b = Board::<RandomEvaluator>::from_fen("8/8/8/3K1p1P/r4k2/3Pq1N1/7p/1B5Q w - - 0 1")
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

        let b = Board::<RandomEvaluator>::from_fen("k2q4/4Ppp1/5P2/6Pp/6P1/8/7P/7K w - h6 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        let mut pawn_dests = 0u64;
        while let Some(m) = stack.pop() {
            if m.played_piece() == PAWN {
                pawn_dests |= 1 << m.dest_square();
            }
        }
        assert_eq!(pawn_dests,
                   1 << H3 | 1 << H4 | 1 << G6 | 1 << E8 | 1 << H5 | 1 << G7 | 1 << H6 | 1 << D8);

        let b = Board::<RandomEvaluator>::from_fen("k2q4/4Ppp1/5P2/6Pp/6P1/8/7P/7K b - - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        let mut pawn_dests = 0u64;
        while let Some(m) = stack.pop() {
            if m.played_piece() == PAWN {
                pawn_dests |= 1 << m.dest_square();
            }
        }
        assert_eq!(pawn_dests, 1 << H4 | 1 << G6 | 1 << G4 | 1 << F6);
    }

    #[test]
    fn test_move_generation_1() {
        let mut stack = MoveStack::new();

        let b = Board::<RandomEvaluator>::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/4K3 w - - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 5);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/6K1 w - - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 7);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("8/8/6NK/2pP4/3PR3/2b1q3/3P4/7k w - - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 8);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/7K w - - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 22);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("8/8/6Nk/2pP4/3PR3/2b1q3/3P4/7K w - c6 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 23);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("K7/8/6N1/2pP4/3PR3/2b1q3/3P4/7k b - - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 25);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("K7/8/6N1/2pP4/3PR2k/2b1q3/3P4/8 b - - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 5);
        stack.clear_all();
    }

    #[test]
    fn test_move_generation_2() {
        let mut stack = MoveStack::new();

        assert!(Board::<RandomEvaluator>::from_fen("8/8/7k/8/4pP2/8/3B4/7K b - f3 0 1").is_err());
        assert!(Board::<RandomEvaluator>::from_fen("8/8/8/8/4pP2/8/3B4/7K b - f3 0 1").is_err());
        assert!(Board::<RandomEvaluator>::from_fen("8/8/8/4k3/4pP2/8/3B4/7K b - f3 0 1").is_ok());

        let b = Board::<RandomEvaluator>::from_fen("8/8/8/7k/5pP1/8/8/5R1K b - g3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 6);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("8/8/8/5k2/5pP1/8/8/5R1K b - g3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 7);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("8/8/8/8/4pP1k/8/8/4B2K b - f3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 5);
        stack.clear_all();
    }

    #[test]
    fn test_move_generation_3() {
        let mut stack = MoveStack::new();

        let b = Board::<RandomEvaluator>::from_fen("8/8/8/8/4RpPk/8/8/7K b - g3 0 1").ok().unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 6);
        stack.clear_all();
    }

    #[test]
    fn test_move_generation_4() {
        let mut stack = MoveStack::new();

        let b = Board::<RandomEvaluator>::from_fen("8/8/8/8/3QPpPk/8/8/7K b - g3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.ply(), 0);
        assert_eq!(stack.len(), 7);
        stack.clear();
        assert_eq!(stack.len(), 0);
        assert_eq!(stack.ply(), 0);
    }

    #[test]
    fn test_move_generation_5() {
        let mut stack = MoveStack::new();

        let b = Board::<RandomEvaluator>::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R w - - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 19 + 5);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R w K - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 19 + 6);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R w KQ - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 19 + 7);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R b KQ - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 19 + 5);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("rn2k2r/8/8/8/8/8/8/R3K2R b KQk - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 19 + 6);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("4k3/8/8/8/8/5n2/8/R3K2R w KQ - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 5);
        stack.clear_all();

        let mut b = Board::<RandomEvaluator>::from_fen("4k3/8/8/8/8/6n1/8/R3K2R w KQ - 0 1")
                        .ok()
                        .unwrap();
        b.generate_moves(true, &mut stack);
        let mut count = 0;
        while let Some(m) = stack.pop() {
            if b.do_move(m).is_some() {
                count += 1;
                b.undo_move(m);
            }
        }
        assert_eq!(count, 19 + 4);

        let b = Board::<RandomEvaluator>::from_fen("4k3/8/8/8/8/4n3/8/R3K2R w KQ - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 19 + 5);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("4k3/8/8/8/8/4n3/8/R3K2R w - - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 19 + 5);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("4k3/8/1b6/8/8/8/8/R3K2R w KQ - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        assert_eq!(stack.len(), 19 + 7);
        stack.clear_all();
    }

    #[test]
    fn test_do_undo_move() {
        let mut stack = MoveStack::new();

        let mut b = Board::<RandomEvaluator>::from_fen("b3k2r/6P1/8/5pP1/8/8/6P1/R3K2R w kKQ f6 \
                                                        0 1")
                        .ok()
                        .unwrap();
        b.generate_moves(true, &mut stack);
        let count = stack.len();
        while let Some(m) = stack.pop() {
            if let Some(h) = b.do_move(m) {
                assert!(h != 0);
                b.undo_move(m);
                let mut other_stack = MoveStack::new();
                b.generate_moves(true, &mut other_stack);
                assert_eq!(count, other_stack.len());
            }
        }
        assert_eq!(stack.len(), 0);
        let mut b = Board::<RandomEvaluator>::from_fen("b3k2r/6P1/8/5pP1/8/8/8/R3K2R b kKQ - 0 1")
                        .ok()
                        .unwrap();
        b.generate_moves(true, &mut stack);
        let count = stack.len();
        while let Some(m) = stack.pop() {
            if b.do_move(m).is_some() {
                b.undo_move(m);
                let mut other_stack = MoveStack::new();
                b.generate_moves(true, &mut other_stack);
                assert_eq!(count, other_stack.len());
            }
        }
    }

    #[test]
    fn test_find_pinned() {
        let b = Board::<RandomEvaluator>::from_fen("k2r4/3r4/3N4/5n2/qp1K2Pq/8/3PPR2/6b1 w - - 0 \
                                                    1")
                    .ok()
                    .unwrap();
        assert_eq!(b.find_pinned(), 1 << F2 | 1 << D6 | 1 << G4);
    }

    #[test]
    fn test_generate_only_captures() {
        let mut stack = MoveStack::new();

        let b = Board::<RandomEvaluator>::from_fen("k6r/P7/8/6p1/6pP/8/8/7K b - h3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(false, &mut stack);
        assert_eq!(stack.len(), 4);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("k7/8/8/4Pp2/4K3/8/8/8 w - f6 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(false, &mut stack);
        assert_eq!(stack.len(), 8);
        stack.clear_all();

        let b = Board::<RandomEvaluator>::from_fen("k7/8/8/4Pb2/4K3/8/8/8 w - - 0 1").ok().unwrap();
        b.generate_moves(false, &mut stack);
        assert_eq!(stack.len(), 7);
        stack.clear_all();
    }

    #[test]
    fn test_null_move() {
        let mut stack = MoveStack::new();

        let mut b = Board::<RandomEvaluator>::from_fen("k7/8/8/5Pp1/8/8/8/4K2R w K g6 0 1")
                        .ok()
                        .unwrap();
        b.generate_moves(true, &mut stack);
        let count = stack.len();
        stack.clear_all();
        let m = b.null_move();
        assert!(b.do_move(m).is_some());
        b.undo_move(m);
        b.generate_moves(true, &mut stack);
        assert_eq!(count, stack.len());
        stack.clear_all();

        let mut b = Board::<RandomEvaluator>::from_fen("k7/4r3/8/8/8/8/8/4K3 w - - 0 1")
                        .ok()
                        .unwrap();
        let m = b.null_move();
        assert!(b.do_move(m).is_none());
    }

    #[test]
    fn test_move_into_check_bug() {
        let mut stack = MoveStack::new();

        let mut b = Board::<RandomEvaluator>::from_fen("rnbq1bn1/pppP3k/8/3P2B1/2B5/5N2/PPPN1PP1/\
                                                        2K4R b - - 0 1")
                        .ok()
                        .unwrap();
        b.generate_moves(true, &mut stack);
        let m = stack.pop().unwrap();
        b.do_move(m);
        assert!(b.is_legal());
    }

    #[test]
    fn test_try_move_digest() {
        use board::BoardEvaluator;
        fn try_all<E: BoardEvaluator>(b: &Board<E>, stack: &MoveStack) {
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
        let b = Board::<RandomEvaluator>::from_fen("rnbqk2r/p1p1pppp/8/8/2Pp4/5NP1/pP1PPPBP/RNBQK\
                                                    2R b KQkq c3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear_all();
        let b = Board::<RandomEvaluator>::from_fen("rnbqk2r/p1p1pppp/8/8/Q1Pp4/5NP1/pP1PPPBP/RNB1\
                                                    K2R b KQkq - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear_all();
        let b = Board::<RandomEvaluator>::from_fen("rnbqk2r/p1p1pppp/3N4/8/Q1Pp4/6P1/pP1PPPBP/RNB\
                                                    1K2R b KQkq - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear_all();
        let b = Board::<RandomEvaluator>::from_fen("rnbq3r/p1p1pppp/8/3k4/2Pp4/5NP1/pP1PPPBP/RNBQ\
                                                    K2R b KQ c3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear_all();
        let b = Board::<RandomEvaluator>::from_fen("rn1qk2r/p1pbpppp/8/8/Q1Pp4/5NP1/pP1PPPBP/RNB1\
                                                    K2R b KQkq - 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear_all();
        let b = Board::<RandomEvaluator>::from_fen("8/8/8/8/4RpPk/8/8/7K b - g3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);

        stack.clear_all();
        let b = Board::<RandomEvaluator>::from_fen("8/8/8/8/5pPk/8/8/7K b - g3 0 1")
                    .ok()
                    .unwrap();
        b.generate_moves(true, &mut stack);
        try_all(&b, &stack);
    }
}
