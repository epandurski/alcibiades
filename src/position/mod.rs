//! Implements the rules of chess and the position evaluation logic.

pub mod tables;
pub mod move_generation;
pub mod bitsets;
mod evaluation;

use std::u16;
use std::mem;
use std::cmp::max;
use std::cell::UnsafeCell;
use std::hash::{Hasher, SipHasher};
use basetypes::*;
use moves::*;
use notation::parse_fen;
use self::bitsets::*;
use self::tables::BoardGeometry;
use self::move_generation::Board;
use self::evaluation::evaluate_board;


/// Represents an illegal possiton error.
pub struct IllegalPosition;


/// Holds the current position, knows the rules of chess, can evaluate
/// the odds.
///
/// `Position` improves on the features of `Board` adding the the
/// following important functionality:
///
/// 1. Threefold/twofold repetition detection.
/// 2. Draw by the 50 moves rule detection.
/// 3. Fast calculation of position's hash value.
/// 4. Exact evaluation of final positions.
/// 5. Static position evaluation.
/// 6. Static exchange evaluation.
/// 7. Quiescence search.
///
/// `Position` presents a convenient interface to the tree-searching
/// algorithm. It encapsulates most of the chess-specific knowledge
/// like the values of pieces, king safety, pawn structure
/// etc. `Position` can generate the all possible moves (plus a "null
/// move") in the current position, play a selected move and take it
/// back. It can also quickly (without doing extensive tree-searching)
/// evaluate the chances of the sides, so that the tree-searching
/// algorithm can use this evaluation to assign realistic game
/// outcomes to its leaf nodes.
///
/// **Important note:** Repeating positions are considered a draw
/// after the first repetition, not after the second one as the
/// official chess rules prescribe. This is done in the sake of
/// efficiency. In order to compensate for that
/// `Position::from_history` "forgets" all positions that have
/// occurred exactly once. Also, the root position is never deemed as
/// a draw due to repetition or rule-50.
pub struct Position {
    /// The current board.
    ///
    /// We use `UnsafeCell` for this, because the
    /// `evaluate_quiescence` method logically is non-mutating, but
    /// internally it tries moves on the board and then undoes them,
    /// always leaving everything the way it was.
    board: UnsafeCell<Board>,

    /// The hash value for the current board.
    board_hash: u64,

    /// The count of half-moves since the beginning of the game.
    halfmove_count: u16,

    /// `true` if the position is deemed as a draw by repetition or
    /// because 50 moves have been played without capturing a piece or
    /// advancing a pawn.
    repeated_or_rule50: bool,

    /// Information needed so as to be able to undo the played moves.
    state_stack: Vec<PositionInfo>,

    /// A list of hashes for the boards that had occurred during the
    /// game. This is needed so as to be able to detect repeated
    /// positions.
    encountered_boards: Vec<u64>,

    /// A hash value for the set of boards that had occurred at least
    /// twice before the root position (the earliest position in
    /// `state_stack`), and are still reachable from the root
    /// position. An empty set has a hash of `0`. We use this value
    /// when we generate position's hash.
    repeated_boards_hash: u64,
}


impl Position {
    /// Creates a new board instance from a FEN string.
    ///
    /// A FEN (Forsyth–Edwards Notation) string defines a particular
    /// position using only the ASCII character set.
    pub fn from_fen(fen: &str) -> Result<Position, IllegalPosition> {
        let (ref placement, to_move, castling, en_passant_square, halfmove_clock, fullmove_number) =
            try!(parse_fen(fen).map_err(|_| IllegalPosition));
        let mut p = Position {
            board: UnsafeCell::new(try!(Board::create(placement,
                                                      to_move,
                                                      castling,
                                                      en_passant_square)
                                            .map_err(|_| IllegalPosition))),
            board_hash: 0,
            halfmove_count: ((fullmove_number - 1) << 1) + to_move as u16,
            repeated_or_rule50: false,
            repeated_boards_hash: 0,
            encountered_boards: vec![0; halfmove_clock as usize],
            state_stack: vec![PositionInfo {
                                  halfmove_clock: if halfmove_clock < 99 {
                                      halfmove_clock
                                  } else {
                                      99
                                  },
                                  last_move: Move::invalid(),
                              }],
        };
        p.board_hash = p.board().calc_hash();
        Ok(p)
    }

    /// Creates a new instance.
    ///
    /// `fen` should be the Forsyth–Edwards Notation of a legal
    /// starting position. `moves` should be an iterator over all the
    /// moves that were played from that position. The move format is
    /// long algebraic notation. Examples: `e2e4`, `e7e5`, `e1g1`
    /// (white short castling), `e7e8q` (for promotion).
    pub fn from_history(fen: &str,
                        moves: &mut Iterator<Item = &str>)
                        -> Result<Position, IllegalPosition> {
        let mut p = try!(Position::from_fen(fen));
        let mut move_stack = MoveStack::new();
        'played_move: for played_move in moves {
            move_stack.clear();
            p.board().generate_moves(true, &mut move_stack);
            for m in move_stack.iter() {
                if played_move == m.notation() {
                    if p.do_move(*m) {
                        continue 'played_move;
                    } else {
                        return Err(IllegalPosition);
                    }
                }
            }
            return Err(IllegalPosition);
        }
        p.declare_as_root();
        Ok(p)
    }

    /// Returns a description of the placement of the pieces on the
    /// board.
    #[inline]
    pub fn pieces(&self) -> &PiecesPlacement {
        self.board().pieces()
    }

    /// Returns the side to move.
    #[inline]
    pub fn to_move(&self) -> Color {
        self.board().to_move()
    }

    /// Returns the castling rights.
    #[inline]
    pub fn castling(&self) -> CastlingRights {
        self.board().castling()
    }

    /// Returns the file on which an en-passant pawn capture is
    /// possible.
    #[inline]
    pub fn en_passant_file(&self) -> Option<File> {
        self.board().en_passant_file()
    }

    /// Returns the number of half-moves since the last piece capture
    /// or pawn advance.
    #[inline]
    pub fn halfmove_clock(&self) -> u8 {
        self.state().halfmove_clock
    }

    /// Returns the count of half-moves since the beginning of the
    /// game.
    ///
    /// At the beginning of the game it starts at `0`, and is
    /// incremented after anyone's move.
    #[inline]
    pub fn halfmove_count(&self) -> u16 {
        self.halfmove_count
    }

    /// Returns if the side to move is in check.
    #[inline]
    pub fn is_check(&self) -> bool {
        self.board().checkers() != 0
    }

    /// Returns if the side to move is unlikely to be in zugzwang.
    ///
    /// In many endgame positions there is a relatively high
    /// probability of zugzwang occurring. For such positions, this
    /// method will return `false`. For all "normal" positions it will
    /// return `true`. This is useful when deciding if it is safe to
    /// try a "null move".
    #[inline]
    pub fn is_zugzwang_unlikely(&self) -> bool {
        // TODO: Write a real implementation.
        true
    }

    /// Returns an almost unique hash value for the position.
    ///
    /// **Important notes:** 1) Two positions that differ in their
    /// sets of previously repeated, still reachable boards will have
    /// different hashes. 2) Two positions that differ only in their
    /// number of played moves without capturing piece or advancing a
    /// pawn will have equal hashes, as long as they both are far from
    /// the rule-50 limit.
    #[inline]
    pub fn hash(&self) -> u64 {
        if self.repeated_or_rule50 {
            // Repeated and rule-50 positions are evaluated as a draw,
            // so for our purposes they can be considered equal, and
            // therefore we generate the same hash for them.
            1
        } else {
            let hash = if self.root_is_unreachable() {
                self.board_hash
            } else {
                // If the repeated positions that occured before the
                // root postition are still reachable, we blend their
                // collective hash into current position's hash.
                self.board_hash ^ self.repeated_boards_hash
            };
            let halfmove_clock = self.state().halfmove_clock;

            if halfmove_clock < HALFMOVE_CLOCK_THRESHOLD {
                hash
            } else {
                // If `halfmove_clock` is close to rule-50, we blend
                // it into the returned hash.
                hash ^ self.board().zobrist().halfmove_clock[halfmove_clock as usize]
            }
        }
    }

    /// Evaluates a final position.
    ///
    /// In final positions this method will return the correct value
    /// of the position (`0` for a draw, `-29999` for a checkmate). A
    /// position is guaranteed to be final if `generate_moves` method
    /// generates no legal moves. (It may generate some pseudo-legal
    /// moves, but if none of them is legal, then the position is
    /// final.)
    ///
    /// **Important note:** Repeated and rule-50 positions are
    /// considered final (a draw).
    #[inline]
    pub fn evaluate_final(&self) -> Value {
        if self.repeated_or_rule50 || self.board().checkers() == 0 {
            // Repetition, rule-50, or stalemate.
            0
        } else {
            // Checkmated.
            -29999
        }
    }

    /// Statically evaluates the position.
    ///
    /// This method considers only static material and positional
    /// properties of the position. If the position is dynamic, with
    /// pending tactical threats, this function will return a grossly
    /// incorrect evaluation. The returned value will be between
    /// `-19999` and `19999`.
    #[inline]
    pub fn evaluate_static(&self) -> Value {
        let v = evaluate_board(self.board());
        debug_assert!(v >= -19999 && v <= 19999);
        v
    }

    /// Performs a "quiescence search" and returns an evaluation.
    ///
    /// The "quiescence search" is a restricted search which considers
    /// only a limited set of moves (for example: winning captures,
    /// pawn promotions to queen, check evasions). The goal is to
    /// statically evaluate only "quiet" positions (positions where
    /// there are no winning tactical moves to be made). Although this
    /// search can cheaply and correctly resolve many tactical issues,
    /// it is blind to other simple tactical threads like most kinds
    /// of forks, checks, even a checkmate in one move.
    /// 
    /// `lower_bound` and `upper_bound` together give the interval
    /// within which an as precise as possible evaluation is
    /// required. If during the calculation is determined that the
    /// exact evaluation is outside of this interval, this method may
    /// return a value that is closer to the the interval bounds than
    /// the exact evaluation, but always staying on the correct side
    /// of the interval. `static_evaluation` should be the value
    /// returned by `self.evaluate_static()`, or `VALUE_UNKNOWN`. The
    /// returned value will be between `-19999` and `19999`. For
    /// repeated and rule-50 positions `0` is returned.
    ///
    /// **Note:** This method will return a reliable result even when
    /// the side to move is in check. In this case it will try all
    /// possible check evasions. (It will will never use the static
    /// evaluation value when in check.)
    #[inline]
    pub fn evaluate_quiescence(&self,
                               lower_bound: Value,
                               upper_bound: Value,
                               static_evaluation: Value)
                               -> (Value, NodeCount) {
        debug_assert!(lower_bound < upper_bound);
        if self.repeated_or_rule50 {
            // This is a final position -- a draw.
            (0, 0)
        } else {
            // Perform `qsearch`.
            let mut searched_nodes = 0;
            let value = MOVE_STACK.with(|s| unsafe {
                self.qsearch(lower_bound,
                             upper_bound,
                             static_evaluation,
                             0,
                             0,
                             &mut *s.get(),
                             &evaluate_board,
                             &mut searched_nodes)
            });
            (value, searched_nodes)
        }
    }

    /// Returns the likely evaluation change (material) to be lost or
    /// gained as a result of a given move.
    ///
    /// This method performs static exchange evaluation (SEE). It
    /// examines the consequence of a series of exchanges on the
    /// destination square after a given move. A positive returned
    /// value indicates a "winning" move. For example, "PxQ" will
    /// always be a win, since the pawn side can choose to stop the
    /// exchange after its pawn is recaptured, and still be ahead. SEE
    /// is just an evaluation calculated without actually trying moves
    /// on the board, and therefore the returned value might be
    /// incorrect.
    /// 
    /// The move passed to this method **must** have been generated by
    /// `generate_moves`, `try_move_digest`, or `null_move` methods
    /// for the current position on the board.
    #[inline]
    pub fn evaluate_move(&self, m: Move) -> Value {
        if m.move_type() == MOVE_PROMOTION {
            // `calc_see` does not handle pawn promotions very well,
            // so for them we simply return some positive value. We
            // could differentiate wining and losing promotions, but
            // this makes no significant difference and may cause
            // funny move ordering, resulting in sometimes promoting
            // rooks and knights for no good reason.
            PIECE_VALUES[PAWN]
        } else {
            self.calc_see(m)
        }
    }

    /// Generates pseudo-legal moves.
    ///
    /// A pseudo-legal move is a move that is otherwise legal, except
    /// it might leave the king in check. Every legal move is a
    /// pseudo-legal move, but not every pseudo-legal move is legal.
    /// The generated moves will be pushed to `move_stack`. If all of
    /// the moves generated by this methods are illegal (this means
    /// that `do_move(m)` returns `false` for all of them), then the
    /// position is final, and `evaluate_final()` will return its
    /// correct value.
    ///
    /// **Important note:** Repeated and rule-50 positions are
    /// considered final (and therefore, this method generates no
    /// moves).
    #[inline]
    pub fn generate_moves(&self, move_stack: &mut MoveStack) {
        if !self.repeated_or_rule50 {
            self.board().generate_moves(true, move_stack);
        }
    }

    /// Returns a null move.
    ///
    /// "Null move" is a pseudo-move that changes only the side to
    /// move. It is sometimes useful to include a speculative null
    /// move in the search tree so as to achieve more aggressive
    /// pruning.
    #[inline]
    pub fn null_move(&self) -> Move {
        self.board().null_move()
    }

    /// Checks if `move_digest` represents a pseudo-legal move.
    ///
    /// If a move `m` exists that would be generated by
    /// `generate_moves` if called for the current position, and for
    /// that move `m.digest() == move_digest`, this method will
    /// return `Some(m)`. Otherwise it will return `None`. This is
    /// useful when playing moves from the transposition table,
    /// without calling `generate_moves`.
    #[inline]
    pub fn try_move_digest(&self, move_digest: MoveDigest) -> Option<Move> {
        if self.repeated_or_rule50 {
            // This is a final position -- no moves.
            None
        } else {
            self.board().try_move_digest(move_digest)
        }
    }

    /// Plays a move on the board.
    ///
    /// It verifies if the move is legal. If the move is legal, the
    /// board is updated and `true` is returned. If the move is
    /// illegal, `false` is returned without updating the board. The
    /// move passed to this method **must** have been generated by
    /// `generate_moves`, `try_move_digest`, or `null_move` methods
    /// for the current position on the board.
    ///
    /// Moves generated by the `null_move` method are exceptions. For
    /// them `do_move(m)` will return `false` only if the king is in
    /// check or the position is a draw due to repetition or rule-50.
    pub fn do_move(&mut self, m: Move) -> bool {
        if self.repeated_or_rule50 && m.is_null() {
            // This is a final position -- null moves are not
            // allowed. We must still allow other moves though,
            // because `from_history` should be able to call `do_move`
            // even in final positions.
            return false;
        }

        if let Some(h) = unsafe { self.board_mut().do_move(m) } {
            let halfmove_clock = if m.is_pawn_advance_or_capure() {
                0
            } else {
                match self.state().halfmove_clock {
                    x if x < 99 => x + 1,
                    _ => {
                        if !self.is_checkmate() {
                            self.repeated_or_rule50 = true;
                        }
                        99
                    }
                }
            };
            self.halfmove_count += 1;
            self.encountered_boards.push(self.board_hash);
            self.board_hash ^= h;
            debug_assert!(halfmove_clock <= 99);
            debug_assert!(self.encountered_boards.len() >= halfmove_clock as usize);

            // Figure out if the new position is repeated.
            if halfmove_clock >= 4 {
                let last_irrev =
                    (self.encountered_boards.len() - (halfmove_clock as usize)) as isize;
                let mut i = (self.encountered_boards.len() - 4) as isize;
                while i >= last_irrev {
                    if self.board_hash == self.encountered_boards[i as usize] {
                        self.repeated_or_rule50 = true;
                        break;
                    }
                    i -= 2;
                }
            }

            // The move is OK.
            self.state_stack.push(PositionInfo {
                halfmove_clock: halfmove_clock,
                last_move: m,
            });
            true

        } else {
            // The move is not OK.
            false
        }
    }

    /// Takes back the last played move.
    #[inline]
    pub fn undo_move(&mut self) {
        debug_assert!(self.state_stack.len() > 1);
        unsafe {
            self.board_mut().undo_move(self.state().last_move);
        }
        self.halfmove_count -= 1;
        self.board_hash = self.encountered_boards.pop().unwrap();
        self.repeated_or_rule50 = false;
        self.state_stack.pop();
    }

    /// A helper method for `evaluate_quiescence`. It is needed
    /// because`qsearch` should be able to call itself recursively,
    /// which should not complicate `evaluate_quiescence`'s
    /// public-facing interface.
    fn qsearch(&self,
               mut lower_bound: Value,
               upper_bound: Value,
               static_evaluation: Value,
               mut recapture_squares: Bitboard,
               ply: u8,
               move_stack: &mut MoveStack,
               eval_func: &Fn(&Board) -> Value,
               searched_nodes: &mut NodeCount)
               -> Value {
        debug_assert!(lower_bound < upper_bound);
        let not_in_check = self.board().checkers() == 0;

        // At the beginning of quiescence, the position's evaluation
        // is used to establish a lower bound on the score
        // (`stand_pat`). We assume that even if none of the capturing
        // moves can improve over the stand pat, there will be at
        // least one "quiet" move that will at least preserve the
        // stand pat value. (Note that this is not true if the the
        // side to move is in check.)
        let stand_pat = if not_in_check {
            if static_evaluation != VALUE_UNKNOWN {
                debug_assert!(static_evaluation > -20000 && static_evaluation < 20000);
                static_evaluation
            } else {
                let v = eval_func(self.board());
                debug_assert!(v > -20000 && v < 20000);
                v
            }
        } else {
            lower_bound
        };
        if stand_pat >= upper_bound {
            return stand_pat;
        }
        if stand_pat > lower_bound {
            lower_bound = stand_pat;
        }

        let obligatory_material_gain = lower_bound - stand_pat - 2 * PIECE_VALUES[PAWN];

        // Generate all non-quiet moves.
        move_stack.save();
        self.board().generate_moves(false, move_stack);

        // Try all generated moves one by one. Moves with higher
        // scores are tried before moves with lower scores.
        while let Some(m) = move_stack.remove_best_move() {
            // Check if the immediate material gain from this move is
            // big enough to warrant trying the move (no less than
            // `obligatory_material_gain`).
            let move_type = m.move_type();
            let captured_piece = m.captured_piece();
            let material_gain = if move_type == MOVE_PROMOTION {
                PIECE_VALUES[captured_piece] +
                PIECE_VALUES[Move::piece_from_aux_data(m.aux_data())] -
                PIECE_VALUES[PAWN]
            } else {
                PIECE_VALUES[captured_piece]
            };
            if material_gain < obligatory_material_gain {
                continue;
            }

            let dest_square = m.dest_square();
            let dest_square_bb = 1 << dest_square;

            // Calculate the static exchange evaluation and decide
            // whether to try the move. (This applis to "normal"
            // captures only -- check evasions, castlings, pawn
            // promotions, and en-passant captures are exempt.)
            if not_in_check && move_type == MOVE_NORMAL {
                // Verify if this is a mandatory recapture. (In order
                // to fix SEE errors due to pinned and overloaded
                // pieces, at least one recapture at the last capture
                // square is always tried.)
                if recapture_squares & dest_square_bb == 0 {
                    match self.calc_see(m) {
                        // This is a losing move -- do not try it.
                        x if x < 0 => continue,

                        // This is an even exchange -- try it only
                        // during the first few plys.
                        0 if ply >= SSE_EXCHANGE_MAX_PLY => continue,

                        // A winning move -- try it always.
                        _ => (),
                    }
                }
            }

            // Recursively call `qsearch` for the next move and update
            // the lower bound according to the recursively calculated
            // value.
            unsafe {
                if self.board_mut().do_move(m).is_some() {
                    *searched_nodes += 1;
                    let value = -self.qsearch(-upper_bound,
                                              -lower_bound,
                                              VALUE_UNKNOWN,
                                              recapture_squares ^ dest_square_bb,
                                              ply + 1,
                                              move_stack,
                                              eval_func,
                                              searched_nodes);
                    self.board_mut().undo_move(m);
                    if value >= upper_bound {
                        lower_bound = value;
                        break;
                    }
                    if value > lower_bound {
                        lower_bound = value;
                    }

                    // Mark that a recapture at this field had been tried.
                    recapture_squares &= !dest_square_bb;
                }
            }
        }
        move_stack.restore();

        // We should make sure that the returned value is between
        // -19999 and 19999, otherwise the engine might decline to
        // checkmate the opponent seeking the huge material gain that
        // `qsearch` had promised.
        match lower_bound {
            x if x < -19999 => -19999,
            x if x > 19999 => 19999,
            x => x,
        }
    }

    /// A helper method for `qsearch` and `evaluate_move`. It
    /// calculates the static evaluation exchange (SSE) value of a
    /// move.
    ///
    /// SEE is just an evaluation calulated without actually trying
    /// moves on the board, the returned value might be incorrect for
    /// many tactical reasons. In addition to that, if during the
    /// calculation it is determined that one of the sides inevitably
    /// loses material, for performance reasons the calculation is
    /// terminated and a value is returned which might not be exact
    /// (but its sign will be correct).
    fn calc_see(&self, m: Move) -> Value {
        // The impemented algorithm creates a swap-list of best case
        // material gains by traversing the "attackers and defenders"
        // set in least valuable piece order from pawn, knight,
        // bishop, rook, queen until king, with alternating sides. The
        // swap-list (an unary tree since there are no branches but
        // just a series of captures) is negamaxed for a final static
        // exchange evaluation.

        let mut us = self.board().to_move();
        let mut piece = m.piece();
        let orig_square = m.orig_square();
        let dest_square = m.dest_square();
        let captured_piece = m.captured_piece();
        debug_assert!(piece < NO_PIECE);
        debug_assert!(captured_piece <= NO_PIECE);
        let board = self.board();
        let mut occupied = board.occupied();
        let mut orig_square_bb = 1 << orig_square;
        let mut attackers_and_defenders = board.attacks_to(WHITE, dest_square) |
                                          board.attacks_to(BLACK, dest_square);

        // `may_xray` holds the set of pieces that may block x-ray
        // attacks from other pieces, so we must consider adding new
        // attackers/defenders every time a piece from the `may_xray`
        // set makes a capture.
        let may_xray = board.pieces().piece_type[PAWN] | board.pieces().piece_type[BISHOP] |
                       board.pieces().piece_type[ROOK] |
                       board.pieces().piece_type[QUEEN];

        unsafe {
            let mut depth = 0;
            let mut gain: [Value; 66] = mem::uninitialized();
            *gain.get_unchecked_mut(depth) = *PIECE_VALUES.get_unchecked(captured_piece);

            // Try each piece in `attackers_and_defenders` one by one,
            // starting with `piece` at `orig_square`.
            while orig_square_bb != 0 {
                // Change the side to move.
                us ^= 1;
                depth += 1;

                // Store a speculative value that will be used if the
                // captured piece happens to be defended.
                *gain.get_unchecked_mut(depth) = *PIECE_VALUES.get_unchecked(piece) -
                                                 *gain.get_unchecked(depth - 1);

                if max(-*gain.get_unchecked(depth - 1), *gain.get_unchecked(depth)) < 0 {
                    // Stopping here may change the exact value that
                    // would otherwise be returned, but will never
                    // change the sign of the returned value, which is
                    // good enough for our purposes.
                    break;
                }

                // Update attackers and defenders.
                attackers_and_defenders &= !orig_square_bb;
                occupied ^= orig_square_bb;
                if orig_square_bb & may_xray != 0 {
                    attackers_and_defenders |= consider_xrays(board.geometry(),
                                                              &board.pieces().piece_type,
                                                              occupied,
                                                              dest_square,
                                                              bitscan_forward(orig_square_bb));
                }

                // Find the next piece to enter the exchange.
                let next_attacker = get_least_valuable_piece(board.pieces(),
                                                             attackers_and_defenders &
                                                             *board.pieces()
                                                                   .color
                                                                   .get_unchecked(us));
                piece = next_attacker.0;
                orig_square_bb = next_attacker.1;
            }

            // Discard the speculative store -- the last attacker can
            // never be captured.
            depth -= 1;

            // Collapse the all values to one. Again, exploit the fact
            // that the side to move can back off from further
            // exchange if it is not favorable.
            while depth > 0 {
                *gain.get_unchecked_mut(depth - 1) = -max(-*gain.get_unchecked(depth - 1),
                                                          *gain.get_unchecked(depth));
                depth -= 1;
            }
            gain[0]
        }
    }

    /// A helper method for `from_history`. It removes all states but
    /// the current one from `state_stack`, and forgets all
    /// encountered boards before the last irreversible move.
    fn declare_as_root(&mut self) {
        let state = *self.state();
        let repeated_boards = {
            // Forget all encountered boards before the last
            // irreversible move.
            let last_irrev = self.encountered_boards.len() - state.halfmove_clock as usize;
            self.encountered_boards = self.encountered_boards.split_off(last_irrev);
            self.encountered_boards.reserve(32);

            // Because we assign a draw score on the first repetition
            // of the same position, we have to remove from
            // `self.encountered_boards` all positions that occurred
            // only once.
            set_non_repeated_values(&mut self.encountered_boards, 0)
        };

        // We calculate a single hash value representing the set
        // of all previously repeated, still reachable boards. We
        // will XOR that value with the board hash each time we
        // calculate position's hash. That way we guarantee that
        // two positions that have the same boards, but differ in
        // their set of previously repeated, still reachable
        // boards will have different hashes.
        self.repeated_boards_hash = if repeated_boards.is_empty() {
            0
        } else {
            let mut hasher = SipHasher::new();
            for x in repeated_boards {
                hasher.write_u64(x);
            }
            hasher.finish()
        };
        self.repeated_or_rule50 = false;

        // Remove all states but the last one from `state_stack`.
        self.state_stack = vec![state];
        self.state_stack.reserve(32);
    }

    /// A helper method for `hash`. It returns `true` if the root
    /// position (the earliest in `state_stack`) can not be reached by
    /// playing moves from the current position, `false` otherwise.
    #[inline(always)]
    fn root_is_unreachable(&self) -> bool {
        self.encountered_boards.len() > self.state().halfmove_clock as usize
    }

    /// A helper method for `do_move`. It returns if the side to move
    /// is checkmated.
    fn is_checkmate(&self) -> bool {
        self.board().checkers() != 0 &&
        MOVE_STACK.with(|s| unsafe {
            // Check if there are no legal moves.
            let board = self.board_mut();
            let move_stack = &mut *s.get();
            let mut no_legal_moves = true;
            move_stack.save();
            board.generate_moves(true, move_stack);
            for m in move_stack.iter() {
                if board.do_move(*m).is_some() {
                    board.undo_move(*m);
                    no_legal_moves = false;
                    break;
                }
            }
            move_stack.restore();
            no_legal_moves
        })
    }

    #[inline(always)]
    fn state(&self) -> &PositionInfo {
        self.state_stack.last().unwrap()
    }

    #[inline(always)]
    fn board(&self) -> &Board {
        unsafe { &*self.board.get() }
    }

    #[inline(always)]
    unsafe fn board_mut(&self) -> &mut Board {
        &mut *self.board.get()
    }
}


impl Clone for Position {
    fn clone(&self) -> Self {
        let mut encountered_boards = Vec::with_capacity(self.encountered_boards.capacity());
        let mut state_stack = Vec::with_capacity(self.state_stack.capacity());
        encountered_boards.extend_from_slice(&self.encountered_boards);
        state_stack.extend_from_slice(&self.state_stack);
        Position {
            board: UnsafeCell::new(self.board().clone()),
            board_hash: self.board_hash,
            halfmove_count: self.halfmove_count,
            repeated_or_rule50: self.repeated_or_rule50,
            repeated_boards_hash: self.repeated_boards_hash,
            encountered_boards: encountered_boards,
            state_stack: state_stack,
        }
    }
}


/// Contains information about a position.
#[derive(Clone, Copy)]
struct PositionInfo {
    /// The number of half-moves since the last piece capture or pawn
    /// advance. (We do not allow `halfmove_clock` to become greater
    /// than 99.)
    halfmove_clock: u8,

    /// The last played move.
    last_move: Move,
}


/// Thread-local storage for the generated moves.
thread_local!(
    static MOVE_STACK: UnsafeCell<MoveStack> = UnsafeCell::new(MoveStack::new())
);


/// The material value of pieces.
const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];


/// Do not try exchanges with SSE==0 in `qsearch` once this ply has
/// been reached.
const SSE_EXCHANGE_MAX_PLY: u8 = 2;


/// Do not blend `halfmove_clock` into position's hash until it gets
/// greater or equal to this number.
const HALFMOVE_CLOCK_THRESHOLD: u8 = 70;


/// Helper function for `Position::from_history`. It sets all unique
/// (non-repeated) values in `slice` to `value`, and returns a sorted
/// vector containing a single value for each duplicated value in
/// `slice`.
fn set_non_repeated_values<T>(slice: &mut [T], value: T) -> Vec<T>
    where T: Copy + Ord
{
    let mut repeated = vec![];
    let mut v = slice.to_vec();
    v.sort();
    let mut prev = value;
    for curr in v {
        if curr != value && curr == prev {
            repeated.push(curr);
        }
        prev = curr;
    }
    repeated.dedup();
    for x in slice.iter_mut() {
        if repeated.binary_search(x).is_err() {
            *x = value;
        }
    }
    repeated
}


/// A helper function for `Position::calc_see`. It returns a bitboard
/// describing the position on the board of the piece that could
/// attack `target_square`, but only when `xrayed_square` becomes
/// vacant. (Returns `0` if there is no such piece.)
#[inline(always)]
fn consider_xrays(geometry: &BoardGeometry,
                  piece_type_array: &[Bitboard; 6],
                  occupied: Bitboard,
                  target_square: Square,
                  xrayed_square: Square)
                  -> Bitboard {
    let candidates = occupied & geometry.squares_behind_blocker[target_square][xrayed_square];

    // Try the straight sliders first, if not, the diagonal sliders.
    let straight_slider_bb = geometry.piece_attacks_from(ROOK, target_square, candidates) &
                             candidates &
                             (piece_type_array[QUEEN] | piece_type_array[ROOK]);
    if straight_slider_bb != 0 {
        straight_slider_bb
    } else {
        geometry.piece_attacks_from(BISHOP, target_square, candidates) & candidates &
        (piece_type_array[QUEEN] | piece_type_array[BISHOP])
    }
}


/// A helper function for `Position::calc_see`. It takes a subset of
/// pieces `set`, and returns the type of the least valuable piece,
/// and a bitboard describing its position on the board.
#[inline(always)]
fn get_least_valuable_piece(pieces: &PiecesPlacement, set: Bitboard) -> (PieceType, Bitboard) {
    for p in (KING..NO_PIECE).rev() {
        let piece_subset = pieces.piece_type[p] & set;
        if piece_subset != 0 {
            return (p, ls1b(piece_subset));
        }
    }
    (NO_PIECE, 0)
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::move_generation::Board;
    use super::PIECE_VALUES;
    use basetypes::*;
    use moves::*;

    // This is a very simple evaluation function used for the testing
    // of `qsearch`.
    #[allow(unused_variables)]
    fn simple_eval(board: &Board) -> Value {
        use basetypes::*;
        use position::bitsets::*;
        let piece_type = board.pieces().piece_type;
        let color = board.pieces().color;
        let us = board.to_move();
        let them = 1 ^ us;
        let mut result = 0;
        for piece in QUEEN..NO_PIECE {
            result += PIECE_VALUES[piece] *
                      (pop_count(piece_type[piece] & color[us]) as i16 -
                       pop_count(piece_type[piece] & color[them]) as i16);
        }
        result
    }

    #[test]
    fn test_fen_parsing() {
        assert!(Position::from_fen("nbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr1/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBN b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR/ b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNRR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP01PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP91PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP*1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 * 1")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 *")
                    .is_err());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b - e3 0 1")
                    .is_ok());
        assert!(Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
                    .is_ok());
        assert!(Position::from_fen("8/8/8/8/8/8/8/8 w - - 0 1").is_err());
        assert!(Position::from_fen("8/8/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/8/8/8/7K w - - 0 1").is_ok());
        assert!(Position::from_fen("k7/8/8/8/8/8/8/6KK w - - 0 1").is_err());
        assert!(Position::from_fen("k7/pppppppp/p7/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/8/7P/PPPPPPPP/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/pppppppp/8/8/8/8/PPPPPPPP/7K w - - 0 1").is_ok());
        assert!(Position::from_fen("k7/1P6/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/1B6/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/1N6/8/8/8/8/8/7K w - - 0 1").is_ok());
        assert!(Position::from_fen("k3P3/8/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k3p3/8/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/8/8/8/pP5K w - - 0 1").is_err());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1").is_ok());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K2B w KQkq - 0 1").is_err());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K3 w KQkq - 0 1").is_err());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K3 w KQkq - 0 1").is_err());
        assert!(Position::from_fen("r3k2r/8/8/8/8/8/8/R3K3 w Qkq - 0 1").is_ok());
        assert!(Position::from_fen("r2k3r/8/8/8/8/8/8/R3K3 w Qkq - 0 1").is_err());
        assert!(Position::from_fen("r2k3r/8/8/8/8/8/8/R3K3 w Qk - 0 1").is_err());
        assert!(Position::from_fen("r2k3r/8/8/8/8/8/8/R3K3 w Q - 0 1").is_ok());
        assert!(Position::from_fen("k7/8/8/8/7P/8/8/7K w - h3 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/7P/8/8/7K b - h3 0 1").is_ok());
        assert!(Position::from_fen("k7/8/8/7P/8/8/8/7K b - h4 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/7P/7P/8/7K b - h3 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/7P/8/7P/7K b - h3 0 1").is_err());
        assert!(Position::from_fen("k7/8/8/8/6P1/7P/8/7K b - h3 0 1").is_err());
        assert!(Position::from_fen("8/8/8/6k1/7P/8/8/7K b - h3 0 1").is_ok());
        assert!(Position::from_fen("8/8/8/6k1/7P/8/8/6RK b - h3 0 1").is_err());
        assert!(Position::from_fen("8/8/8/6k1/3P4/8/8/2B4K b - d3 0 1").is_ok());
        assert!(Position::from_fen("8/8/8/6k1/7P/4B3/8/7K b - h3 0 1").is_err());
        assert!(Position::from_fen("8/8/8/6k1/7P/8/8/7K b - h3 0 0").is_err());
    }

    #[test]
    fn test_evaluate_static() {
        assert!(Position::from_fen("krq5/p7/8/8/8/8/8/KRQ5 w - - 0 1")
                    .ok()
                    .unwrap()
                    .evaluate_static() < -20);
    }

    #[test]
    fn test_evaluate_move() {
        let p = Position::from_fen("8/4P1kP/8/8/8/7p/8/7K w - - 0 1")
                    .ok()
                    .unwrap();
        let mut s = MoveStack::new();
        p.generate_moves(&mut s);
        while let Some(m) = s.pop() {
            if m.notation() == "e7e8q" {
                assert!(p.evaluate_move(m) >= 0);
            }
            if m.notation() == "e7e8r" {
                assert!(p.evaluate_move(m) >= 0);
            }
            if m.notation() == "h7h8r" {
                assert_eq!(p.evaluate_move(m), 100);
            }
            if m.notation() == "h1h2" {
                assert_eq!(p.evaluate_move(m), 0);
            }
            if m.notation() == "h1g2" {
                assert_eq!(p.evaluate_move(m), -10000);
            }
        }
        assert_eq!(p.evaluate_move(p.null_move()), 0);
        let p = Position::from_fen("6k1/1P6/8/4b3/8/8/8/1R3K2 w - - 0 1")
                    .ok()
                    .unwrap();
        p.generate_moves(&mut s);
        while let Some(m) = s.pop() {
            if m.notation() == "b7b8q" {
                assert!(p.evaluate_move(m) >= 0);
            }
            if m.notation() == "b7b8q" {
                assert!(p.evaluate_move(m) >= 0);
            }
        }
    }

    #[test]
    fn test_qsearch() {
        let mut s = MoveStack::new();
        let p = Position::from_fen("8/8/8/8/6k1/6P1/8/6K1 b - - 0 1").ok().unwrap();
        assert_eq!(p.qsearch(-1000,
                             1000,
                             VALUE_UNKNOWN,
                             0,
                             0,
                             &mut s,
                             &simple_eval,
                             &mut 0),
                   0);

        let p = Position::from_fen("8/8/8/8/6k1/6P1/8/5bK1 b - - 0 1").ok().unwrap();
        assert_eq!(p.qsearch(-1000,
                             1000,
                             VALUE_UNKNOWN,
                             0,
                             0,
                             &mut s,
                             &simple_eval,
                             &mut 0),
                   225);

        let p = Position::from_fen("8/8/8/8/5pkp/6P1/5P1P/6K1 b - - 0 1").ok().unwrap();
        assert_eq!(p.qsearch(-1000,
                             1000,
                             VALUE_UNKNOWN,
                             0,
                             0,
                             &mut s,
                             &simple_eval,
                             &mut 0),
                   0);

        let p = Position::from_fen("8/8/8/8/5pkp/6P1/5PKP/8 b - - 0 1").ok().unwrap();
        assert_eq!(p.qsearch(-1000,
                             1000,
                             VALUE_UNKNOWN,
                             0,
                             0,
                             &mut s,
                             &simple_eval,
                             &mut 0),
                   -100);

        let p = Position::from_fen("r1bqkbnr/pppp2pp/2n2p2/4p3/2N1P2B/3P1N2/PPP2PPP/R2QKB1R w - \
                                    - 5 1")
                    .ok()
                    .unwrap();
        assert_eq!(p.qsearch(-1000,
                             1000,
                             VALUE_UNKNOWN,
                             0,
                             0,
                             &mut s,
                             &simple_eval,
                             &mut 0),
                   0);

        let p = Position::from_fen("r1bqkbnr/pppp2pp/2n2p2/4N3/4P2B/3P1N2/PPP2PPP/R2QKB1R b - - \
                                    5 1")
                    .ok()
                    .unwrap();
        assert_eq!(p.qsearch(-1000,
                             1000,
                             VALUE_UNKNOWN,
                             0,
                             0,
                             &mut s,
                             &simple_eval,
                             &mut 0),
                   -100);

        let p = Position::from_fen("rn2kbnr/ppppqppp/8/4p3/2N1P1b1/3P1N2/PPP2PPP/R1BKQB1R w - - \
                                    5 1")
                    .ok()
                    .unwrap();
        assert_eq!(p.qsearch(-1000,
                             1000,
                             VALUE_UNKNOWN,
                             0,
                             0,
                             &mut s,
                             &simple_eval,
                             &mut 0),
                   0);

        let p = Position::from_fen("8/8/8/8/8/7k/7q/7K w - - 0 1").ok().unwrap();
        assert!(p.qsearch(-10000,
                          10000,
                          VALUE_UNKNOWN,
                          0,
                          0,
                          &mut s,
                          &simple_eval,
                          &mut 0) <= -10000);

        let p = Position::from_fen("8/8/8/8/8/6qk/7P/7K b - - 0 1").ok().unwrap();
        assert_eq!(p.evaluate_quiescence(-10000, 10000, VALUE_UNKNOWN).1, 1);
    }

    #[test]
    fn test_from_history_repeated() {
        let moves: Vec<&str> = vec!["g4f3", "g1f1", "f3g4", "f1g1", "g4f3", "g1f1", "f3g4"];
        let p = Position::from_history("8/8/8/8/6k1/6P1/8/6K1 b - - 0 1", &mut moves.into_iter())
                    .ok()
                    .unwrap();
        let mut v = MoveStack::new();
        p.generate_moves(&mut v);
        assert_eq!(v.count(), 5);
    }

    #[test]
    fn test_set_non_repeated_values() {
        use super::set_non_repeated_values;
        let mut v = vec![0, 1, 2, 7, 9, 0, 0, 1, 2];
        let dups = set_non_repeated_values(&mut v, 0);
        assert_eq!(v, vec![0, 1, 2, 0, 0, 0, 0, 1, 2]);
        assert_eq!(dups, vec![1, 2]);
    }

    #[test]
    fn is_repeated() {
        let mut p = Position::from_fen("8/5p1b/5Pp1/6P1/6p1/3p1pPk/3PpP2/4B2K w - - 0 1")
                        .ok()
                        .unwrap();
        let mut v = MoveStack::new();
        let mut count = 0;
        for _ in 0..100 {
            p.generate_moves(&mut v);
            while let Some(m) = v.pop() {
                if p.do_move(m) {
                    count += 1;
                    v.clear();
                    break;
                }
            }
        }
        assert_eq!(count, 4);
    }

    #[test]
    fn is_checkmate() {
        let p = Position::from_fen("8/8/8/8/8/7K/8/5R1k b - - 0 1")
                    .ok()
                    .unwrap();
        assert!(p.is_checkmate());

        let p = Position::from_fen("8/8/8/8/8/7K/6p1/5R1k b - - 0 1")
                    .ok()
                    .unwrap();
        assert!(!p.is_checkmate());

        let p = Position::from_fen("8/8/8/8/8/7K/8/5N1k b - - 0 1")
                    .ok()
                    .unwrap();
        assert!(!p.is_checkmate());
    }

    #[test]
    fn test_static_exchange_evaluation() {
        let mut v = MoveStack::new();

        let p = Position::from_fen("5r2/8/8/4q1p1/3P4/k3P1P1/P2b1R1B/K4R2 w - - 0 1").ok().unwrap();
        p.generate_moves(&mut v);
        while let Some(m) = v.pop() {
            if m.notation() == "f2f4" {
                assert!(p.evaluate_move(m) <= -400);
            }
            if m.notation() == "e3e4" {
                assert_eq!(p.evaluate_move(m), -100);
            }
            if m.notation() == "g3g4" {
                assert_eq!(p.evaluate_move(m), 0);
            }
            if m.notation() == "f1e1" {
                assert_eq!(p.evaluate_move(m), -500);
            }
            if m.notation() == "f1d1" {
                assert_eq!(p.evaluate_move(m), 0);
            }
        }

        let p = Position::from_fen("5r2/8/8/4q1p1/3P4/k3P1P1/P2b1R1B/K4R2 b - - 0 1").ok().unwrap();
        p.generate_moves(&mut v);
        while let Some(m) = v.pop() {
            if m.notation() == "e5e3" {
                assert_eq!(p.evaluate_move(m), 100);
            }
            if m.notation() == "e5d4" {
                assert_eq!(p.evaluate_move(m), -875);
            }
            if m.notation() == "a3a2" {
                assert_eq!(p.evaluate_move(m), -9900);
            }
        }
    }

    #[test]
    fn test_repeated_boards_hash() {
        let p1 = Position::from_fen("8/8/8/8/8/7k/8/7K w - - 0 1").ok().unwrap();
        let moves: Vec<&str> = vec![];
        let p2 = Position::from_history("8/8/8/8/8/7k/8/7K w - - 0 1", &mut moves.into_iter())
                     .ok()
                     .unwrap();
        assert_eq!(p1.board_hash, p2.board_hash);
        assert_eq!(p1.hash(), p2.hash());
        let moves: Vec<&str> = vec!["f1g1", "f3g3", "g1h1", "g3h3"];
        let p2 = Position::from_history("8/8/8/8/8/5k2/8/5K2 w - - 0 1", &mut moves.into_iter())
                     .ok()
                     .unwrap();
        assert_eq!(p1.board_hash, p2.board_hash);
        assert_eq!(p1.hash(), p2.hash());
        let moves: Vec<&str> = vec!["f1g1", "f3g3", "g1f1", "g3f3", "f1g1", "f3g3", "g1h1", "g3h3"];
        let p3 = Position::from_history("8/8/8/8/8/5k2/8/5K2 w - - 0 1", &mut moves.into_iter())
                     .ok()
                     .unwrap();
        assert_eq!(p1.board_hash, p2.board_hash);
        assert!(p1.hash() != p3.hash());
    }
}
