//! Implements the rules of chess and position evaluation logic.

pub mod bitsets;
pub mod tables;
pub mod board;
pub mod evaluation;
pub mod notation;

use std::mem;
use std::cmp::max;
use std::cell::UnsafeCell;
use std::hash::{Hasher, SipHasher};
use basetypes::*;
use chess_move::*;
use self::bitsets::*;
use self::tables::*;
use self::board::Board;
use self::evaluation::evaluate_board;


#[derive(Clone, Copy)]
struct StateInfo {
    // The number of halfmoves since the last pawn advance or capture.
    halfmove_clock: u8,

    // The last played move.
    last_move: Move,
}


// Contains two killer moves with their respective counters.
#[derive(Clone, Copy)]
struct KillersRecord {
    slot1: (MoveDigest, u32),
    slot2: (MoveDigest, u32),
}


impl Default for KillersRecord {
    fn default() -> KillersRecord {
        KillersRecord {
            slot1: (0, 0),
            slot2: (0, 0),
        }
    }
}


/// Represents an illegal possiton error.
pub struct IllegalPosition;


/// Represents a chess position.
///
/// `Position` is intended as a convenient interface for the
/// tree-searching algorithm. It encapsulates most of the
/// chess-specific knowledge like the chess rules, values of pieces,
/// static exchange evaluation, king safety, pawn structure
/// etc. `Position` can be instantiated from a FEN string, can
/// generate the all possible moves (plus a "null move") in the
/// current position, play a selected move and take it back. It can
/// also approximately (without doing extensive tree-searching)
/// evaluate the chances of the sides, so that tree-searching
/// algorithms can use this evaluation to assign realistic game
/// outcomes to their leaf nodes.
///
/// The implementation correctly detects repeating positions and
/// evaluates them as a draw. It **is not** "aware" of the 50-moves
/// rule, partly because it is not that important in practice, but
/// mainly because it impossible to implement it 100% correctly with
/// relation to the transposition table.
pub struct Position {
    // The current board.  We use `UnsafeCell` for it, because the
    // `evaluate_quiescence` method logically is non-mutating, but
    // internally it tries moves on the board and then undoes them,
    // always leaving everything the way it was.
    board: UnsafeCell<Board>,

    // The Zobrist hash value for the current board.
    board_hash: u64,

    // The count of half-moves since the beginning of the game.
    halfmove_count: u16,

    // `true ` if the position is a draw by repetition.
    is_repeated: bool,

    // A hash value for the set of boards that are still reachable
    // from the root position, and had occurred at least twice before
    // the root position. An empty set has a hash of `0`.
    repeated_boards_hash: u64,

    // Information needed so as to be able to undo the played moves.
    state_stack: Vec<StateInfo>,

    // A list of boards that had occurred during the game. This is
    // needed so as to be able to detect repeated positions.
    encountered_boards: Vec<u64>,

    // Killer moves.
    killer_moves: [KillersRecord; MAX_DEPTH as usize + 1],
}


impl Position {
    /// Creates a new instance.
    ///
    /// `fen` should be the Forsyth–Edwards Notation of a legal
    /// starting position.
    pub fn from_fen(fen: &str) -> Result<Position, IllegalPosition> {
        let (ref placement, to_move, castling, en_passant_square, halfmove_clock, fullmove_number) =
            try!(notation::parse_fen(fen).map_err(|_| IllegalPosition));
        let mut p = Position {
            board: UnsafeCell::new(try!(Board::create(placement,
                                                      to_move,
                                                      castling,
                                                      en_passant_square)
                                            .map_err(|_| IllegalPosition))),
            board_hash: 0,
            halfmove_count: ((fullmove_number - 1) << 1) + to_move as u16,
            is_repeated: false,
            repeated_boards_hash: 0,
            encountered_boards: vec![0; halfmove_clock as usize],
            state_stack: vec![StateInfo {
                                  halfmove_clock: halfmove_clock,
                                  last_move: Move::invalid(),
                              }],
            killer_moves: [Default::default(); MAX_DEPTH as usize + 1],
        };
        p.board_hash = p.board().calc_hash();
        Ok(p)
    }

    /// Creates a new instance.
    ///
    /// `fen` should be the Forsyth–Edwards Notation of a legal
    /// starting position. `moves` should be an iterator over all the
    /// moves that were played from that position. The move format is
    /// in long algebraic notation. Examples: `e2e4`, `e7e5`, `e1g1`
    /// (white short castling), `e7e8q` (for promotion).
    pub fn from_history(fen: &str,
                        moves: &mut Iterator<Item = &str>)
                        -> Result<Position, IllegalPosition> {
        let mut p = try!(Position::from_fen(fen));
        let mut move_stack = MoveStack::new();
        'played_move: for played_move in moves {
            p.board().generate_moves(true, &mut move_stack);
            while let Some(m) = move_stack.pop() {
                if played_move == m.notation() {
                    if p.do_move(m) {
                        move_stack.clear();
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

    /// Returns the board associated with the current position.
    #[inline(always)]
    pub fn board(&self) -> &Board {
        unsafe { &*self.board.get() }
    }

    /// Returns if the position is unlikely to be a zugzwang.
    ///
    /// In many endgame positions there is a relatively high
    /// probability of zugzwang occurring. For such positions, this
    /// method will return `false`. For all "normal" positions it will
    /// return `true`.
    #[inline]
    pub fn is_zugzwang_safe(&self) -> bool {
        // TODO: Write a real implementation.
        true
    }

    /// Returns if the position is a draw due to repetition.
    ///
    /// **Important note:** Repeating positions are considered a draw
    /// after the first repetition, not after the second one as the
    /// official chess rules prescribe. This is done in the sake of
    /// efficiency. In order to compensate for that
    /// `Position::from_history` "forgets" all positions that have
    /// occurred exactly once. Also, for the root positions created by
    /// `Position::from_history`, `is_repeated` will always return
    /// `false`.
    #[inline(always)]
    pub fn is_repeated(&self) -> bool {
        self.is_repeated
    }

    /// Returns the count of half-moves since the beginning of the
    /// game.
    ///
    /// At the beginning of the game it starts at `0`, and is
    /// incremented after anyone's move.
    #[inline(always)]
    pub fn halfmove_count(&self) -> u16 {
        self.halfmove_count
    }

    /// Returns an almost unique hash value for the position.
    ///
    /// **Important note:** Two positions having the same boards, but
    /// differing in their set of previously repeated, still reachable
    /// boards will have different hashes.
    #[inline]
    pub fn hash(&self) -> u64 {
        if self.is_repeated() {
            // All repeated positions are evaluated as a draw, so for
            // our purposes they can be considered equal, and
            // therefore we generate the same hash for them.
            1
        } else {
            if self.root_is_unreachable() {
                self.board_hash
            } else {
                self.board_hash ^ self.repeated_boards_hash
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
    /// **Important note:** Repeating positions are considered final
    /// (a draw).
    #[inline]
    pub fn evaluate_final(&self) -> Value {
        if self.is_repeated() || self.board().checkers() == 0 {
            // Repetition or stalemate.
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
        evaluate_board(self.board())
    }

    /// Performs a "quiescence search" and returns an evaluation.
    ///
    /// The "quiescence search" is a restricted tree which considers
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
    /// repeated positions `0` is returned.
    ///
    /// **Note:** This method will return a reliable result even when
    /// the side to move is in check. In this case it will try all
    /// possible check evasions, and will never use the static
    /// evaluation when in check.
    #[inline]
    pub fn evaluate_quiescence(&self,
                               lower_bound: Value,
                               upper_bound: Value,
                               static_evaluation: Value)
                               -> (Value, NodeCount) {
        assert!(lower_bound < upper_bound);
        thread_local!(
            static MOVE_STACK: UnsafeCell<MoveStack> = UnsafeCell::new(MoveStack::new())
        );
        if self.is_repeated() {
            (0, 0)
        } else {
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
    /// value indicates a "winning" move. For example, PxQ will always
    /// be a win, since the pawn side can choose to stop the exchange
    /// after its pawn is recaptured, and still be ahead. The move
    /// passed to this method **must** have been generated by
    /// `generate_moves`, `try_move_digest`, or `null_move` methods
    /// for the current position on the board.
    #[inline]
    pub fn evaluate_move(&self, m: Move) -> Value {
        self.calc_see(self.board().to_move(),
                      m.piece(),
                      m.orig_square(),
                      m.dest_square(),
                      m.captured_piece())
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
    /// correct value. This method will generate no moves if the
    /// position is a draw due to repetition.
    ///
    /// **Important note:** Repeating positions are considered final
    /// (and therefore, this method generates no moves).
    #[inline]
    pub fn generate_moves(&self, move_stack: &mut MoveStack) {
        if !self.is_repeated() {
            self.board().generate_moves(true, move_stack);
        }
    }

    /// Returns a null move.
    ///
    /// "Null move" is an illegal pseudo-move that changes only the
    /// side to move. It is sometimes useful to include a speculative
    /// null move in the search tree so as to achieve more aggressive
    /// pruning. For the move generated by this method, `do_move(m)`
    /// will return `false` only if the king is in check or the
    /// position is a draw due to repetition.
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
        if self.is_repeated() {
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
    /// check or the position is a draw due to repetition.
    #[inline]
    pub fn do_move(&mut self, m: Move) -> bool {
        if self.is_repeated() && m.is_null() {
            return false;
        }
        if let Some(h) = unsafe { self.board_mut().do_move(m) } {
            let halfmove_clock = if m.is_pawn_advance_or_capure() {
                0
            } else {
                self.state().halfmove_clock + 1
            };
            self.halfmove_count += 1;
            self.encountered_boards.push(self.board_hash);
            self.board_hash ^= h;
            assert!(self.encountered_boards.len() >= halfmove_clock as usize);

            // Figure out if the new position is repeated.
            if halfmove_clock >= 4 {
                let last_irrev =
                    (self.encountered_boards.len() - (halfmove_clock as usize)) as isize;
                let mut i = (self.encountered_boards.len() - 4) as isize;
                while i >= last_irrev {
                    if self.board_hash ==
                       unsafe { *self.encountered_boards.get_unchecked(i as usize) } {
                        self.is_repeated = true;
                        break;
                    }
                    i -= 2;
                }
            }

            self.state_stack.push(StateInfo {
                halfmove_clock: halfmove_clock,
                last_move: m,
            });
            true
        } else {
            false
        }
    }

    /// Takes back the last played move.
    #[inline]
    pub fn undo_move(&mut self) {
        assert!(self.state_stack.len() > 1);
        unsafe {
            self.board_mut().undo_move(self.state().last_move);
        }
        self.halfmove_count -= 1;
        self.board_hash = self.encountered_boards.pop().unwrap();
        self.is_repeated = false;
        self.state_stack.pop();
    }

    /// Registers that the last move was a killer move.
    #[inline]
    pub fn register_killer(&mut self) {
        let last_move_digest = self.state().last_move.digest();
        assert!(self.state_stack.len() - 1 <= MAX_DEPTH as usize);
        let record = unsafe { self.killer_moves.get_unchecked_mut(self.state_stack.len() - 1) };
        if record.slot1.0 == last_move_digest {
            record.slot1.1 += 1;
            return;
        }
        if record.slot2.0 == last_move_digest {
            record.slot2.1 += 1;
            return;
        }
        let slot = if record.slot1.1 <= record.slot2.1 {
            &mut record.slot1
        } else {
            &mut record.slot2
        };
        *slot = (last_move_digest, 0);
    }

    // A helper method for `evaluate_quiescence`. It is needed
    // because`qsearch` should be able to call itself recursively,
    // which should not complicate `evaluate_quiescence`'s
    // public-facing interface.
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
        assert!(lower_bound < upper_bound);
        let not_in_check = self.board().checkers() == 0;

        // At the beginning of quiescence, the position's evaluation
        // is used to establish a lower bound on the score
        // (`stand_pat`). We assume that even if none of the capturing
        // moves can improve over the stand pat, there will be at
        // least one "quiet" move that will at least preserve the
        // stand pat value. (Note that this is not true if the the
        // side to move is in check!)
        let stand_pat = if not_in_check {
            if static_evaluation != VALUE_UNKNOWN {
                assert!(static_evaluation > -20000 && static_evaluation < 20000);
                static_evaluation
            } else {
                let v = eval_func(self.board());
                assert!(v > -20000 && v < 20000);
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
            // Check if the potential material gain from this move is
            // big enough to warrant trying the move.
            let move_type = m.move_type();
            let captured_piece = m.captured_piece();
            let material_gain = if move_type == MOVE_PROMOTION {
                PIECE_VALUES[captured_piece] +
                PIECE_VALUES[Move::piece_from_aux_data(m.aux_data())] -
                PIECE_VALUES[PAWN]
            } else {
                unsafe { *PIECE_VALUES.get_unchecked(captured_piece) }
            };
            if material_gain < obligatory_material_gain {
                continue;
            }

            let dest_square = m.dest_square();
            let dest_square_bb = 1 << dest_square;

            // Calculate the static exchange evaluation, and decide
            // whether to try the move. (But first, make sure that we
            // are dealing with a normal capture move.)
            if not_in_check && move_type == MOVE_NORMAL {

                // Verify if this is a mandatory recapture. (At least
                // one recapture at the capture square is tried, no
                // matter the SSE.)
                if recapture_squares & dest_square_bb == 0 {
                    match self.calc_see(self.board().to_move(),
                                        m.piece(),
                                        m.orig_square(),
                                        dest_square,
                                        captured_piece) {
                        x if x < 0 => continue,
                        0 if ply >= SSE_EXCHANGE_MAX_PLY => continue,
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

    // A helper method for `qsearch` and `evaluate_move`. It
    // calculates the static evaluation exchange (SSE) value of a
    // capture. SSE heuristics is used to recognize those captures
    // that are likely to lead to material loss.
    //
    // The returned value is the material that is expected to be
    // gained in the exchange by the attacking side (`us`), when
    // capturing the `captured_piece` on the `dest_square`. The
    // `orig_square` specifies the square from which the `piece` makes
    // the capture.
    #[inline]
    fn calc_see(&self,
                mut us: Color,
                mut piece: PieceType,
                orig_square: Square,
                dest_square: Square,
                captured_piece: PieceType)
                -> Value {
        // The impemented algorithm creates a swap-list of best case
        // material gains by traversing the "attackers and defenders"
        // set in least valuable piece order from pawn, knight,
        // bishop, rook, queen until king, with alternating sides. The
        // swap-list (an unary tree since there are no branches but
        // just a series of captures) is negamaxed for a final static
        // exchange evaluation.

        assert!(us <= 1);
        assert!(piece < NO_PIECE);
        assert!(orig_square <= 63);
        assert!(dest_square <= 63);
        assert!(captured_piece <= NO_PIECE);
        let board = self.board();
        let mut occupied = board.occupied();
        let mut orig_square_bb = 1 << orig_square;
        let mut attackers_and_defenders = board.attacks_to(WHITE, dest_square) |
                                          board.attacks_to(BLACK, dest_square);

        // `may_xray` holds the set of pieces that may block x-ray
        // attacks from other pieces, so we must consider adding new
        // attackers/defenders every time a piece from the `may_xray`
        // set makes a capture.
        let may_xray = board.piece_type()[PAWN] | board.piece_type()[BISHOP] |
                       board.piece_type()[ROOK] | board.piece_type()[QUEEN];

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
                                                              &board.piece_type(),
                                                              occupied,
                                                              dest_square,
                                                              bitscan_forward(orig_square_bb));
                }

                // Find the next piece to enter the exchange.
                let next_attacker = get_least_valuable_piece(board.piece_type(),
                                                             attackers_and_defenders &
                                                             *board.color().get_unchecked(us));
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

    // A helper method for `Position::from_history`. It removes all
    // states but the current one from `state_stack`. It also forgets
    // all encountered boards before the last irreversible move.
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
        self.is_repeated = false;

        // Remove all states but the last one from `state_stack`.
        self.state_stack = vec![state];
        self.state_stack.reserve(32);
    }

    // A helper method for `Position::hash`. It returns `true` if the
    // root position can not be reached from the current position,
    // `false` otherwise.
    #[inline(always)]
    fn root_is_unreachable(&self) -> bool {
        self.encountered_boards.len() > self.state().halfmove_clock as usize
    }

    #[inline(always)]
    fn state(&self) -> &StateInfo {
        self.state_stack.last().unwrap()
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
            is_repeated: self.is_repeated,
            repeated_boards_hash: self.repeated_boards_hash,
            encountered_boards: encountered_boards,
            state_stack: state_stack,
            killer_moves: self.killer_moves,
        }
    }
}


// The material value of pieces.
const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];

// Do not try exchanges with SSE==0 once this ply has been reached.
const SSE_EXCHANGE_MAX_PLY: u8 = 2;


// Helper function for `Position::from_history`. It sets all unique
// (non-repeated) values in `slice` to `value`, and returns a sorted
// vector containing a single value for each duplicated value in
// `slice`.
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


// A helper function for `calc_see`. It returns a bitboard describing
// the position on the board of the piece that could attack
// `target_square`, but only when `xrayed_square` becomes
// vacant. Returns `0` if there is no such piece.
#[inline]
fn consider_xrays(geometry: &BoardGeometry,
                  piece_type_array: &[Bitboard; 6],
                  occupied: Bitboard,
                  target_square: Square,
                  xrayed_square: Square)
                  -> Bitboard {
    unsafe {
        let candidates = occupied &
                         *geometry.squares_behind_blocker
                                  .get_unchecked(target_square)
                                  .get_unchecked(xrayed_square);

        // Try the straight sliders first, if not, the diagonal sliders.
        let straight_slider_bb = geometry.piece_attacks_from(candidates, ROOK, target_square) &
                                 candidates &
                                 (piece_type_array[QUEEN] | piece_type_array[ROOK]);
        if straight_slider_bb != 0 {
            straight_slider_bb
        } else {
            geometry.piece_attacks_from(candidates, BISHOP, target_square) & candidates &
            (piece_type_array[QUEEN] | piece_type_array[BISHOP])
        }
    }
}


// A helper function for `calc_see`. It takes a subset of pieces
// `set`, and returns the type of the least valuable piece, and a
// bitboard describing its position on the board.
#[inline]
fn get_least_valuable_piece(piece_type_array: &[Bitboard; 6],
                            set: Bitboard)
                            -> (PieceType, Bitboard) {
    for p in (KING..NO_PIECE).rev() {
        let piece_subset = unsafe { *piece_type_array.get_unchecked(p) } & set;
        if piece_subset != BB_EMPTY_SET {
            return (p, ls1b(piece_subset));
        }
    }
    (NO_PIECE, 0)
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::board::Board;
    use super::PIECE_VALUES;
    use basetypes::*;
    use chess_move::*;

    // This is a very simple evaluation function used for the testing
    // of `qsearch`.
    #[allow(unused_variables)]
    fn simple_eval(board: &Board) -> Value {
        use basetypes::*;
        use position::bitsets::*;
        let piece_type = board.piece_type();
        let color = board.color();
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
                assert_eq!(p.evaluate_move(m), -100);
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
        assert_eq!(v.len(), 5);
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
    fn test_static_exchange_evaluation() {
        let p = Position::from_fen("5r2/8/8/4q1p1/3P4/k3P1P1/P2b1R1B/K4R2 w - - 0 1").ok().unwrap();
        assert_eq!(p.calc_see(BLACK, QUEEN, E5, E3, PAWN), 100);
        assert_eq!(p.calc_see(BLACK, QUEEN, E5, D4, PAWN), -875);
        assert_eq!(p.calc_see(WHITE, PAWN, G3, F4, PAWN), 100);
        assert_eq!(p.calc_see(BLACK, KING, A3, A2, PAWN), -9900);
        assert_eq!(p.calc_see(WHITE, PAWN, D4, D5, NO_PIECE), -100);
        assert_eq!(p.calc_see(WHITE, PAWN, G3, G4, NO_PIECE), 0);
        assert_eq!(p.calc_see(WHITE, ROOK, F1, E1, NO_PIECE), -500);
        assert_eq!(p.calc_see(WHITE, ROOK, F1, D1, NO_PIECE), 0);
        assert!(p.calc_see(WHITE, ROOK, F2, F4, NO_PIECE) <= -400);
        assert_eq!(p.calc_see(WHITE, ROOK, F1, F1, NO_PIECE), 0);
        assert_eq!(p.calc_see(WHITE, PAWN, E3, E3, NO_PIECE), -100);
        let mut v = MoveStack::new();
        p.generate_moves(&mut v);
        while let Some(m) = v.pop() {
            if m.notation() == "f2f4" {
                assert!(p.evaluate_move(m) <= -400);
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
