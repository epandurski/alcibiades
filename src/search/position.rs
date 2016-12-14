//! Implements `Position`.

use std::cmp::min;
use std::cell::UnsafeCell;
use std::hash::{Hasher, SipHasher};
use uci::{SetOption, OptionDescription};
use chesstypes::*;
use board::*;
use board::notation::*;
use board::tables::*;
use search::{SearchNode, MoveStack};


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


/// A generic implementation of `SearchNode` trait.
///
/// `Position` delegates some of the work to the underlying
/// `MoveGenerator` instance, adding the following functionality:
///
/// 1. Smart position hashing.
/// 2. Exact evaluation of final positions.
/// 3. Static exchange evaluation.
/// 4. Quiescence search.
/// 5. 50 move rule awareness.
/// 6. Threefold/twofold repetition detection.
pub struct Position<T: MoveGenerator> {
    zobrist: &'static ZobristArrays,
    position: UnsafeCell<T>,

    /// Information needed so as to be able to undo the played moves.
    state_stack: Vec<PositionInfo>,

    /// `true` if the position is deemed as a draw by repetition or
    /// because 50 moves have been played without capturing a piece or
    /// advancing a pawn.
    repeated_or_rule50: bool,

    /// The hash value for the underlying `Board` instance.
    board_hash: u64,

    /// A list of hash values for the `Board` instances that had
    /// occurred during the game. This is needed so as to be able to
    /// detect repeated positions.
    encountered_boards: Vec<u64>,

    /// A collective hash value for the set of boards that had
    /// occurred at least twice before the root position (the earliest
    /// position in `state_stack`), and can still be reached by
    /// playing moves from the root position. An empty set has a hash
    /// of `0`. We use this value when we generate position's hash.
    repeated_boards_hash: u64,
}


impl<T: MoveGenerator + 'static> SearchNode for Position<T> {
    type BoardEvaluator = T::BoardEvaluator;

    fn from_history(fen: &str,
                    moves: &mut Iterator<Item = &str>)
                    -> Result<Position<T>, NotationError> {
        let mut p: Position<T> = try!(Position::from_fen(fen));
        let mut move_stack = MoveStack::new();
        'played_moves: for played_move in moves {
            move_stack.clear();
            p.position().generate_all(&mut move_stack);
            for m in move_stack.iter() {
                if played_move == m.notation() {
                    if p.do_move(*m) {
                        continue 'played_moves;
                    }
                    break;
                }
            }
            return Err(NotationError);
        }
        p.declare_as_root();
        Ok(p)
    }

    #[inline]
    fn hash(&self) -> u64 {
        // Notes:
        //
        // 1. Two positions that differ in their sets of previously
        //    repeated, still reachable boards will have different
        //    hashes.
        //
        // 2. Two positions that differ only in their number of played
        //    moves without capturing piece or advancing a pawn will
        //    have equal hashes, as long as they both are far from the
        //    rule-50 limit.

        if self.repeated_or_rule50 {
            // All repeated and rule-50 positions are a draw, so for
            // practical purposes they can be considered to be the
            // exact same position, and therefore we can generate the
            // same hash value for all of them. This has the important
            // practical advantage that we get two separate records in
            // the transposition table for the first and the second
            // occurrence of the same position. (The second occurrence
            // being deemed as a draw.)
            1
        } else {
            let hash = if self.root_is_reachable() {
                // If the repeated positions that occured before the
                // root postition are still reachable, we blend their
                // collective hash into current position's hash.
                self.board_hash ^ self.repeated_boards_hash
            } else {
                self.board_hash
            };
            let halfmove_clock = self.state().halfmove_clock;
            if halfmove_clock >= HALFMOVE_CLOCK_THRESHOLD {
                // If `halfmove_clock` is close to rule-50, we blend
                // it into the returned hash.
                hash ^ self.zobrist.halfmove_clock[halfmove_clock as usize]
            } else {
                hash
            }
        }
    }

    #[inline(always)]
    fn board(&self) -> &Board {
        self.position().board()
    }

    #[inline(always)]
    fn halfmove_clock(&self) -> u8 {
        self.state().halfmove_clock
    }

    #[inline(always)]
    fn is_check(&self) -> bool {
        self.position().checkers() != 0
    }

    #[inline(always)]
    fn evaluator(&self) -> &Self::BoardEvaluator {
        self.position().evaluator()
    }

    #[inline]
    fn evaluate_final(&self) -> Value {
        if self.repeated_or_rule50 || !self.is_check() {
            0
        } else {
            VALUE_MIN
        }
    }

    #[inline]
    fn evaluate_quiescence(&self,
                           lower_bound: Value,
                           upper_bound: Value,
                           static_evaluation: Value)
                           -> (Value, u64) {
        debug_assert!(lower_bound < upper_bound);
        if self.repeated_or_rule50 {
            (0, 0)
        } else {
            let mut searched_nodes = 0;
            let value = MOVE_STACK.with(|s| unsafe {
                qsearch(self.position_mut(),
                        lower_bound,
                        upper_bound,
                        static_evaluation,
                        0,
                        0,
                        &mut *s.get(),
                        &mut searched_nodes)
            });
            (value, searched_nodes)
        }
    }

    #[inline(always)]
    fn evaluate_move(&self, m: Move) -> Value {
        self.position().calc_see(m)
    }

    #[inline]
    fn generate_moves<U: AddMove>(&self, moves: &mut U) {
        if !self.repeated_or_rule50 {
            self.position().generate_all(moves);
        }
    }

    #[inline]
    fn try_move_digest(&self, move_digest: MoveDigest) -> Option<Move> {
        if self.repeated_or_rule50 {
            None
        } else {
            self.position().try_move_digest(move_digest)
        }
    }

    fn legal_moves(&self) -> Vec<Move> {
        let mut legal_moves = Vec::with_capacity(96);
        MOVE_STACK.with(|s| unsafe {
            let position = self.position_mut();
            let move_stack = &mut *s.get();
            move_stack.save();
            self.generate_moves(move_stack);
            for m in move_stack.iter() {
                if position.do_move(*m).is_some() {
                    legal_moves.push(*m);
                    position.undo_move(*m);
                }
            }
            move_stack.restore();
        });
        legal_moves
    }

    #[inline(always)]
    fn null_move(&self) -> Move {
        self.position().null_move()
    }

    fn do_move(&mut self, m: Move) -> bool {
        if self.repeated_or_rule50 && m.is_null() {
            // This is a final position -- null moves are not
            // allowed. We must still allow other moves though,
            // because `from_history` should be able to call `do_move`
            // even in final positions.
            return false;
        }

        if let Some(h) = unsafe { self.position_mut().do_move(m) } {
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
            self.encountered_boards.push(self.board_hash);
            self.board_hash ^= h;
            debug_assert!(halfmove_clock <= 99);
            debug_assert!(self.encountered_boards.len() >= halfmove_clock as usize);

            // Figure out if the new position is repeated (a draw).
            if halfmove_clock >= 4 {
                let boards = &self.encountered_boards;
                let last_irrev = (boards.len() - (halfmove_clock as usize)) as isize;
                unsafe {
                    let mut i = (boards.len() - 4) as isize;
                    while i >= last_irrev {
                        if self.board_hash == *boards.get_unchecked(i as usize) {
                            self.repeated_or_rule50 = true;
                            break;
                        }
                        i -= 2;
                    }
                }
            }

            self.state_stack.push(PositionInfo {
                halfmove_clock: halfmove_clock,
                last_move: m,
            });
            return true;
        }

        false
    }

    #[inline]
    fn undo_move(&mut self) {
        debug_assert!(self.state_stack.len() > 1);
        unsafe {
            self.position_mut().undo_move(self.state().last_move);
        }
        self.board_hash = self.encountered_boards.pop().unwrap();
        self.repeated_or_rule50 = false;
        self.state_stack.pop();
    }
}


impl<T: MoveGenerator + 'static> Clone for Position<T> {
    fn clone(&self) -> Self {
        Position {
            zobrist: self.zobrist,
            position: UnsafeCell::new(self.position().clone()),
            board_hash: self.board_hash,
            repeated_or_rule50: self.repeated_or_rule50,
            repeated_boards_hash: self.repeated_boards_hash,
            encountered_boards: self.encountered_boards.clone(),
            state_stack: self.state_stack.clone(),
        }
    }
}


impl<T: MoveGenerator + 'static> SetOption for Position<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        T::options()
    }

    fn set_option(name: &str, value: &str) {
        T::set_option(name, value)
    }
}


impl<T: MoveGenerator + 'static> Position<T> {
    /// Creates a new instance from a Forsyth–Edwards Notation (FEN)
    /// string.
    pub fn from_fen(fen: &str) -> Result<Position<T>, NotationError> {
        let (board, halfmove_clock, _) = try!(parse_fen(fen));
        let g = try!(T::from_board(board).ok_or(NotationError));
        Ok(Position {
            zobrist: ZobristArrays::get(),
            board_hash: g.hash(),
            position: UnsafeCell::new(g),
            repeated_or_rule50: false,
            repeated_boards_hash: 0,
            encountered_boards: vec![0; halfmove_clock as usize],
            state_stack: vec![PositionInfo {
                                  halfmove_clock: min(halfmove_clock, 99),
                                  last_move: Move::invalid(),
                              }],
        })
    }

    /// Forgets the previous playing history, preserves only the set
    /// of previously repeated, still reachable boards.
    fn declare_as_root(&mut self) {
        let state = *self.state();

        // The root position is never deemed as a draw due to
        // repetition or rule-50.
        self.repeated_or_rule50 = false;

        // Calculate the set of previously repeated, still reachable boards.
        let repeated_boards = {
            // Forget all encountered boards before the last irreversible move.
            let last_irrev = self.encountered_boards.len() - state.halfmove_clock as usize;
            self.encountered_boards = self.encountered_boards.split_off(last_irrev);
            self.encountered_boards.reserve(32);

            // Forget all encountered boards that occurred only once.
            set_non_repeated_values(&mut self.encountered_boards, 0)
        };

        // Calculate a collective hash value representing the set of
        // previously repeated, still reachable boards. (We will use
        // this value when calculating position's hash.)
        self.repeated_boards_hash = if repeated_boards.is_empty() {
            0
        } else {
            let mut hasher = SipHasher::new();
            for x in repeated_boards {
                hasher.write_u64(x);
            }
            hasher.finish()
        };

        // Forget all played moves.
        self.state_stack = vec![state];
        self.state_stack.reserve(32);
    }

    /// Returns if the root position (the earliest in `state_stack`)
    /// can be reached by playing moves from the current position.
    #[inline(always)]
    fn root_is_reachable(&self) -> bool {
        self.encountered_boards.len() <= self.state().halfmove_clock as usize
    }

    /// Returns if the side to move is checkmated.
    fn is_checkmate(&self) -> bool {
        self.is_check() &&
        MOVE_STACK.with(|s| unsafe {
            // Check if there are no legal moves.
            let position = self.position_mut();
            let move_stack = &mut *s.get();
            let mut no_legal_moves = true;
            move_stack.save();
            position.generate_all(move_stack);
            for m in move_stack.iter() {
                if position.do_move(*m).is_some() {
                    position.undo_move(*m);
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
    fn position(&self) -> &T {
        unsafe { &*self.position.get() }
    }

    #[inline(always)]
    unsafe fn position_mut(&self) -> &mut T {
        &mut *self.position.get()
    }
}


/// Thread-local storage for the generated moves.
thread_local!(
    static MOVE_STACK: UnsafeCell<MoveStack> = UnsafeCell::new(MoveStack::new())
);


/// The material value of pieces.
const PIECE_VALUES: [Value; 7] = [10000, 975, 500, 325, 325, 100, 0];


/// Exchanges with SEE==0 will not be tried in `qsearch` once this ply
/// has been reached.
const SEE_EXCHANGE_MAX_PLY: u8 = 2;


/// `halfmove_clock` will not be blended into position's hash until it
/// gets greater or equal to this number.
const HALFMOVE_CLOCK_THRESHOLD: u8 = 70;


/// Performs a "quiescence search" and returns an evaluation.
///
/// The "quiescence search" is a restricted search which considers
/// only a limited set of moves (for example: winning captures,
/// pawn promotions to queen, check evasions). The goal is to
/// statically evaluate only "quiet" positions (positions where
/// there are no winning tactical moves to be made).
fn qsearch<T: MoveGenerator>(position: &mut T,
                             mut lower_bound: Value, // alpha
                             upper_bound: Value, // beta
                             mut stand_pat: Value, // position's static evaluation
                             mut recapture_squares: Bitboard,
                             ply: u8, // the reached `qsearch` depth
                             move_stack: &mut MoveStack,
                             searched_nodes: &mut u64)
                             -> Value {
    debug_assert!(lower_bound < upper_bound);
    debug_assert!(stand_pat == VALUE_UNKNOWN ||
                  stand_pat == position.evaluator().evaluate(position.board()));
    let in_check = position.checkers() != 0;

    // At the beginning of quiescence, position's static
    // evaluation (`stand_pat`) is used to establish a lower bound
    // on the result. We assume that even if none of the forcing
    // moves can improve over the stand pat, there will be at
    // least one "quiet" move that will at least preserve the
    // stand pat value. (Note that this assumption is not true if
    // the the side to move is in check, because in this case all
    // possible check evasions will be tried.)
    if in_check {
        // Position's static evaluation is useless when in check.
        stand_pat = lower_bound;
    } else if stand_pat == VALUE_UNKNOWN {
        stand_pat = position.evaluator().evaluate(position.board());
    }
    if stand_pat >= upper_bound {
        return stand_pat;
    }
    if stand_pat > lower_bound {
        lower_bound = stand_pat;
    }
    let obligatory_material_gain = (lower_bound as isize) - (stand_pat as isize) -
                                   (PIECE_VALUES[KNIGHT] - 4 * PIECE_VALUES[PAWN] / 3) as isize;

    // Generate all forcing moves. (Include checks only during the
    // first ply.)
    move_stack.save();
    position.generate_forcing(ply == 0, move_stack);

    // Consider the generated moves one by one. See if any of them
    // can raise the lower bound.
    'trymoves: while let Some(m) = move_stack.remove_best() {
        let move_type = m.move_type();
        let dest_square_bb = 1 << m.dest_square();
        let captured_piece = m.captured_piece();

        // Decide whether to try the move. Check evasions,
        // en-passant captures (for them SEE is often wrong), and
        // mandatory recaptures are always tried. (In order to
        // correct SEE errors due to pinned and overloaded pieces,
        // at least one mandatory recapture is always tried at the
        // destination squares of previous moves.) For all other
        // moves, a static exchange evaluation is performed to
        // decide if the move should be tried.
        if !in_check && move_type != MOVE_ENPASSANT && recapture_squares & dest_square_bb == 0 {
            match position.calc_see(m) {
                // A losing move -- do not try it.
                x if x < 0 => continue 'trymoves,

                // An even exchange -- try it only during the first few plys.
                0 if ply >= SEE_EXCHANGE_MAX_PLY && captured_piece < NO_PIECE => continue 'trymoves,

                // A safe or winning move -- try it always.
                _ => (),
            }
        }

        // Try the move.
        if position.do_move(m).is_some() {
            // If the move does not give check, ensure that
            // the immediate material gain from the move is
            // big enough.
            if position.checkers() == 0 {
                let material_gain = if move_type == MOVE_PROMOTION {
                    PIECE_VALUES[captured_piece] +
                    PIECE_VALUES[Move::piece_from_aux_data(m.aux_data())] -
                    PIECE_VALUES[PAWN]
                } else {
                    PIECE_VALUES[captured_piece]
                };
                if (material_gain as isize) < obligatory_material_gain {
                    position.undo_move(m);
                    continue 'trymoves;
                }
            }

            // Recursively call `qsearch`.
            *searched_nodes += 1;
            let value = -qsearch(position,
                                 -upper_bound,
                                 -lower_bound,
                                 VALUE_UNKNOWN,
                                 recapture_squares ^ dest_square_bb,
                                 ply + 1,
                                 move_stack,
                                 searched_nodes);
            position.undo_move(m);

            // Update the lower bound.
            if value >= upper_bound {
                lower_bound = value;
                break 'trymoves;
            }
            if value > lower_bound {
                lower_bound = value;
            }

            // Mark that a recapture at this square has been tried.
            recapture_squares &= !dest_square_bb;
        }
    }
    move_stack.restore();

    // Return the determined lower bound. (We should make sure
    // that the returned value is between `VALUE_EVAL_MIN` and
    // `VALUE_EVAL_MAX`, regardless of the initial bounds passed
    // to `qsearch`. If we do not take this precautions, the
    // search algorithm will abstain from checkmating the
    // opponent, seeking the huge material gain that `qsearch`
    // promised.)
    match lower_bound {
        x if x < VALUE_EVAL_MIN => VALUE_EVAL_MIN,
        x if x > VALUE_EVAL_MAX => VALUE_EVAL_MAX,
        x => x,
    }
}


/// A helper function. It sets all unique (non-repeated) values in
/// `slice` to `value`, and returns a sorted vector containing a
/// single value for each duplicated value in `slice`.
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


#[cfg(test)]
mod tests {
    use super::*;
    use chesstypes::*;
    use search::{SearchNode, MoveStack};
    use board::evaluators::MaterialEval;
    use board::{BoardEvaluator, Generator};

    #[test]
    fn test_fen_parsing() {
        assert!(Position::<Generator<MaterialEval>>::from_fen("nbqkbnr/pppppppp/8/8\
                                                                            /4P3/8/PPPP1PPP/RNBQ\
                                                                            KBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr1/pppppppp/8\
                                                                            /8/4P3/8/PPPP1PPP/RN\
                                                                            BQKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPPP1PPP/RNB\
                                                                            QKBN b KQkq e3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPPP1PPP/RNB\
                                                                            QKBNR/ b KQkq e3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPPP1PPP/RNB\
                                                                            QKBNRR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPP01PPP/RNB\
                                                                            QKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPP91PPP/RNB\
                                                                            QKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPP*1PPP/RNB\
                                                                            QKBNR b KQkq e3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPPP1PPP/RNB\
                                                                            QKBNR b KQkq e3 * 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPPP1PPP/RNB\
                                                                            QKBNR b KQkq e3 0 *")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPPP1PPP/RNB\
                                                                            QKBNR b - e3 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("rnbqkbnr/pppppppp/8/\
                                                                            8/4P3/8/PPPP1PPP/RNB\
                                                                            QKBNR b KQkq e3 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/8/8/8/8 w - \
                                                                            - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/8/8/8/7K w \
                                                                            - - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/8/8/8/8/7K w \
                                                                            - - 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/8/8/8/8/6KK \
                                                                            w - - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/pppppppp/p7/8/8/8\
                                                                            /8/7K w - - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/8/8/7P/PPPPPP\
                                                                            PP/7K w - - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/pppppppp/8/8/8/8/\
                                                                            PPPPPPPP/7K w - - 0 \
                                                                            1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/1P6/8/8/8/8/8/7K \
                                                                            w - - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/1B6/8/8/8/8/8/7K \
                                                                            w - - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/1N6/8/8/8/8/8/7K \
                                                                            w - - 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k3P3/8/8/8/8/8/8/7K \
                                                                            w - - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k3p3/8/8/8/8/8/8/7K \
                                                                            w - - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/8/8/8/8/pP5K \
                                                                            w - - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("r3k2r/8/8/8/8/8/8/R3\
                                                                            K2R w KQkq - 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("r3k2r/8/8/8/8/8/8/R3\
                                                                            K2B w KQkq - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("r3k2r/8/8/8/8/8/8/R3\
                                                                            K3 w KQkq - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("r3k2r/8/8/8/8/8/8/R3\
                                                                            K3 w KQkq - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("r3k2r/8/8/8/8/8/8/R3\
                                                                            K3 w Qkq - 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("r2k3r/8/8/8/8/8/8/R3\
                                                                            K3 w Qkq - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("r2k3r/8/8/8/8/8/8/R3\
                                                                            K3 w Qk - 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("r2k3r/8/8/8/8/8/8/R3\
                                                                            K3 w Q - 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/8/7P/8/8/7K \
                                                                            w - h3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/8/7P/8/8/7K \
                                                                            b - h3 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/7P/8/8/8/7K \
                                                                            b - h4 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/8/7P/7P/8/7K \
                                                                            b - h3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/8/7P/8/7P/7K \
                                                                            b - h3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("k7/8/8/8/6P1/7P/8/7K \
                                                                            b - h3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("8/8/8/6k1/7P/8/8/7K \
                                                                            b - h3 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("8/8/8/6k1/7P/8/8/6RK \
                                                                            b - h3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("8/8/8/6k1/3P4/8/8/2B\
                                                                            4K b - d3 0 1")
                    .is_ok());
        assert!(Position::<Generator<MaterialEval>>::from_fen("8/8/8/6k1/7P/4B3/8/7\
                                                                            K b - h3 0 1")
                    .is_err());
        assert!(Position::<Generator<MaterialEval>>::from_fen("8/8/8/6k1/7P/8/8/7K \
                                                                            b - h3 0 0")
                    .is_err());
    }

    #[test]
    fn test_evaluate_static() {
        let p = Position::<Generator<MaterialEval>>::from_fen("krq5/p7/8/8/8/8/8/\
                                                                            KRQ5 w - - 0 1")
                    .ok()
                    .unwrap();
        assert!(p.evaluator().evaluate(p.board()) < -20);
    }

    #[test]
    fn test_evaluate_move() {
        let p = Position::<Generator<MaterialEval>>::from_fen("8/4P1kP/8/8/8/7p/8/7K \
                                                                            w - - 0 1")
                    .ok()
                    .unwrap();
        let mut s = MoveStack::new();
        p.generate_moves(&mut s);
        while let Some(m) = s.pop() {
            if m.notation() == "e7e8q" {
                assert!(p.evaluate_move(m) > 0);
            }
            if m.notation() == "e7e8r" {
                assert!(p.evaluate_move(m) > 0);
            }
            if m.notation() == "h7h8r" {
                assert!(p.evaluate_move(m) < 0);
            }
            if m.notation() == "h1h2" {
                assert_eq!(p.evaluate_move(m), 0);
            }
            if m.notation() == "h1g2" {
                assert_eq!(p.evaluate_move(m), -10000);
            }
        }
        assert_eq!(p.evaluate_move(p.null_move()), 0);
        let p = Position::<Generator<MaterialEval>>::from_fen("6k1/1P6/8/4b3/8/8/8/1\
                                                                            R3K2 w - - 0 1")
                    .ok()
                    .unwrap();
        p.generate_moves(&mut s);
        while let Some(m) = s.pop() {
            if m.notation() == "b7b8q" {
                assert!(p.evaluate_move(m) > 0);
            }
            if m.notation() == "b7b8k" {
                assert!(p.evaluate_move(m) > 0);
            }
        }
    }

    #[test]
    fn test_qsearch() {
        use super::qsearch;
        unsafe {
            let mut s = MoveStack::new();
            let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/6k1/6P1/8/6K1 b - - 0 \
                                                                   1")
                        .ok()
                        .unwrap();
            assert_eq!(qsearch(p.position_mut(),
                               -1000,
                               1000,
                               VALUE_UNKNOWN,
                               0,
                               0,
                               &mut s,
                               &mut 0),
                       0);

            let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/6k1/6P1/8/5bK1 b - - \
                                                                   0 1")
                        .ok()
                        .unwrap();
            assert_eq!(qsearch(p.position_mut(),
                               -1000,
                               1000,
                               VALUE_UNKNOWN,
                               0,
                               0,
                               &mut s,
                               &mut 0),
                       225);

            let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/5pkp/6P1/5P1P/6K1 b - \
                                                                   - 0 1")
                        .ok()
                        .unwrap();
            assert_eq!(qsearch(p.position_mut(),
                               -1000,
                               1000,
                               VALUE_UNKNOWN,
                               0,
                               0,
                               &mut s,
                               &mut 0),
                       0);

            let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/5pkp/6P1/5PKP/8 b - - \
                                                                   0 1")
                        .ok()
                        .unwrap();
            assert_eq!(qsearch(p.position_mut(),
                               -1000,
                               1000,
                               VALUE_UNKNOWN,
                               0,
                               0,
                               &mut s,
                               &mut 0),
                       -100);

            let p = Position::<Generator<MaterialEval>>::from_fen("r1bqkbnr/pppp2pp/2n2p2/4p3/2N1\
                                                                   P2B/3P1N2/PPP2PPP/R2QKB1R w - \
                                                                   - 5 1")
                        .ok()
                        .unwrap();
            assert_eq!(qsearch(p.position_mut(),
                               -1000,
                               1000,
                               VALUE_UNKNOWN,
                               0,
                               0,
                               &mut s,
                               &mut 0),
                       0);

            let p = Position::<Generator<MaterialEval>>::from_fen("r1bqkbnr/pppp2pp/2n2p2/4N3/4P2\
                                                                   B/3P1N2/PPP2PPP/R2QKB1R b - - \
                                                                   5 1")
                        .ok()
                        .unwrap();
            assert_eq!(qsearch(p.position_mut(),
                               -1000,
                               1000,
                               VALUE_UNKNOWN,
                               0,
                               0,
                               &mut s,
                               &mut 0),
                       -100);

            let p = Position::<Generator<MaterialEval>>::from_fen("rn2kbnr/ppppqppp/8/4p3/2N1P1b1\
                                                                   /3P1N2/PPP2PPP/R1BKQB1R w - - \
                                                                   5 1")
                        .ok()
                        .unwrap();
            assert_eq!(qsearch(p.position_mut(),
                               -1000,
                               1000,
                               VALUE_UNKNOWN,
                               0,
                               0,
                               &mut s,
                               &mut 0),
                       0);

            let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/8/7k/7q/7K w - - 0 1")
                        .ok()
                        .unwrap();
            assert!(qsearch(p.position_mut(),
                            -10000,
                            10000,
                            VALUE_UNKNOWN,
                            0,
                            0,
                            &mut s,
                            &mut 0) <= -10000);

            let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/8/6qk/7P/7K b - - 0 1")
                        .ok()
                        .unwrap();
            assert_eq!(p.evaluate_quiescence(-10000, 10000, VALUE_UNKNOWN).1, 1);
        }
    }

    #[test]
    fn test_create_repeated() {
        let moves: Vec<&str> = vec!["g4f3", "g1f1", "f3g4", "f1g1", "g4f3", "g1f1", "f3g4"];
        let p = Position::<Generator<MaterialEval>>::from_history("8/8/8/8/6k1/6P1/8/6K1 \
                                                                           b - - 0 1",
                                                                  &mut moves.into_iter())
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
        let mut p = Position::<Generator<MaterialEval>>::from_fen("8/5p1b/5Pp1/6P1/6\
                                                                                p1/3p1pPk/3PpP2/4\
                                                                                B2K w - - 0 1")
                        .ok()
                        .unwrap();
        let mut v = MoveStack::new();
        let mut count = 0;
        for _ in 0..100 {
            p.generate_moves(&mut v);
            while let Some(m) = v.pop() {
                if p.do_move(m) {
                    count += 1;
                    v.clear_all();
                    break;
                }
            }
        }
        assert_eq!(count, 4);
    }

    #[test]
    fn is_checkmate() {
        let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/8/7K/8/5R1k \
                                                                            b - - 0 1")
                    .ok()
                    .unwrap();
        assert!(p.is_checkmate());

        let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/8/7K/6p1/5R1k \
                                                                            b - - 0 1")
                    .ok()
                    .unwrap();
        assert!(!p.is_checkmate());

        let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/8/7K/8/5N1k \
                                                                            b - - 0 1")
                    .ok()
                    .unwrap();
        assert!(!p.is_checkmate());
    }

    #[test]
    fn test_static_exchange_evaluation() {
        let mut v = MoveStack::new();

        let p = Position::<Generator<MaterialEval>>::from_fen("5r2/8/8/4q1p1/3P4/k3P\
                                                                            1P1/P2b1R1B/K4R2 w - \
                                                                            - 0 1")
                    .ok()
                    .unwrap();
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

        let p = Position::<Generator<MaterialEval>>::from_fen("5r2/8/8/4q1p1/3P4/k3P\
                                                                            1P1/P2b1R1B/K4R2 b - \
                                                                            - 0 1")
                    .ok()
                    .unwrap();
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

        let p = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/8/8/2pkpKp1/8 b - - 0 1")
                    .ok()
                    .unwrap();
        p.generate_moves(&mut v);
        while let Some(m) = v.pop() {
            if m.notation() == "c2c1q" {
                assert!(p.evaluate_move(m) > 0);
            }
            if m.notation() == "c2c1n" {
                assert!(p.evaluate_move(m) > 0);
            }
            if m.notation() == "e2e1q" {
                assert!(p.evaluate_move(m) > 0);
            }
            if m.notation() == "e2e1n" {
                assert!(p.evaluate_move(m) > 0);
            }
            if m.notation() == "g2g1q" {
                assert!(p.evaluate_move(m) < 0);
            }
            if m.notation() == "g2g1r" {
                assert!(p.evaluate_move(m) < 0);
            }
        }
    }

    #[test]
    fn test_repeated_boards_hash() {
        let p1 = Position::<Generator<MaterialEval>>::from_fen("8/8/8/8/8/7k/8/7K w \
                                                                             - - 0 1")
                     .ok()
                     .unwrap();
        let moves: Vec<&str> = vec![];
        let p2 = Position::<Generator<MaterialEval>>::from_history("8/8/8/8/8/7k/8/7K w \
                                                                            - - 0 1",
                                                                   &mut moves.into_iter())
                     .ok()
                     .unwrap();
        assert_eq!(p1.board_hash, p2.board_hash);
        assert_eq!(p1.hash(), p2.hash());
        let moves: Vec<&str> = vec!["f1g1", "f3g3", "g1h1", "g3h3"];
        let p2 = Position::<Generator<MaterialEval>>::from_history("8/8/8/8/8/5k2/8/5K2 \
                                                                            w - - 0 1",
                                                                   &mut moves.into_iter())
                     .ok()
                     .unwrap();
        assert_eq!(p1.board_hash, p2.board_hash);
        assert_eq!(p1.hash(), p2.hash());
        let moves: Vec<&str> = vec!["f1g1", "f3g3", "g1f1", "g3f3", "f1g1", "f3g3", "g1h1", "g3h3"];
        let p3 = Position::<Generator<MaterialEval>>::from_history("8/8/8/8/8/5k2/8/5K2 \
                                                                            w - - 0 1",
                                                                   &mut moves.into_iter())
                     .ok()
                     .unwrap();
        assert_eq!(p1.board_hash, p2.board_hash);
        assert!(p1.hash() != p3.hash());
    }
}
