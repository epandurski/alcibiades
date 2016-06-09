//! Implements the rules of chess and position evaluation logic.

pub mod board_geometry;
pub mod board;

use std::cell::UnsafeCell;
use basetypes::*;
use bitsets::*;
use chess_move::*;
use notation;
use self::board::Board;


#[derive(Clone, Copy)]
struct StateInfo {
    halfmove_clock: u16,
    last_move: Move,
}


/// Evaluation value in centipawns.
///
/// Positive values mean that the position is favorable for the side
/// to move. Negative values mean the position is favorable for the
/// other side (not to move). A value of `0` means that the chances
/// are equal. For example: a value of `100` might mean that the side
/// to move is a pawn ahead.
///
/// Values over `20000` and under `-20000` designate a certain
/// win/loss.
pub type Value = i16;


/// Represents an illegal possiton error.
pub struct IllegalPosition;


/// Represents a chess position.
///
/// `Position` is intended as a convenient interface for the
/// tree-searching algorithm. It encapsulates most of the
/// chess-specific knowledge like the chess rules, values of pieces,
/// king safety, pawn structure etc. `Position` can be instantiated
/// from a FEN string, can generate the all possible moves (plus a
/// "null move") in the current position, play a selected move and
/// take it back. It can also approximately (without doing extensive
/// tree-searching) evaluate the chances of the sides, so that
/// tree-searching algorithms can use this evaluation to assign
/// realistic game outcomes to their leaf nodes.
pub struct Position {
    board: UnsafeCell<Board>,
    halfmove_count: UnsafeCell<u16>,
    state_stack: UnsafeCell<Vec<StateInfo>>,
    encountered_boards: UnsafeCell<Vec<u64>>,
}


impl Position {
    /// Creates a new instance.
    ///
    /// `fen` should be the Forsyth–Edwards Notation of a legal
    /// starting position.
    pub fn from_fen(fen: &str) -> Result<Position, IllegalPosition> {
        let (ref placement, to_move, castling, en_passant_square, halfmove_clock, fullmove_number) =
            try!(notation::parse_fen(fen).map_err(|_| IllegalPosition));
        Ok(Position {
            board: UnsafeCell::new(try!(Board::create(placement,
                                                      to_move,
                                                      castling,
                                                      en_passant_square)
                                            .map_err(|_| IllegalPosition))),
            halfmove_count: UnsafeCell::new(((fullmove_number - 1) << 1) + to_move as u16),
            encountered_boards: UnsafeCell::new(vec![0; halfmove_clock as usize]),
            state_stack: UnsafeCell::new(vec![StateInfo {
                                                  halfmove_clock: halfmove_clock,
                                                  last_move: Move::from_u32(0),
                                              }]),
        })
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
        let mut move_stack = Vec::with_capacity(128);
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

    /// Returns the count of half-moves since the beginning of the
    /// game.
    ///
    /// At the beginning of the game it starts at `0`, and is
    /// incremented after anyone's move.
    #[inline(always)]
    pub fn halfmove_count(&self) -> u16 {
        unsafe { *self.halfmove_count.get() }
    }

    /// Evaluates a final position.
    ///
    /// In final positions this method is guaranteed to return the
    /// correct value of the position (white wins, black wins, a
    /// draw). A position is guaranteed to be final if
    /// `generate_moves` method generates no legal moves. (It may
    /// generate some pseudo-legal moves, but if none of them is
    /// legal, then the position is final.)
    ///
    /// **Important note:** Repeating positions are considered final
    /// (a draw) after the first repetition, not after the second one
    /// as the official chess rules prescribe. This is done in the
    /// sake of efficiency. In order to compensate for that
    /// `Position::from_history` "forgets" all positions that have
    /// occurred exactly once.
    #[inline]
    pub fn evaluate_final(&self) -> Value {
        if self.is_repeated() || self.board().checkers() == 0 {
            0
        } else {
            -20000
        }
    }

    /// Statically evaluates the position.
    ///
    /// This method considers only static material and positional
    /// properties of the position. If the position is dynamic, with
    /// pending tactical threats, this function will return a grossly
    /// incorrect evaluation. Therefore, it should be relied upon only
    /// for reasonably "quiet" positions.
    /// 
    /// `lower_bound` and `upper_bound` together give the interval
    /// within which an as precise as possible evaluation is
    /// required. If during the calculation it is determined that the
    /// evaluation is outside this interval, this method may return
    /// any value outside of the interval (including the bounds), but
    /// always staying on the correct side of the interval.
    #[allow(unused_variables)]
    #[inline]
    pub fn evaluate_static(&self, lower_bound: Value, upper_bound: Value) -> Value {
        assert!(lower_bound <= upper_bound);
        // TODO: Implement a real evaluation.

        let board = self.board();
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

    /// Performs "quiescence search" and returns an evaluation.
    ///
    /// The goal of the "quiescence search" is to statically evaluate
    /// only "quiet" positions (positions where there are no winning
    /// tactical moves to be made). In order to do that, without
    /// analyzing too much nodes, it considers only captures, pawn
    /// promotions, and check evasions. Even then, the search tree can
    /// get quite large quickly. So, static exchange evaluation
    /// heuristics is used to eliminate those captures that are likely
    /// to lead to a material loss. Although "quiescence search" can
    /// cheaply and correctly resolve many tactical issues, it is
    /// particularly blind to other simple tactical threads like all
    /// kinds of forks, checks, and even a checkmate in one move.
    /// 
    /// `lower_bound` and `upper_bound` together give the interval
    /// within which an as precise as possible evaluation is
    /// required. If during the calculation it is determined that the
    /// evaluation is outside this interval, this method may return
    /// any value outside of the interval (including the bounds), but
    /// always staying on the correct side of the interval.
    #[inline]
    pub fn evaluate_quiescence(&self, lower_bound: Value, upper_bound: Value) -> Value {
        assert!(lower_bound <= upper_bound);
        thread_local!(
            static MOVE_STACK: UnsafeCell<Vec<Move>> = UnsafeCell::new(
                Vec::with_capacity(MOVE_STACK_CAPACITY))
        );
        MOVE_STACK.with(|x| {
            unsafe {
                self.qsearch(lower_bound,
                             upper_bound,
                             &mut *x.get(),
                             &Position::evaluate_static)
            }
        })
    }

    /// Returns an almost unique hash value for the position.
    #[inline(always)]
    pub fn hash(&self) -> u64 {
        self.board().hash()
    }

    /// Plays a move on the board.
    ///
    /// It verifies if the move is legal. If the move is legal, the
    /// board is updated and `true` is returned. If the move is
    /// illegal, `false` is returned without updating the board. The
    /// move passed to this method **must** have been generated by
    /// `generate_moves` or `null_move` methods for the current
    /// position on the board.
    ///
    /// Moves generated by the `null_move` method are exceptions. For
    /// them `do_move(m)` will return `false` if and only if the king
    /// is in check.
    #[inline]
    pub fn do_move(&mut self, m: Move) -> bool {
        unsafe { self.do_move_unsafe(m) }
    }

    /// Takes back the last played move.
    #[inline]
    pub fn undo_move(&mut self) {
        unsafe { self.undo_move_unsafe() }
    }

    /// Generates pseudo-legal moves.
    ///
    /// The generated moves will be pushed to `move_sink`. If all of
    /// the moves generated by this methods are illegal (this means
    /// that `do_move(m)` returns `false` for all of them), then the
    /// position is final, and `evaluate_final()` will return its
    /// correct value.
    ///
    /// **Important note:** Repeating positions are considered final
    /// (and therefore, this method generates no moves) after the
    /// first repetition, not after the second one as the official
    /// chess rules prescribe. This is done in the sake of efficiency.
    /// In order to compensate for that `Position::from_history`
    /// "forgets" all positions that have occurred exactly once.
    #[inline]
    pub fn generate_moves(&self, move_sink: &mut MoveSink) {
        if !self.is_repeated() {
            self.board().generate_moves(true, move_sink);
        }
    }

    /// Returns a null move.
    ///
    /// "Null move" is an illegal pseudo-move that changes only the
    /// side to move. It is sometimes useful to include a speculative
    /// null move in the search tree so as to achieve more aggressive
    /// pruning. For the move generated by this method, `do_move(m)`
    /// will return `false` if and only if the king is in check.
    #[inline]
    pub fn null_move(&self) -> Move {
        self.board().null_move()
    }

    // Returns the current move number.
    //
    // At the beginning of the game it starts at `1`, and is
    // incremented after black's move.
    #[inline]
    fn fullmove_number(&self) -> u16 {
        1 + (self.halfmove_count() >> 1)
    }

    // Returns `true` if the current position is a repetition of a
    // previously encountered position, `false` otherwise.
    #[inline]
    fn is_repeated(&self) -> bool {
        let halfmove_clock = self.state().halfmove_clock as usize;
        assert!(self.encountered_boards().len() >= halfmove_clock);
        if halfmove_clock >= 4 {
            let boards = self.encountered_boards();
            let last_irrev = (boards.len() - halfmove_clock) as isize;
            let mut i = (boards.len() - 4) as isize;
            while i >= last_irrev {
                if self.board().hash() == unsafe { *boards.get_unchecked(i as usize) } {
                    return true;
                }
                i -= 2;
            }
        }
        false
    }

    // A helper method for `do_move` and `qsearch`. It is needed
    // because`qsearch` plays moves and undoes them without having a
    // mutable reference to `self`.
    #[inline]
    unsafe fn do_move_unsafe(&self, m: Move) -> bool {
        let board = self.board_mut();
        let old_board_hash = board.hash();
        board.do_move(m) &&
        {
            let new_halfmove_clock = if m.is_pawn_advance_or_capure() {
                0
            } else {
                self.state().halfmove_clock + 1
            };
            *self.halfmove_count_mut() += 1;
            self.encountered_boards_mut().push(old_board_hash);
            self.state_stack_mut().push(StateInfo {
                halfmove_clock: new_halfmove_clock,
                last_move: m,
            });
            true
        }
    }

    // A helper method for `do_move` and `qsearch`. It is needed
    // because`qsearch` plays moves and undoes them without having a
    // mutable reference to `self`.
    #[inline]
    unsafe fn undo_move_unsafe(&self) {
        assert!(self.state_stack_mut().len() > 1);
        self.board_mut().undo_move(self.state().last_move);
        *self.halfmove_count_mut() -= 1;
        self.encountered_boards_mut().pop();
        self.state_stack_mut().pop();
    }

    // A helper method for `evaluate`. It is needed because`qsearch`
    // should be able to call itself recursively, which should not
    // complicate `evaluate`'s public-facing interface.
    #[inline]
    unsafe fn qsearch(&self,
                      mut lower_bound: Value,
                      upper_bound: Value,
                      move_stack: &mut Vec<Move>,
                      eval_func: &Fn(&Position, Value, Value) -> Value)
                      -> Value {
        assert!(lower_bound <= upper_bound);
        let stand_pat = eval_func(self, lower_bound, upper_bound);

        // At the beginning of quiescence, the position's evaluation
        // is used to establish a lower bound on the score
        // (`stand_pat`). We assume that even if none of the capturing
        // moves can improve over the stand pat, there will be at
        // least one "quiet" move that will at least preserve the
        // stand pat value.
        if stand_pat >= upper_bound {
            return stand_pat;
        }
        if stand_pat > lower_bound {
            lower_bound = stand_pat;
        }

        // Generate all non-quiet moves.
        let length_at_start = move_stack.len();
        self.board().generate_moves(false, move_stack);

        // Try all generated moves one by one. Moves with higher
        // scores are tried before moves with lower scores.
        let pruning_threshold = lower_bound - stand_pat - 2 * PIECE_VALUES[PAWN];
        let mut i = length_at_start;
        while i < move_stack.len() {
            // Find the move with the best score among the
            // remaining moves, so as to try that move next.
            let mut next_move = *move_stack.get_unchecked(i);
            let mut j = i;
            while j < move_stack.len() {
                if *move_stack.get_unchecked(j) > next_move {
                    *move_stack.get_unchecked_mut(i) = *move_stack.get_unchecked_mut(j);
                    *move_stack.get_unchecked_mut(j) = next_move;
                    next_move = *move_stack.get_unchecked(i);
                }
                j += 1;
            }
            i += 1;

            // Check if the material gain from this move is big
            // enough to warrant trying the move.
            let material_gain = PIECE_VALUES[next_move.captured_piece()] +
                                if next_move.move_type() == MOVE_PROMOTION {
                PIECE_VALUES[Move::piece_from_aux_data(next_move.aux_data())] - PIECE_VALUES[PAWN]
            } else {
                0
            };
            if material_gain < pruning_threshold {
                continue;
            }

            // Recursively call `qsearch` for the next move.
            if !self.do_move_unsafe(next_move) {
                continue;  // illegal move
            }
            let value = -self.qsearch(-upper_bound, -lower_bound, move_stack, eval_func);
            self.undo_move_unsafe();

            // Update the lower bound according to the recursively
            // calculated value.
            if value >= upper_bound {
                lower_bound = value;
                break;
            }
            if value > lower_bound {
                lower_bound = value;
            }
        }

        // Restore the move stack to its original length and return.
        move_stack.truncate(length_at_start);
        lower_bound
    }

    // A helper method for `Position::from_history`. It removes all
    // states but the current one from `state_stack`. It also forgets
    // all encountered boards before the last irreversible one.
    fn declare_as_root(&mut self) {
        let state = *self.state();
        let last_irrev = self.encountered_boards().len() - state.halfmove_clock as usize;
        unsafe {
            *self.state_stack_mut() = vec![state];
            *self.encountered_boards_mut() = self.encountered_boards_mut().split_off(last_irrev);
            self.state_stack_mut().reserve(32);
            self.encountered_boards_mut().reserve(32);

            // Because we assign a draw score on the first repetition
            // of the same position, we have to remove all positions
            // that occurred only once from `self.encountered_boards`.
            set_non_repeating_values(self.encountered_boards_mut(), 0);
        }
    }

    #[inline(always)]
    fn board(&self) -> &Board {
        unsafe { &*self.board.get() }
    }

    #[inline(always)]
    fn state(&self) -> &StateInfo {
        unsafe { (&*self.state_stack.get()).last().unwrap() }
    }

    #[inline(always)]
    fn encountered_boards(&self) -> &Vec<u64> {
        unsafe { &*self.encountered_boards.get() }
    }

    #[inline(always)]
    unsafe fn board_mut(&self) -> &mut Board {
        &mut *self.board.get()
    }

    #[inline(always)]
    unsafe fn halfmove_count_mut(&self) -> &mut u16 {
        &mut *self.halfmove_count.get()
    }

    #[inline(always)]
    unsafe fn state_stack_mut(&self) -> &mut Vec<StateInfo> {
        &mut *self.state_stack.get()
    }

    #[inline(always)]
    unsafe fn encountered_boards_mut(&self) -> &mut Vec<u64> {
        &mut *self.encountered_boards.get()
    }
}


// This should be big enough to contain all the moves generated for
// the quiescence search. In this case it is 32 plys * 128 moves.
const MOVE_STACK_CAPACITY: usize = 4096;

// Teh material value of pieces.
const PIECE_VALUES: [Value; 7] = [10000, 975, 500, 325, 325, 100, 0];


// Helper function for `Posittion::from_history`. It sets all unique
// (non-repeating) values in `slice` to `value`.
fn set_non_repeating_values<T>(slice: &mut [T], value: T)
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
    for x in slice.iter_mut() {
        if repeated.binary_search(x).is_err() {
            *x = value;
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::PIECE_VALUES;

    // This is a very simple evaluation function used for the testing
    // of `qsearch`.
    #[allow(dead_code)]
    #[allow(unused_variables)]
    fn simple_eval(p: &Position, lower_bound: Value, upper_bound: Value) -> Value {
        use basetypes::*;
        use bitsets::*;
        let board = p.board();
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
        assert_eq!(Position::from_fen("k7/8/8/8/8/8/8/7K w - - 0 11")
                       .ok()
                       .unwrap()
                       .fullmove_number(),
                   11);
        assert_eq!(Position::from_fen("k7/8/8/8/8/8/8/7K b - - 0 11")
                       .ok()
                       .unwrap()
                       .fullmove_number(),
                   11);
    }

    #[test]
    fn test_evaluate_static_parsing() {
        assert_eq!(Position::from_fen("krq5/p7/8/8/8/8/8/KRQ5 w - - 0 1")
                       .ok()
                       .unwrap()
                       .evaluate_static(-1000, 1000),
                   -100);
    }

    #[test]
    fn test_qsearch() {
        let p = Position::from_fen("8/8/8/8/6k1/6P1/8/6K1 b - - 0 1").ok().unwrap();
        assert_eq!(p.evaluate_quiescence(-1000, 1000), 0);

        let p = Position::from_fen("8/8/8/8/6k1/6P1/8/5bK1 b - - 0 1").ok().unwrap();
        assert_eq!(p.evaluate_quiescence(-1000, 1000), 225);

        let p = Position::from_fen("8/8/8/8/5pkp/6P1/5P1P/6K1 b - - 0 1").ok().unwrap();
        assert_eq!(p.evaluate_quiescence(-1000, 1000), 0);

        let p = Position::from_fen("8/8/8/8/5pkp/6P1/5PKP/8 b - - 0 1").ok().unwrap();
        assert_eq!(p.evaluate_quiescence(-1000, 1000), -100);
    }

    #[test]
    fn test_from_history_and_do_move() {
        let moves: Vec<&str> = vec!["g4f3", "g1f1", "f3g4", "f1g1"];
        let p = Position::from_history("8/8/8/8/6k1/6P1/8/6K1 b - - 0 1", &mut moves.into_iter())
                    .ok()
                    .unwrap();
        let mut v = Vec::new();
        p.generate_moves(&mut v);
        assert_eq!(v.len(), 8);

        let moves: Vec<&str> = vec!["g4f3", "g1f1", "f3g4", "f1g1", "g4f3", "g1f1", "f3g4", "f1g1"];
        let p = Position::from_history("8/8/8/8/6k1/6P1/8/6K1 b - - 0 1", &mut moves.into_iter())
                    .ok()
                    .unwrap();
        let mut v = Vec::new();
        p.generate_moves(&mut v);
        assert_eq!(v.len(), 0);
        assert_eq!(p.evaluate_final(), 0);

        let moves: Vec<&str> = vec!["g4f3", "g1f1", "f3g4", "f1g1", "g4f3", "g1f1", "f3g4"];
        let p = Position::from_history("8/8/8/8/6k1/6P1/8/6K1 b - - 0 1", &mut moves.into_iter())
                    .ok()
                    .unwrap();
        let mut v = Vec::new();
        p.generate_moves(&mut v);
        assert_eq!(v.len(), 5);
    }

    #[test]
    fn test_set_non_repeating_values() {
        use super::set_non_repeating_values;
        let mut v = vec![0, 1, 2, 7, 9, 0, 0, 1, 2];
        set_non_repeating_values(&mut v, 0);
        assert_eq!(v, vec![0, 1, 2, 0, 0, 0, 0, 1, 2]);
    }
}
