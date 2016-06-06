//! Implements the rules of chess and static position evaluation
//! logic.

#![allow(dead_code)]
#![allow(unused_variables)]

pub mod board_geometry;
pub mod board;

// use notation;
use std::cell::RefCell;
use basetypes::*;
use bitsets::*;
use chess_move::*;
use self::board::{Board, IllegalBoard};


const MOVE_STACK_CAPACITY: usize = 4096;

const PIECE_VALUES: [Value; 7] = [10000, 975, 500, 325, 325, 100, 0];

pub type Value = i16;


/// Represents an illegal possiton error.
pub struct IllegalPosition;


/// Represents a chess position.
///
/// `Position` can generate all possible moves in the current
/// position, play a selected move, and take it back. It can also
/// statically (without doing extensive tree-searching) evaluate the
/// chances of the sides, so that tree-searching algorithms can use
/// this evaluation to assign realistic game outcomes to their leaf
/// nodes. `Position` can also fabricate a "null move" that can be
/// used to aggressively prune the search tree.
pub struct Position {
    board: RefCell<Board>,
    halfmove_clock: u32,
    fullmove_number: u32,
    // encountered_boards: Vec<u64>,
    // state_stack: Vec<StateInfo>,
    
    /* move_stack
     * move_history (including fullmove_number?)
     * ply
     * hply?
     * various hash tables
     * first_move_index[usize; MAX_PLY]
     * undo_move data stack */
}


impl Position {
    fn from_fen(fen: &str) -> Result<Position, IllegalBoard> {
        let parts: Vec<_> = fen.split_whitespace().collect();

        if parts.len() == 6 {
            let p = Position {
                board: RefCell::new(try!(Board::from_fen(fen))),
                halfmove_clock: try!(parts[4].parse::<u32>().map_err(|_| IllegalBoard)),
                fullmove_number: try!(parts[5].parse::<u32>().map_err(|_| IllegalBoard)),
            };
            Ok(p)
        } else {
            Err(IllegalBoard)
        }
    }


    /// Creates a new instance.
    ///
    /// `fen` should be the Forsyth–Edwards Notation of a legal
    /// starting position. `moves` should be an iterator over all the
    /// moves that were played from that position.
    pub fn from_history(fen: &str,
                        moves: &mut Iterator<Item = &str>)
                        -> Result<Position, IllegalPosition> {
        Err(IllegalPosition)
    }


    /// Evaluates a final position.
    ///
    /// In final positions this method is guaranteed to return the
    /// correct value of the position (white wins, black wins,
    /// draw). A position is guaranteed to be final if
    /// `generate_moves` method generates no legal moves (it may
    /// generate some pseudo-legal moves, but if none of them is
    /// legal, then the position is guaranteed to be a final
    /// position).
    pub fn evaluate_final(&self) -> Value {
        0
    }


    /// Statically evaluates the position.
    ///
    /// This method considers only static material and positional
    /// properties of the position. If the position is dynamic, with
    /// pending tactical threats, this function will return a grossly
    /// incorrect evaluation. Therefore, if should be relied upon only
    /// for reasonably "quiet" positions.
    /// 
    /// `lower_bound` and `upper_bound` together give the interval
    /// within which an as precise as possible evaluation is
    /// required. If during the calculation it is determined that the
    /// evaluation is outside this interval, this method may return
    /// any value outside of the interval (including the bounds), but
    /// always staying on the correct side of the interval.
    pub fn evaluate_static(&self, lower_bound: Value, upper_bound: Value) -> Value {
        // TODO: Implement a real evaluation.

        let board = self.board.borrow();
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
    /// The purpose of the "quiescence search" is to only evaluate
    /// "quiet" positions (positions where there are no winning
    /// tactical moves to be made).
    /// 
    /// `lower_bound` and `upper_bound` together give the interval
    /// within which an as precise as possible evaluation is
    /// required. If during the calculation it is determined that the
    /// evaluation is outside this interval, this method may return
    /// any value outside of the interval (including the bounds), but
    /// always staying on the correct side of the interval.
    ///
    /// TODO: Add more details for the algorithm used.
    pub fn evaluate(&self, lower_bound: Value, upper_bound: Value) -> Value {
        thread_local!(
            static MOVE_STACK: RefCell<Vec<Move>> = RefCell::new(
                Vec::with_capacity(MOVE_STACK_CAPACITY))
        );
        MOVE_STACK.with(|x| {
            let mut move_stack = x.borrow_mut();
            self.qsearch(lower_bound,
                         upper_bound,
                         &mut move_stack,
                         &Position::evaluate_static)
        })
    }


    /// Returns an almost unique hash value for the position.
    #[inline(always)]
    pub fn hash(&self) -> u64 {
        0
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
    pub fn do_move(&self, m: Move) -> bool {
        false
    }


    /// Takes back the last played move.
    #[inline]
    pub fn undo_move(&self) {}


    /// Generates pseudo-legal moves and pushes them to `move_sink`.
    ///
    /// If all of the moves generated by this methods are illegal
    /// (this means that `do_move(m)` returns `false` for all of
    /// them), then it is guaranteed that the position is final, and
    /// `evaluate_final()` will return its correct value.
    #[inline]
    pub fn generate_moves(&self, move_sink: &mut MoveSink) {}


    /// Returns a null move.
    ///
    /// "Null move" is an illegal pseudo-move that changes only the
    /// side to move. For the move generated by this method,
    /// `do_move(m)` will return `false` if and only if the king is in
    /// check.
    #[inline]
    pub fn null_move(&self) -> Move {
        use castling_rights::CastlingRights;
        Move::new(WHITE,
                  0,
                  MOVE_NORMAL,
                  KING,
                  0,
                  0,
                  NO_PIECE,
                  8, // no en-passant file
                  CastlingRights::new(),
                  0)
    }

    #[inline]
    fn qsearch(&self,
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
        self.board.borrow().generate_moves(false, move_stack);

        // Try all generated moves one by one. Moves with higher
        // scores are tried before moves with lower scores.
        let pruning_threshold = lower_bound - stand_pat - 2 * PIECE_VALUES[PAWN];
        let mut i = length_at_start;
        while i < move_stack.len() {
            unsafe {
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
                    PIECE_VALUES[Move::piece_from_aux_data(next_move.aux_data())] -
                    PIECE_VALUES[PAWN]
                } else {
                    0
                };
                if material_gain < pruning_threshold {
                    continue;
                }

                // Recursively call `qsearch` for the next move.
                if !self.board.borrow_mut().do_move(next_move) {
                    continue;  // illegal move
                }
                let value = -self.qsearch(-upper_bound, -lower_bound, move_stack, eval_func);
                self.board.borrow_mut().undo_move(next_move);

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
        }

        // Restore the move stack to its original length and return.
        move_stack.truncate(length_at_start);
        lower_bound
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::PIECE_VALUES;

    // This is a very simple evaluation function used for the testing
    // of `qsearch`.
    fn simple_eval(p: &Position, lower_bound: Value, upper_bound: Value) -> Value {
        use basetypes::*;
        use bitsets::*;
        let board = p.board.borrow();
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
        assert_eq!(p.evaluate(-1000, 1000), 0);

        let p = Position::from_fen("8/8/8/8/6k1/6P1/8/5bK1 b - - 0 1").ok().unwrap();
        assert_eq!(p.evaluate(-1000, 1000), 225);

        let p = Position::from_fen("8/8/8/8/5pkp/6P1/5P1P/6K1 b - - 0 1").ok().unwrap();
        assert_eq!(p.evaluate(-1000, 1000), 0);

        let p = Position::from_fen("8/8/8/8/5pkp/6P1/5PKP/8 b - - 0 1").ok().unwrap();
        assert_eq!(p.evaluate(-1000, 1000), -100);
    }
}
