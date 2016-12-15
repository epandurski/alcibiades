//! Facilities for implementing quiescence searching.
//!
//! Quiescence search is a restricted search which considers only a
//! limited set of moves (for example: winning captures, pawn
//! promotions to queen, check evasions). The goal is to statically
//! evaluate only "quiet" positions (positions where there are no
//! winning tactical moves to be made). Although this search can
//! cheaply and correctly resolve many simple tactical issues, it is
//! completely blind to the more complex ones.

use std::cell::UnsafeCell;
use std::marker::PhantomData;
use uci::{SetOption, OptionDescription};
use chesstypes::*;
use board::{MoveGenerator, BoardEvaluator};
use search::MoveStack;


/// Parameters describing a quiescence search.
pub struct QsearchParams<'a, T: MoveGenerator + 'a> {
    /// A mutable reference to the root position for the search.
    ///
    /// **Important note:** The search routine may use this reference
    /// to do and undo moves, but when the search is finished, all
    /// played moves must be taken back so that the board is restored
    /// to its original state.
    pub position: &'a mut T,

    /// The requested search depth.
    ///
    /// This is the depth at which the main search stops and the
    /// quiescence search takes on. It should be between `DEPTH_MIN`
    /// and `DEPTH_MAX`. Normally, it will be zero or less. The
    /// quiescence search implementation may decide to perform less
    /// thorough analysis when `depth` is smaller.
    pub depth: Depth,

    /// The lower bound for the new search.
    ///
    /// Should be no lesser than `VALUE_MIN`.
    pub lower_bound: Value,

    /// The upper bound for the new search.
    ///
    /// Should be greater than `lower_bound`, but no greater than
    /// `VALUE_MAX`.
    pub upper_bound: Value,

    /// Position's static evaluation, or `VALUE_UNKNOWN`.
    ///
    /// Saves the re-calculation if position's static evaluation is
    /// already available.
    pub static_evaluation: Value,
}


/// A trait for quiescence searches' results.
pub trait QsearchResult {
    /// Creates a new instance.
    ///
    /// * `value` -- the calculated evaluation for the position. Must
    ///   be between `VALUE_EVAL_MIN` and `VALUE_EVAL_MAX`.
    ///
    /// * `searched_nodes` -- the number of positions searched to
    ///   calculate the evaluation.
    fn new(value: Value, searched_nodes: u64) -> Self;

    /// Returns the calculated evaluation for the position.
    ///
    /// Will always be between `VALUE_EVAL_MIN` and `VALUE_EVAL_MAX`.
    fn value(&self) -> Value;

    /// Retruns the number of positions searched to calculate the evaluation.
    fn searched_nodes(&self) -> u64;
}


/// A trait for performing quiescence searches.
pub trait Qsearch: SetOption + Send {
    /// The type of move generator that the implementation works with.
    type MoveGenerator: MoveGenerator;

    /// The type of result object that the search produces.
    type QsearchResult: QsearchResult;

    /// Performs a quiescence search and returns a result object.
    fn qsearch(params: QsearchParams<Self::MoveGenerator>) -> Self::QsearchResult;
}


/// Implements the `QsearchResult` trait.
pub struct StandardQsearchResult {
    value: Value,
    searched_nodes: u64,
}

impl QsearchResult for StandardQsearchResult {
    #[inline(always)]
    fn new(value: Value, searched_nodes: u64) -> Self {
        debug_assert!(VALUE_EVAL_MIN <= value && value <= VALUE_EVAL_MAX);
        StandardQsearchResult {
            value: value,
            searched_nodes: searched_nodes,
        }
    }

    #[inline(always)]
    fn value(&self) -> Value {
        self.value
    }

    #[inline(always)]
    fn searched_nodes(&self) -> u64 {
        self.searched_nodes
    }
}


/// Implements the `Qsearch` trait.
pub struct StandardQsearch<T: MoveGenerator> {
    phantom: PhantomData<T>,
}

impl<T: MoveGenerator> Qsearch for StandardQsearch<T> {
    type MoveGenerator = T;

    type QsearchResult = StandardQsearchResult;

    fn qsearch(params: QsearchParams<Self::MoveGenerator>) -> Self::QsearchResult {
        let mut searched_nodes = 0;
        let value = MOVE_STACK.with(|s| unsafe {
            qsearch(params.position,
                    params.lower_bound,
                    params.upper_bound,
                    params.static_evaluation,
                    0,
                    -params.depth,
                    &mut *s.get(),
                    &mut searched_nodes)
        });
        StandardQsearchResult::new(value, searched_nodes)
    }
}

impl<T: MoveGenerator> SetOption for StandardQsearch<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        T::options()
    }

    fn set_option(name: &str, value: &str) {
        T::set_option(name, value)
    }
}


/// A classical recursive quiescence search implementation.
fn qsearch<T: MoveGenerator>(position: &mut T,
                             mut lower_bound: Value, // alpha
                             upper_bound: Value, // beta
                             mut stand_pat: Value, // position's static evaluation
                             mut recapture_squares: Bitboard,
                             ply: i8, // the reached `qsearch` depth
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
    position.generate_forcing(ply <= 0, move_stack);

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


/// Thread-local storage for the generated moves.
thread_local!(
    static MOVE_STACK: UnsafeCell<MoveStack> = UnsafeCell::new(MoveStack::new())
);


/// The material value of pieces.
const PIECE_VALUES: [Value; 7] = [10000, 975, 500, 325, 325, 100, 0];


/// Exchanges with SEE==0 will not be tried in `qsearch` once this ply
/// has been reached.
const SEE_EXCHANGE_MAX_PLY: i8 = 2;


#[cfg(test)]
mod tests {
    use chesstypes::*;
    use search::MoveStack;
    use board::evaluators::MaterialEval;
    use board::{Board, MoveGenerator, Generator};

    type Pos = Generator<MaterialEval>;

    #[test]
    fn test_qsearch() {
        use super::qsearch;
        let mut s = MoveStack::new();
        let mut p = Pos::from_board(Board::from_fen("8/8/8/8/6k1/6P1/8/6K1 b - - 0 1")
                                        .ok()
                                        .unwrap())
                        .unwrap();
        assert_eq!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0),
                   0);

        let mut p = Pos::from_board(Board::from_fen("8/8/8/8/6k1/6P1/8/5bK1 b - - 0 1")
                                        .ok()
                                        .unwrap())
                        .unwrap();
        assert_eq!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0),
                   225);

        let mut p = Pos::from_board(Board::from_fen("8/8/8/8/5pkp/6P1/5P1P/6K1 b - - 0 1")
                                        .ok()
                                        .unwrap())
                        .unwrap();
        assert_eq!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0),
                   0);

        let mut p = Pos::from_board(Board::from_fen("8/8/8/8/5pkp/6P1/5PKP/8 b - - 0 1")
                                        .ok()
                                        .unwrap())
                        .unwrap();
        assert_eq!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0),
                   -100);

        let mut p = Pos::from_board(Board::from_fen("r1bqkbnr/pppp2pp/2n2p2/4p3/2N1P2B/3P1N2/PPP\
                                                     2PPP/R2QKB1R w - - 5 1")
                                        .ok()
                                        .unwrap())
                        .unwrap();
        assert_eq!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0),
                   0);

        let mut p = Pos::from_board(Board::from_fen("r1bqkbnr/pppp2pp/2n2p2/4N3/4P2B/3P1N2/PPP2P\
                                                     PP/R2QKB1R b - - 5 1")
                                        .ok()
                                        .unwrap())
                        .unwrap();
        assert_eq!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0),
                   -100);

        let mut p = Pos::from_board(Board::from_fen("rn2kbnr/ppppqppp/8/4p3/2N1P1b1/3P1N2/PPP2PP\
                                                     P/R1BKQB1R w - - 5 1")
                                        .ok()
                                        .unwrap())
                        .unwrap();
        assert_eq!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0),
                   0);

        let mut p = Pos::from_board(Board::from_fen("8/8/8/8/8/7k/7q/7K w - - 0 1")
                                        .ok()
                                        .unwrap())
                        .unwrap();
        assert!(qsearch(&mut p, -10000, 10000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0) <= -10000);
    }
}
