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


/// Results from a quiescence search.
pub struct QsearchResult<T: Default> {
    /// The calculated evaluation for the analyzed position.
    ///
    /// Should always be between `VALUE_EVAL_MIN` and `VALUE_EVAL_MAX`.
    pub value: Value,

    /// The number of positions that were searched in order to
    /// calculate the evaluation.
    pub searched_nodes: u64,

    /// Auxiliary hints regarding the analyzed position.
    pub hints: T,
}


/// A trait for performing quiescence searches.
pub trait Qsearch: SetOption + Send {
    type MoveGenerator: MoveGenerator;

    type Hints: Default;

    /// TODO
    ///
    /// **Important note:** This should return a reliable result even
    /// when the side to move is in check.
    fn qsearch(params: QsearchParams<Self::MoveGenerator>) -> QsearchResult<Self::Hints>;
}


/// Implements a classical quiescence search routine.
pub struct StandardQsearch<T: MoveGenerator> {
    phantom: PhantomData<T>,
}


impl<T: MoveGenerator> Qsearch for StandardQsearch<T> {
    type MoveGenerator = T;

    type Hints = ();

    fn qsearch(params: QsearchParams<Self::MoveGenerator>) -> QsearchResult<Self::Hints> {
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
        QsearchResult {
            value: value,
            searched_nodes: searched_nodes,
            hints: (),
        }
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
