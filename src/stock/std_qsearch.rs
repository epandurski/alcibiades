//! Implements `StdQsearch` and `StdQsearchResult`.

use std::cell::UnsafeCell;
use std::marker::PhantomData;
use uci::{SetOption, OptionDescription};
use board::*;
use value::*;
use depth::*;
use moves::*;
use evaluator::Evaluator;
use qsearch::{Qsearch, QsearchParams, QsearchResult};
use move_generator::MoveGenerator;
use utils::MoveStack;


/// Implements the `QsearchResult` trait.
#[derive(Clone, Debug)]
pub struct StdQsearchResult {
    value: Value,
    searched_nodes: u64,
}

impl QsearchResult for StdQsearchResult {
    #[inline]
    fn new(value: Value, searched_nodes: u64) -> Self {
        debug_assert!(VALUE_EVAL_MIN <= value && value <= VALUE_EVAL_MAX);
        StdQsearchResult {
            value: value,
            searched_nodes: searched_nodes,
        }
    }

    #[inline]
    fn value(&self) -> Value {
        self.value
    }

    #[inline]
    fn searched_nodes(&self) -> u64 {
        self.searched_nodes
    }
}


/// Implements the `Qsearch` trait.
/// 
/// Performs classical quiescence search with stand pat, delta
/// pruning, static exchange evaluation, check evasions, limited
/// checks and recaptures.
pub struct StdQsearch<T: MoveGenerator> {
    phantom: PhantomData<T>,
}

impl<T: MoveGenerator> Qsearch for StdQsearch<T> {
    type MoveGenerator = T;

    type QsearchResult = StdQsearchResult;

    fn qsearch(params: QsearchParams<Self::MoveGenerator>) -> Self::QsearchResult {
        debug_assert!(DEPTH_MIN <= params.depth && params.depth <= 0);
        debug_assert!(params.lower_bound >= VALUE_MIN);
        debug_assert!(params.upper_bound <= VALUE_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        thread_local!(
            static MOVE_STACK: UnsafeCell<MoveStack> = UnsafeCell::new(MoveStack::new())
        );
        let mut searched_nodes = 0;
        let value = MOVE_STACK.with(|s| unsafe {
            qsearch(params.position,
                    params.lower_bound,
                    params.upper_bound,
                    params.static_eval,
                    0,
                    (-params.depth / DEPTH_ONE_PLY) as i8,
                    &mut *s.get(),
                    &mut searched_nodes)
        });
        StdQsearchResult::new(value, searched_nodes)
    }
}

impl<T: MoveGenerator> SetOption for StdQsearch<T> {
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
    const PIECE_VALUES: [Value; 8] = [10000, 975, 500, 325, 325, 100, 0, 0];

    let in_check = position.checkers() != 0;

    // At the beginning of quiescence, position's static evaluation
    // (`stand_pat`) is used to establish a lower bound on the
    // result. We assume that even if none of the forcing moves can
    // improve on the stand pat, there will be at least one "quiet"
    // move that will at least preserve the stand pat value. (Note
    // that this assumption is not true if the the side to move is in
    // check, because in this case all possible check evasions will be
    // tried.)
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
    'trymoves: while let Some(m) = move_stack.pull_best() {
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
            match position.evaluate_move(m) {
                // A losing move -- do not try it.
                x if x < 0 => continue 'trymoves,

                // An even exchange -- try it only during the first few plys.
                0 if ply >= 2 && captured_piece < PIECE_NONE => continue 'trymoves,

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
                    unsafe { *PIECE_VALUES.get_unchecked(captured_piece) }
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


#[cfg(test)]
mod tests {
    use board::*;
    use value::*;
    use move_generator::*;
    use stock::{SimpleEvaluator, StdMoveGenerator};
    use utils::MoveStack;
    type P = StdMoveGenerator<SimpleEvaluator>;

    #[test]
    fn qsearch() {
        use super::qsearch;
        let mut s = MoveStack::new();
        let d = 32;

        let fen = "8/8/8/8/6k1/6P1/8/6K1 b - - 0 1";
        let board = Board::from_fen(fen).ok().unwrap();
        let mut p = P::from_board(board).ok().unwrap();
        assert!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0).abs() <= d);

        let fen = "8/8/8/8/6k1/6P1/8/5bK1 b - - 0 1";
        let board = Board::from_fen(fen).ok().unwrap();
        let mut p = P::from_board(board).ok().unwrap();
        assert!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0) > 225 - d);

        let fen = "8/8/8/8/5pkp/6P1/5P1P/6K1 b - - 0 1";
        let board = Board::from_fen(fen).ok().unwrap();
        let mut p = P::from_board(board).ok().unwrap();
        assert!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0).abs() <= d);

        let fen = "8/8/8/8/5pkp/6P1/5PKP/8 b - - 0 1";
        let board = Board::from_fen(fen).ok().unwrap();
        let mut p = P::from_board(board).ok().unwrap();
        assert!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0) <= -100 + d);

        let fen = "r1bqkbnr/pppp2pp/2n2p2/4p3/2N1P2B/3P1N2/PPP2PPP/R2QKB1R w - - 5 1";
        let board = Board::from_fen(fen).ok().unwrap();
        let mut p = P::from_board(board).ok().unwrap();
        assert!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0).abs() <= d);

        let fen = "r1bqkbnr/pppp2pp/2n2p2/4N3/4P2B/3P1N2/PPP2PPP/R2QKB1R b - - 5 1";
        let board = Board::from_fen(fen).ok().unwrap();
        let mut p = P::from_board(board).ok().unwrap();
        assert!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0) <= -100 + d);

        let fen = "rn2kbnr/ppppqppp/8/4p3/2N1P1b1/3P1N2/PPP2PPP/R1BKQB1R w - - 5 1";
        let board = Board::from_fen(fen).ok().unwrap();
        let mut p = P::from_board(board).ok().unwrap();
        assert!(qsearch(&mut p, -1000, 1000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0).abs() <= d);

        let fen = "8/8/8/8/8/7k/7q/7K w - - 0 1";
        let board = Board::from_fen(fen).ok().unwrap();
        let mut p = P::from_board(board).ok().unwrap();
        assert!(qsearch(&mut p, -10000, 10000, VALUE_UNKNOWN, 0, 0, &mut s, &mut 0) <= -10000);
    }
}
