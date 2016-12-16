//! Facilities for implementing quiescence searching.
//!
//! Quiescence search is a restricted search which considers only a
//! limited set of moves (for example: winning captures, pawn
//! promotions to queen, check evasions). The goal is to statically
//! evaluate only "quiet" positions (positions where there are no
//! winning tactical moves to be made). Although this search can
//! cheaply and correctly resolve many simple tactical issues, it is
//! completely blind to the more complex ones.
//!
//! `StandardQsearch` implements a classic quiescence search algorithm
//! with stand pat, delta pruning, static exchange evaluation, check
//! evasions, limited checks and recaptures. If you decide to write
//! your own quiescence search routine, you should define your own
//! type that implements the `Qsearch` trait.

use std::mem::uninitialized;
use std::cmp::max;
use std::cell::UnsafeCell;
use std::marker::PhantomData;
use uci::{SetOption, OptionDescription};
use chesstypes::*;
use board::*;
use board::bitsets::*;
use board::tables::BoardGeometry;
use search::{QsearchResult, MoveStack};


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


/// A trait for move generators.
///
/// A `MoveGenerator` holds a chess position, can generate all legal
/// moves, play a selected move and take it back. It provides a
/// position evaluator, and can calculate Zobrist hashes.
///
/// **Important note:** `MoveGenerator` is unaware of repeating
/// positions and the fifty-move rule.
pub trait MoveGenerator: Sized + Send + Clone + SetOption {
    /// The type of static evaluator that the implementation works
    /// with.
    type BoardEvaluator: BoardEvaluator;

    /// Creates a new instance, consuming the supplied `Board`
    /// instance.
    ///
    /// Returns `None` if the position is illegal.
    fn from_board(board: Board) -> Option<Self>;

    /// Returns the Zobrist hash value for the underlying `Board`
    /// instance.
    ///
    /// Zobrist hashing is a technique to transform a board position
    /// into a number of a fixed length, with an equal distribution
    /// over all possible numbers, invented by Albert Zobrist. The key
    /// property of this method is that two similar positions generate
    /// entirely different hash numbers.
    ///
    /// **Important note:** This method calculates the hash value
    /// "from scratch", which can be too slow for some use cases. (See
    /// `do_move`.)
    fn hash(&self) -> u64;

    /// Returns a reference to the underlying `Board` instance.
    #[inline(always)]
    fn board(&self) -> &Board;

    /// Returns a bitboard with all pieces of color `us` that attack
    /// `square`.
    fn attacks_to(&self, us: Color, square: Square) -> Bitboard;

    /// Returns a bitboard with all enemy pieces that attack the king.
    ///
    /// **Important note:** The bitboard of checkers is calculated on
    /// the first call to `checkers`, and is stored in case another
    /// call is made before doing/undoing any moves. In that case
    /// `checkers` returns the saved bitboard instead of
    /// re-calculating it, thus saving time.
    fn checkers(&self) -> Bitboard;

    /// Returns a reference to a static evaluator bound to the current
    /// position.
    fn evaluator(&self) -> &Self::BoardEvaluator;

    /// Generates all legal moves, possibly including some
    /// pseudo-legal moves too.
    ///
    /// The moves are added to `moves`. All generated moves with
    /// pieces other than the king will be legal. Some of the
    /// generated king's moves may be illegal because the destination
    /// square is under attack. This arrangement has two important
    /// advantages:
    ///
    /// * `do_move` can do its work without knowing the set of
    ///   checkers and pinned pieces, so there is no need to keep
    ///   those around.
    ///
    /// * A beta cut-off may make the verification that king's
    ///   destination square is not under attack unnecessary, thus
    ///   saving time.
    ///
    /// **Note:** A pseudo-legal move is a move that is otherwise
    /// legal, except it might leave the king in check.
    fn generate_all<T: AddMove>(&self, moves: &mut T);

    /// Generates moves for the quiescence search.
    ///
    /// The moves are added to `moves`. This method always generates a
    /// **subset** of the moves generated by `generate_all`:
    ///
    /// * If the king is in check, all legal moves are included.
    ///
    /// * Captures and pawn promotions to queen are always included.
    ///
    /// * If `generate_checks` is `true`, moves that give check are
    ///   included too. Discovered checks and checks given by castling
    ///   can be omitted for speed.
    fn generate_forcing<T: AddMove>(&self, generate_checks: bool, moves: &mut T);

    /// Checks if `move_digest` represents a pseudo-legal move.
    ///
    /// If a move `m` exists that would be generated by
    /// `generate_all` if called for the current position on the
    /// board, and for that move `m.digest() == move_digest`, this
    /// method will return `Some(m)`. Otherwise it will return
    /// `None`. This is useful when playing moves from the
    /// transposition table, without calling `generate_all`.
    fn try_move_digest(&self, move_digest: MoveDigest) -> Option<Move>;

    /// Returns a null move.
    ///
    /// "Null move" is a pseudo-move that changes only the side to
    /// move. It is sometimes useful to include a speculative null
    /// move in the search tree so as to achieve more aggressive
    /// pruning. Null moves are represented as king's moves for which
    /// the origin and destination squares are the same.
    fn null_move(&self) -> Move;

    /// Plays a move on the board.
    ///
    /// It verifies if the move is legal. If the move is legal, the
    /// board is updated and an `u64` value is returned, which should
    /// be XOR-ed with old board's hash value to obtain new board's
    /// hash value. If the move is illegal, `None` is returned without
    /// updating the board. The move passed to this method **must**
    /// have been generated by `generate_all`, `generate_forcing`,
    /// `try_move_digest`, or `null_move` methods for the current
    /// position on the board.
    ///
    /// The moves generated by the `null_move` method are
    /// exceptions. For them `do_move` will return `None` if and only
    /// if the king is in check.
    fn do_move(&mut self, m: Move) -> Option<u64>;

    /// Takes back last played move.
    ///
    /// The move passed to this method **must** be the last move passed
    /// to `do_move`.
    fn undo_move(&mut self, m: Move);

    /// Calculates the static exchange evaluation (SEE) value for a
    /// move.
    ///
    /// This method returns the likely evaluation change (material) to
    /// be lost or gained as a result of a given move. It examines the
    /// consequence of a series of exchanges on the destination square
    /// after a given move. The result is calculated without actually
    /// doing any moves on the board.
    fn calc_see(&self, m: Move) -> Value {
        debug_assert!(m.played_piece() < NO_PIECE);
        debug_assert!(m.captured_piece() <= NO_PIECE);
        const PIECE_VALUES: [Value; 7] = [10000, 975, 500, 325, 325, 100, 0];

        let dest_square = m.dest_square();  // the exchange square
        let occupied = self.board().occupied;
        let geometry = BoardGeometry::get();
        let behind_blocker: &[Bitboard; 64] = &geometry.squares_behind_blocker[dest_square];
        let piece_type: &[Bitboard; 6] = &self.board().pieces.piece_type;
        let color: &[Bitboard; 2] = &self.board().pieces.color;

        // These variables will be updated on each capture:
        let mut us = self.board().to_move;
        let mut depth = 0;
        let mut piece = m.played_piece();
        let mut orig_square_bb = 1 << m.orig_square();
        let mut attackers_and_defenders = self.attacks_to(WHITE, dest_square) |
                                          self.attacks_to(BLACK, dest_square);

        // `may_xray` holds the set of pieces that may block attacks
        // from other pieces, and therefore we must consider adding
        // new attackers/defenders every time a piece from the
        // `may_xray` set makes a capture.
        let may_xray = piece_type[PAWN] | piece_type[BISHOP] | piece_type[ROOK] | piece_type[QUEEN];

        // The `gain` array will hold the total material gained at
        // each `depth`, from the viewpoint of the side that made the
        // last capture (`us`).
        let mut gain: [Value; 34] = unsafe { uninitialized() };
        let captured_piece_value = PIECE_VALUES[m.captured_piece()];
        gain[depth] = if m.move_type() == MOVE_PROMOTION {
            // Adding `1` guarantees that SEE will be greater than
            // zero if the promoted pawn is protected.
            captured_piece_value + 1
        } else {
            captured_piece_value
        };

        // Examine the possible exchanges, fill the `gain` array.
        'exchange: while orig_square_bb != 0 {
            // Store a speculative value that will be used if the
            // captured piece happens to be defended.
            gain[depth + 1] = PIECE_VALUES[piece] - gain[depth];

            if max(-gain[depth], gain[depth + 1]) < 0 {
                // The side that made the last capture wins even if
                // the captured piece happens to be defended. This is
                // good enough for our purposes, so we stop here.
                break;
            }

            // Register that `orig_square_bb` is now vacant.
            attackers_and_defenders &= !orig_square_bb;

            // Consider adding new attackers/defenders, now that
            // `orig_square_bb` is vacant.
            if orig_square_bb & may_xray != 0 {
                attackers_and_defenders |= {
                    let candidates = occupied & behind_blocker[bitscan_forward(orig_square_bb)];
                    let bb = geometry.attacks_from(ROOK, dest_square, candidates) & candidates &
                             (piece_type[QUEEN] | piece_type[ROOK]);
                    if bb != 0 {
                        // a straight slider
                        bb
                    } else {
                        // a diagonal slider
                        geometry.attacks_from(BISHOP, dest_square, candidates) & candidates &
                        (piece_type[QUEEN] | piece_type[BISHOP])
                    }
                };
            }

            // Change the side to move.
            us ^= 1;

            // Find the next piece to enter the exchange. (The least
            // valuable piece belonging to the side to move.)
            let candidates = attackers_and_defenders & color[us];
            for p in (KING..NO_PIECE).rev() {
                let bb = candidates & piece_type[p];
                if bb != 0 {
                    depth += 1;
                    piece = p;
                    orig_square_bb = ls1b(bb);
                    continue 'exchange;
                }
            }
            break 'exchange;
        }

        // Negamax the `gain` array for the final static exchange
        // evaluation. (The `gain` array actually represents an unary
        // tree, at each node of which the player can either continue
        // the exchange or back off.)
        unsafe {
            while depth > 0 {
                *gain.get_unchecked_mut(depth - 1) = -max(-*gain.get_unchecked(depth - 1),
                                                          *gain.get_unchecked(depth));
                depth -= 1;
            }
        }
        gain[0]
    }
}


#[cfg(test)]
mod tests {
    use chesstypes::*;
    use search::MoveStack;
    use search::quiescence::MoveGenerator;
    use move_generation::Generator;
    use board::evaluators::MaterialEval;
    use board::Board;

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
