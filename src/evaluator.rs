//! Defines the `Evaluator` trait.

use uci::SetOption;
use board::Board;
use moves::Move;
use value::*;


/// A trait used to statically evaluate positions.
/// 
/// An evaluation function is used to heuristically determine the
/// relative value of a position, i.e. the chances of winning. If we
/// could see to the end of the game in every line, the evaluation
/// would only have values of "loss", "draw", and "win". In practice,
/// however, we do not know the exact value of a position, so we must
/// make an approximation. Beginning chess players learn to do this
/// starting with the value of the pieces themselves. Computer
/// evaluation functions also use the value of the material as the
/// most significant aspect and then add other considerations.
///
/// Static evaluation is an evaluation that considers only the static
/// material and positional properties of the current position,
/// without analyzing any tactical variations. Therefore, if the
/// position has pending tactical threats, the static evaluation will
/// be grossly incorrect. To implement your own static evaluator, you
/// must define a type that implements the `Evaluator` trait.
pub trait Evaluator: Clone + Send + SetOption {
    /// Creates a new instance and binds it to a given position.
    ///
    /// When a new instance is created, it is bound to a particular
    /// chess position (given by the `board` parameter). And for a
    /// moment, this is the only position that can be correctly
    /// evaluated. The instance then can be re-bound to the next (or
    /// the previous) position in the line of play by issuing calls to
    /// `will_do_move` and `done_move` methods (or respectively,
    /// `will_undo_move` and `undone_move` methods) .
    fn new(board: &Board) -> Self;

    /// Evaluates the the position to which the instance is bound.
    ///
    /// `board` points to the position to which the instance is bound.
    ///
    /// The returned value must be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`.
    fn evaluate(&self, board: &Board) -> Value;

    /// Returns whether the position is zugzwangy.
    ///
    /// In many endgame positions there is a relatively high
    /// probability of zugzwang occurring. For such positions, this
    /// method returns `true`. This is useful when we want to decide
    /// whether it is safe to try a null move.
    fn is_zugzwangy(&self, board: &Board) -> bool;

    /// Updates evaluator's state to keep up with a move that will be
    /// played.
    ///
    /// `board` points to the position to which the instance is bound.
    ///
    /// `m` is a legal move, or (if not in check) a "null move".
    #[inline]
    #[allow(unused_variables)]
    fn will_do_move(&mut self, board: &Board, m: Move) {}

    /// Updates evaluator's state to keep up with a move that was
    /// played.
    ///
    /// `board` points to the new position to which the instance is
    /// bound.
    #[inline]
    #[allow(unused_variables)]
    fn done_move(&mut self, board: &Board, m: Move) {}

    /// Updates evaluator's state to keep up with a move that will be
    /// taken back.
    ///
    /// `board` points to the position to which the instance is bound.
    #[inline]
    #[allow(unused_variables)]
    fn will_undo_move(&mut self, board: &Board, m: Move) {}

    /// Updates evaluator's state in accordance with a move that was
    /// taken back.
    ///
    /// `board` points to the new position to which the instance is
    /// bound.
    #[inline]
    #[allow(unused_variables)]
    fn undone_move(&mut self, board: &Board, m: Move) {}
}