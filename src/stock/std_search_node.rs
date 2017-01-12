//! Implements `StdSearchNode`.

use std::cmp::min;
use std::cell::UnsafeCell;
use std::hash::Hasher;
use std::collections::hash_map::DefaultHasher;
use uci::{SetOption, OptionDescription};
use board::{Board, IllegalBoard};
use value::*;
use qsearch::{Qsearch, QsearchParams, QsearchResult};
use moves::{Move, MoveDigest, AddMove};
use move_generator::MoveGenerator;
use search_node::SearchNode;
use utils::{ZobristArrays, parse_fen};


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


/// Implements the `SearchNode` trait.
pub struct StdSearchNode<T: Qsearch> {
    zobrist: &'static ZobristArrays,
    position: UnsafeCell<T::MoveGenerator>,

    /// Information needed so as to be able to undo the played moves.
    state_stack: Vec<PositionInfo>,

    /// The count of half-moves since the beginning of the game.
    halfmove_count: u16,

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


impl<T: Qsearch> SearchNode for StdSearchNode<T> {
    type Evaluator = <<T as Qsearch>::MoveGenerator as MoveGenerator>::Evaluator;

    type QsearchResult = T::QsearchResult;

    fn from_history(fen: &str, moves: &mut Iterator<Item = &str>) -> Result<Self, IllegalBoard> {
        let mut p: StdSearchNode<T> = try!(StdSearchNode::from_fen(fen));
        let mut move_list = Vec::new();
        'played_moves: for played_move in moves {
            move_list.clear();
            p.position().generate_all(&mut move_list);
            for m in move_list.iter() {
                if played_move == m.notation() {
                    if p.do_move(*m) {
                        continue 'played_moves;
                    }
                    break;
                }
            }
            return Err(IllegalBoard);
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
            if halfmove_clock >= 70 {
                // If `halfmove_clock` is close to rule-50, we blend
                // it into the returned hash.
                hash ^ self.zobrist.halfmove_clock[halfmove_clock as usize]
            } else {
                hash
            }
        }
    }

    #[inline]
    fn board(&self) -> &Board {
        self.position().board()
    }

    #[inline]
    fn halfmove_clock(&self) -> u8 {
        self.state().halfmove_clock
    }

    #[inline]
    fn fullmove_number(&self) -> u16 {
        1 + (self.halfmove_count >> 1)
    }

    #[inline]
    fn is_check(&self) -> bool {
        self.position().checkers() != 0
    }

    #[inline]
    fn evaluator(&self) -> &Self::Evaluator {
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
                           static_eval: Value)
                           -> Self::QsearchResult {
        debug_assert!(lower_bound < upper_bound);
        if self.repeated_or_rule50 {
            Self::QsearchResult::new(0, 0)
        } else {
            T::qsearch(QsearchParams {
                position: unsafe { self.position_mut() },
                depth: 0,
                lower_bound: lower_bound,
                upper_bound: upper_bound,
                static_eval: static_eval,
            })
        }
    }

    #[inline]
    fn evaluate_move(&self, m: Move) -> Value {
        self.position().evaluate_move(m)
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

    #[inline]
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
            self.halfmove_count += 1;
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
    fn undo_last_move(&mut self) {
        debug_assert!(self.state_stack.len() > 1);
        unsafe {
            self.position_mut().undo_move(self.state().last_move);
        }
        self.halfmove_count -= 1;
        self.board_hash = self.encountered_boards.pop().unwrap();
        self.repeated_or_rule50 = false;
        self.state_stack.pop();
    }
}


impl<T: Qsearch> Clone for StdSearchNode<T> {
    fn clone(&self) -> Self {
        StdSearchNode {
            position: UnsafeCell::new(self.position().clone()),
            encountered_boards: self.encountered_boards.clone(),
            state_stack: self.state_stack.clone(),
            ..*self
        }
    }
}


impl<T: Qsearch> SetOption for StdSearchNode<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        T::options()
    }

    fn set_option(name: &str, value: &str) {
        T::set_option(name, value)
    }
}


impl<T: Qsearch> StdSearchNode<T> {
    /// Creates a new instance from Forsyth–Edwards Notation (FEN).
    pub fn from_fen(fen: &str) -> Result<StdSearchNode<T>, IllegalBoard> {
        let (board, halfmove_clock, fullmove_number) = try!(parse_fen(fen));
        let gen = try!(T::MoveGenerator::from_board(board));
        Ok(StdSearchNode {
            zobrist: ZobristArrays::get(),
            halfmove_count: ((fullmove_number - 1) << 1) + gen.board().to_move as u16,
            board_hash: gen.hash(),
            position: UnsafeCell::new(gen),
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
            let mut hasher = DefaultHasher::new();
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
        thread_local!(
            static MOVE_LIST: UnsafeCell<Vec<Move>> = UnsafeCell::new(Vec::new())
        );

        self.is_check() &&
        MOVE_LIST.with(|s| unsafe {
            // Check if there are no legal moves.
            let position = self.position_mut();
            let move_list = &mut *s.get();
            let mut no_legal_moves = true;
            position.generate_all(move_list);
            for m in move_list.iter() {
                if position.do_move(*m).is_some() {
                    position.undo_move(*m);
                    no_legal_moves = false;
                    break;
                }
            }
            move_list.clear();
            no_legal_moves
        })
    }

    #[inline]
    fn state(&self) -> &PositionInfo {
        self.state_stack.last().unwrap()
    }

    #[inline]
    fn position(&self) -> &T::MoveGenerator {
        unsafe { &*self.position.get() }
    }

    #[inline]
    unsafe fn position_mut(&self) -> &mut T::MoveGenerator {
        &mut *self.position.get()
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
    use utils::MoveStack;
    use value::*;
    use search_node::*;
    use evaluator::*;
    use qsearch::*;
    use stock::{StdSearchNode, StdQsearch, StdMoveGenerator, SimpleEvaluator};
    type P = StdSearchNode<StdQsearch<StdMoveGenerator<SimpleEvaluator>>>;

    #[test]
    fn is_legal() {
        assert!(P::from_fen("8/8/8/8/8/8/8/8 w - - 0 1").is_err());
        assert!(P::from_fen("8/8/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(P::from_fen("k7/8/8/8/8/8/8/7K w - - 0 1").is_ok());
        assert!(P::from_fen("k7/8/8/8/8/8/8/6KK w - - 0 1").is_err());
        assert!(P::from_fen("k7/pppppppp/p7/8/8/8/8/7K w - - 0 1").is_err());
        assert!(P::from_fen("k7/8/8/8/8/7P/PPPPPPPP/7K w - - 0 1").is_err());
        assert!(P::from_fen("k7/pppppppp/8/8/8/8/PPPPPPPP/7K w - - 0 1").is_ok());
        assert!(P::from_fen("k7/1P6/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(P::from_fen("k7/1B6/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(P::from_fen("k7/1N6/8/8/8/8/8/7K w - - 0 1").is_ok());
        assert!(P::from_fen("k3P3/8/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(P::from_fen("k3p3/8/8/8/8/8/8/7K w - - 0 1").is_err());
        assert!(P::from_fen("k7/8/8/8/8/8/8/pP5K w - - 0 1").is_err());
        assert!(P::from_fen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1").is_ok());
        assert!(P::from_fen("r3k2r/8/8/8/8/8/8/R3K2B w KQkq - 0 1").is_err());
        assert!(P::from_fen("r3k2r/8/8/8/8/8/8/R3K3 w KQkq - 0 1").is_err());
        assert!(P::from_fen("r3k2r/8/8/8/8/8/8/R3K3 w KQkq - 0 1").is_err());
        assert!(P::from_fen("r3k2r/8/8/8/8/8/8/R3K3 w Qkq - 0 1").is_ok());
        assert!(P::from_fen("r2k3r/8/8/8/8/8/8/R3K3 w Qkq - 0 1").is_err());
        assert!(P::from_fen("r2k3r/8/8/8/8/8/8/R3K3 w Qk - 0 1").is_err());
        assert!(P::from_fen("r2k3r/8/8/8/8/8/8/R3K3 w Q - 0 1").is_ok());
        assert!(P::from_fen("k7/8/8/8/7P/8/8/7K w - h3 0 1").is_err());
        assert!(P::from_fen("k7/8/8/8/7P/8/8/7K b - h3 0 1").is_ok());
        assert!(P::from_fen("k7/8/8/7P/8/8/8/7K b - h4 0 1").is_err());
        assert!(P::from_fen("k7/8/8/8/7P/7P/8/7K b - h3 0 1").is_err());
        assert!(P::from_fen("k7/8/8/8/7P/8/7P/7K b - h3 0 1").is_err());
        assert!(P::from_fen("k7/8/8/8/6P1/7P/8/7K b - h3 0 1").is_err());
        assert!(P::from_fen("8/8/8/6k1/7P/8/8/7K b - h3 0 1").is_ok());
        assert!(P::from_fen("8/8/8/6k1/7P/8/8/6RK b - h3 0 1").is_err());
        assert!(P::from_fen("8/8/8/6k1/3P4/8/8/2B4K b - d3 0 1").is_ok());
        assert!(P::from_fen("8/8/8/6k1/7P/4B3/8/7K b - h3 0 1").is_err());
        assert!(P::from_fen("8/8/8/6k1/7P/8/8/7K b - h3 0 0").is_err());
    }

    #[test]
    fn evaluate_fullmove_number() {
        let mut p = P::from_fen("krq5/p7/8/8/8/8/8/KRQ5 w - - 6 31").ok().unwrap();
        assert_eq!(p.fullmove_number(), 31);
        let m = p.legal_moves()[0];
        p.do_move(m);
        assert_eq!(p.fullmove_number(), 31);
        p.undo_last_move();
        assert_eq!(p.fullmove_number(), 31);

        let mut p = P::from_fen("krq5/p7/8/8/8/8/8/KRQ5 b - - 6 31").ok().unwrap();
        assert_eq!(p.fullmove_number(), 31);
        let m = p.legal_moves()[0];
        p.do_move(m);
        assert_eq!(p.fullmove_number(), 32);
        p.undo_last_move();
        assert_eq!(p.fullmove_number(), 31);
    }

    #[test]
    fn evaluate_static() {
        let p = P::from_fen("krq5/p7/8/8/8/8/8/KRQ5 w - - 0 1").ok().unwrap();
        assert!(p.evaluator().evaluate(p.board()) < -20);
    }

    #[test]
    fn evaluate_move() {
        let mut s = MoveStack::new();

        let p = P::from_fen("8/4P1kP/8/8/8/7p/8/7K w - - 0 1").ok().unwrap();
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
                assert!(p.evaluate_move(m) < -5000);
            }
        }

        let p = P::from_fen("6k1/1P6/8/4b3/8/8/8/1R3K2 w - - 0 1").ok().unwrap();
        p.generate_moves(&mut s);
        while let Some(m) = s.pop() {
            if m.notation() == "b7b8q" {
                assert!(p.evaluate_move(m) > 0);
            }
            if m.notation() == "b7b8k" {
                assert!(p.evaluate_move(m) > 0);
            }
        }

        let p = P::from_fen("5r2/8/8/4q1p1/3P4/k3P1P1/P2b1R1B/K4R2 w - - 0 1").ok().unwrap();
        p.generate_moves(&mut s);
        while let Some(m) = s.pop() {
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

        let p = P::from_fen("5r2/8/8/4q1p1/3P4/k3P1P1/P2b1R1B/K4R2 b - - 0 1").ok().unwrap();
        p.generate_moves(&mut s);
        while let Some(m) = s.pop() {
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

        let p = P::from_fen("8/8/8/8/8/8/2pkpKp1/8 b - - 0 1").ok().unwrap();
        p.generate_moves(&mut s);
        while let Some(m) = s.pop() {
            if m.notation() == "c2c1r" {
                assert_eq!(p.evaluate_move(m), 400);
            }
            if m.notation() == "c2c1n" {
                assert_eq!(p.evaluate_move(m), 225);
            }
            if m.notation() == "e2e1q" {
                assert_eq!(p.evaluate_move(m), 875);
            }
            if m.notation() == "e2e1n" {
                assert_eq!(p.evaluate_move(m), 225);
            }
            if m.notation() == "g2g1q" {
                assert_eq!(p.evaluate_move(m), -100);
            }
            if m.notation() == "g2g1r" {
                assert_eq!(p.evaluate_move(m), -100);
            }
        }
        assert_eq!(p.evaluate_move(p.null_move()), 0);
    }

    #[test]
    fn repeated_root_position() {
        let moves: Vec<&str> = vec!["g4f3", "g1f1", "f3g4", "f1g1", "g4f3", "g1f1", "f3g4", "f1g1"];
        let p = P::from_history("8/8/8/8/6k1/6P1/8/6K1 b - - 0 1", &mut moves.into_iter())
                    .ok()
                    .unwrap();
        let mut v = MoveStack::new();
        p.generate_moves(&mut v);
        assert!(v.list().len() != 0);
    }

    #[test]
    fn set_non_repeated_values() {
        use super::set_non_repeated_values;
        let mut v = vec![0, 1, 2, 7, 9, 0, 0, 1, 2];
        let dups = set_non_repeated_values(&mut v, 0);
        assert_eq!(v, vec![0, 1, 2, 0, 0, 0, 0, 1, 2]);
        assert_eq!(dups, vec![1, 2]);
    }

    #[test]
    fn evaluate_quiescence() {
        let p = P::from_fen("8/8/8/8/8/6qk/7P/7K b - - 0 1").ok().unwrap();
        assert_eq!(p.evaluate_quiescence(-10000, 10000, VALUE_UNKNOWN).searched_nodes(),
                   1);
    }

    #[test]
    fn is_repeated() {
        let mut p = P::from_fen("8/5p1b/5Pp1/6P1/6p1/3p1pPk/3PpP2/4B2K w - - 0 1").ok().unwrap();
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
        let p = P::from_fen("8/8/8/8/8/7K/8/5R1k b - - 0 1").ok().unwrap();
        assert!(p.is_checkmate());

        let p = P::from_fen("8/8/8/8/8/7K/6p1/5R1k b - - 0 1").ok().unwrap();
        assert!(!p.is_checkmate());

        let p = P::from_fen("8/8/8/8/8/7K/8/5N1k b - - 0 1").ok().unwrap();
        assert!(!p.is_checkmate());
    }

    #[test]
    fn repeated_boards_hash() {
        let p1 = P::from_fen("8/8/8/8/8/7k/8/7K w - - 0 1").ok().unwrap();
        let moves: Vec<&str> = vec![];
        let p2 = P::from_history("8/8/8/8/8/7k/8/7K w - - 0 1", &mut moves.into_iter())
                     .ok()
                     .unwrap();
        assert_eq!(p1.board_hash, p2.board_hash);
        assert_eq!(p1.hash(), p2.hash());
        let moves: Vec<&str> = vec!["f1g1", "f3g3", "g1h1", "g3h3"];
        let p2 = P::from_history("8/8/8/8/8/5k2/8/5K2 w - - 0 1", &mut moves.into_iter())
                     .ok()
                     .unwrap();
        assert_eq!(p1.board_hash, p2.board_hash);
        assert_eq!(p1.hash(), p2.hash());
        let moves: Vec<&str> = vec!["f1g1", "f3g3", "g1f1", "g3f3", "f1g1", "f3g3", "g1h1", "g3h3"];
        let p3 = P::from_history("8/8/8/8/8/5k2/8/5K2 w - - 0 1", &mut moves.into_iter())
                     .ok()
                     .unwrap();
        assert_eq!(p1.board_hash, p2.board_hash);
        assert!(p1.hash() != p3.hash());
    }
}
