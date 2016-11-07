//! Implements single-threaded alpha-beta searching.

use std::mem;
use std::cmp::max;
use basetypes::*;
use moves::*;
use tt::*;
use search::{SearchNode, MAX_DEPTH};


/// Represents a terminated search condition.
pub struct TerminatedSearch;


/// Represents a game tree search.        
pub struct Search<'a> {
    tt: &'a Tt,
    killers: KillerTable,
    position: Box<SearchNode>,
    moves: &'a mut MoveStack,
    moves_starting_ply: usize,
    state_stack: Vec<NodeState>,
    reported_nodes: NodeCount,
    unreported_nodes: NodeCount,
    report_function: &'a mut FnMut(NodeCount) -> bool,
}


impl<'a> Search<'a> {
    /// Creates a new instance.
    ///
    /// `report_function` should be a function that registers the
    /// search progress. It will be called with the number of searched
    /// positions from the beginning of the search to this moment. The
    /// function should return `true` if the search should be
    /// terminated, otherwise it should return `false`.
    pub fn new(root: Box<SearchNode>,
               tt: &'a Tt,
               move_stack: &'a mut MoveStack,
               report_function: &'a mut FnMut(NodeCount) -> bool)
               -> Search<'a> {
        let moves_starting_ply = move_stack.ply();
        Search {
            tt: tt,
            killers: KillerTable::new(),
            position: root,
            moves: move_stack,
            moves_starting_ply: moves_starting_ply,
            state_stack: Vec::with_capacity(32),
            reported_nodes: 0,
            unreported_nodes: 0,
            report_function: report_function,
        }
    }

    /// Performs a game tree search and returns the result.
    ///
    /// `alpha` and `beta` together give the interval within which an
    /// as precise as possible evaluation is required. If during the
    /// search is determined that the exact evaluation is outside of
    /// this interval, this method may return a value that is closer
    /// to the the interval bounds than the exact evaluation, but
    /// always staying on the correct side of the interval. `depth` is
    /// the desired search depth in half-moves. `last_move` should be
    /// the move that led to the current position, or `Move::invalid()`
    /// if the last move is unknown.
    ///
    /// **Important note**: This method may leave un-restored move
    /// lists in `move_stack` (see the parametes passed to
    /// `Search::new`). Call the `reset` method if you want the move
    /// stack to be restored to the state it had when the search
    /// instance was created.
    pub fn run(&mut self,
               mut alpha: Value, // lower bound
               beta: Value, // upper bound
               depth: u8,
               last_move: Move)
               -> Result<Value, TerminatedSearch> {
        // This implementation performs a modified alpha-beta search.
        // It uses zero window searches with reduced depth for late
        // moves.
        //
        // The classical alpha-beta search is a huge enhancement to
        // the minimax search algorithm, that eliminates the need to
        // search large portions of the game tree by applying a
        // branch-and-bound technique. It does this without any
        // potential of overlooking a better move. If one already has
        // found a quite good move and searches for alternatives, one
        // refutation is enough to avoid it -- no need to look for
        // even stronger refutations. The algorithm maintains two
        // values, alpha and beta. They represent the minimum score
        // that the maximizing player is assured of and the maximum
        // score that the minimizing player is assured of
        // respectively.

        debug_assert!(alpha < beta);
        let mut value = VALUE_UNKNOWN;

        if let Some(v) = try!(self.node_begin(alpha, beta, depth, last_move)) {
            // We already have the final result.
            value = v;

        } else {
            // Initial guests.
            debug_assert!(depth > 0);
            let mut bound = BOUND_EXACT;
            let mut best_move = Move::invalid();

            // Try moves.
            while let Some(m) = self.do_move() {
                try!(self.report_progress(1));
                let reduced_depth = if depth < 2 {
                    0
                } else {
                    depth - 2
                };

                // Make a recursive call.
                let mut v = if m.score() > REDUCTION_THRESHOLD {
                    // The moves that have good chances to cause a
                    // beta cut-off we analyze with a full depth and
                    // fully open window (alpha, beta). Most probably
                    // at least one of these moves will raise `alpha`.
                    -try!(self.run(-beta, -alpha, depth - 1, m))
                } else {
                    // For the rest of the moves we first try to prove
                    // that they are not better than our current best
                    // move. For this purpose we search them with a
                    // reduced depth and a null window (alpha, alpha +
                    // 1). Only if it seems that the move is better
                    // than our current best move, we do a full-depth,
                    // full-window search.
                    match -try!(self.run(-alpha - 1, -alpha, reduced_depth, m)) {
                        v if v <= alpha => v,
                        _ => -try!(self.run(-beta, -alpha, depth - 1, m)),
                    }
                };
                self.undo_move();
                debug_assert!(v > VALUE_UNKNOWN);

                // Increase/decrease the value for a checkmate by one
                // on every half-move. This way the engine will seek
                // for the fastest checkmate possible.
                if v < VALUE_EVAL_MIN - 1 {
                    v += 1;
                } else if v > VALUE_EVAL_MAX + 1 {
                    v -= 1;
                }

                // See how good this move was.
                if v >= beta {
                    // This move is so good, that the opponent will
                    // probably not allow this line of play to happen.
                    // Therefore we should not lose any more time on
                    // this position (beta cut-off).
                    best_move = m;
                    value = v;
                    bound = BOUND_LOWER;
                    self.register_killer_move(m);
                    break;
                }
                if v > value {
                    // We found a new best move.
                    best_move = m;
                    value = v;
                    bound = if v > alpha {
                        alpha = v;
                        BOUND_EXACT
                    } else {
                        BOUND_UPPER
                    };
                }
            }

            // Check if we are in a final position (no legal moves).
            if value == VALUE_UNKNOWN {
                value = self.position.evaluate_final();
                debug_assert_eq!(bound, BOUND_EXACT);
            }

            // Store the result to the transposition table.
            self.store(value, bound, depth, best_move);
        }

        self.node_end();
        Ok(value)
    }

    /// Returns the number of searched positions.
    #[inline(always)]
    pub fn node_count(&self) -> NodeCount {
        self.reported_nodes + self.unreported_nodes
    }

    /// Resets the instance to the state it had when it was created.
    #[inline]
    pub fn reset(&mut self) {
        while self.moves.ply() > self.moves_starting_ply {
            self.moves.restore();
        }
        self.state_stack.clear();
        self.reported_nodes = 0;
        self.unreported_nodes = 0;
        self.killers.forget_all();
    }

    /// A helper method for `run`. Each call to `run` begins with a
    /// call to `node_begin`.
    ///
    /// This method tries to calculate and return the value for the
    /// node. It basically does 3 things:
    ///
    /// 1. Checks if the transposition table has the result.
    /// 2. On leaf nodes, performs quiescence search.
    /// 3. Performs null move pruning if possible.
    fn node_begin(&mut self,
                  alpha: Value,
                  beta: Value,
                  depth: u8,
                  last_move: Move)
                  -> Result<Option<Value>, TerminatedSearch> {
        // Probe the transposition table.
        let hash = self.position.hash();
        let entry = if let Some(e) = self.tt.probe(hash) {
            e
        } else {
            TtEntry::new(0, BOUND_NONE, 0, 0, self.position.evaluate_static())
        };
        self.state_stack.push(NodeState {
            phase: NodePhase::Pristine,
            entry: entry,
            killer: None,
            is_check: unsafe { mem::uninitialized() }, // We will initialize this soon!
        });

        // Check if the TT entry gives the result.
        if entry.depth() >= depth {
            let value = entry.value();
            let bound = entry.bound();
            if (value >= beta && bound & BOUND_LOWER != 0) ||
               (value <= alpha && bound & BOUND_UPPER != 0) ||
               (bound == BOUND_EXACT) {
                return Ok(Some(value));
            };
        };

        // On leaf nodes, do quiescence search.
        if depth == 0 {
            let (value, nodes) = self.position
                                     .evaluate_quiescence(alpha, beta, entry.eval_value());
            try!(self.report_progress(nodes));
            let bound = if value >= beta {
                BOUND_LOWER
            } else if value <= alpha {
                BOUND_UPPER
            } else {
                BOUND_EXACT
            };
            self.tt.store(hash, TtEntry::new(value, bound, 0, 0, entry.eval_value()));
            return Ok(Some(value));
        }

        // Save the current move list. Also, save checkers and pinned
        // bitboards, because we will need them at later phases.
        {
            self.moves.save();
            let state = self.state_stack.last_mut().unwrap();
            state.phase = NodePhase::ConsideredNullMove;
            state.is_check = self.position.is_check();
        }

        // Consider null move pruning. In positions that are not prone
        // to zugzwang, we attempt to reduce the search space by
        // trying a "null" or "passing" move, then seeing if the score
        // of the sub-tree search is still high enough to cause a beta
        // cutoff. Nodes are saved by reducing the depth of the
        // sub-tree under the null move.
        if !last_move.is_null() && entry.eval_value() >= beta &&
           self.position.is_zugzwang_unlikely() {
            // Calculate the reduced depth.
            let reduced_depth = if depth > 7 {
                depth as i8 - NULL_MOVE_REDUCTION as i8 - 1
            } else {
                depth as i8 - NULL_MOVE_REDUCTION as i8
            };

            // Check if the TT indicates that trying a null move is
            // futile. We rely on the fact that if no normal move can
            // reach `beta`, a null move will not do it either.
            if entry.depth() >= max(0, reduced_depth) as u8 && entry.value() < beta &&
               entry.bound() & BOUND_UPPER != 0 {
                return Ok(None);
            }

            // Play a null move and search.
            let m = self.position.null_move();
            if self.position.do_move(m) {
                let value = -try!(self.run(-beta, -alpha, max(0, reduced_depth - 1) as u8, m));
                self.position.undo_move();
                if value >= beta {
                    // The result we are about to return is more or
                    // less a lie (because of the depth reduction),
                    // and therefore we better tell a smaller lie and
                    // return `beta` here instead of `value`.
                    self.tt.store(hash,
                                  TtEntry::new(beta, BOUND_LOWER, depth, 0, entry.eval_value()));
                    return Ok(Some(beta));
                }
            }
        }

        // Well, we do not know the value yet.
        Ok(None)
    }

    /// A helper method for `run`. Each call to `run` ends with a call
    /// to `_end`.
    #[inline]
    fn node_end(&mut self) {
        // Restore the move list from the previous ply (half-move) and
        // pop the state stack.
        if let NodePhase::Pristine = self.state_stack.last().unwrap().phase {
            // For pristine nodes we have not saved a new move list,
            // so we should not call `restore`.
        } else {
            self.moves.restore();
        }
        self.state_stack.pop();

        // Killer moves for distant plys are gradually becoming
        // outdated, so we should downgrade them.
        let downgraded_ply = self.state_stack.len() + KILLERS_DOWNGRADE_DISTANCE;
        if downgraded_ply < MAX_DEPTH as usize {
            self.killers.downgrade(downgraded_ply);
        }
    }

    /// A helper method for `run`. It plays the next legal move in the
    /// current position and returns it.
    ///
    /// Each call to `do_move` for the same position will play and
    /// return a different move. When all legal moves has been played,
    /// `None` will be returned. `do_move` will do whatever it can to
    /// play the best moves first, and the worst last. It will also
    /// try to be efficient, for example it will generate the list of
    /// all pseudo-legal moves at the last possible moment.
    #[inline]
    fn do_move(&mut self) -> Option<Move> {
        debug_assert!(self.state_stack.len() > 0);
        let ply = self.state_stack.len() - 1;
        let state = &mut self.state_stack[ply];
        debug_assert!(if let NodePhase::Pristine = state.phase {
            false
        } else {
            true
        });
        debug_assert!(ply < MAX_DEPTH as usize);

        // Try the hash move.
        if let NodePhase::ConsideredNullMove = state.phase {
            state.phase = NodePhase::TriedHashMove;
            if let Some(mut m) = self.position.try_move_digest(state.entry.move16()) {
                if self.position.do_move(m) {
                    m.set_score(Move::max_score());
                    return Some(m);
                }
            }
        }

        // Generate all pseudo-legal moves.
        if let NodePhase::TriedHashMove = state.phase {
            state.phase = NodePhase::GeneratedMoves;
            self.position.generate_moves(self.moves);

            // We should not forget to remove the already tried hash
            // move from the list.
            if state.entry.move16() != 0 {
                self.moves.remove(state.entry.move16());
            }

            // We set new move scores to all captures and promotions
            // to queen according to their static exchange evaluation.
            for m in self.moves.iter_mut() {
                if m.score() == Move::max_score() {
                    let see = self.position.evaluate_move(*m);
                    let new_move_score = if see > 0 {
                        Move::max_score() - 1
                    } else if see == 0 {
                        Move::max_score() - 2
                    } else {
                        0
                    };
                    m.set_score(new_move_score);
                }
            }
        }

        // Try the generated moves.
        while let Some(mut m) = if let NodePhase::TriedLosingCaptures = state.phase {
            // After we have tried the losing captures, we try the
            // rest of the moves in the order in which they reside in
            // the move stack, because at this stage the probability
            // of cut-off is low, so the move ordering is not
            // important.
            self.moves.pop()
        } else {
            self.moves.remove_best()
        } {
            // First -- the winning and even captures and promotions
            // to queen.
            if let NodePhase::GeneratedMoves = state.phase {
                if m.score() > REDUCTION_THRESHOLD {
                    if self.position.do_move(m) {
                        return Some(m);
                    }
                    continue;
                }
                state.phase = NodePhase::TriedWinningMoves;
            }

            // Second -- the killer moves. We try two killer moves in
            // two sequential iterations of the loop. `state.killer`
            // remembers where we are.
            if let NodePhase::TriedWinningMoves = state.phase {
                self.moves.push(m);
                let killer = if let Some(k2) = state.killer {
                    state.phase = NodePhase::TriedKillerMoves;
                    k2
                } else {
                    let (k1, k2) = self.killers.get(ply);
                    state.killer = Some(k2);
                    k1
                };
                if killer != 0 {
                    if let Some(mut m) = self.moves.remove(killer) {
                        if self.position.do_move(m) {
                            m.set_score(Move::max_score());
                            return Some(m);
                        }
                    }
                }
                continue;
            }

            // Third -- the losing captures.
            if let NodePhase::TriedKillerMoves = state.phase {
                if m.captured_piece() < NO_PIECE {
                    if self.position.do_move(m) {
                        m.set_score(Move::max_score());
                        return Some(m);
                    }
                    continue;
                }
                state.phase = NodePhase::TriedLosingCaptures;
                self.moves.push(m);

                // TODO: Pull selected quiet moves to the top of the
                // move stack here, using the history
                // heuristics.
                continue;
            }

            // Fourth -- the remaining quiet moves.
            if self.position.do_move(m) {
                if state.is_check || self.position.is_check() || m.move_type() == MOVE_PROMOTION {
                    // When evading check, giving check, or promoting
                    // a pawn -- set a high move score to avoid search
                    // depth reductions.
                    m.set_score(Move::max_score());
                }
                return Some(m);
            }
        }
        None
    }

    /// A helper method for `run`. It takes back the last move played
    /// by `do_move`.
    #[inline]
    fn undo_move(&mut self) {
        self.position.undo_move();
    }

    /// A helper method for `run`. It stores the updated node
    /// information in the transposition table.
    #[inline]
    fn store(&mut self, value: Value, bound: BoundType, depth: u8, best_move: Move) {
        let entry = &self.state_stack.last().unwrap().entry;
        self.tt.store(self.position.hash(),
                      TtEntry::new(value, bound, depth, best_move.digest(), entry.eval_value()));
    }

    /// A helper method for `run`. It reports search progress.
    ///
    /// From time to time, we should report how many nodes has been
    /// searched since the beginning of the search. This also gives an
    /// opportunity for the search to be terminated.
    #[inline]
    fn report_progress(&mut self, new_nodes: NodeCount) -> Result<(), TerminatedSearch> {
        self.unreported_nodes += new_nodes;
        if self.unreported_nodes >= NODE_COUNT_REPORT_INTERVAL {
            self.reported_nodes += self.unreported_nodes;
            self.unreported_nodes = 0;
            if (*self.report_function)(self.reported_nodes) {
                return Err(TerminatedSearch);
            }
        }
        Ok(())
    }

    /// A helper method for `run`. It registers that the move `m`
    /// caused a beta cut-off (a killer move).
    #[inline]
    fn register_killer_move(&mut self, m: Move) {
        self.killers.register(self.state_stack.len() - 1, m);
    }
}


/// The number of nodes that will be searched without reporting search
/// progress.
///
/// If this value is too small the engine may become slow, if this
/// value is too big the engine may become unresponsive.
const NODE_COUNT_REPORT_INTERVAL: NodeCount = 10000;


/// The number of half-moves with which the search depth will be
/// reduced when trying null moves.
const NULL_MOVE_REDUCTION: u8 = 3;


/// Moves with move scores higher than this number will be searched at
/// full depth. Moves with move scores lesser or equal to this number
/// will be searched at reduced depth.
const REDUCTION_THRESHOLD: usize = 0;


/// When this distance in half-moves is reached, the old killer moves
/// will be downgraded. This affects for how long the successful old
/// killer moves are kept.
const KILLERS_DOWNGRADE_DISTANCE: usize = 3;


/// Tells where we are in the move generation sequence.
enum NodePhase {
    Pristine,
    ConsideredNullMove,
    TriedHashMove,
    GeneratedMoves,
    TriedWinningMoves,
    TriedKillerMoves,
    TriedLosingCaptures,
}


/// Holds information about the state of a node in the search tree.
struct NodeState {
    phase: NodePhase,
    entry: TtEntry,
    killer: Option<MoveDigest>,
    is_check: bool,
}



/// Holds two killer moves with their hit counters for every
/// half-move.
///
/// "Killer move" is a move which caused beta cut-off in a sibling
/// node, or any other earlier branch in the search tree with the same
/// distance to the root position. The idea is to try that move early
/// -- directly after the hash move and the winning captures.
struct KillerTable {
    array: [KillerPair; MAX_DEPTH as usize],
}

impl KillerTable {
    /// Creates a new instance.
    #[inline]
    pub fn new() -> KillerTable {
        KillerTable { array: [Default::default(); MAX_DEPTH as usize] }
    }

    /// Registers a new killer move for the specified `half_move`.
    #[inline]
    pub fn register(&mut self, half_move: usize, m: Move) {
        debug_assert!(half_move < self.array.len());
        if m.captured_piece() != NO_PIECE || m.move_type() == MOVE_PROMOTION {
            // We do not want to waste our precious killer-slots on
            // captures and promotions.
            return;
        }
        let pair = &mut self.array[half_move];
        let minor = &mut pair.minor;
        let major = &mut pair.major;
        let digest = m.digest();
        debug_assert!(digest != 0);

        // Register the move in one of the slots.
        if major.digest == digest {
            major.hits = major.hits.wrapping_add(1);
            return;
        } else if minor.digest == digest {
            minor.hits = minor.hits.wrapping_add(1);
        } else {
            *minor = Killer {
                digest: digest,
                hits: 1,
            };
        }

        // Swap the slots if the minor killer has got at least as much
        // hits as the major killer.
        if minor.hits >= major.hits {
            mem::swap(minor, major);
        }
    }

    /// Returns the two killer moves for the specified `half_move`.
    ///
    /// This method will not return capture and promotion moves as
    /// killers, because those are tried early anyway. The move
    /// returned in the first slot should be treated as the better one
    /// of the two. If no killer move is available for one or both of
    /// the slots -- `0` is returned instead.
    #[inline]
    pub fn get(&self, half_move: usize) -> (MoveDigest, MoveDigest) {
        debug_assert!(half_move < self.array.len());
        let pair = &self.array[half_move];
        (pair.major.digest, pair.minor.digest)
    }

    /// Reduces the hit counters for the specified `half_move` by a
    /// factor of two.
    #[inline]
    pub fn downgrade(&mut self, half_move: usize) {
        debug_assert!(half_move < self.array.len());
        let pair = &mut self.array[half_move];
        pair.minor.hits >>= 1;
        pair.major.hits >>= 1;
    }

    /// Forgets all registered killer moves.
    #[inline]
    pub fn forget_all(&mut self) {
        for pair in self.array.iter_mut() {
            *pair = Default::default();
        }
    }
}


/// A killer move with its hit counter.
#[derive(Clone, Copy)]
struct Killer {
    pub digest: MoveDigest,
    pub hits: u16,
}


/// A pair of two killer moves.
///
/// The `major` killer is always the more important one. The killers
/// are swapped if at some moment the minor killer has got at least as
/// much hits as the major killer.
#[derive(Clone, Copy)]
struct KillerPair {
    pub minor: Killer,
    pub major: Killer,
}

impl Default for KillerPair {
    fn default() -> KillerPair {
        KillerPair {
            minor: Killer {
                digest: 0,
                hits: 0,
            },
            major: Killer {
                digest: 0,
                hits: 0,
            },
        }
    }
}



#[cfg(test)]
mod tests {
    use super::{Search, KillerTable};
    use basetypes::*;
    use tt::*;
    use moves::*;
    use position::*;
    use search::SearchNode;

    #[test]
    fn test_search() {
        assert!(Move::max_score() - 2 > super::REDUCTION_THRESHOLD);
        let p = Position::from_fen("8/8/8/8/3q3k/7n/6PP/2Q2R1K b - - 0 1").ok().unwrap();
        let tt = Tt::new();
        let mut moves = MoveStack::new();
        let mut report = |_| false;
        let mut search = Search::new(Box::new(p), &tt, &mut moves, &mut report);
        let value = search.run(VALUE_MIN, VALUE_MAX, 2, Move::invalid())
                          .ok()
                          .unwrap();
        assert!(value < -300);
        search.reset();
        let value = search.run(VALUE_MIN, VALUE_MAX, 8, Move::invalid())
                          .ok()
                          .unwrap();
        assert!(value > VALUE_EVAL_MAX);
    }

    #[test]
    fn test_killers() {
        use basetypes::*;
        let mut killers = KillerTable::new();
        let mut p = Position::from_fen("5r2/8/8/4q1p1/3P4/k3P1P1/P2b1R1B/K4R2 w - - 0 1")
                        .ok()
                        .unwrap();
        let mut v = MoveStack::new();
        p.generate_moves(&mut v);
        let mut i = 1;
        let mut previous_move_digest = 0;
        while let Some(m) = v.pop() {
            if m.captured_piece() == NO_PIECE && p.do_move(m) {
                for _ in 0..i {
                    killers.register(0, m);
                }
                i += 1;
                p.undo_move();
                let (killer1, killer2) = killers.get(0);
                assert!(killer1 == m.digest());
                assert!(killer2 == previous_move_digest);
                previous_move_digest = m.digest();
            }
        }
        assert!(killers.get(1) == (0, 0));
    }
}
