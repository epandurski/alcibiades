//! Implements single-threaded game tree search.

use std::i16::MIN as VALUE_MINIMUM;
use std::cmp::max;
use basetypes::*;
use bitsets::*;
use chess_move::*;
use tt::*;
use position::Position;
use super::R;


/// Represents a terminated search condition.
pub struct TerminatedSearch;


/// Represents a game tree search.        
pub struct Search<'a> {
    tt: &'a TranspositionTable,
    position: Position,
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
    pub fn new(root: Position,
               tt: &'a TranspositionTable,
               move_stack: &'a mut MoveStack,
               report_function: &'a mut FnMut(NodeCount) -> bool)
               -> Search<'a> {
        let moves_starting_ply = move_stack.ply();
        Search {
            tt: tt,
            position: root,
            moves: move_stack,
            moves_starting_ply: moves_starting_ply,
            state_stack: Vec::with_capacity(32),
            reported_nodes: 0,
            unreported_nodes: 0,
            report_function: report_function,
        }
    }

    /// Performs a principal variation search (PVS) and returns a
    /// result.
    ///
    /// PVS is an enhancement to the alpha-beta search, based on zero
    /// window searches of none PV-nodes. (The principal variation
    /// (PV) is a sequence of moves that the program considers best
    /// and therefore expects to be played.) The classical alpha-beta
    /// search is a huge enhancement to the minimax search algorithm,
    /// that eliminates the need to search large portions of the game
    /// tree by applying a branch-and-bound technique. Remarkably, it
    /// does this without any potential of overlooking a better
    /// move. If one already has found a quite good move and searches
    /// for alternatives, one refutation is enough to avoid it -- no
    /// need to look for even stronger refutations. The algorithm
    /// maintains two values, alpha and beta. They represent the
    /// minimum score that the maximizing player is assured of and the
    /// maximum score that the minimizing player is assured of
    /// respectively.
    ///
    /// `alpha` and `beta` together give the interval within which an
    /// as precise as possible evaluation is required. If during the
    /// search it is determined that the exact evaluation is outside
    /// of this interval, this method may return a value that is
    /// closer to the the interval bounds than the exact evaluation,
    /// but always staying on the correct side of the
    /// interval. `depth` is the desired search depth in
    /// half-moves. `last_move` is the move that led to the current
    /// position.
    ///
    /// **Important note**: This method may leave un-restored move
    /// lists in the move stack. Call `reset` if you want the move
    /// stack to be restored to the state it had when the search
    /// instance was created.
    pub fn run(&mut self,
               mut alpha: Value, // lower bound
               beta: Value, // upper bound
               depth: u8,
               last_move: Move)
               -> Result<Value, TerminatedSearch> {
        assert!(alpha < beta);
        let mut value = VALUE_MINIMUM;

        if let Some(v) = try!(self.node_begin(alpha, beta, depth, last_move)) {
            // We already have the final result.
            value = v;

        } else {
            // Initial guests.
            let mut bound = BOUND_EXACT;
            let mut best_move = Move::invalid();

            // Try moves.
            while let Some(m) = self.do_move() {
                try!(self.report_progress(1));

                // Decide whether to apply depth reduction or not.
                let next_depth = if m.score() > REDUCTION_THRESHOLD {
                    if last_move.score() <= REDUCTION_THRESHOLD &&
                       self.position.board().checkers() != 0 {
                        // The last move was erroneously reduced
                        // because we did not know that it gives a
                        // check -- +1 extension.
                        depth
                    } else {
                        // no reduction
                        depth - 1
                    }
                } else {
                    // -1 reduction
                    max(0, depth as i8 - 2) as u8
                };

                // Make a recursive call.
                let v = if value == VALUE_MINIMUM {
                    // The first move we analyze with a fully open window
                    // (alpha, beta). If this happens to be a good move,
                    // it will probably raise `alpha`.
                    -try!(self.run(-beta, -alpha, next_depth, m))
                } else {
                    // For the next moves we first try to prove that they
                    // are not better than our current best move. For this
                    // purpose we analyze them with a null window (alpha,
                    // alpha + 1). This is faster than a full window
                    // search. Only if we are certain that the move is
                    // better than our current best move, we do a
                    // full-window search.
                    match -try!(self.run(-alpha - 1, -alpha, next_depth, m)) {
                        v if v <= alpha => v,
                        _ => -try!(self.run(-beta, -alpha, next_depth, m)),
                    }
                };
                self.undo_move();
                assert!(v > VALUE_MINIMUM);

                // See how good this move was.
                if v >= beta {
                    // This move is so good, that the opponent will
                    // probably not allow this line of play to
                    // happen. Therefore we should not lose any more time
                    // on this position.
                    best_move = m;
                    value = v;
                    bound = BOUND_LOWER;
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
            if value == VALUE_MINIMUM {
                value = self.position.evaluate_final();
                assert_eq!(bound, BOUND_EXACT);
            }

            // Store the result to the TT.
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
    }

    // A helper method for `Search::run`. Each call to `Search::run`
    // begins with a call to `Search::node_begin`. This method tries
    // to calculate and return the value for the node. It basically
    // does 3 things:
    //
    // 1. Checks if the transposition table has the result.
    // 2. On leaf nodes, performs quiescence search.
    // 3. Performs null move pruning if possible.
    #[inline]
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
            EntryData::new(0, BOUND_NONE, 0, 0, self.position.evaluate_static())
        };
        self.state_stack.push(NodeState {
            phase: NodePhase::Pristine,
            entry: entry,
            checkers: BB_UNIVERSAL_SET,
            pinned: BB_UNIVERSAL_SET,
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
                                     .evaluate_quiescence(alpha, beta, Some(entry.eval_value()));
            try!(self.report_progress(nodes));
            let bound = if value >= beta {
                BOUND_LOWER
            } else if value <= alpha {
                BOUND_UPPER
            } else {
                BOUND_EXACT
            };
            self.tt.store(hash, EntryData::new(value, bound, 0, 0, entry.eval_value()));
            return Ok(Some(value));
        }

        // Consider null move pruning. In positions that are not prone
        // to zugzwang, we attempt to reduce the search space by
        // trying a "null" or "passing" move, then seeing if the score
        // of the sub-tree search is still high enough to cause a beta
        // cutoff. Nodes are saved by reducing the depth of the
        // sub-tree under the null move.
        {
            // Save the current move list. Also, save checkers and
            // pinned bitboards, because we will need them at later
            // phases.
            self.moves.save();
            let state = self.state_stack.last_mut().unwrap();
            state.checkers = self.position.board().checkers();
            state.pinned = self.position.board().pinned();
            state.phase = NodePhase::ConsideredNullMove;
        }
        if !last_move.is_null() && entry.eval_value() >= beta && self.position.is_zugzwang_safe() {
            // Calculate the reduced depth.
            //
            // TODO: See if we can increase `R` in case `depth > 7`.
            // This probably will not work without implementing
            // extensions/reductions first.
            let reduced_depth = depth as i8 - R as i8;

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
                                  EntryData::new(beta, BOUND_LOWER, depth, 0, entry.eval_value()));
                    return Ok(Some(beta));
                }
            }
        }

        // Well, we do not know the value yet.
        Ok(None)
    }

    // A helper method for `Search::run`. Each call to `Search::run`
    // ends with a call to `Search::node_end`.
    #[inline]
    fn node_end(&mut self) {
        if let NodePhase::Pristine = self.state_stack.last().unwrap().phase {
            // For pristine nodes we have not saved the move list
            // yet, so we should not restore it.
        } else {
            self.moves.restore();
        }
        self.state_stack.pop();
    }

    // A helper method for `Search::run`. It plays the next legal move
    // in the current position and returns it.
    //
    // Each call to `do_move` for the same position will play and
    // return a different move. When all legal moves has been played,
    // `None` will be returned. `do_move` will do whatever it can to
    // play the best moves first, and the worst last. It will also try
    // to be efficient, for example it will generate the list of all
    // pseudo-legal moves at the last possible moment.
    #[inline]
    fn do_move(&mut self) -> Option<Move> {
        let state = self.state_stack.last_mut().unwrap();

        assert!(if let NodePhase::Pristine = state.phase {
            false
        } else {
            true
        });

        // Try the hash move first.
        if let NodePhase::ConsideredNullMove = state.phase {
            state.phase = NodePhase::TriedHashMove;
            if state.entry.move16() != 0 {
                assert_eq!(self.position.board().checkers(), state.checkers);
                assert_eq!(self.position.board().pinned(), state.pinned);
                self.position.board()._checkers.set(state.checkers);
                self.position.board()._pinned.set(state.pinned);
                if let Some(mut m) = self.position.try_move_digest(state.entry.move16()) {
                    if self.position.do_move(m) {
                        m.set_score(MAX_MOVE_SCORE);
                        return Some(m);
                    }
                }
            }
        }

        // After the hash move, we generate all pseudo-legal moves. We
        // should not forget to remove the already tried hash move
        // from the list.
        if let NodePhase::TriedHashMove = state.phase {
            assert_eq!(self.position.board().checkers(), state.checkers);
            assert_eq!(self.position.board().pinned(), state.pinned);
            self.position.board()._checkers.set(state.checkers);
            self.position.board()._pinned.set(state.pinned);
            self.position.generate_moves(self.moves);
            if state.entry.move16() != 0 {
                self.moves.remove_move(state.entry.move16());
            }
            state.phase = NodePhase::GeneratedMoves;
        }

        // Spit out the generated moves.
        while let Some(mut m) = self.moves.remove_best_move() {

            // First, the good captures.
            if let NodePhase::GeneratedMoves = state.phase {
                if m.score() == MAX_MOVE_SCORE {
                    if self.position.evaluate_move(m) >= 0 {
                        if self.position.do_move(m) {
                            return Some(m);
                        }
                        continue;
                    }
                    // This is a bad capture -- push it back to the
                    // move stack with a lesser score.
                    m.set_score(MAX_MOVE_SCORE - 1);
                    self.moves.push(m);
                    continue;
                }
                state.phase = NodePhase::TriedGoodCaptures;
            }

            // TODO: play the killer moves here.

            // Second, the bad captures.
            if let NodePhase::TriedGoodCaptures = state.phase {
                if m.score() == MAX_MOVE_SCORE - 1 {
                    if self.position.do_move(m) {
                        return Some(m);
                    }
                    continue;
                }
                state.phase = NodePhase::TriedBadCaptures;
            }

            // Before trying the quiet moves, we should assign proper
            // move scores to them.
            if let NodePhase::TriedBadCaptures = state.phase {
                // TODO: Assign the moves scores here using the
                // history and countermove heuristics. Temporarily, we
                // apply a very simple quiet move ordering. Moves
                // which destination square is more advanced into
                // enemy's territory are tried first.
                const SCORE_LOOKUP: [[u32; 8]; 2] = [// white
                                                     [0, 1, 2, 3, 4, 5, 6, 7],
                                                     // black
                                                     [7, 6, 5, 4, 3, 2, 1, 0]];
                for m in self.moves.iter_mut() {
                    let rank = rank(m.dest_square());
                    m.set_score(unsafe {
                        *SCORE_LOOKUP.get_unchecked(self.position.board().to_move())
                                     .get_unchecked(rank)
                    });
                }

                state.phase = NodePhase::SortedQuietMoves;
            }

            // Last, quiet moves.
            if self.position.do_move(m) {
                if state.checkers != 0 {
                    // When in check, we set a high move score to all
                    // moves to avoid search depth reductions.
                    m.set_score(MAX_MOVE_SCORE - 1);
                }
                return Some(m);
            }
        }
        None
    }

    // A helper method for `Search::run`. It takes back the last move
    // played by `Search::do_move`.
    #[inline]
    fn undo_move(&mut self) {
        self.position.undo_move();
    }

    // A helper method for `Search::run`. It stores the updated node
    // information in the transposition table.
    #[inline]
    fn store(&mut self, value: Value, bound: BoundType, depth: u8, best_move: Move) {
        let entry = &self.state_stack.last().unwrap().entry;
        self.tt.store(self.position.hash(),
                      EntryData::new(value, bound, depth, best_move.digest(), entry.eval_value()));
    }

    // A helper method for `Search::run`. It reports search progress.
    //
    // From time to time, we should report how many nodes has been
    // searched since the beginning of the search. This also gives an
    // opportunity for the search to be terminated.
    #[inline]
    fn report_progress(&mut self, new_nodes: NodeCount) -> Result<(), TerminatedSearch> {
        self.unreported_nodes += new_nodes;
        if self.unreported_nodes >= super::NODE_COUNT_REPORT_INTERVAL {
            self.reported_nodes += self.unreported_nodes;
            self.unreported_nodes = 0;
            if (*self.report_function)(self.reported_nodes) {
                return Err(TerminatedSearch);
            }
        }
        Ok(())
    }
}


// Moves with move scores higher than this number will be searched at
// full depth. Moves with move scores lesser or equal to this number
// will be searched at reduced depth.
const REDUCTION_THRESHOLD: u32 = MAX_MOVE_SCORE >> 1;


enum NodePhase {
    Pristine,
    ConsideredNullMove,
    TriedHashMove,
    GeneratedMoves,
    TriedGoodCaptures,
    TriedBadCaptures,
    SortedQuietMoves,
}


struct NodeState {
    phase: NodePhase,
    entry: EntryData,
    checkers: u64,
    pinned: u64,
}


#[cfg(test)]
mod tests {
    use super::Search;
    use chess_move::*;
    use tt::*;
    use position::Position;

    #[test]
    fn test_search() {
        let p = Position::from_fen("8/8/8/8/3q3k/7n/6PP/2Q2R1K b - - 0 1").ok().unwrap();
        let tt = TranspositionTable::new();
        let mut moves = MoveStack::new();
        let mut report = |_| false;
        let mut search = Search::new(p, &tt, &mut moves, &mut report);
        let value = search.run(-30000, 30000, 2, Move::invalid())
                          .ok()
                          .unwrap();
        assert!(value < -300);
        search.reset();
        let value = search.run(-30000, 30000, 8, Move::invalid())
                          .ok()
                          .unwrap();
        assert!(value >= 20000);
    }
}
