//! Implements single-threaded game tree search.

use basetypes::*;
use bitsets::*;
use chess_move::*;
use tt::*;
use position::Position;


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
    /// The alpha-beta algorithm is a significant enhancement to the
    /// minimax search algorithm that eliminates the need to search
    /// large portions of the game tree applying a branch-and-bound
    /// technique. Remarkably, it does this without any potential of
    /// overlooking a better move. If one already has found a quite
    /// good move and search for alternatives, one refutation is
    /// enough to avoid it. No need to look for even stronger
    /// refutations. The algorithm maintains two values, alpha and
    /// beta. They represent the minimum score that the maximizing
    /// player is assured of and the maximum score that the minimizing
    /// player is assured of respectively. PVS is an enhancement to
    /// the alpha-beta search, based on zero (null) window searches of
    /// none PV-nodes, to prove a move is worse or not than an already
    /// safe score from the principal variation. (The principal
    /// variation (PV) is a sequence of moves that the program
    /// considers best and therefore expects to be played.)
    ///
    /// **Important note**: This method may leave un-restored move
    /// lists in the move stack. Call `reset` if you want the move
    /// stack to be restored to the state it had when the search
    /// instance was created.
    pub fn run(&mut self,
               mut alpha: Value, // lower bound
               beta: Value, // upper bound
               depth: u8)
               -> Result<Value, TerminatedSearch> {
        assert!(alpha < beta);

        let entry = self.node_begin();

        // Check if the TT entry gives the result.
        if entry.depth() >= depth {
            let value = entry.value();
            let bound = entry.bound();
            if value >= beta && bound & BOUND_LOWER != 0 {
                self.node_end();
                return Ok(beta);
            }
            if value <= alpha && bound & BOUND_UPPER != 0 {
                self.node_end();
                return Ok(alpha);
            }
            if bound == BOUND_EXACT {
                self.node_end();
                return Ok(value);
            }
        }

        // Initial guests for the final result.
        let mut bound = BOUND_UPPER;
        let mut best_move = Move::invalid();

        if depth == 0 {
            // On leaf nodes, do quiescence search.
            let (value, nodes) = self.position
                                     .evaluate_quiescence(alpha, beta, Some(entry.eval_value()));
            try!(self.report_progress(nodes));

            // See how good this position is.
            if value >= beta {
                alpha = beta;
                bound = BOUND_LOWER;
            } else if value > alpha {
                alpha = value;
                bound = BOUND_EXACT;
            }

        } else {
            // On non-leaf nodes, try moves.
            let mut no_moves_yet = true;
            while let Some(m) = self.do_move() {
                try!(self.report_progress(1));

                // Make a recursive call.
                let value = if no_moves_yet {
                    // The first move we analyze with a fully open window
                    // (alpha, beta). If this happens to be a good move,
                    // it will probably raise `alpha`.
                    no_moves_yet = false;
                    -try!(self.run(-beta, -alpha, depth - 1))
                } else {
                    // For the next moves we first try to prove that they
                    // are not better than our current best move. For this
                    // purpose we analyze them with a null window (alpha,
                    // alpha + 1). This is faster than a full window
                    // search. Only if we are certain that the move is
                    // better than our current best move, we do a
                    // full-window search.
                    match -try!(self.run(-alpha - 1, -alpha, depth - 1)) {
                        x if x <= alpha => x,
                        _ => -try!(self.run(-beta, -alpha, depth - 1)),
                    }
                };
                self.undo_move();

                // See how good this move was.
                if value >= beta {
                    // This move is so good, that the opponent will
                    // probably not allow this line of play to
                    // happen. Therefore we should not lose any more time
                    // on this position.
                    alpha = beta;
                    bound = BOUND_LOWER;
                    best_move = m;
                    break;
                }
                if value > alpha {
                    // We found a new best move.
                    alpha = value;
                    bound = BOUND_EXACT;
                    best_move = m;
                }
            }

            // Check if we are in a final position (no legal moves).
            if no_moves_yet {
                let value = self.position.evaluate_final();
                if value >= beta {
                    alpha = beta;
                    bound = BOUND_LOWER;
                }
                if value > alpha {
                    alpha = value;
                    bound = BOUND_EXACT;
                }
            }
        }

        self.store(alpha, bound, depth, best_move);
        self.node_end();
        Ok(alpha)
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

    // Declares that we are starting to process a new node.
    //
    // Each recursive call to `run` begins with a call to
    // `node_begin`. The returned value is a TT entry telling
    // everything we know about the current position.
    #[inline]
    fn node_begin(&mut self) -> EntryData {
        // Consult the transposition table.
        let entry = if let Some(e) = self.tt.probe(self.position.hash()) {
            e
        } else {
            EntryData::new(0, BOUND_NONE, 0, 0, self.position.evaluate_static())
        };
        self.state_stack.push(NodeState {
            phase: NodePhase::Pristine,
            entry: entry,
            checkers: BB_UNIVERSAL_SET,
        });
        entry
    }

    // Declares that we are done processing the current node.
    //
    // Each recursive call to `run` ends with a call to `node_end`.
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

    // TODO: Implement do_null_move() method?

    // Plays the next legal move in the current position and returns
    // it.
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

        // Try the hash move first.
        if let NodePhase::Pristine = state.phase {
            // We save the move list at the last possible moment,
            // because most of the nodes are leafs.
            self.moves.save();

            // We save the checkers bitboard also, because we will
            // need this information later many times, and we do not
            // want to recalculate it needlessly.
            //
            // TODO: Verify if this is the best place to calculate
            // checkers, especially in relation to trying null moves.
            state.checkers = self.position.board().checkers();

            state.phase = NodePhase::TriedHashMove;
            if state.entry.move16() != 0 {
                if let Some(mut m) = self.position.try_move_digest(state.entry.move16()) {
                    if self.position.do_move(m) {
                        m.set_score(MAX_MOVE_SCORE);
                        return Some(m);
                    }
                }
            }
        }

        // After the hash move, we generate all pseudo-legal
        // moves. But we should not forget to remove the already tried
        // hash move from the list.
        if let NodePhase::TriedHashMove = state.phase {
            // TODO: `generate_moves` needs `_checkers` and `_pinned`
            // to do its work. At this time we already have generated
            // them, so we should use them.
            self.position.board()._checkers.set(state.checkers);

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
                    // move stack.
                    m.set_score(MAX_MOVE_SCORE - 1);
                    self.moves.push(m);
                    continue;
                }
                state.phase = NodePhase::TriedGoodCaptures;
            }

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
                // TODO: Assign the moves scores here using the killer
                // move heuristics and the history heuristics.

                // We use the score field (2 bits) to properly order
                // quiet movies. Moves which destination square is
                // more advanced into enemy's territory are tried
                // first. The logic is that those moves are riskier,
                // so if such a move loses material this will be
                // detected early and the search tree will be pruned,
                // but if the move does not lose material, chances are
                // that it is a very good move.
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

    // Takes the last played move back.
    #[inline]
    fn undo_move(&mut self) {
        self.position.undo_move();
    }

    // Stores updated node information in the transposition table.
    #[inline]
    fn store(&mut self, value: Value, bound: BoundType, depth: u8, best_move: Move) {
        let entry = &self.state_stack.last().unwrap().entry;
        self.tt.store(self.position.hash(),
                      EntryData::new(value, bound, depth, best_move.digest(), entry.eval_value()));
    }

    // Reports search progress.
    //
    // From time to time, we should report how many nodes had been
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


enum NodePhase {
    Pristine,
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
        let value = search.run(-30000, 30000, 2)
                          .ok()
                          .unwrap();
        assert!(value < -300);
        search.reset();
        let value = search.run(-30000, 30000, 4)
                          .ok()
                          .unwrap();
        assert!(value >= 20000);
    }
}
