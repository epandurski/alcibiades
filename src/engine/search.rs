use std::thread;
use std::cell::UnsafeCell;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver, RecvError};
use basetypes::*;
use chess_move::*;
use tt::*;
use position::Position;


pub enum Command {
    Search {
        search_id: usize,
        position: Position,
        depth: u8,
        lower_bound: Value,
        upper_bound: Value,
    },
    Stop,
    Exit,
}


pub enum Report {
    Progress {
        search_id: usize,
        searched_nodes: NodeCount,
        depth: u8,
    },
    Done {
        search_id: usize,
        searched_nodes: NodeCount,
        value: Option<Value>,
    },
}


pub fn run_deepening(tt: Arc<TranspositionTable>,
                     commands: Receiver<Command>,
                     reports: Sender<Report>) {
    let (slave_commands_tx, slave_commands_rx) = channel();
    let (slave_reports_tx, slave_reports_rx) = channel();
    let slave = thread::spawn(move || {
        run(tt, slave_commands_rx, slave_reports_tx);
    });
    let mut pending_command = None;
    loop {
        // If there is a pending command, we take it, otherwise we
        // block and wait to receive a new one.
        let command = match pending_command.take() {
            Some(cmd) => cmd,
            None => commands.recv().or::<RecvError>(Ok(Command::Exit)).unwrap(),
        };

        match command {
            Command::Search { search_id, position, depth, lower_bound, upper_bound } => {
                let mut searched_nodes_final = 0;
                let mut value_final = None;
                'depthloop: for n in 1..(depth + 1) {
                    slave_commands_tx.send(Command::Search {
                                         search_id: n as usize,
                                         position: position.clone(),
                                         depth: n,
                                         lower_bound: lower_bound,
                                         upper_bound: upper_bound,
                                     })
                                     .unwrap();
                    loop {
                        match slave_reports_rx.recv().unwrap() {
                            Report::Progress { searched_nodes, .. } => {
                                reports.send(Report::Progress {
                                           search_id: search_id,
                                           searched_nodes: searched_nodes_final + searched_nodes,
                                           depth: n,
                                       })
                                       .ok();
                                if pending_command.is_none() {
                                    pending_command = match commands.try_recv() {
                                        Ok(cmd) => {
                                            slave_commands_tx.send(Command::Stop).unwrap();
                                            Some(cmd)
                                        }
                                        _ => None,
                                    }
                                }
                            }
                            Report::Done { searched_nodes, value, .. } => {
                                searched_nodes_final += searched_nodes;
                                if n == depth {
                                    value_final = value;
                                }
                                if pending_command.is_some() {
                                    break 'depthloop;
                                }
                                break;
                            }
                        }
                    }
                }
                reports.send(Report::Done {
                           search_id: search_id,
                           searched_nodes: searched_nodes_final,
                           value: value_final,
                       })
                       .ok();
            }
            Command::Stop => {
                slave_commands_tx.send(Command::Stop).unwrap();
                continue;
            }
            Command::Exit => {
                slave_commands_tx.send(Command::Exit).unwrap();
                break;
            }
        }
    }
    slave.join().unwrap();
}


pub fn run(tt: Arc<TranspositionTable>, commands: Receiver<Command>, reports: Sender<Report>) {
    thread_local!(
        static MOVE_STACK: UnsafeCell<MoveStack> = UnsafeCell::new(MoveStack::new())
    );
    MOVE_STACK.with(|s| {
        let mut move_stack = unsafe { &mut *s.get() };
        let mut pending_command = None;
        loop {
            // If there is a pending command, we take it, otherwise we
            // block and wait to receive a new one.
            let command = match pending_command.take() {
                Some(cmd) => cmd,
                None => commands.recv().or::<RecvError>(Ok(Command::Exit)).unwrap(),
            };

            match command {
                Command::Search { search_id, position, depth, lower_bound, upper_bound } => {
                    let mut state = SearchState {
                        tt: &tt,
                        position: position,
                        moves: move_stack,
                        state_stack: vec![],
                        reported_nodes: 0,
                        unreported_nodes: 0,
                        report_func: &mut |n| {
                            reports.send(Report::Progress {
                                       search_id: search_id,
                                       searched_nodes: n,
                                       depth: depth,
                                   })
                                   .ok();
                            match commands.try_recv() {
                                Ok(x) => {
                                    pending_command = Some(x);
                                    Err(TerminatedSearch)
                                }
                                _ => Ok(()),
                            }
                        },
                    };
                    let value = search(&mut state, lower_bound, upper_bound, depth).ok();
                    reports.send(Report::Progress {
                               search_id: search_id,
                               searched_nodes: state.node_count(),
                               depth: depth,
                           })
                           .ok();
                    reports.send(Report::Done {
                               search_id: search_id,
                               searched_nodes: state.node_count(),
                               value: value,
                           })
                           .ok();
                    state.clear();
                }
                Command::Stop => continue,
                Command::Exit => break,
            }
        }
    })
}


// Represents a terminated search condition.
struct TerminatedSearch;


enum NodePhase {
    Pristine,
    TriedHashMove,
    GeneratedMoves,
}


struct NodeState {
    phase: NodePhase,
    entry: EntryData,
}


const NODE_COUNT_REPORT_INTERVAL: NodeCount = 10000;


struct SearchState<'a> {
    tt: &'a TranspositionTable,
    position: Position,
    moves: &'a mut MoveStack,
    state_stack: Vec<NodeState>,
    reported_nodes: NodeCount,
    unreported_nodes: NodeCount,
    report_func: &'a mut FnMut(NodeCount) -> Result<(), TerminatedSearch>,
}


impl<'a> SearchState<'a> {
    #[inline(always)]
    pub fn position(&self) -> &Position {
        &self.position
    }

    #[inline(always)]
    pub fn node_count(&self) -> NodeCount {
        self.reported_nodes + self.unreported_nodes
    }

    #[inline]
    pub fn node_begin(&mut self) -> EntryData {
        // Consult the transposition table.
        let entry = if let Some(e) = self.tt.probe(self.position.hash()) {
            e
        } else {
            EntryData::new(0, BOUND_NONE, 0, 0, self.position.evaluate_static())
        };
        self.state_stack.push(NodeState {
            phase: NodePhase::Pristine,
            entry: entry,
        });
        entry
    }

    // Store the updated info in the transposition table.
    #[inline]
    pub fn node_store(&mut self, value: Value, bound: BoundType, depth: u8, best_move: Move) {
        let entry = &self.state_stack.last().unwrap().entry;
        let move16 = match best_move.digest() {
            0 => entry.move16(),
            x => x,
        };
        self.tt.store(self.position.hash(),
                      EntryData::new(value, bound, depth, move16, entry.eval_value()));
    }

    // Go back to the parent node.
    #[inline]
    pub fn node_end(&mut self) {
        if let NodePhase::Pristine = self.state_stack.last().unwrap().phase {
            // For pristine nodes we have not saved the move list
            // yet, so we should not restore it.
        } else {
            self.moves.restore();
        }
        self.state_stack.pop();
    }

    #[inline]
    pub fn do_move(&mut self) -> Option<Move> {
        let state = self.state_stack.last_mut().unwrap();

        if let NodePhase::Pristine = state.phase {
            // We save the move list in the last possible moment,
            // because most of the nodes are leafs.
            self.moves.save();

            // We always try the hash move first.
            state.phase = NodePhase::TriedHashMove;
            if state.entry.move16() != 0 {
                if let Some(m) = self.position.try_move_digest(state.entry.move16()) {
                    if self.position.do_move(m) {
                        return Some(m);
                    }
                }
            }
        }

        if let NodePhase::TriedHashMove = state.phase {
            // After the hash move, we generate all pseudo-legal
            // moves. But we should not forget to remove the already
            // tried hash move from the list.
            self.position.generate_moves(self.moves);
            if state.entry.move16() != 0 {
                self.moves.remove_move(state.entry.move16());
            }
            state.phase = NodePhase::GeneratedMoves;
        }

        // For the last, we spit the generated moves out.
        while let Some(m) = self.moves.remove_best_move() {
            if self.position.do_move(m) {
                return Some(m);
            }
        }
        None
    }

    #[inline]
    pub fn undo_move(&mut self) {
        self.position.undo_move();
    }

    // From time to time, we report how many nodes had been searched
    // since the last report. This also gives an opportunity for the
    // search to be terminated.
    #[inline]
    pub fn report_progress(&mut self, new_nodes: NodeCount) -> Result<(), TerminatedSearch> {
        self.unreported_nodes += new_nodes;
        if self.unreported_nodes > NODE_COUNT_REPORT_INTERVAL {
            self.reported_nodes += self.unreported_nodes;
            self.unreported_nodes = 0;
            try!((*self.report_func)(self.reported_nodes));
        }
        Ok(())
    }

    #[inline]
    pub fn clear(&mut self) {
        self.moves.clear();
        self.state_stack.clear();
        self.reported_nodes = 0;
        self.unreported_nodes = 0;
    }
}


// Helper function for `run()`. It implements the principal variation
// search algorithm. When returning `Err(TerminatedSearch)`, this
// function may leave un-restored move lists in `moves`.
fn search(state: &mut SearchState,
          mut alpha: Value, // lower bound
          beta: Value, // upper bound
          depth: u8)
          -> Result<Value, TerminatedSearch> {
    assert!(alpha < beta);

    let entry = state.node_begin();

    // Check if the TT entry gives the results.
    if entry.depth() >= depth {
        let value = entry.value();
        let bound = entry.bound();
        if (value >= beta && bound == BOUND_LOWER) || (value <= alpha && bound == BOUND_UPPER) ||
           (bound == BOUND_EXACT) {
            state.node_end();
            return Ok(value);
        }
    }

    // Initial guests for the final result.
    let mut bound_type = BOUND_UPPER;
    let mut best_move = Move::invalid();

    if depth == 0 {
        // On leaf nodes, do quiescence search.
        let (value, nodes) = state.position()
                                  .evaluate_quiescence(alpha, beta, Some(entry.eval_value()));
        try!(state.report_progress(nodes));
        if value >= beta {
            alpha = beta;
            bound_type = BOUND_LOWER;
        } else if value > alpha {
            alpha = value;
            bound_type = BOUND_EXACT;
        }

    } else {
        // On non-leaf nodes, try moves and make recursive calls.
        let mut no_moves_yet = true;

        // Try some moves.
        while let Some(m) = state.do_move() {
            try!(state.report_progress(1));

            // Make a recursive call.
            let value = if no_moves_yet {
                // The first move we analyze with a fully open
                // window (alpha, beta).
                -try!(search(state, -beta, -alpha, depth - 1))
            } else {
                // For the next moves we first try to prove that
                // they are not better than our current best
                // move. For this purpose we analyze them with a
                // null window (alpha, alpha + 1). This is faster
                // than a full window search. Only when we are
                // certain that the move is better than our
                // current best move, we do a full-window search.
                match -try!(search(state, -alpha - 1, -alpha, depth - 1)) {
                    x if x <= alpha => x,
                    _ => -try!(search(state, -beta, -alpha, depth - 1)),
                }
            };

            // See how good this move is.
            state.undo_move();
            no_moves_yet = false;
            if value >= beta {
                // This move is too good, so that the opponent
                // will not allow this line of play to
                // happen. Therefore we can stop here.
                alpha = beta;
                bound_type = BOUND_LOWER;
                best_move = m;
                break;
            }
            if value > alpha {
                // We found ourselves a new best move.
                alpha = value;
                bound_type = BOUND_EXACT;
                best_move = m;
            }
        }

        // Check if we are in a final position (no legal moves). Then
        // we are done.
        if no_moves_yet {
            // Final positions we can evaluate 100% correctly.
            alpha = state.position.evaluate_final();
            bound_type = BOUND_EXACT;
        }
    }

    state.node_store(alpha, bound_type, depth, best_move);
    state.node_end();
    Ok(alpha)
}


#[cfg(test)]
mod tests {
    use super::{search, SearchState};
    use chess_move::*;
    use tt::*;
    use position::Position;

    #[test]
    fn test_search() {
        let p = Position::from_fen("8/8/8/8/3q3k/7n/6PP/2Q2R1K b - - 0 1").ok().unwrap();
        let mut state = SearchState {
            tt: &TranspositionTable::new(),
            position: p,
            moves: &mut MoveStack::new(),
            state_stack: vec![],
            reported_nodes: 0,
            unreported_nodes: 0,
            report_func: &mut |_| Ok(()),
        };
        let value = search(&mut state, -30000, 30000, 2)
                        .ok()
                        .unwrap();
        assert!(value < -300);
        state.clear();
        let value = search(&mut state, -30000, 30000, 4)
                        .ok()
                        .unwrap();
        assert!(value >= 20000);
    }
}
