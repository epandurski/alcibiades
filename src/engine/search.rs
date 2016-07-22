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
                Command::Search { search_id, mut position, depth, lower_bound, upper_bound } => {
                    let mut reported_nodes = 0;
                    let mut unreported_nodes = 0;
                    let value = search(&tt,
                                       &mut position,
                                       move_stack,
                                       &mut unreported_nodes,
                                       &mut |n| {
                                           reported_nodes += n;
                                           reports.send(Report::Progress {
                                                      search_id: search_id,
                                                      searched_nodes: reported_nodes,
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
                                       lower_bound,
                                       upper_bound,
                                       depth)
                                    .ok();
                    reported_nodes += unreported_nodes;
                    reports.send(Report::Progress {
                               search_id: search_id,
                               searched_nodes: reported_nodes,
                               depth: depth,
                           })
                           .ok();
                    reports.send(Report::Done {
                               search_id: search_id,
                               searched_nodes: reported_nodes,
                               value: value,
                           })
                           .ok();
                    move_stack.clear();
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


struct SearchState<'a> {
    tt: &'a TranspositionTable,
    position: Position,
    played_move: bool,
    moves: &'a mut MoveStack,
    state_stack: Vec<NodeState>,
    node_count: NodeCount,
    report_func: &'a mut FnMut(NodeCount) -> Result<(), TerminatedSearch>,
}


impl<'a> SearchState<'a> {
    #[inline(always)]
    pub fn position(&self) -> &Position {
        &self.position
    }

    #[inline]
    pub fn report_nodes(&mut self, n: NodeCount) -> Result<(), TerminatedSearch> {
        self.node_count += n;
        if self.node_count > NODE_COUNT_REPORT_INTERVAL {
            try!((*self.report_func)(self.node_count));
            self.node_count = 0;
        }
        Ok(())
    }

    #[inline]
    pub fn do_move(&mut self) -> Option<Move> {
        assert!(!self.played_move);
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
                        self.played_move = true;
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
                self.played_move = true;
                return Some(m);
            }
        }
        None
    }

    #[inline]
    pub fn undo_move(&mut self) {
        self.position.undo_move();
        if self.played_move {
            // We do not leave the current node.
            self.played_move = false;
        } else {
            // We go back to the parent node.
            if let NodePhase::Pristine = self.state_stack.last().unwrap().phase {
                // For pristine nodes we have not saved the move list
                // yet, so we should not restore it.
            } else {
                self.moves.restore();
            }
            self.state_stack.pop();
        }
    }

    #[inline]
    fn new_node(&mut self) -> EntryData {
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
}


// Helper function for `run()`. It implements the principal variation
// search algorithm. When returning `Err(TerminatedSearch)`, this
// function may leave un-restored move lists in `moves`.
fn search(tt: &TranspositionTable,
          p: &mut Position,
          moves: &mut MoveStack,
          nc: &mut NodeCount,
          report: &mut FnMut(NodeCount) -> Result<(), TerminatedSearch>,
          mut alpha: Value, // lower bound
          beta: Value, // upper bound
          depth: u8)
          -> Result<Value, TerminatedSearch> {
    assert!(alpha < beta);

    // Consult the transposition table.
    let (move16, eval_value) = if let Some(entry) = tt.probe(p.hash()) {
        if entry.depth() >= depth {
            let value = entry.value();
            let bound = entry.bound();
            if (value >= beta && bound == BOUND_LOWER) ||
               (value <= alpha && bound == BOUND_UPPER) || (bound == BOUND_EXACT) {
                return Ok(value);
            }
        }
        (entry.move16(), entry.eval_value())
    } else {
        (0, p.evaluate_static())
    };

    // Initial guests for the final result.
    let mut bound_type = BOUND_UPPER;
    let mut best_move = Move::invalid();

    if depth == 0 {
        // On leaf nodes, do quiescence search.
        let (value, nodes) = p.evaluate_quiescence(alpha, beta, Some(eval_value));
        *nc += nodes;
        if value >= beta {
            alpha = beta;
            bound_type = BOUND_LOWER;
        } else if value > alpha {
            alpha = value;
            bound_type = BOUND_EXACT;
        }

    } else {
        // On non-leaf nodes, try moves and make recursive calls.
        moves.save();
        let mut no_moves_yet = true;

        // TODO: This is ugly.
        if move16 != 0 {
            if let Some(m) = p.try_move_digest(move16) {
                moves.push(m);
            }
        }
        let mut generated = false;

        // Try some moves.
        loop {
            // TODO: This is ugly.
            let m = match moves.remove_best_move() {
                None if generated => break,
                None => {
                    p.generate_moves(moves);
                    moves.remove_move(move16);
                    generated = true;
                    continue;
                }
                Some(x) => x,
            };

            if p.do_move(m) {
                // From time to time, we report how many nodes had
                // been searched since the last report. This also
                // gives an opportunity for the search to be
                // terminated.
                *nc += 1;
                if *nc > NODE_COUNT_REPORT_INTERVAL {
                    try!(report(*nc));
                    *nc = 0;
                }

                // Make a recursive call.
                let value = if no_moves_yet {
                    // The first move we analyze with a fully open
                    // window (alpha, beta).
                    -try!(search(tt, p, moves, nc, report, -beta, -alpha, depth - 1))
                } else {
                    // For the next moves we first try to prove that
                    // they are not better than our current best
                    // move. For this purpose we analyze them with a
                    // null window (alpha, alpha + 1). This is faster
                    // than a full window search. Only when we are
                    // certain that the move is better than our
                    // current best move, we do a full-window search.
                    match -try!(search(tt, p, moves, nc, report, -alpha - 1, -alpha, depth - 1)) {
                        x if x <= alpha => x,
                        _ => -try!(search(tt, p, moves, nc, report, -beta, -alpha, depth - 1)),
                    }
                };

                // See how good this move is.
                p.undo_move();
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
        }

        // Check if we are in a final position (no legal moves). Then
        // we are done.
        if no_moves_yet {
            // Final positions we can evaluate 100% correctly.
            alpha = p.evaluate_final();
            bound_type = BOUND_EXACT;
        }
        moves.restore();
    }

    // Store the final result in the transposition table and return.
    tt.store(p.hash(),
             EntryData::new(alpha, bound_type, depth, best_move.digest(), eval_value));
    Ok(alpha)
}


const NODE_COUNT_REPORT_INTERVAL: NodeCount = 10000;


#[cfg(test)]
mod tests {
    use super::search;
    use chess_move::*;
    use tt::*;
    use position::Position;

    #[test]
    fn test_search() {
        let mut p = Position::from_fen("8/8/8/8/3q3k/7n/6PP/2Q2R1K b - - 0 1").ok().unwrap();
        let value = search(&TranspositionTable::new(),
                           &mut p,
                           &mut MoveStack::new(),
                           &mut 0,
                           &mut |_| Ok(()),
                           -30000,
                           30000,
                           2)
                        .ok()
                        .unwrap();
        assert!(value < -300);
        let value = search(&TranspositionTable::new(),
                           &mut p,
                           &mut MoveStack::new(),
                           &mut 0,
                           &mut |_| Ok(()),
                           -30000,
                           30000,
                           4)
                        .ok()
                        .unwrap();
        assert!(value >= 20000);
    }
}
