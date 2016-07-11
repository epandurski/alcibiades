use std::thread;
use std::cell::UnsafeCell;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver, RecvError};
use basetypes::*;
use chess_move::{MoveStack, MoveDigest};
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


// Represents a terminated search condition.
struct TerminatedSearch;


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
    if depth == 0 {
        // On leaf nodes, do quiescence search.
        let (value, nodes) = p.evaluate_quiescence(alpha, beta, None);
        *nc += nodes;
        Ok(value)
    } else {
        // Consult the transposition table.
        let hash_move16 = if let Some(entry) = tt.probe(p.hash()) {
            if entry.depth() >= depth && entry.bound() == BOUND_EXACT {
                // We already know the exact value for this position
                // for same depth or higher.
                return Ok(entry.value());
            }
            entry.move_digest()
        } else {
            MoveDigest::invalid()
        };

        moves.save();

        if hash_move16.is_valid() {
            if let Some(m) = p.try_move16(hash_move16) {
                moves.push(m);
            }
        }
        let mut generated = false;

        let mut bound_type = BOUND_UPPER;
        let mut move16 = MoveDigest::invalid();
        let mut no_moves_yet = true;
        loop {
            // TODO: This is ugly, and probably inefficient.
            let m = match moves.remove_best_move() {
                None if generated => break,
                None => {
                    p.generate_moves(moves);
                    moves.remove_move(hash_move16);
                    generated = true;
                    continue;
                },
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
                no_moves_yet = false;
                p.undo_move();

                if value >= beta {
                    // This move is too good, so that the opponent
                    // will not allow this line of play to
                    // happen. Therefore we can stop here.
                    alpha = beta;
                    bound_type = BOUND_LOWER;
                    move16 = m.digest();
                    break;
                }
                if value > alpha {
                    // We found ourselves a new best move.
                    alpha = value;
                    bound_type = BOUND_EXACT;
                    move16 = m.digest();
                }
            }
        }
        moves.restore();
        if no_moves_yet {
            // No legal moves -- this is a final position.
            alpha = p.evaluate_final();
            bound_type = BOUND_EXACT;
        }
        // TODO: Do not store values with too small `depth`.
        tt.store(p.hash(),
                 EntryData::new(alpha, bound_type, depth, move16, 0));
        Ok(alpha)
    }
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
                           -20000,
                           20000,
                           2)
                        .ok()
                        .unwrap();
        assert!(value < -300);
        let value = search(&TranspositionTable::new(),
                           &mut p,
                           &mut MoveStack::new(),
                           &mut 0,
                           &mut |_| Ok(()),
                           -20000,
                           20000,
                           3)
                        .ok()
                        .unwrap();
        assert!(value >= 20000);
    }
}
