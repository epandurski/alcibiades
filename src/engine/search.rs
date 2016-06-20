use std::cell::UnsafeCell;
use std::sync::mpsc::{Sender, Receiver, RecvError, TryRecvError};
use basetypes::*;
use chess_move::MoveStack;
use tt::*;
use position::Position;


pub struct Parameters {
    id: usize,
    position: Position,
    depth: u8,
    lower_bound: Value,
    upper_bound: Value,
}


pub enum Command {
    Search(Parameters),
    Stop,
    Exit,
}


pub struct Progress {
    search_id: usize,
    searched_nodes: NodeCount,
}


pub struct Done {
    search_id: usize,
    value: Option<Value>,
}


#[allow(unused_must_use)]
pub fn run(tt: &TranspositionTable,
           commands: Receiver<Command>,
           reports: Sender<Progress>,
           results: Sender<Done>) {
    thread_local!(
        static MOVE_STACK: UnsafeCell<MoveStack> = UnsafeCell::new(MoveStack::new())
    );
    MOVE_STACK.with(|s| {
        let mut move_stack = unsafe { &mut *s.get() };
        let mut pending_command = None;
        loop {
            // If there is a pending command, we take it, otherwise we
            // block and wait to receive a new one.
            let command = pending_command.take()
                                         .unwrap_or(commands.recv()
                                                            .or::<RecvError>(Ok(Command::Exit))
                                                            .unwrap());
            match command {
                Command::Search(mut params) => {
                    let search_id = params.id;
                    let mut reported_nodes = 0;
                    let mut unreported_nodes = 0;
                    let value = search(tt,
                                       &mut params.position,
                                       move_stack,
                                       &mut unreported_nodes,
                                       &mut |n| {
                                           reported_nodes += n;
                                           reports.send(Progress {
                                               search_id: search_id,
                                               searched_nodes: reported_nodes,
                                           });
                                           match commands.try_recv() {
                                               Ok(x) => {
                                                   // There is a new command pending -- we
                                                   // should terminate the current search.
                                                   pending_command = Some(x);
                                                   Err(TerminatedSearch)
                                               }
                                               Err(_) => Ok(()),
                                           }
                                       },
                                       params.lower_bound,
                                       params.upper_bound,
                                       params.depth)
                                    .ok();
                    reports.send(Progress {
                        search_id: search_id,
                        searched_nodes: reported_nodes + unreported_nodes,
                    });
                    results.send(Done {
                        search_id: search_id,
                        value: value,
                    });
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
        let (value, nodes) = p.evaluate_quiescence(alpha, beta);
        *nc += nodes;
        Ok(value)
    } else {
        moves.save();
        p.generate_moves(moves);
        // TODO: Consult `tt` here.

        let mut bound_type = BOUND_UPPER;
        let mut move16 = 0;
        let mut no_moves_yet = true;
        while let Some(m) = moves.remove_best_move() {
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
                    move16 = m.move16();
                    break;
                }
                if value > alpha {
                    // We found ourselves a new best move.
                    alpha = value;
                    bound_type = BOUND_EXACT;
                    move16 = m.move16();
                }
            }
        }
        moves.restore();
        if no_moves_yet {
            // No legal moves -- this is a final position.
            alpha = p.evaluate_final();
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
    use super::*;
    use super::search;
    use basetypes::*;
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
