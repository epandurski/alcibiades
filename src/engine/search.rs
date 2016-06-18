use basetypes::*;
use chess_move::MoveStack;
use tt::*;
use position::Position;


/// Represents a terminated search condition.
pub struct TerminatedSearch;


pub fn search(tt: &TranspositionTable,
              p: &mut Position,
              moves: &mut MoveStack,
              nc: &mut NodeCount,
              report: &Fn(NodeCount) -> Result<(), TerminatedSearch>,
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
        tt.store(p.hash(), EntryData::new(alpha, bound_type, depth, move16, 0));
        Ok(alpha)
    }
}


const NODE_COUNT_REPORT_INTERVAL: NodeCount = 10000;