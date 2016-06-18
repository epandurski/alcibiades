use basetypes::*;
use chess_move::MoveStack;
use tt::*;
use position::{Position, Value};


pub fn search(tt: &TranspositionTable,
              p: &mut Position,
              moves: &mut MoveStack,
              nc: &mut NodeCount,
              report_nc: &Fn(NodeCount) -> bool,
              mut alpha: Value, // lower bound
              beta: Value, // upper bound
              depth: usize)
              -> Value {
    if depth == 0 {
        let (value, nodes) = p.evaluate_quiescence(alpha, beta);
        *nc += nodes;
        value
    } else {
        moves.save();
        p.generate_moves(moves);
        let mut no_moves_yet = true;
        while let Some(m) = moves.remove_best_move() {
            if p.do_move(m) {
                let value = if no_moves_yet {
                    -search(tt, p, moves, nc, report_nc, -beta, -alpha, depth - 1)
                } else {
                    match -search(tt, p, moves, nc, report_nc, -alpha - 1, -alpha, depth - 1) {
                        x if x > alpha => {
                            -search(tt, p, moves, nc, report_nc, -beta, -alpha, depth - 1)
                        }
                        x => x,
                    }
                };
                p.undo_move();
                *nc += 1;
                no_moves_yet = false;
                if value >= beta {
                    alpha = value;
                    break;
                }
                if value > alpha {
                    alpha = value;
                }
            }
        }
        moves.restore();
        if no_moves_yet {
            p.evaluate_final()
        } else {
            alpha
        }
    }
}
