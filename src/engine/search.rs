use basetypes::*;
use chess_move::MoveStack;
use tt::*;
use position::{Position, Value};


pub fn search(tt: &TranspositionTable,
              p: &mut Position,
              mut alpha: Value, // lower bound
              beta: Value, // upper bound
              depth: usize,
              node_count: &mut NodeCount,
              move_stack: &mut MoveStack,
              report_node_count: &Fn(NodeCount) -> bool)
              -> Value {
    if depth == 0 {
        let (value, nodes) = p.evaluate_quiescence(alpha, beta);
        *node_count += nodes;
        value
    } else {
        move_stack.save();
        p.generate_moves(move_stack);
        while let Some(m) = move_stack.remove_best_move() {
            if p.do_move(m) {
                *node_count += 1;
                let value = -search(tt,
                                    p,
                                    -beta,
                                    -alpha,
                                    depth - 1,
                                    node_count,
                                    move_stack,
                                    report_node_count);
                p.undo_move();
                if value >= beta {
                    alpha = value;
                    break;
                }
            }

        }
        move_stack.restore();
        alpha
    }
}
