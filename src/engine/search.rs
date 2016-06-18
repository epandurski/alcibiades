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
              report_node_count: &Fn(NodeCount) -> bool)
              -> Value {
    if depth == 0 {
        let (value, nodes) = p.evaluate_quiescence(alpha, beta);
        *node_count += nodes;
        value
    } else {
        // TODO: use something faster than `Vec`.
        let mut move_stack = MoveStack::new();
        move_stack.save();
        p.generate_moves(&mut move_stack);
        while let Some(next_move) = move_stack.remove_best_move() {
            if p.do_move(next_move) {
                *node_count += 1;
                let value = -search(tt,
                                    p,
                                    -beta,
                                    -alpha,
                                    depth - 1,
                                    node_count,
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
