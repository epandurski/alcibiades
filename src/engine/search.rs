use basetypes::*;
use position::{Position, Value};


pub fn search(p: Position,
              alpha: Value, // lower bound
              beta: Value, // upper bound
              depth: usize,
              send_node_count: &Fn(NodeCount) -> bool) {

}
