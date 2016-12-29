//! Implements `perft`.

use depth::*;
use move_generator::MoveGenerator;
use utils::MoveStack;


/// Performs move path enumeration.
///
/// `perft` is a debugging function to walk the move generation tree
/// of strictly legal moves and count all the leaf nodes of a certain
/// depth, which can be compared to predetermined values and used to
/// isolate bugs. In perft, nodes are only counted at the end after
/// the last `do_move`. Thus "higher" terminal nodes (e.g. mate or
/// stalemate) are not counted. Perft ignores draws by repetition, by
/// the fifty-move rule and by insufficient material. By recording the
/// amount of time taken for each iteration, it's possible to compare
/// the performance of different move generators or the same generator
/// on different machines.
pub fn perft<T: MoveGenerator>(generator: &mut T, depth: Depth) -> u64 {
    if depth <= 0 {
        return 1;
    }
    let mut nodes = 0;
    let mut s = MoveStack::new();
    generator.generate_all(&mut s);
    while let Some(m) = s.pop() {
        if generator.do_move(m).is_some() {
            nodes += perft(generator, depth - 1);
            generator.undo_move(m);
        }
    }
    nodes
}
