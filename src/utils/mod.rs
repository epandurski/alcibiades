//! Defines generally useful functions and types.

mod board_geometry;
mod zobrist_arrays;
mod move_stack;
mod notation;

pub use self::board_geometry::BoardGeometry;
pub use self::zobrist_arrays::ZobristArrays;
pub use self::move_stack::MoveStack;
pub use self::notation::parse_fen;


use move_generator::MoveGenerator;

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
pub fn perft<T: MoveGenerator>(position: &mut T, depth: u8) -> u64 {
    fn pft<T: MoveGenerator>(s: &mut MoveStack, p: &mut T, d: u8) -> u64 {
        if d == 0 {
            return 1;
        }
        let mut nodes = 0;
        s.save();
        p.generate_all(s);
        while let Some(m) = s.pop() {
            if p.do_move(m).is_some() {
                nodes += pft(s, p, d - 1);
                p.undo_move(m);
            }
        }
        s.restore();
        nodes
    }

    let mut s = MoveStack::new();
    pft(&mut s, position, depth)
}
