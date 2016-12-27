//! Implements `MoveStack`.

use moves::{Move, MoveDigest, AddMove};


/// Stores a list of moves for each position in a given line of play.
pub struct MoveStack {
    moves: Vec<Move>,
    savepoints: Vec<usize>,
    first_move_index: usize,
}


impl AddMove for MoveStack {
    /// Appends a move to the end of the current move list.
    #[inline(always)]
    fn add_move(&mut self, m: Move) {
        self.push(m);
    }
}


impl MoveStack {
    /// Creates a new (empty) instance.
    pub fn new() -> MoveStack {
        MoveStack {
            moves: Vec::with_capacity(32 * 64),
            savepoints: Vec::with_capacity(32),
            first_move_index: 0,
        }
    }

    /// Saves the current move list and replaces it with an empty one.
    ///
    /// This method can be called many times. At each call the current
    /// move list will be saved to the stack of lists that can later
    /// be restored. After calling `save` the new current move list
    /// will be empty.
    #[inline]
    pub fn save(&mut self) {
        self.savepoints.push(self.first_move_index);
        self.first_move_index = self.moves.len();
    }

    /// Restores the last saved move list.
    ///
    /// The current move list is lost.
    ///
    /// # Panics
    ///
    /// Panics if there are no saved move lists left.
    #[inline]
    pub fn restore(&mut self) {
        self.moves.truncate(self.first_move_index);
        self.first_move_index = self.savepoints.pop().unwrap();
    }

    /// Returns the number of saved move lists.
    ///
    /// The number of saved move lists starts at zero. It is
    /// incremented on each call to `save`, and decremented on each
    /// call to `restore`.
    #[inline]
    pub fn ply(&self) -> usize {
        self.savepoints.len()
    }

    /// Clears the current move list, removing all moves from it.
    #[inline]
    pub fn clear(&mut self) {
        self.moves.truncate(self.first_move_index);
    }

    /// Clears the current move list and deletes all saved move lists.
    #[inline]
    pub fn clear_all(&mut self) {
        self.moves.clear();
        self.savepoints.clear();
        self.first_move_index = 0;
    }

    /// Appends a move to the end of the current move list.
    #[inline]
    pub fn push(&mut self, m: Move) {
        debug_assert!(self.moves.len() >= self.first_move_index);
        self.moves.push(m);
    }

    /// Removes the last move from the current move list and returns it.
    ///
    /// If the current move list is empty, `None` is returned.
    #[inline]
    pub fn pop(&mut self) -> Option<Move> {
        debug_assert!(self.moves.len() >= self.first_move_index);
        if self.moves.len() > self.first_move_index {
            self.moves.pop()
        } else {
            None
        }
    }

    /// Removes the move at given index from the current move list and
    /// returns it.
    ///
    /// Move's slot is taken by the last move in the current list, and
    /// the last slot is discarded.
    ///
    /// # Panics
    ///
    /// Panics if there is no move at the given index.
    #[inline]
    pub fn pull(&mut self, index: usize) -> Move {
        let last_move = *self.moves.last().unwrap();
        let m;
        {
            let requested_slot = &mut self.moves[self.first_move_index + index];
            m = *requested_slot;
            *requested_slot = last_move;
        }
        self.moves.pop();
        m
    }

    /// Removes a specific move from the current move list and returns it.
    ///
    /// This method tries to find a move `m` for which `m.digest() ==
    /// move_digest`. If such move is found, `Some(m)` is returned,
    /// move's slot is taken by the last move in the current list, and
    /// the last slot is discarded. If no such move is found, `None`
    /// is returned.
    #[inline]
    pub fn pull_move(&mut self, move_digest: MoveDigest) -> Option<Move> {
        debug_assert!(self.moves.len() >= self.first_move_index);

        // The last move in `self.moves` will take the place of the
        // removed move.
        let last_move = if let Some(last) = self.moves.last() {
            *last
        } else {
            return None;
        };

        let m;
        'moves: loop {
            for curr in self.list_mut().iter_mut() {
                if curr.digest() == move_digest {
                    m = *curr;
                    *curr = last_move;
                    break 'moves;
                }
            }
            return None;
        }
        debug_assert!(!self.moves.is_empty());
        self.moves.pop();
        Some(m)
    }

    /// Removes the move with the highest value from the current move
    /// list and returns it.
    ///
    /// Best move's slot is taken by the last move in the current
    /// list, and the last slot is discarded. `None` is returned if
    /// the current move list is empty.
    #[inline]
    pub fn pull_best(&mut self) -> Option<Move> {
        debug_assert!(self.moves.len() >= self.first_move_index);
        let moves = &mut self.moves;
        if moves.len() > self.first_move_index {
            let last = moves.len() - 1;
            unsafe {
                let mut best_move = *moves.get_unchecked(last);
                let mut curr = last;
                loop {
                    if *moves.get_unchecked(curr) > best_move {
                        // Swap the new best move candidate (`curr`)
                        // with the previous candidate (`last`).
                        *moves.get_unchecked_mut(last) = *moves.get_unchecked_mut(curr);
                        *moves.get_unchecked_mut(curr) = best_move;
                        best_move = *moves.get_unchecked(last);
                    }
                    if curr == self.first_move_index {
                        break;
                    }
                    curr -= 1;
                }
                moves.pop();
                return Some(best_move);
            }
        }
        None
    }

    /// Returns the current move list as a slice.
    #[inline]
    pub fn list(&self) -> &[Move] {
        debug_assert!(self.moves.len() >= self.first_move_index);
        &self.moves[self.first_move_index..]
    }

    /// Returns the current move list as a mutable slice.
    #[inline]
    pub fn list_mut(&mut self) -> &mut [Move] {
        debug_assert!(self.moves.len() >= self.first_move_index);
        &mut self.moves[self.first_move_index..]
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use board::*;
    use squares::*;
    use moves::*;

    #[test]
    fn test_move_stack() {
        let cr = CastlingRights::new(0);
        let m = Move::new(MOVE_NORMAL, E2, E4, 0, PIECE_NONE, PAWN, cr, 8, 0);
        let mut s = MoveStack::new();
        assert_eq!(s.ply(), 0);
        assert!(s.pull_best().is_none());
        s.save();
        assert_eq!(s.ply(), 1);
        s.push(m);
        assert_eq!(s.pull_best().unwrap(), m);
        assert!(s.pull_best().is_none());
        s.restore();
        assert!(s.pull_best().is_none());
        assert_eq!(s.list().len(), 0);
        assert!(s.pop().is_none());
        assert!(s.pull_move(m.digest()).is_none());
        s.push(m);
        s.push(m);
        assert_eq!(s.list().len(), 2);
        assert_eq!(s.pop().unwrap(), m);
        assert_eq!(s.list().len(), 1);
        s.push(m);
        assert_eq!(s.pull_move(m.digest()).unwrap(), m);
        assert_eq!(s.list().len(), 1);
        s.push(m);
        assert_eq!(s.list().iter().count(), 2);
        s.save();
        s.push(m);
        s.restore();
        assert_eq!(s.pull_best().unwrap(), m);
        assert_eq!(s.pull_best().unwrap(), m);
        assert!(s.pull_best().is_none());
        assert_eq!(s.ply(), 0);
        s.push(m);
        s.clear();
        assert_eq!(s.ply(), 0);
        assert_eq!(s.list().len(), 0);
        s.save();
        s.save();
        s.push(m);
        assert_eq!(s.ply(), 2);
        s.clear_all();
        assert_eq!(s.ply(), 0);
        assert_eq!(s.list().len(), 0);
    }
}
