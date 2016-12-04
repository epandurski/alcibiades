//! Implements `MoveStack`.

use std::slice;
use chesstypes::*;


/// Stores a list of moves for each position in a given line of play.
pub struct MoveStack {
    moves: Vec<Move>,
    savepoints: Vec<usize>,
    first_move_index: usize,
}


impl PushMove for MoveStack {
    /// Appends a move to the end of the current move list.
    #[inline]
    fn push_move(&mut self, m: Move) {
        debug_assert!(self.moves.len() >= self.first_move_index);
        self.moves.push(m);
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

    /// Returns the number of moves in the current move list.
    #[inline]
    pub fn len(&self) -> usize {
        debug_assert!(self.moves.len() >= self.first_move_index);
        self.moves.len() - self.first_move_index
    }

    /// Removes the last move from the current move list and returns
    /// it, or `None` if empty.
    #[inline]
    pub fn pop(&mut self) -> Option<Move> {
        debug_assert!(self.moves.len() >= self.first_move_index);
        if self.moves.len() > self.first_move_index {
            self.moves.pop()
        } else {
            None
        }
    }

    /// Removes a specific move from the current move list and returns
    /// it.
    ///
    /// This method tries to find a move `m` for which `m.digest() ==
    /// move_digest`. Then it removes it from the current move list,
    /// and returns it. If such move is not found, `None` is returned.
    #[inline]
    pub fn remove(&mut self, move_digest: MoveDigest) -> Option<Move> {
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
            for curr in self.iter_mut() {
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
    /// Returns `None` if the current move list is empty.
    #[inline]
    pub fn remove_best(&mut self) -> Option<Move> {
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

    /// Returns an iterator over each move in the current move list.
    #[inline]
    pub fn iter(&self) -> slice::Iter<Move> {
        self.moves[self.first_move_index..].iter()
    }

    /// Returns an iterator that allows modifying each move in the
    /// current move list.
    #[inline]
    pub fn iter_mut(&mut self) -> slice::IterMut<Move> {
        self.moves[self.first_move_index..].iter_mut()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use chesstypes::*;
    const NO_ENPASSANT_FILE: usize = 8;

    #[test]
    fn test_move_stack() {
        let m = Move::new(MOVE_NORMAL,
                          E2,
                          E4,
                          0,
                          NO_PIECE,
                          PAWN,
                          CastlingRights::new(0),
                          NO_ENPASSANT_FILE,
                          0);
        let mut s = MoveStack::new();
        assert!(s.remove_best().is_none());
        s.save();
        s.push_move(m);
        assert_eq!(s.remove_best().unwrap(), m);
        assert!(s.remove_best().is_none());
        s.restore();
        assert!(s.remove_best().is_none());
        s.push_move(m);
        s.push_move(m);
        s.save();
        s.push_move(m);
        s.restore();
        assert_eq!(s.remove_best().unwrap(), m);
        assert_eq!(s.remove_best().unwrap(), m);
        assert!(s.remove_best().is_none());
    }
}
