//! Implements `DummyTtable`.

use ttable::*;
use stock::std_ttable_entry::StdTtableEntry;


/// Implements a transposition table that never stores anything.
///
/// This is useful when the search algorithm does not use a
/// transposition table.
pub struct DummyTtable;

impl Ttable for DummyTtable {
    type Entry = StdTtableEntry;

    fn new(_: Option<usize>) -> DummyTtable {
        DummyTtable
    }

    /// Does nothing.
    fn new_search(&self) {}

    /// Does nothing.
    #[inline]
    fn store(&self, _: u64, _: Self::Entry) {}

    /// Returns `None`.
    #[inline]
    fn probe(&self, _: u64) -> Option<Self::Entry> {
        None
    }

    /// Does nothing.
    fn clear(&self) {}
}

unsafe impl Sync for DummyTtable {}
