//! Implements `DummyHashTable`.

use hash_table::*;
use stock::std_hash_table_entry::StdHashTableEntry;


/// Implements a hash table that never stores anything.
///
/// This is useful when the search algorithm does not use a hash
/// table.
pub struct DummyHashTable;

impl HashTable for DummyHashTable {
    type Entry = StdHashTableEntry;

    fn new(_: Option<usize>) -> DummyHashTable {
        DummyHashTable
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

unsafe impl Sync for DummyHashTable {}
