//! This module implements a database that stores results of
//! previously performed searches.
//!
//! Using such a database (called "transposition table") is a way to
//! greatly reduce the search space of a chess tree with little
//! negative impact. Chess programs, during their brute-force search,
//! encounter the same positions again and again, but from different
//! sequences of moves, which is called a transposition.
//!
//! When the search encounters a transposition, it is beneficial to
//! "remember" what was determined last time the position was
//! examined, rather than redoing the entire search again. For this
//! reason, chess programs have a transposition table, which is a
//! large hash table storing information about positions previously
//! searched, how deeply they were searched, and what we concluded
//! about them.

use std;
use std::cell::UnsafeCell;
use std::mem::transmute;


/// The value is void.
pub const BOUND_NONE: u8 = 0;

/// All-Node, the value is "Upper Bound".
pub const BOUND_UPPER: u8 = 0b10;

/// Cut-Node, the value is "Lower Bound".
pub const BOUND_LOWER: u8 = 0b01;

/// PV-Node, the value is "Exact".
pub const BOUND_EXACT: u8 = BOUND_UPPER | BOUND_LOWER;


/// Stores information about a particular position.
#[derive(Copy, Clone)]
pub struct EntryData {
    move16: u16,
    value: i16,
    eval_value: i16,
    gen_bound: u8,
    depth: u8,
}


impl EntryData {
    /// Creates a new instance.
    ///
    /// The following information can be stored:
    /// 
    /// * `value` -- the value assigned to the position,
    /// 
    /// * `bound` -- the meaning of the assigned value (`BOUND_EXACT`,
    ///    `BOUND_LOWER`, `BOUND_UPPER`, or `BOUND_NONE`),
    /// 
    /// * `depth` -- the depth of search,
    /// 
    /// * `move16` -- best or refutation move,
    /// 
    /// * `eval_value` -- the calculated static evaluation for the
    ///    position.
    pub fn new(value: i16, bound: u8, depth: u8, move16: u16, eval_value: i16) -> EntryData {
        assert!(bound <= 0b11);
        assert!(depth < 128);
        EntryData {
            move16: move16,
            value: value,
            eval_value: eval_value,
            gen_bound: bound, // Stores the entry's generation and the bound.
            depth: depth,
        }
    }

    #[inline(always)]
    pub fn move16(&self) -> u16 {
        self.move16
    }

    #[inline(always)]
    pub fn value(&self) -> i16 {
        self.value
    }

    #[inline(always)]
    pub fn eval_value(&self) -> i16 {
        self.eval_value
    }

    #[inline(always)]
    pub fn bound(&self) -> u8 {
        self.gen_bound & 0b11
    }

    #[inline(always)]
    pub fn depth(&self) -> u8 {
        self.depth
    }
}


// Represents transposition table entry.
//
// It is 16 bytes, defined as below:
//
// * key        64 bit
// * move16     16 bit
// * value      16 bit
// * eval value 16 bit
// * generation  6 bit
// * bound type  2 bit
// * depth       8 bit
#[derive(Copy, Clone)]
struct Entry {
    key: u64,
    data: EntryData,
}


impl Default for Entry {
    fn default() -> Entry {
        Entry {
            key: 0,
            data: unsafe { transmute(0u64) },
        }
    }
}


impl Entry {
    // Returns the whole contained data as one `u64` value.
    //
    // This is needed for implementing the lock-less probing and
    // storing.
    #[inline(always)]
    fn data_u64(&self) -> u64 {
        unsafe { transmute(self.data) }
    }

    // Returns entry's generation.
    #[inline(always)]
    fn generation(&self) -> u8 {
        self.data.gen_bound & 0b11111100
    }

    // Updates entry's generation.
    // 
    // Since the `key` is saved xored with the data, when we change
    // the data, we have to change the stored `key` as well.
    #[inline(always)]
    fn update_generation(&mut self, generation: u8) {
        assert_eq!(generation & 0b11, 0);
        let old_data_u64 = self.data_u64();
        self.data.gen_bound = generation | self.data.bound();
        self.key ^= old_data_u64 ^ self.data_u64();
    }
}


/// A large hash-table that stores results of previously performed
/// searches.
pub struct TranspositionTable {
    generation: u8,
    cluster_count: usize,
    table: UnsafeCell<Vec<[Entry; 4]>>,
}


impl TranspositionTable {
    /// Creates a new transposition table.
    ///
    /// The newly created table has the minimum possible size. Before
    /// using the new table for anything, `resize()` should be called
    /// on it, specifying the desired size.
    pub fn new() -> TranspositionTable {
        TranspositionTable {
            generation: 0,
            cluster_count: 1,
            table: UnsafeCell::new(vec![Default::default()]),
        }
    }

    /// Signals that a new search is about to begin.
    ///
    /// Internally, the transposition table maintains a generation
    /// counter that is used to implement an efficient replacement
    /// strategy. This method increases this counter.
    pub fn new_search(&mut self) {
        self.generation += 0b100;  // Lower 2 bits are used by bound type
        assert_eq!(self.generation & 0b11, 0);
    }

    /// Resizes the transpositon table.
    ///
    /// `size_mb` is the desired new size in Mbytes. All entries in
    /// the table will be lost.
    pub fn resize(&mut self, size_mb: usize) {
        // The cluster count should be in the form "2**n", so the best
        // we can do is to ensure that the new cluster count will be
        // as close as possible but no greater than the requested one.
        let requested_cluster_count = (size_mb * 1024 * 1024) / std::mem::size_of::<[Entry; 4]>();

        // First, make sure `requested_cluster_count` is exceeded.
        let mut new_cluster_count = 1;
        while requested_cluster_count >= new_cluster_count {
            new_cluster_count <<= 1;
        }

        // Then make one step back, being careful so that the new
        // cluster count is not zero.
        if new_cluster_count > 1 {
            new_cluster_count >>= 1;
        } else {
            new_cluster_count = 1;
        }

        // Finally, reallocate the vector of clusters.
        if new_cluster_count != self.cluster_count {
            self.cluster_count = new_cluster_count;
            self.table = UnsafeCell::new(vec![Default::default(); new_cluster_count]);
        }
    }

    /// Clears the transpositon table.
    ///
    /// All remaining entries in the table will be removed.
    pub fn clear(&mut self) {
        self.table = UnsafeCell::new(vec![Default::default(); self.cluster_count]);
    }

    /// Probes for data by a specific key.
    #[inline]
    pub fn probe(&self, key: u64) -> Option<EntryData> {
        let cluster = unsafe { self.cluster_mut(key) };
        for entry in cluster.iter_mut() {
            if entry.key ^ entry.data_u64() == key {
                // If `key` and `data` were written simultaneously by
                // different search instances with different keys,
                // this will yield in a mismatch of the above
                // comparison (except for the rare and inherent key
                // collisions).
                entry.update_generation(self.generation);
                return Some(entry.data);
            }
        }
        None
    }

    /// Stores data by a specific key.
    ///
    /// After being stored, the data might be retrieved by
    /// `probe(key)`. This is not guaranteed though, because in the
    /// meantime it might have been overwritten.
    #[inline]
    pub fn store(&self, key: u64, mut data: EntryData) {
        // `store` and `probe` jointly implement a clever lock-less
        // hashing method. Rather than to store two disjoint items,
        // the key is stored xored with data, while data is stored
        // additionally as usual.
        
        data.gen_bound |= self.generation;  // Sets the generation.
        let mut cluster = unsafe { self.cluster_mut(key) };
        let mut replace_index = 0;
        let mut replace_score = 0xff;
        for (i, entry) in cluster.iter_mut().enumerate() {
            // Check if this is an empty slot, or an old entry for the
            // same key. If this this is the case we will use this
            // slot for the new entry.
            if entry.key == 0 || entry.key ^ entry.data_u64() == key {
                if data.move16 == 0 {
                    data.move16 = entry.data.move16;  // Preserve any existing move.
                }
                replace_index = i;
                break;
            }
            // If we can not find empty/old slot, the replaced entry
            // will be the entry with the lowest score.
            let entry_score = self.calc_score(entry);
            if entry_score < replace_score {
                replace_index = i;
                replace_score = entry_score;
            }
        }
        unsafe {
            cluster.get_unchecked_mut(replace_index).key = key ^ transmute::<EntryData, u64>(data);
            cluster.get_unchecked_mut(replace_index).data = data;
        }
    }

    // A helper method for `store`.
    //
    // Implements our replacement strategy. Should return higher
    // values for the entries that are move likely to save CPU work in
    // the future.
    #[inline]
    fn calc_score(&self, entry: &Entry) -> u8 {
        (if entry.generation() == self.generation {
            128
        } else {
            0
        }) + entry.data.depth
    }

    // A helper method for `probe` and `store`.
    #[inline]
    unsafe fn cluster_mut(&self, key: u64) -> &mut [Entry; 4] {
        let cluster_index = (key & (self.cluster_count - 1) as u64) as usize;
        self.table.get().as_mut().unwrap().get_unchecked_mut(cluster_index)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::Entry;
    use std;

    #[test]
    fn test_cluster_size() {
        assert_eq!(std::mem::size_of::<[Entry; 4]>(), 64);
        assert_eq!(std::mem::size_of::<Entry>(), 16);
    }

    #[test]
    fn test_tt_resize() {
        let mut tt = TranspositionTable::new();
        assert_eq!(unsafe { &*tt.table.get() }.capacity(), 1);
        tt.resize(1);
        assert_eq!(unsafe { &*tt.table.get() }.capacity(),
                   1024 * 1024 / std::mem::size_of::<[Entry; 4]>());
        tt.clear();
    }

    #[test]
    fn test_store_and_probe() {
        let mut tt = TranspositionTable::new();
        assert!(tt.probe(1).is_none());
        let data = EntryData::new(0, 0, 100, 666, 0);
        assert_eq!(data.depth(), 100);
        assert_eq!(data.move16(), 666);
        tt.store(1, data);
        assert_eq!(tt.probe(1).unwrap().depth(), 100);
        tt.store(1, EntryData::new(0, 0, 100, 666, 0));
        assert_eq!(tt.probe(1).unwrap().depth(), 100);
        assert_eq!(tt.probe(1).unwrap().move16(), 666);
        for i in 2..100 {
            tt.store(i, EntryData::new(i as i16, 0, i as u8, i as u16, i as i16));
        }
        assert_eq!(tt.probe(1).unwrap().depth(), 100);
        assert_eq!(tt.probe(99).unwrap().depth(), 99);
        assert_eq!(tt.probe(98).unwrap().depth(), 98);
        assert_eq!(tt.probe(97).unwrap().depth(), 97);
        tt.clear();
        assert!(tt.probe(1).is_none());
        tt.store(1, data);
        tt.new_search();
        tt.probe(1);
        assert!(tt.probe(1).is_some());
    }
}
