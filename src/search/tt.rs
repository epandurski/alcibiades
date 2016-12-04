//! Implements `HashTable` and `HashTableEntry` traits.

use std::cell::{UnsafeCell, Cell};
use std::cmp::min;
use std::mem::{transmute, size_of};
use chesstypes::*;
use search::*;


/// Contains information about a particular position.
#[derive(Copy, Clone, Debug)]
pub struct StandardTtEntry {
    value: Value,

    // The transposition table maintains a generation number for each
    // entry, which is used to implement an efficient replacement
    // strategy. This field stores the entry's generation (the highest
    // 6 bits) and the bound type (the lowest 2 bits).
    gen_bound: u8,

    depth: u8,
    move_digest: MoveDigest,
    eval_value: Value,
}

impl HashTableEntry for StandardTtEntry {
    fn new(value: Value,
           bound: BoundType,
           depth: u8,
           move_digest: MoveDigest,
           eval_value: Value)
           -> StandardTtEntry {
        debug_assert!(value != VALUE_UNKNOWN);
        debug_assert!(bound <= 0b11);
        debug_assert!(depth <= DEPTH_MAX);
        StandardTtEntry {
            value: value,
            gen_bound: bound,
            depth: depth,
            move_digest: move_digest,
            eval_value: eval_value,
        }
    }

    #[inline(always)]
    fn value(&self) -> Value {
        self.value
    }

    #[inline(always)]
    fn bound(&self) -> BoundType {
        self.gen_bound & 0b11
    }

    #[inline(always)]
    fn depth(&self) -> u8 {
        self.depth
    }

    #[inline(always)]
    fn move_digest(&self) -> MoveDigest {
        self.move_digest
    }

    /// Returns the `eval_value` passed to the constructor.
    #[inline(always)]
    fn eval_value(&self) -> Value {
        self.eval_value
    }
}

impl StandardTtEntry {
    /// Returns the contained data as one `u64` value.
    #[inline(always)]
    fn as_u64(&self) -> u64 {
        unsafe { transmute(*self) }
    }
}


/// A large hash-table that stores results of previously performed
/// searches ("transposition table").
pub struct StandardTt {
    /// The current generation number. The lowest 2 bits will always
    /// be zeros.
    generation: Cell<u8>,

    /// The number of clusters in the table.
    cluster_count: usize,

    /// The transposition table consists of a vector of clusters. Each
    /// cluster stores 4 records.
    table: UnsafeCell<Vec<[Record; 4]>>,
}

impl HashTable for StandardTt {
    type Entry = StandardTtEntry;

    fn new(size_mb: Option<usize>) -> StandardTt {
        let size_mb = size_mb.unwrap_or(16);
        let requested_cluster_count = (size_mb * 1024 * 1024) / size_of::<[Record; 4]>();

        // Calculate the cluster count. (To do this, first we make
        // sure that `requested_cluster_count` is exceeded. Then we
        // make one step back.)
        let mut n = 1;
        while n <= requested_cluster_count && n != 0 {
            n <<= 1;
        }
        if n > 1 {
            n >>= 1;
        } else {
            n = 1;
        }
        assert!(n > 0);

        StandardTt {
            generation: Cell::new(0),
            cluster_count: n,
            table: UnsafeCell::new(vec![Default::default(); n]),
        }
    }

    fn new_search(&self) {
        const N: usize = 128;

        loop {
            // Increment `self.generation` (with wrapping).
            self.generation.set(self.generation.get().wrapping_add(0b100));
            debug_assert_eq!(self.generation.get() & 0b11, 0);

            // Count how many staled records from this generation
            // there are among the first `N` clusters.
            let mut staled = 0;
            let mut cluster_iter = unsafe { &*self.table.get() }.iter();
            for _ in 0..min(N, self.cluster_count) {
                for record in cluster_iter.next().unwrap() {
                    if record.key != 0 && record.generation() == self.generation.get() {
                        staled += 1;
                    }
                }
            }

            if staled < N {
                // Note that we will continue to increment
                // `self.generation` if the staled records from this
                // generation are too many. (This may happen if a very
                // long search was executed long time ago.)
                break;
            }
        }
    }

    fn store(&self, key: u64, mut data: Self::Entry) {
        // `store` and `probe` jointly implement a clever lock-less
        // hashing strategy. Rather than storing two disjoint items,
        // the key is stored XOR-ed with data, while data is stored
        // additionally as usual.

        // Set entry's generation.
        data.gen_bound = self.generation.get() | data.bound();

        // Choose a slot to which to write the data. (Each cluster has
        // 4 slots.)
        let mut cluster = unsafe { self.cluster_mut(key) };
        let mut replace_index = 0;
        let mut replace_score = 0xff;
        for (i, record) in cluster.iter_mut().enumerate() {
            // Check if this is an empty slot, or an old record for
            // the same key. If this this is the case we will use this
            // slot for the new record.
            if record.key == 0 || record.key ^ record.data.as_u64() == key {
                if data.move_digest == 0 {
                    data.move_digest = record.data.move_digest; // Preserve any existing move.
                }
                replace_index = i;
                break;
            }

            // Calculate the score for this record. If we can not find
            // an empty slot or an old record, the replaced record
            // will be the record with the lowest score.
            let record_score = self.calc_score(record);
            if record_score < replace_score {
                replace_index = i;
                replace_score = record_score;
            }
        }

        // Write the data to the chosen slot.
        cluster[replace_index] = Record {
            key: key ^ data.as_u64(),
            data: data,
        };
    }

    #[inline]
    fn probe(&self, key: u64) -> Option<Self::Entry> {
        // `store` and `probe` jointly implement a clever lock-less
        // hashing strategy. Rather than storing two disjoint items,
        // the key is stored XOR-ed with data, while data is stored
        // additionally as usual.

        let cluster = unsafe { self.cluster_mut(key) };
        for record in cluster.iter_mut() {
            if record.key ^ record.data.as_u64() == key {
                // If `key` and `data` were written simultaneously by
                // different search instances with different keys,
                // this will yield in a mismatch of the above
                // comparison (except for the rare and inherent key
                // collisions).
                record.set_generation(self.generation.get());
                return Some(record.data);
            }
        }
        None
    }

    fn clear(&self) {
        let table = unsafe { &mut *self.table.get() };
        for cluster in table {
            for record in cluster.iter_mut() {
                *record = Default::default();
            }
        }
        self.generation.set(0);
    }
}

impl StandardTt {
    /// A helper method for `store`. It implements the record
    /// replacement strategy.
    #[inline(always)]
    fn calc_score(&self, record: &Record) -> u8 {
        // Here we try to return higher values for the records that
        // are move likely to save CPU work in the future:

        // Positions from the current generation are always scored
        // higher than positions from older generations.
        (if record.generation() == self.generation.get() {
            128
        } else {
            0
        }) 
            
        // Positions with higher search depths are scored higher.
        + record.data.depth()
            
        // Positions with exact evaluations are given slight advantage.
        + (if record.data.bound() == BOUND_EXACT {
            1
        } else {
            0
        })
    }

    /// A helper method for `probe` and `store`. It returns the
    /// cluster for a given key.
    #[inline(always)]
    unsafe fn cluster_mut(&self, key: u64) -> &mut [Record; 4] {
        let cluster_index = (key & (self.cluster_count - 1) as u64) as usize;
        &mut (&mut *self.table.get())[cluster_index]
    }
}

unsafe impl Sync for StandardTt {}


/// Represents a record in the transposition table.
///
/// It consists of 16 bytes, and is laid out the following way:
///
/// * key         64 bit
/// * move_digest 16 bit
/// * value       16 bit
/// * eval value  16 bit
/// * depth        8 bit
/// * generation   6 bit
/// * bound type   2 bit
#[derive(Copy, Clone)]
struct Record {
    key: u64,
    data: StandardTtEntry,
}

impl Default for Record {
    fn default() -> Record {
        Record {
            key: 0,
            data: unsafe { transmute(0u64) },
        }
    }
}

impl Record {
    #[inline(always)]
    fn generation(&self) -> u8 {
        self.data.gen_bound & 0b11111100
    }

    #[inline(always)]
    fn set_generation(&mut self, generation: u8) {
        debug_assert_eq!(generation & 0b11, 0);

        // Since the `key` is saved XOR-ed with the data, when we
        // change the data, we have to change the stored `key` as
        // well.
        let old_data_as_u64 = self.data.as_u64();
        self.data.gen_bound = generation | self.data.bound();
        self.key ^= old_data_as_u64 ^ self.data.as_u64();
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::Record;
    use std;
    use search::{HashTable, HashTableEntry, DEPTH_MAX};

    #[test]
    fn test_max_depth() {
        assert!(DEPTH_MAX < 127);
    }

    #[test]
    fn test_cluster_size() {
        assert_eq!(std::mem::size_of::<[Record; 4]>(), 64);
        assert_eq!(std::mem::size_of::<Record>(), 16);
    }

    #[test]
    fn test_store_and_probe() {
        let tt = StandardTt::new(None);
        assert!(tt.probe(1).is_none());
        let data = StandardTtEntry::new(0, 0, 50, 666, 0);
        assert_eq!(data.depth(), 50);
        assert_eq!(data.move_digest(), 666);
        tt.store(1, data);
        assert_eq!(tt.probe(1).unwrap().depth(), 50);
        tt.store(1, StandardTtEntry::new(0, 0, 50, 666, 0));
        assert_eq!(tt.probe(1).unwrap().depth(), 50);
        assert_eq!(tt.probe(1).unwrap().move_digest(), 666);
        for i in 2..50 {
            tt.store(i,
                     StandardTtEntry::new(i as i16, 0, i as u8, i as u16, i as i16));
        }
        assert_eq!(tt.probe(1).unwrap().depth(), 50);
        assert_eq!(tt.probe(49).unwrap().depth(), 49);
        assert_eq!(tt.probe(48).unwrap().depth(), 48);
        assert_eq!(tt.probe(47).unwrap().depth(), 47);
        tt.clear();
        assert!(tt.probe(1).is_none());
        tt.store(1, data);
        tt.new_search();
        tt.probe(1);
        assert!(tt.probe(1).is_some());
    }

    #[test]
    fn test_new_search() {
        let tt = StandardTt::new(None);
        assert_eq!(tt.generation.get(), 0 << 2);
        tt.new_search();
        assert_eq!(tt.generation.get(), 1 << 2);
        for _ in 0..64 {
            tt.new_search();
        }
        assert_eq!(tt.generation.get(), 1 << 2);
    }
}
