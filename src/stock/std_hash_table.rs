//! Implements `StdHashTable` and `StdHashTableEntry`.

use libc;
use libc::c_void;
use std::sync::atomic::{AtomicPtr, Ordering};
use std::slice;
use std::isize;
use std::cell::Cell;
use std::cmp::{min, max};
use std::mem;
use value::*;
use depth::*;
use hash_table::*;
use moves::MoveDigest;


/// Implements the `HashTableEntry` trait.
#[derive(Copy, Clone, Debug)]
pub struct StdHashTableEntry {
    value: Value,

    // The transposition table maintains a generation number for each
    // entry, which is used to implement an efficient replacement
    // strategy. This field stores the entry's generation (the highest
    // 6 bits) and the bound type (the lowest 2 bits).
    gen_bound: u8,

    depth: Depth,
    move_digest: MoveDigest,
    static_eval: Value,
}

impl HashTableEntry for StdHashTableEntry {
    #[inline]
    fn new(value: Value,
           bound: BoundType,
           depth: Depth,
           move_digest: MoveDigest)
           -> StdHashTableEntry {
        Self::with_static_eval(value, bound, depth, move_digest, VALUE_UNKNOWN)
    }

    #[inline]
    fn with_static_eval(value: Value,
                        bound: BoundType,
                        depth: Depth,
                        move_digest: MoveDigest,
                        static_eval: Value)
                        -> StdHashTableEntry {
        debug_assert!(value != VALUE_UNKNOWN);
        debug_assert!(bound <= 0b11);
        debug_assert!(DEPTH_MIN <= depth && depth <= DEPTH_MAX);
        StdHashTableEntry {
            value: value,
            gen_bound: bound,
            depth: depth,
            move_digest: move_digest,
            static_eval: static_eval,
        }
    }

    #[inline]
    fn value(&self) -> Value {
        self.value
    }

    #[inline]
    fn bound(&self) -> BoundType {
        self.gen_bound & 0b11
    }

    #[inline]
    fn depth(&self) -> Depth {
        self.depth
    }

    #[inline]
    fn move_digest(&self) -> MoveDigest {
        self.move_digest
    }

    /// Returns the `static_eval` passed to the constructor.
    #[inline]
    fn static_eval(&self) -> Value {
        self.static_eval
    }
}

impl StdHashTableEntry {
    /// Returns the contained data as one `u64` value.
    #[inline]
    fn as_u64(&self) -> u64 {
        unsafe { mem::transmute(*self) }
    }
}


/// Implements the `HashTable` trait.
pub struct StdHashTable {
    /// The current generation number. The lowest 2 bits will always
    /// be zeros.
    generation: Cell<u8>,

    /// The number of buckets in the table.
    bucket_count: usize,

    /// The raw pointer obtained from `libc::calloc`. It will be
    /// passed to `libc::free` before the transposition table is
    /// dropped.
    alloc_ptr: AtomicPtr<c_void>,

    /// The transposition table consists of a vector of buckets. Each
    /// buckets stores 4 records.
    table_ptr: AtomicPtr<[Record; 4]>,
}

impl HashTable for StdHashTable {
    type Entry = StdHashTableEntry;

    fn new(size_mb: Option<usize>) -> StdHashTable {
        let size_mb = size_mb.unwrap_or(16);
        let bucket_size = mem::size_of::<[Record; 4]>();
        let bucket_count = {
            let n = max(1, ((size_mb * 1024 * 1024) / bucket_size) as u64);
            1 << (63 - n.leading_zeros())
        };
        let alloc_ptr;
        let table_ptr = unsafe {
            // Make sure that the first bucket is optimally
            // aligned. This may improve performance when cache line's
            // size is divisible to bucket's size or vice versa.
            alloc_ptr = libc::calloc(bucket_count + 1, bucket_size);
            let mut addr = mem::transmute::<*mut c_void, usize>(alloc_ptr);
            addr += bucket_size;
            addr &= !(bucket_size - 1);
            mem::transmute::<usize, *mut [Record; 4]>(addr)
        };

        StdHashTable {
            generation: Cell::new(0),
            bucket_count: bucket_count,
            alloc_ptr: AtomicPtr::new(alloc_ptr),
            table_ptr: AtomicPtr::new(table_ptr),
        }
    }

    fn new_search(&self) {
        const N: usize = 128;

        loop {
            // Increment `self.generation` (with wrapping).
            self.generation.set(self.generation.get().wrapping_add(0b100));
            debug_assert_eq!(self.generation.get() & 0b11, 0);

            // Count how many staled records from this generation
            // there are among the first `N` buckets.
            let mut staled = 0;
            let mut bucket_iter = self.table().iter();
            for _ in 0..min(N, self.bucket_count) {
                for record in bucket_iter.next().unwrap() {
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

        // Choose a slot to which to write the data. (Each bucket has
        // 4 slots.)
        let mut bucket = self.bucket_mut(key);
        let mut replace_index = 0;
        let mut replace_score = isize::MAX;
        for (i, record) in bucket.iter_mut().enumerate() {
            // Check if this is an empty slot, or an old record for
            // the same key. If this this is the case we will use this
            // slot for the new record.
            if record.key == 0 || record.key ^ record.data.as_u64() == key {
                if data.move_digest == MoveDigest::invalid() {
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
        bucket[replace_index] = Record {
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

        let bucket = self.bucket_mut(key);
        for record in bucket.iter_mut() {
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
        for bucket in self.table_mut() {
            for record in bucket.iter_mut() {
                *record = Default::default();
            }
        }
        self.generation.set(0);
    }
}

impl StdHashTable {
    /// A helper method for `store`. It implements the record
    /// replacement strategy.
    #[inline]
    fn calc_score(&self, record: &Record) -> isize {
        // Here we try to return higher values for the records that
        // are move likely to save CPU work in the future:

        // Positions from the current generation are always scored
        // higher than positions from older generations.
        (if record.generation() == self.generation.get() {
            DEPTH_MAX as isize + 2
        } else {
            0
        }) 
            
        // Positions with higher search depths are scored higher.
        + record.data.depth() as isize
            
        // Positions with exact evaluations are given slight advantage.
        + (if record.data.bound() == BOUND_EXACT {
            1
        } else {
            0
        })
    }

    /// A helper method for `probe` and `store`. It returns the bucket
    /// for a given key.
    #[inline]
    fn bucket_mut(&self, key: u64) -> &mut [Record; 4] {
        unsafe {
            let index = (key & (self.bucket_count - 1) as u64) as usize;
            self.table_mut().get_unchecked_mut(index)
        }
    }

    #[inline]
    fn table(&self) -> &[[Record; 4]] {
        unsafe { slice::from_raw_parts(self.table_ptr.load(Ordering::Relaxed), self.bucket_count) }
    }

    #[inline]
    fn table_mut(&self) -> &mut [[Record; 4]] {
        unsafe {
            slice::from_raw_parts_mut(self.table_ptr.load(Ordering::Relaxed), self.bucket_count)
        }
    }
}

impl Drop for StdHashTable {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.alloc_ptr.load(Ordering::Relaxed));
        }
    }
}

unsafe impl Sync for StdHashTable {}


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
    data: StdHashTableEntry,
}

impl Default for Record {
    fn default() -> Record {
        Record {
            key: 0,
            data: unsafe { mem::transmute(0u64) },
        }
    }
}

impl Record {
    #[inline]
    fn generation(&self) -> u8 {
        self.data.gen_bound & 0b11111100
    }

    #[inline]
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
    use depth::*;
    use hash_table::*;
    use moves::*;

    #[test]
    fn bucket_size() {
        assert_eq!(std::mem::size_of::<[Record; 4]>(), 64);
        assert_eq!(std::mem::size_of::<Record>(), 16);
    }

    #[test]
    fn store_and_probe() {
        let tt = StdHashTable::new(None);
        assert!(tt.probe(1).is_none());
        let data = StdHashTableEntry::new(0, 0, 50, MoveDigest::invalid());
        assert_eq!(data.depth(), 50);
        assert_eq!(data.move_digest(), MoveDigest::invalid());
        tt.store(1, data);
        assert_eq!(tt.probe(1).unwrap().depth(), 50);
        tt.store(1, StdHashTableEntry::new(0, 0, 50, MoveDigest::invalid()));
        assert_eq!(tt.probe(1).unwrap().depth(), 50);
        assert_eq!(tt.probe(1).unwrap().move_digest(), MoveDigest::invalid());
        let digest = MoveDigest::invalid();
        for i in 2..50 {
            tt.store(i, StdHashTableEntry::new(i as i16, 0, i as Depth, digest));
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
    fn new_search() {
        let tt = StdHashTable::new(None);
        assert_eq!(tt.generation.get(), 0 << 2);
        tt.new_search();
        assert_eq!(tt.generation.get(), 1 << 2);
        for _ in 0..64 {
            tt.new_search();
        }
        assert_eq!(tt.generation.get(), 1 << 2);
    }
}
