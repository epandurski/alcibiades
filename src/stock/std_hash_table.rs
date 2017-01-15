//! Implements `StdHashTable` and `StdHashTableEntry`.

use libc;
use libc::c_void;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::marker::PhantomData;
use std::isize;
use std::cell::Cell;
use std::cmp::max;
use std::mem;
use value::*;
use depth::*;
use hash_table::*;
use moves::MoveDigest;


/// Implements the `HashTableEntry` trait.
#[derive(Copy, Clone, Debug)]
pub struct StdHashTableEntry {
    value: Value,
    bound: BoundType,
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
            bound: bound,
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
        self.bound
    }

    #[inline]
    fn depth(&self) -> Depth {
        self.depth
    }

    #[inline]
    fn move_digest(&self) -> MoveDigest {
        self.move_digest
    }

    #[inline]
    fn set_move_digest(&mut self, move_digest: MoveDigest) {
        self.move_digest = move_digest;
    }

    /// Returns the `static_eval` passed to the constructor.
    #[inline]
    fn static_eval(&self) -> Value {
        self.static_eval
    }
}


/// Implements the `HashTable` trait.
pub struct StdHashTable<T: HashTableEntry> {
    phantom: PhantomData<T>,

    /// The current generation number.
    ///
    /// This is always between 1 and 31. Generation `0` is reserved
    /// for empty records.
    generation: Cell<usize>,

    /// The number of buckets in the table.
    bucket_count: usize,

    /// The raw pointer obtained from `libc::calloc`.
    ///
    /// This pointer will be passed to `libc::free` before the
    /// transposition table is dropped.
    alloc_ptr: *mut c_void,

    /// Optimally aligned raw pointer to the transposition table.
    ///
    /// Aligning table's buckets to machine's cache lines may in some
    /// cases improve performance.
    table_ptr: *mut c_void,
}

impl<T: HashTableEntry> HashTable for StdHashTable<T> {
    type Entry = T;

    fn new(size_mb: Option<usize>) -> StdHashTable<T> {
        assert_eq!(mem::size_of::<c_void>(), 1);
        let size_mb = size_mb.unwrap_or(16);
        let bucket_count = {
            // Make sure that the number of buckets is a power of 2.
            let n = max(1, ((size_mb * 1024 * 1024) / BUCKET_SIZE) as u64);
            1 << (63 - n.leading_zeros())
        };
        let alloc_ptr;
        let table_ptr = unsafe {
            // Make sure that the first bucket is optimally aligned.
            alloc_ptr = libc::calloc(bucket_count + 1, BUCKET_SIZE);
            let mut addr = mem::transmute::<*mut c_void, usize>(alloc_ptr);
            addr += BUCKET_SIZE;
            addr &= !(BUCKET_SIZE - 1);
            mem::transmute::<usize, *mut c_void>(addr)
        };
        StdHashTable {
            phantom: PhantomData,
            generation: Cell::new(1),
            bucket_count: bucket_count,
            alloc_ptr: alloc_ptr,
            table_ptr: table_ptr,
        }
    }

    fn new_search(&self) {
        const N: usize = 128;

        loop {
            // Increment the generation number (with wrapping).
            self.generation.set(match self.generation.get() {
                n @ 1...30 => n + 1,
                31 => 1,
                _ => panic!("invalid generation number"),
            });
            debug_assert!(self.generation.get() > 0);
            debug_assert!(self.generation.get() < 32);

            // Count how many staled records from this generation
            // there are among the first `N` buckets.
            let mut staled = 0;
            for bucket in self.buckets().take(N) {
                for slot in 0..Bucket::<Record<T>>::len() {
                    if bucket.get_generation(slot) == self.generation.get() {
                        staled += 1;
                    }
                }
            }

            // If the staled records from this generation are too
            // many, we should continue to increment the generation
            // number. (This may happen if a very long search was
            // executed long time ago.)
            if staled < N {
                break;
            }
        }
    }

    fn store(&self, key: u64, mut data: Self::Entry) {
        let bucket = self.bucket(key);
        let trimmed_key = (key >> 32) as u32;

        // Choose a slot to which to write the data. (Each bucket has
        // several slots.)
        let mut replace_slot = 0;
        let mut replace_score = isize::MAX;
        for slot in 0..Bucket::<Record<T>>::len() {
            let record = unsafe { bucket.get(slot).as_ref().unwrap() };

            // Verify if this is an old record for the same key.
            if record.key == trimmed_key {
                if data.move_digest() == MoveDigest::invalid() {
                    // Keep the move from the old record.
                    data.set_move_digest(record.data.move_digest());
                }
                replace_slot = slot;
                break;
            }

            // Calculate the score for the record in this slot. The
            // replaced slot will be the slot with the lowest score.
            let score = self.calc_score(&record.data, bucket.get_generation(slot));
            if score < replace_score {
                replace_slot = slot;
                replace_score = score;
            }
        }

        // Write the data to the chosen slot.
        unsafe {
            *bucket.get(replace_slot) = Record::new(trimmed_key, data);
            bucket.set_generation(replace_slot, self.generation.get());
        }
    }

    #[inline]
    fn probe(&self, key: u64) -> Option<Self::Entry> {
        let bucket = self.bucket(key);
        let trimmed_key = (key >> 32) as u32;
        for slot in 0..Bucket::<Record<T>>::len() {
            if bucket.get_generation(slot) != 0 {
                let record = unsafe { bucket.get(slot).as_ref().unwrap() };
                if record.key == trimmed_key {
                    bucket.set_generation(slot, self.generation.get());
                    return Some(record.data);
                }
            }
        }
        None
    }

    fn clear(&self) {
        for bucket in self.buckets() {
            for slot in 0..Bucket::<Record<T>>::len() {
                bucket.set_generation(slot, 0);
            }
        }
        self.generation.set(1);
    }
}

impl<T: HashTableEntry> StdHashTable<T> {
    /// A helper method for `store`. It implements the record
    /// replacement strategy.
    #[inline]
    fn calc_score(&self, data: &T, generation: usize) -> isize {
        // Here we try to return higher values for the records that
        // are move likely to save CPU work in the future:

        // Positions from the current generation are always scored
        // higher than positions from older generations.
        (if generation == self.generation.get() {
            DEPTH_MAX as isize + 2
        } else {
            0
        }) 
            
        // Positions with higher search depths are scored higher.
        + data.depth() as isize
            
        // Positions with exact evaluations are given slight advantage.
        + (if data.bound() == BOUND_EXACT {
            1
        } else {
            0
        })
    }

    /// A helper method for `probe` and `store`. For given key it
    /// returns a bucket.
    #[inline]
    fn bucket(&self, key: u64) -> Bucket<Record<T>> {
        unsafe {
            let byte_offset = (key as usize & (self.bucket_count - 1)) * BUCKET_SIZE;
            Bucket::new(self.table_ptr.offset(byte_offset as isize))
        }
    }

    #[inline]
    fn buckets(&self) -> BucketIter<Record<T>> {
        BucketIter::new(self.table_ptr, self.bucket_count)
    }
}

impl<T: HashTableEntry> Drop for StdHashTable<T> {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.alloc_ptr);
        }
    }
}

unsafe impl<T: HashTableEntry> Sync for StdHashTable<T> {}

unsafe impl<T: HashTableEntry> Send for StdHashTable<T> {}


/// Represents a record in the transposition table.
///
/// It consists of a 32 bit key plus data.
#[derive(Copy, Clone)]
struct Record<T: HashTableEntry> {
    key: u32,
    data: T,
}

impl<T: HashTableEntry> Record<T> {
    #[inline]
    pub fn new(key: u32, data: T) -> Record<T> {
        Record {
            key: key,
            data: data,
        }
    }
}


struct Bucket<T> {
    first: *mut T,
    info: *mut AtomicUsize,
}

const BUCKET_LOCK: usize = 1 << 31;

impl<T> Bucket<T> {
    #[inline]
    pub unsafe fn new(p: *mut c_void) -> Bucket<T> {
        // Acquire the bucket lock.
        //
        // TODO: Obtaining this lock is expensive. It is not clear if
        // not having a lock at all would cause any problems in
        // practice. May be we should be optimistic.
        let byte_offset = BUCKET_SIZE - mem::size_of::<usize>();
        let info = (p.offset(byte_offset as isize) as *mut AtomicUsize).as_mut().unwrap();
        loop {
            let value = info.load(Ordering::Relaxed);
            if value & BUCKET_LOCK == 0 {
                if info.compare_exchange_weak(value,
                                              value | BUCKET_LOCK,
                                              Ordering::Acquire,
                                              Ordering::Relaxed)
                       .is_ok() {
                    break;
                }
            }
        }

        Bucket {
            first: p as *mut T,
            info: info as *mut AtomicUsize,
        }
    }

    #[inline]
    pub fn len() -> usize {
        // **Important note:** Because `AtomicU32` is unstable at the
        // moment, we use `AtomicUsize` for the `info` field. So, on
        // 64-bit platforms the `info` field may overlap with the last
        // record in the bucket. But that is OK, because we read and
        // manipulate only the last 32 bits of the `info` field.
        (BUCKET_SIZE - 4) / mem::size_of::<T>()
    }

    #[inline]
    pub fn get(&self, slot: usize) -> *mut T {
        assert!(slot < Self::len());
        unsafe { self.first.offset(slot as isize) }
    }

    #[inline]
    pub fn get_generation(&self, slot: usize) -> usize {
        let info = unsafe { (*self.info).load(Ordering::Relaxed) };
        info >> (5 * slot) & 31
    }

    #[inline]
    pub fn set_generation(&self, slot: usize, generation: usize) {
        assert!(generation <= 31);
        unsafe {
            let mut info = (*self.info).load(Ordering::Relaxed);
            info &= [!(31 << 0),
                     !(31 << 5),
                     !(31 << 10),
                     !(31 << 15),
                     !(31 << 20),
                     !(31 << 25)][slot];
            info |= generation << (5 * slot);
            (*self.info).store(info, Ordering::Relaxed);
        }
    }
}

impl<T> Drop for Bucket<T> {
    #[inline]
    fn drop(&mut self) {
        // Release the bucket lock.
        let info = unsafe { self.info.as_mut().unwrap() };
        let value = info.load(Ordering::Relaxed);
        info.store(value & !BUCKET_LOCK, Ordering::Release);
    }
}


struct BucketIter<T> {
    base: *mut c_void,
    count: usize,
    current_index: usize,
    phantom: PhantomData<T>,
}

impl<T> BucketIter<T> {
    fn new(p: *mut c_void, count: usize) -> BucketIter<T> {
        BucketIter {
            base: p,
            count: count,
            current_index: 0,
            phantom: PhantomData,
        }
    }
}

impl<T> Iterator for BucketIter<T> {
    type Item = Bucket<T>;

    fn next(&mut self) -> Option<Self::Item> {
        debug_assert!(self.current_index <= self.count);
        if self.current_index == self.count {
            None
        } else {
            let byte_offset = (self.current_index * BUCKET_SIZE) as isize;
            let bucket = unsafe { Bucket::new(self.base.offset(byte_offset)) };
            self.current_index += 1;
            Some(bucket)
        }
    }
}


// Equals the most common cache line size.
const BUCKET_SIZE: usize = 64;


#[cfg(test)]
mod tests {
    use super::*;
    use depth::*;
    use hash_table::*;
    use moves::*;

    #[test]
    fn bucket() {
        use libc;
        use moves::MoveDigest;
        use super::{Bucket, Record};
        unsafe {
            let p = libc::malloc(64);
            let b = Bucket::<Record<StdHashTableEntry>>::new(p);
            assert_eq!(b.get_generation(0), 0);
            assert_eq!(b.get_generation(1), 0);
            let mut record = b.get(0).as_mut().unwrap();
            let entry = StdHashTableEntry::new(0, BOUND_NONE, 10, MoveDigest::invalid());
            *record = Record::new(0, entry);
            b.set_generation(0, 12);
            b.set_generation(1, 13);
            assert_eq!(record.data.depth, 10);
            assert_eq!(b.get_generation(0), 12);
            assert_eq!(b.get_generation(1), 13);
            assert_eq!(Bucket::<Record<StdHashTableEntry>>::len(), 5);
            libc::free(p);
        }
    }

    #[test]
    fn store_and_probe() {
        let tt = StdHashTable::<StdHashTableEntry>::new(None);
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
        let tt = StdHashTable::<StdHashTableEntry>::new(None);
        assert_eq!(tt.generation.get(), 1);
        tt.new_search();
        assert_eq!(tt.generation.get(), 2);
        for _ in 3..34 {
            tt.new_search();
        }
        assert_eq!(tt.generation.get(), 2);
    }
}
