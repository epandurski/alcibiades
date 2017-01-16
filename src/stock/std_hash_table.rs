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
///
/// `StdHashTable` provides a generic transposition table
/// implementation that can efficiently pack in memory wide range of
/// hash table entry types. The only condition is that `T` has a size
/// between 6 and 16 bytes, and alignment requirements of 4 bytes or
/// less.
pub struct StdHashTable<T: HashTableEntry> {
    entries: PhantomData<T>,

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
        assert_eq!(BUCKET_SIZE, 64);
        assert!(mem::align_of::<T>() <= 4,
                format!("too restrictive hash table entry alignment: {} bytes",
                        mem::align_of::<T>()));
        assert!(Bucket::<Record<T>>::len() >= 3,
                format!("too big hash table entry: {} bytes", mem::size_of::<T>()));
        assert!(Bucket::<Record<T>>::len() <= 6,
                format!("too small hash table entry: {} bytes", mem::size_of::<T>()));
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
            entries: PhantomData,
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
        let key = chop_key(key);

        // Choose a slot to which to write the data.
        let mut replace_slot = 0;
        let mut replace_score = isize::MAX;
        for slot in 0..Bucket::<Record<T>>::len() {
            let record = unsafe { bucket.get(slot).as_ref().unwrap() };

            // Verify if this is an old record for the same key. If
            // this is the case, we will use this slot for the new
            // record.
            if record.key == key {
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
            *bucket.get(replace_slot) = Record {
                key: key,
                data: data,
            };
            bucket.set_generation(replace_slot, self.generation.get());
        }
    }

    #[inline]
    fn probe(&self, key: u64) -> Option<Self::Entry> {
        let bucket = self.bucket(key);
        let key = chop_key(key);
        for slot in 0..Bucket::<Record<T>>::len() {
            if bucket.get_generation(slot) != 0 {
                let record = unsafe { bucket.get(slot).as_ref().unwrap() };
                if record.key == key {
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
    /// Implements the record replacement strategy.
    ///
    /// In this method we try to return higher values for the records
    /// that are move likely to save CPU work in the future.
    #[inline]
    fn calc_score(&self, data: &T, generation: usize) -> isize {
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

    /// Returns the bucket for a given key.
    #[inline]
    fn bucket(&self, key: u64) -> Bucket<Record<T>> {
        unsafe {
            let byte_offset = (key as usize & (self.bucket_count - 1)) * BUCKET_SIZE;
            Bucket::new(self.table_ptr.offset(byte_offset as isize))
        }
    }

    /// Returns an iterator over the buckets in the table.
    #[inline]
    fn buckets(&self) -> Iter<T> {
        Iter {
            entries: PhantomData,
            table_ptr: self.table_ptr,
            bucket_count: self.bucket_count,
            iterated: 0,
        }
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
/// Consists of a hash table entry plus the highest 32 bits of the
/// key. The key is split into two `u16` values to allow more flexible
/// alignment.
#[derive(Copy, Clone)]
struct Record<T: HashTableEntry> {
    key: (u16, u16),
    data: T,
}


/// A handle to a set of records (a bucket) in the hash table.
///
/// `R` gives records' type. Each bucket can hold up to 6 records,
/// depending on their size. A 5-bit generation number is stored for
/// each record.
struct Bucket<R> {
    first: *mut R,

    // Bits 0-29 are used to store records' generation numbers (6
    // slots, 5 bits each). Bit 30 is not used, bit 31 is used as a
    // locking flag.
    info: *mut AtomicUsize,
}

/// The size of each bucket in bytes.
///
/// `64` is the most common cache line size.
const BUCKET_SIZE: usize = 64;

// A locking flag in the `info` field.
#[cfg(any(target_pointer_width = "32", target_endian = "big"))]
const BUCKET_LOCKING_FLAG: usize = 1 << 31;
#[cfg(all(target_pointer_width = "64", target_endian = "little"))]
const BUCKET_LOCKING_FLAG: usize = 1 << 63;

#[cfg(any(target_pointer_width = "32", target_endian = "big"))]
const BUCKET_GENERATION_SHIFTS: [usize; 6] = [0, 5, 10, 15, 20, 25];
#[cfg(all(target_pointer_width = "64", target_endian = "little"))]
const BUCKET_GENERATION_SHIFTS: [usize; 6] = [32, 37, 42, 47, 52, 57];

#[cfg(any(target_pointer_width = "32", target_endian = "big"))]
const BUCKET_GENERATION_MASKS: [usize; 6] = [!(31 << 0),
                                             !(31 << 5),
                                             !(31 << 10),
                                             !(31 << 15),
                                             !(31 << 20),
                                             !(31 << 25)];
#[cfg(all(target_pointer_width = "64", target_endian = "little"))]
const BUCKET_GENERATION_MASKS: [usize; 6] = [!(31 << 32),
                                             !(31 << 37),
                                             !(31 << 42),
                                             !(31 << 47),
                                             !(31 << 52),
                                             !(31 << 57)];

impl<R> Bucket<R> {
    /// Creates a new instance from a raw pointer.
    #[inline]
    pub unsafe fn new(p: *mut c_void) -> Bucket<R> {
        // Acquire the lock for the bucket.
        //
        // **Important note:** Acquiring the lock is expensive. It is
        // entirely possible that having no lock at all will not cause
        // any problems in practice, considering that on most
        // platforms buckets will be aligned to machine's cache lines.
        let byte_offset = BUCKET_SIZE - mem::size_of::<usize>();
        let info = (p.offset(byte_offset as isize) as *mut AtomicUsize).as_mut().unwrap();
        loop {
            let old = info.load(Ordering::Relaxed);
            if old & BUCKET_LOCKING_FLAG == 0 {
                let new = old | BUCKET_LOCKING_FLAG;
                if info.compare_exchange_weak(old, new, Ordering::Acquire, Ordering::Relaxed)
                       .is_ok() {
                    break;
                }
            }
        }

        Bucket {
            first: p as *mut R,
            info: info as *mut AtomicUsize,
        }
    }

    /// Returns the number of slots in the bucket.
    #[inline]
    pub fn len() -> usize {
        // **Important note:** Because `AtomicU32` is unstable at the
        // moment of this writing, we use `AtomicUsize` for the `info`
        // field. So, on 64-bit platforms the `info` field may overlap
        // with the last record in the bucket. But that is OK, because
        // we mind machine's endianness and read and manipulate only
        // the last 4 bytes of the `info` field.
        (BUCKET_SIZE - 4) / mem::size_of::<R>()
    }

    /// Returns a raw pointer to the record in a given slot.
    #[inline]
    pub fn get(&self, slot: usize) -> *mut R {
        assert!(slot < Self::len());
        unsafe { self.first.offset(slot as isize) }
    }

    /// Returns the generation number for a given slot.
    #[inline]
    pub fn get_generation(&self, slot: usize) -> usize {
        let info = unsafe { (*self.info).load(Ordering::Relaxed) };
        info >> BUCKET_GENERATION_SHIFTS[slot] & 31
    }

    /// Sets the generation number for a given slot.
    #[inline]
    pub fn set_generation(&self, slot: usize, generation: usize) {
        debug_assert!(generation <= 31);
        unsafe {
            let mut info = (*self.info).load(Ordering::Relaxed);
            info &= BUCKET_GENERATION_MASKS[slot];
            info |= generation << BUCKET_GENERATION_SHIFTS[slot];
            (*self.info).store(info, Ordering::Relaxed);
        }
    }
}

impl<R> Drop for Bucket<R> {
    #[inline]
    fn drop(&mut self) {
        // Release the lock for the bucket.
        let info = unsafe { self.info.as_mut().unwrap() };
        let value = info.load(Ordering::Relaxed);
        info.store(value & !BUCKET_LOCKING_FLAG, Ordering::Release);
    }
}


/// A helper type for `StdHashTable`. It iterates over the buckets in
/// the table.
struct Iter<T: HashTableEntry> {
    entries: PhantomData<T>,
    table_ptr: *mut c_void,
    bucket_count: usize,
    iterated: usize,
}

impl<T: HashTableEntry> Iterator for Iter<T> {
    type Item = Bucket<Record<T>>;

    fn next(&mut self) -> Option<Self::Item> {
        debug_assert!(self.iterated <= self.bucket_count);
        if self.iterated == self.bucket_count {
            None
        } else {
            let byte_offset = (self.iterated * BUCKET_SIZE) as isize;
            let bucket = unsafe { Bucket::new(self.table_ptr.offset(byte_offset)) };
            self.iterated += 1;
            Some(bucket)
        }
    }
}


/// A helper function for `StdHashTable`. It takes the highest 32 bits
/// of an `u64` value and splits them into two `u16` values.
#[inline]
fn chop_key(key: u64) -> (u16, u16) {
    unsafe { mem::transmute::<u32, (u16, u16)>((key >> 32) as u32) }
}


#[cfg(test)]
mod tests {
    use libc;
    use super::*;
    use super::{Bucket, Record};
    use depth::*;
    use value::*;
    use moves::*;
    use hash_table::*;

    #[test]
    fn bucket() {
        unsafe {
            let p = libc::malloc(64);
            let b = Bucket::<Record<StdHashTableEntry>>::new(p);
            assert_eq!(b.get_generation(0), 0);
            assert_eq!(b.get_generation(1), 0);
            let mut record = b.get(0).as_mut().unwrap();
            let entry = StdHashTableEntry::new(0, BOUND_NONE, 10, MoveDigest::invalid());
            *record = Record {
                key: (0, 0),
                data: entry,
            };
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
    fn bucket_endianness() {
        unsafe {
            let p = libc::malloc(64);
            let b = Bucket::<Record<StdHashTableEntry>>::new(p);
            let mut record = b.get(4).as_mut().unwrap();
            let entry = StdHashTableEntry::with_static_eval(0,
                                                            BOUND_NONE,
                                                            10,
                                                            MoveDigest::invalid(),
                                                            VALUE_UNKNOWN);
            *record = Record {
                key: (0, 0),
                data: entry,
            };
            b.set_generation(0, 12);
            b.set_generation(1, 12);
            b.set_generation(2, 12);
            b.set_generation(3, 12);
            b.set_generation(4, 12);
            assert_eq!(record.data.static_eval, VALUE_UNKNOWN);
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
