//! Implements `StdTtable`.

use libc;
use libc::c_void;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::marker::PhantomData;
use std::isize;
use std::cell::Cell;
use std::cmp::max;
use std::mem;
use ttable::*;
use moves::MoveDigest;


/// Represents a record in the transposition table.
///
/// Consists of a transposition table entry plus the highest 32 bits
/// of the key. The key is split into two `u16` values to allow more
/// flexible alignment.
#[derive(Copy, Clone)]
struct Record<T: TtableEntry> {
    key: (u16, u16),
    data: T,
}


/// A handle to a set of records (a bucket) in the transposition
/// table.
///
/// `R` gives records' type. Each bucket can hold up to 6 records,
/// depending on their size. A 5-bit generation number is stored for
/// each record.
struct Bucket<R> {
    first: *mut R,

    // This field is laid out the following way: 30 of the bits are
    // used to store records' generation numbers (6 slots, 5 bits
    // each); 1 bit is not used; 1 bit is used as a locking flag.
    //
    // **Important note:** Because `AtomicU32` is unstable at the time
    // of writing, we use `AtomicUsize` for the `info` field. So, on
    // 64-bit platforms the `info` field may overlap with the last
    // record in the bucket. But that is OK, because we mind machine's
    // endianness and read and manipulate only the last 4 bytes of the
    // `info` field.
    info: *mut AtomicUsize,
}

/// The size of each bucket in bytes.
///
/// `64` is the most common cache line size.
const BUCKET_SIZE: usize = 64;

impl<R> Bucket<R> {
    /// Creates a new instance from a raw pointer.
    #[inline]
    pub unsafe fn new(p: *mut c_void) -> Bucket<R> {
        let byte_offset = BUCKET_SIZE - mem::size_of::<usize>();
        let info = (p.offset(byte_offset as isize) as *mut AtomicUsize)
            .as_mut()
            .unwrap();

        // Acquire the lock for the bucket.
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
        let info = unsafe { self.info.as_mut().unwrap() };
        info.load(Ordering::Relaxed) >> GENERATION_SHIFTS[slot] & 31
    }

    /// Sets the generation number for a given slot.
    #[inline]
    pub fn set_generation(&self, slot: usize, generation: usize) {
        debug_assert!(generation <= 31);
        let info = unsafe { self.info.as_mut().unwrap() };
        let mut v = info.load(Ordering::Relaxed);
        v &= GENERATION_MASKS[slot];
        v |= generation << GENERATION_SHIFTS[slot];
        info.store(v, Ordering::Relaxed);
    }
}

impl<R> Drop for Bucket<R> {
    #[inline]
    fn drop(&mut self) {
        // Release the lock for the bucket.
        let info = unsafe { self.info.as_mut().unwrap() };
        let old = info.load(Ordering::Relaxed);
        let new = old & !BUCKET_LOCKING_FLAG;
        info.store(new, Ordering::Release);
    }
}


/// Implements the `Ttable` trait.
///
/// `StdTtable` provides a generic transposition table implementation
/// that can efficiently pack in memory a wide range of transposition
/// table entry types. The only condition is that `T` has a size
/// between 6 and 16 bytes, and alignment requirements of 4 bytes or
/// less.
pub struct StdTtable<T: TtableEntry> {
    entries: PhantomData<T>,

    /// The current generation number.
    ///
    /// A generation number is assigned to each record, so as to be
    /// able to determine which records are from the current search,
    /// and which are from previous searches. Records from previous
    /// searches will be replaced before records from the current
    /// search. The generation number is always between 1 and
    /// 31. Generation `0` is reserved for empty records.
    generation: Cell<usize>,

    /// The number of buckets in the table.
    ///
    /// Each bucket can hold 3 to 6 records, depending on their size.
    /// `bucket_count` should always be a power of 2.
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

impl<T: TtableEntry> Ttable for StdTtable<T> {
    type Entry = T;

    fn new(size_mb: Option<usize>) -> StdTtable<T> {
        // Assert our basic premises.
        assert_eq!(mem::size_of::<c_void>(), 1);
        assert_eq!(BUCKET_SIZE, 64);
        assert!(mem::align_of::<T>() <= 4,
                format!("too restrictive transposition table entry alignment: {} bytes",
                        mem::align_of::<T>()));
        assert!(Bucket::<Record<T>>::len() >= 3,
                format!("too big transposition table entry: {} bytes",
                        mem::size_of::<T>()));
        assert!(Bucket::<Record<T>>::len() <= 6,
                format!("too small transposition table entry: {} bytes",
                        mem::size_of::<T>()));

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

        StdTtable {
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
            self.generation
                .set(match self.generation.get() {
                         n @ 1...30 => n + 1,
                         31 => 1,
                         _ => unreachable!(),
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

    #[inline]
    fn store(&self, key: u64, mut data: Self::Entry) {
        let bucket = self.bucket(key);
        let key = chop_key(key);

        // Choose a bucket slot to which to write the data.
        let mut replace_slot = 0;
        let mut replace_score = isize::MAX;
        for slot in 0..Bucket::<Record<T>>::len() {
            let record = unsafe { &mut *bucket.get(slot) };
            let generation = bucket.get_generation(slot);

            // Use this slot if it is empty.
            if generation == 0 {
                replace_slot = slot;
                break;
            }

            // Use this slot if it contains an old record for the same key.
            if record.key == key {
                if record.data.bound() == BOUND_EXACT &&
                   record.data.importance() > data.importance() {
                    // Keep the old record if we are certain that it
                    // is more important than the new one.
                    //
                    // **Note:** We do not keep old records with
                    // inexact bounds because they can be useless,
                    // regardless of their depth.
                    data = record.data;
                } else if data.move_digest() == MoveDigest::invalid() {
                    // Keep the move from the old record if the new
                    // record has no move.
                    data = data.set_move_digest(record.data.move_digest());
                }
                replace_slot = slot;
                break;
            }

            // Calculate the score for the record in this slot. The
            // replaced record will be the one with the lowest score.
            let mut score = record.data.importance() as isize;
            if generation == self.generation.get() {
                // Positions from the current generation are always
                // scored higher than positions from older generations.
                score += 1 << 16;
            };
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
                let record = unsafe { &mut *bucket.get(slot) };
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

impl<T: TtableEntry> StdTtable<T> {
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

impl<T: TtableEntry> Drop for StdTtable<T> {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.alloc_ptr);
        }
    }
}

unsafe impl<T: TtableEntry> Sync for StdTtable<T> {}

unsafe impl<T: TtableEntry> Send for StdTtable<T> {}


/// A helper type for `StdTtable`. It iterates over the buckets in the
/// table.
struct Iter<T: TtableEntry> {
    entries: PhantomData<T>,
    table_ptr: *mut c_void,
    bucket_count: usize,
    iterated: usize,
}

impl<T: TtableEntry> Iterator for Iter<T> {
    type Item = Bucket<Record<T>>;

    #[inline]
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


/// A helper function for `StdTtable`. It takes the highest 32 bits of
/// an `u64` value and splits them into two `u16` values.
#[inline]
fn chop_key(key: u64) -> (u16, u16) {
    unsafe { mem::transmute::<u32, (u16, u16)>((key >> 32) as u32) }
}


#[cfg(any(target_pointer_width = "32", target_endian = "big"))]
const BUCKET_LOCKING_FLAG: usize = 1 << 31;
#[cfg(all(target_pointer_width = "64", target_endian = "little"))]
const BUCKET_LOCKING_FLAG: usize = 1 << 63;

#[cfg(any(target_pointer_width = "32", target_endian = "big"))]
const GENERATION_SHIFTS: [usize; 6] = [0, 5, 10, 15, 20, 25];
#[cfg(all(target_pointer_width = "64", target_endian = "little"))]
const GENERATION_SHIFTS: [usize; 6] = [32, 37, 42, 47, 52, 57];

#[cfg(any(target_pointer_width = "32", target_endian = "big"))]
const GENERATION_MASKS: [usize; 6] = [!(31 << 0),
                                      !(31 << 5),
                                      !(31 << 10),
                                      !(31 << 15),
                                      !(31 << 20),
                                      !(31 << 25)];
#[cfg(all(target_pointer_width = "64", target_endian = "little"))]
const GENERATION_MASKS: [usize; 6] = [!(31 << 32),
                                      !(31 << 37),
                                      !(31 << 42),
                                      !(31 << 47),
                                      !(31 << 52),
                                      !(31 << 57)];



#[cfg(test)]
mod tests {
    use libc;
    use super::*;
    use super::{Bucket, Record};
    use depth::*;
    use value::*;
    use moves::*;
    use stock::std_ttable_entry::*;

    #[test]
    fn bucket() {
        unsafe {
            let p = libc::calloc(1, 64);
            let b = Bucket::<Record<StdTtableEntry>>::new(p);
            assert_eq!(b.get_generation(0), 0);
            assert_eq!(b.get_generation(1), 0);
            let mut record = b.get(0).as_mut().unwrap();
            let entry = StdTtableEntry::new(0, BOUND_NONE, 10);
            *record = Record {
                key: (0, 0),
                data: entry,
            };
            b.set_generation(0, 12);
            b.set_generation(1, 13);
            assert_eq!(record.data.depth(), 10);
            assert_eq!(b.get_generation(0), 12);
            assert_eq!(b.get_generation(1), 13);
            assert_eq!(Bucket::<Record<StdTtableEntry>>::len(), 5);
            libc::free(p);
        }
    }

    #[test]
    fn bucket_endianness() {
        unsafe {
            let p = libc::calloc(1, 64);
            let b = Bucket::<Record<StdTtableEntry>>::new(p);
            let mut record = b.get(4).as_mut().unwrap();
            let entry = StdTtableEntry::new(0, BOUND_NONE, 10);
            *record = Record {
                key: (0, 0),
                data: entry,
            };
            b.set_generation(0, 12);
            b.set_generation(1, 12);
            b.set_generation(2, 12);
            b.set_generation(3, 12);
            b.set_generation(4, 12);
            assert_eq!(record.data.static_eval(), VALUE_UNKNOWN);
            libc::free(p);
        }
    }

    #[test]
    fn store_and_probe() {
        let tt = StdTtable::<StdTtableEntry>::new(None);
        assert!(tt.probe(1).is_none());
        let data = StdTtableEntry::new(0, 0, 50);
        assert_eq!(data.depth(), 50);
        assert_eq!(data.move_digest(), MoveDigest::invalid());
        tt.store(1, data);
        assert_eq!(tt.probe(1).unwrap().depth(), 50);
        tt.store(1, StdTtableEntry::new(0, 0, 50));
        assert_eq!(tt.probe(1).unwrap().depth(), 50);
        assert_eq!(tt.probe(1).unwrap().move_digest(), MoveDigest::invalid());
        for i in 2..50 {
            tt.store(i, StdTtableEntry::new(i as i16, 0, i as Depth));
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
        let tt = StdTtable::<StdTtableEntry>::new(None);
        assert_eq!(tt.generation.get(), 1);
        tt.new_search();
        assert_eq!(tt.generation.get(), 2);
        for _ in 3..34 {
            tt.new_search();
        }
        assert_eq!(tt.generation.get(), 2);
    }
}
