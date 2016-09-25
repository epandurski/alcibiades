//! Implements a large hash-table that stores results of previously
//! performed searches ("transposition table").
//!
//! Chess programs, during their brute-force search, encounter the
//! same positions again and again, but from different sequences of
//! moves, which is called a "transposition". When the search
//! encounters a transposition, it is beneficial to "remember" what
//! was determined last time the position was examined, rather than
//! redoing the entire search again. For this reason, chess programs
//! have a transposition table, which is a large hash table storing
//! information about positions previously searched, how deeply they
//! were searched, and what we concluded about them.

use std;
use std::cell::{UnsafeCell, Cell};
use std::mem::transmute;
use basetypes::Value;
use chess_move::MoveDigest;


/// `BOUND_EXACT`, `BOUND_LOWER`, `BOUND_UPPER`, or `BOUND_NONE`.
///
/// For the majority of chess positions our evaluations will be more
/// or less inaccurate, and there is nothing we can do about it. But
/// sometimes we know that a given evaluation is probably inaccurate,
/// and we know the sign of the error. `BoundType` defines the
/// direction of such **known inaccuracies**.
///
/// * `BOUND_EXACT` means that the evaluation is exact (as far as we know).
///
/// * `BOUND_LOWER` means that the real value is greater or equal to
///    the evaluation (as far as we know).
///
/// * `BOUND_UPPER` means that the real value is lesser or equal to
///   the evaluation (as far as we know).
///
/// * `BOUND_NONE` means that the real value might be lesser, equal,
///   or grater than the evaluation.
pub type BoundType = u8;

pub const BOUND_NONE: BoundType = 0;
pub const BOUND_UPPER: BoundType = 0b10;
pub const BOUND_LOWER: BoundType = 0b01;
pub const BOUND_EXACT: BoundType = BOUND_UPPER | BOUND_LOWER;



/// Contains information about a particular position.
#[derive(Copy, Clone)]
pub struct TtEntry {
    move16: MoveDigest,
    value: Value,
    eval_value: Value,
    depth: u8,

    // The transposition table maintains a generation number for each
    // entry, which is used to implement an efficient replacement
    // strategy. This field stores the entry's generation (the highest
    // 6 bits) and the bound type (the lowest 2 bits).
    gen_bound: u8,
}


impl TtEntry {
    /// Creates a new instance.
    ///
    /// * `value` -- the value assigned to the position;
    /// 
    /// * `bound` -- the accuracy of the assigned `value`;
    /// 
    /// * `depth` -- the depth of search;
    /// 
    /// * `move16` -- best or refutation move, or `0` if no move is
    ///   available;
    /// 
    /// * `eval_value` -- the calculated static evaluation for the
    ///   position.
    pub fn new(value: Value,
               bound: BoundType,
               depth: u8,
               move16: MoveDigest,
               eval_value: Value)
               -> TtEntry {
        assert!(bound <= 0b11);
        assert!(depth < 127);
        TtEntry {
            move16: move16,
            value: value,
            eval_value: eval_value,
            depth: depth,
            gen_bound: bound,
        }
    }

    #[inline(always)]
    pub fn value(&self) -> Value {
        self.value
    }

    #[inline(always)]
    pub fn bound(&self) -> BoundType {
        self.gen_bound & 0b11
    }

    #[inline(always)]
    pub fn depth(&self) -> u8 {
        self.depth
    }

    #[inline(always)]
    pub fn move16(&self) -> MoveDigest {
        self.move16
    }

    #[inline(always)]
    pub fn eval_value(&self) -> Value {
        self.eval_value
    }
}



/// A transposition table.
///
/// `Tt` methods (except `resize`) do not require a mutable reference
/// to do their work. This allows one transposition table instance to
/// be shared safely between many threads.
pub struct Tt {
    /// The current generation number. The lowest 2 bits will always
    /// be zeros.
    generation: Cell<u8>,
    
    /// The number of clusters in the table.
    cluster_count: usize,
    
    /// The table consists of a vector of clusters. Each cluster
    /// stores 4 records.
    table: UnsafeCell<Vec<[Record; 4]>>,
}


impl Tt {
    /// Creates a new transposition table.
    ///
    /// The newly created table has the minimum possible size. Before
    /// using the new table for anything, `resize()` should be called
    /// on it, specifying the desired size.
    pub fn new() -> Tt {
        Tt {
            generation: Cell::new(0),
            cluster_count: 1,
            table: UnsafeCell::new(vec![Default::default()]),
        }
    }

    /// Resizes the transpositon table. All entries in the table will
    /// be lost.
    ///
    /// `size_mb` is the desired new size in Mbytes. If `size_mb` is
    /// not in the form "2**n", the new size of the transposition
    /// table will be as close as possible, but less than `size_mb`.
    pub fn resize(&mut self, size_mb: usize) {
        let requested_cluster_count = (size_mb * 1024 * 1024) / std::mem::size_of::<[Record; 4]>();

        // Calculate the new cluster count. (To do this, first we make
        // sure that `requested_cluster_count` is exceeded. Then we
        // make one step back.)
        let mut new_cluster_count = 1;
        while new_cluster_count <= requested_cluster_count && new_cluster_count != 0 {
            new_cluster_count <<= 1;
        }
        if new_cluster_count > 1 {
            new_cluster_count >>= 1;
        } else {
            new_cluster_count = 1;
        }
        assert!(new_cluster_count > 0);

        // Allocate the new vector of clusters.
        if new_cluster_count != self.cluster_count {
            self.cluster_count = new_cluster_count;
            self.table = UnsafeCell::new(vec![Default::default(); new_cluster_count]);
        }
    }

    /// Returns the size of the transposition table in Mbytes.
    pub fn size(&self) -> usize {
        unsafe { &*self.table.get() }.len() * std::mem::size_of::<[Record; 4]>() / 1024 / 1024
    }

    /// Signals that a new search is about to begin.
    pub fn new_search(&self) {
        self.generation.set(self.generation.get().wrapping_add(0b100));
        assert_eq!(self.generation.get() & 0b11, 0);
    }

    /// Stores data by a specific key.
    ///
    /// After being stored, the data might be retrieved by
    /// `probe(key)`. This is not guaranteed though, because the entry
    /// might have been overwritten in the meantime.
    pub fn store(&self, key: u64, mut data: TtEntry) {
        // `store` and `probe` jointly implement a clever lock-less
        // hashing strategy. Rather than storing two disjoint items,
        // the key is stored XOR-ed with data, while data is stored
        // additionally as usual.

        // Set the entry's generation.
        data.gen_bound |= self.generation.get();

        // Choose a slot to which to write the data. (Each cluster has
        // 4 slots.)
        let mut cluster = unsafe { self.cluster_mut(key) };
        let mut replace_index = 0;
        let mut replace_score = 0xff;
        for (i, record) in cluster.iter_mut().enumerate() {
            // Check if this is an empty slot, or an old record for
            // the same key. If this this is the case we will use this
            // slot for the new record.
            if record.key == 0 || record.key ^ record.data_u64() == key {
                if data.move16 == 0 {
                    data.move16 = record.data.move16; // Preserve any existing move.
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
        unsafe {
            cluster.get_unchecked_mut(replace_index).key = key ^ transmute::<TtEntry, u64>(data);
            cluster.get_unchecked_mut(replace_index).data = data;
        }
    }

    /// Probes for data by a specific key.
    ///
    /// **Note:** This method may write to the transposition table
    /// (for example, to update the generation of the entry).
    #[inline]
    pub fn probe(&self, key: u64) -> Option<TtEntry> {
        let cluster = unsafe { self.cluster_mut(key) };
        for record in cluster.iter_mut() {
            if record.key ^ record.data_u64() == key {
                // If `key` and `data` were written simultaneously by
                // different search instances with different keys,
                // this will yield in a mismatch of the above
                // comparison (except for the rare and inherent key
                // collisions).
                record.update_generation(self.generation.get());
                return Some(record.data);
            }
        }
        None
    }

    /// Peeks for data by a specific key.
    ///
    /// This method does the same as `probe`, except it will never
    /// write to the transposition table. This is useful, for example,
    /// when we want to extract the primary variation from the the
    /// transposition table, without affecting any entries in the
    /// table.
    #[inline]
    pub fn peek(&self, key: u64) -> Option<TtEntry> {
        let cluster = unsafe { self.cluster_mut(key) };
        for record in cluster.iter_mut() {
            if record.key ^ record.data_u64() == key {
                return Some(record.data);
            }
        }
        None
    }

    /// Removes all entries in the table.
    pub fn clear(&self) {
        let table = unsafe { self.table.get().as_mut().unwrap() };
        for cluster in table {
            for record in cluster.iter_mut() {
                *record = Default::default();
            }
        }
        self.generation.set(0);
    }

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
    #[inline]
    unsafe fn cluster_mut(&self, key: u64) -> &mut [Record; 4] {
        let cluster_index = (key & (self.cluster_count - 1) as u64) as usize;
        self.table.get().as_mut().unwrap().get_unchecked_mut(cluster_index)
    }
}


unsafe impl Sync for Tt {}




/// Represents a record in the transposition table.
///
/// It consists of 16 bytes, and is laid out the following way:
///
/// * key        64 bit
/// * move16     16 bit
/// * value      16 bit
/// * eval value 16 bit
/// * depth       8 bit
/// * generation  6 bit
/// * bound type  2 bit
#[derive(Copy, Clone)]
struct Record {
    key: u64,
    data: TtEntry,
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
    /// Returns the contained data as one `u64` value.
    #[inline(always)]
    fn data_u64(&self) -> u64 {
        unsafe { transmute(self.data) }
    }

    /// Returns record's generation.
    #[inline(always)]
    fn generation(&self) -> u8 {
        self.data.gen_bound & 0b11111100
    }

    /// Updates record's generation.
    #[inline(always)]
    fn update_generation(&mut self, generation: u8) {
        assert_eq!(generation & 0b11, 0);

        // Since the `key` is saved XOR-ed with the data, when we
        // change the data, we have to change the stored `key` as
        // well.
        let old_data_u64 = self.data_u64();
        self.data.gen_bound = generation | self.data.bound();
        self.key ^= old_data_u64 ^ self.data_u64();
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use super::Record;
    use std;

    #[test]
    fn test_cluster_size() {
        assert_eq!(std::mem::size_of::<[Record; 4]>(), 64);
        assert_eq!(std::mem::size_of::<Record>(), 16);
    }

    #[test]
    fn test_tt_resize() {
        let mut tt = Tt::new();
        assert_eq!(unsafe { &*tt.table.get() }.capacity(), 1);
        tt.resize(1);
        assert_eq!(tt.size(), 1);
        tt.clear();
    }

    #[test]
    fn test_store_and_probe() {
        let tt = Tt::new();
        assert!(tt.probe(1).is_none());
        let data = TtEntry::new(0, 0, 100, 666, 0);
        assert_eq!(data.depth(), 100);
        assert_eq!(data.move16(), 666);
        tt.store(1, data);
        assert_eq!(tt.probe(1).unwrap().depth(), 100);
        tt.store(1, TtEntry::new(0, 0, 100, 666, 0));
        assert_eq!(tt.probe(1).unwrap().depth(), 100);
        assert_eq!(tt.probe(1).unwrap().move16(), 666);
        for i in 2..100 {
            tt.store(i, TtEntry::new(i as i16, 0, i as u8, i as u16, i as i16));
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

    #[test]
    fn test_new_search() {
        let tt = Tt::new();
        assert_eq!(tt.generation.get(), 0 << 2);
        tt.new_search();
        assert_eq!(tt.generation.get(), 1 << 2);
        for _ in 0..64 {
            tt.new_search();
        }
        assert_eq!(tt.generation.get(), 1 << 2);
    }
}
