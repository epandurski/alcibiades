//! This module implements an efficient transposition table.

use std;
use std::cell::UnsafeCell;
use std::mem::transmute;


#[derive(Copy, Clone)]
pub struct EntryData {
    move16: u16,
    value: i16,
    eval_value: i16,
    gen_bound: u8,
    depth: u8,
}


impl EntryData {
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
    pub fn depth(&self) -> u8 {
        self.depth
    }

    #[inline(always)]
    pub fn bound(&self) -> u8 {
        self.gen_bound & 0b11
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
    fn new(key: u64,
           value: i16,
           bound: u8,
           depth: u8,
           move16: u16,
           generation: u8,
           eval_value: i16)
           -> Entry {
        Entry {
            key: key,
            data: EntryData {
                move16: move16,
                value: value,
                eval_value: eval_value,
                gen_bound: generation | bound,
                depth: depth, // TODO: DEPTH_NONE?
            },
        }
    }

    #[inline(always)]
    fn key(&self) -> u64 {
        self.key
    }

    #[inline(always)]
    fn data(&self) -> u64 {
        unsafe { transmute(self.data) }
    }

    #[inline(always)]
    fn generation(&self) -> u8 {
        self.data.gen_bound & 0b11111100
    }
}


#[derive(Default, Clone)]
struct Cluster {
    entries: [Entry; 4],
}


pub struct TranspositionTable {
    generation: u8,
    cluster_count: usize,
    table: UnsafeCell<Vec<Cluster>>,
}


impl TranspositionTable {
    pub fn new() -> TranspositionTable {
        TranspositionTable {
            generation: 0,
            cluster_count: 1,
            table: UnsafeCell::new(vec![Default::default()]),
        }
    }

    pub fn new_search(&mut self) {
        self.generation += 0b100;  // Lower 2 bits are used by bound type
    }

    pub fn resize(&mut self, size_mb: usize) {
        // The cluster count should be in the form "2**n", so the best
        // we can do is to ensure that the new cluster count will be
        // as close as possible but no greater than the requested one.
        let requested_cluster_count = (size_mb * 1024 * 1024) / std::mem::size_of::<Cluster>();

        // First we make sure `requested_cluster_count` is exceeded.
        let mut new_cluster_count = 1;
        while requested_cluster_count >= new_cluster_count {
            new_cluster_count <<= 1;
        }

        // Then we make one step back, making sure that the new
        // cluster count is not zero.
        if new_cluster_count > 1 {
            new_cluster_count >>= 1;
        } else {
            new_cluster_count = 1;
        }
        if new_cluster_count != self.cluster_count {
            self.cluster_count = new_cluster_count;
            self.table = UnsafeCell::new(vec![Default::default(); new_cluster_count]);
        }
    }

    pub fn clear(&mut self) {
        self.table = UnsafeCell::new(vec![Default::default(); self.cluster_count]);
    }

    pub fn probe(&self, key: u64) -> Option<EntryData> {
        let cluster = self.cluster_mut(key);
        for entry in cluster.entries.iter_mut() {
            if entry.key() ^ entry.data() == key {
                // Transposition table hit. Refresh entry's generation
                // and return the entry.
                entry.data.gen_bound = self.generation | entry.data.bound();
                return Some(entry.data);
            }
        }
        // Transposition table miss.
        None
    }

    pub fn store(&self, key: u64, mut data: EntryData) {
        let mut entries = self.cluster_mut(key).entries;
        let mut replace_index = 0;
        for i in 0..4 {
            let e = entries[i];
            if e.key() == 0 || e.key() ^ e.data() == key {
                // This is either an empty entry, or we overwrite an
                // old entry for the same key.
                if data.move16 == 0 {
                    // Preserve any existing move.
                    data.move16 = e.data.move16;
                }
                replace_index = i;
                break;
            }
            // Implement replace strategy
            // if (e.generation() == self.generation || e.data.bound() == Bound::BOUND_EXACT) -
            //    (entries[replace_index].generation() == self.generation) -
            //    (e.data.depth < entries[replace_index].data.depth) < 0 {
            //     replace_index = i;

            // }
        }
        unsafe {
            entries.get_unchecked_mut(replace_index).key = key ^ transmute::<EntryData, u64>(data);
            entries.get_unchecked_mut(replace_index).data = data;
        }
    }

    #[inline(always)]
    fn cluster_mut(&self, key: u64) -> &mut Cluster {
        let cluster_index = (key & (self.cluster_count - 1) as u64) as usize;
        unsafe { (&mut *self.table.get()).get_unchecked_mut(cluster_index) }
    }


    // void TranspositionTable::store(const Key key, Value v, Bound b, Depth d, Move m, Value statV) {

    //   TTEntry *tte, *replace;
    //   uint16_t key16 = key >> 48; // Use the high 16 bits as key inside the cluster

    // tte = replace = first_entry(key);

    //   for (unsigned i = 0; i < TTClusterSize; ++i, ++tte)
    //   {
    //       if (!tte->key16 || tte->key16 == key16) // Empty or overwrite old
    //       {
    //           if (!m)
    //               m = tte->move(); // Preserve any existing ttMove

    //           replace = tte;
    //           break;
    //       }

    //       // Implement replace strategy
    //       if (  ((    tte->genBound8 & 0xFC) == generation || tte->bound() == BOUND_EXACT)
    //           - ((replace->genBound8 & 0xFC) == generation)
    //           - (tte->depth8 < replace->depth8) < 0)
    //           replace = tte;
    //   }

    //   replace->save(key16, v, b, d, m, generation, statV);
    // }




    // const TTEntry* TranspositionTable::probe(const Key key) const {

    //   TTEntry* tte = first_entry(key);
    //   uint16_t key16 = key >> 48;

    //   for (unsigned i = 0; i < TTClusterSize; ++i, ++tte)
    //       if (tte->key16 == key16)
    //       {
    //           tte->genBound8 = generation | tte->bound(); // Refresh
    //           return tte;
    //       }

    //   return NULL;
    // }
}


pub enum Bound {
    BOUND_NONE = 0,
    BOUND_UPPER = 1,
    BOUND_LOWER = 2,
    BOUND_EXACT = 3,
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::{Cluster, Entry};
    use std;
    use basetypes::*;

    #[test]
    fn test_cluster_size() {
        assert_eq!(std::mem::size_of::<Cluster>(), 64);
        assert_eq!(std::mem::size_of::<Entry>(), 16);
    }

    #[test]
    fn test_tt_resize() {
        let mut tt = TranspositionTable::new();
        assert_eq!(unsafe { &*tt.table.get() }.capacity(), 1);
        tt.resize(1);
        assert_eq!(unsafe { &*tt.table.get() }.capacity(),
                   1024 * 1024 / std::mem::size_of::<Cluster>());
        tt.clear();
    }
}
