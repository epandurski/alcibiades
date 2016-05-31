//! This module implements an efficient transposition table.

use std;
use std::cell::UnsafeCell;


#[derive(Default, Clone)]
struct Cluster {
    entries: [Entry; 4],
}


/// Represents transposition table entry.
///
/// It is 10 bytes, defined as below:
///
/// * key        16 bit
/// * move16     16 bit
/// * value      16 bit
/// * eval value 16 bit
/// * generation  6 bit
/// * bound type  2 bit
/// * depth       8 bit
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Entry {
    key: u64,
    move16: u16,
    value: i16,
    eval_value: i16,
    gen_bound: u8,
    depth: u8,
}


impl Default for Entry {
    fn default() -> Entry {
        Entry {
            key: 0,
            move16: 0,
            value: 0,
            eval_value: 0,
            gen_bound: 0,
            depth: 0,
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
            move16: move16,
            value: value,
            eval_value: eval_value,
            gen_bound: generation | bound,
            depth: depth,
        }
    }

    #[inline(always)]
    fn key(&self) -> u64 {
        self.key
    }

    #[inline(always)]
    fn data(&self) -> u64 {
        self.key
    }

    #[inline(always)]
    fn move16(&self) -> u16 {
        self.move16
    }

    #[inline(always)]
    fn value(&self) -> i16 {
        self.value
    }

    #[inline(always)]
    fn eval_value(&self) -> i16 {
        self.eval_value
    }

    #[inline(always)]
    fn depth(&self) -> u8 {
        self.depth
    }

    #[inline(always)]
    fn bound(&self) -> u8 {
        self.gen_bound & 0b11
    }
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

    pub fn probe(&self, key: u64) -> Option<Entry> {
        let cluster: &mut Cluster = unsafe {
            (&mut *self.table.get()).get_unchecked_mut(self.cluster_index(key))
        };

        for i in 0..4 {
            let entry = cluster.entries[i];
            if entry.key() ^ entry.data() == key {
                return Some(entry);
            }
        }
        None
    }

    #[inline(always)]
    fn cluster_index(&self, key: u64) -> usize {
        (key & (self.cluster_count - 1) as u64) as usize
    }


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


#[cfg(test)]
mod tests {
    use super::*;
    use super::Cluster;
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
