//! This module implements an efficient transposition table.


#[derive(Default)]
struct Cluster {
    entries: [Entry; 3],
    _padding: [u8; 2],
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
pub struct Entry {
    key: u16,
    move16: u16,
    value: i16,
    eval_value: i16,
    gen_bound: u8,
    depth: u8,
}


impl Entry {
    fn new(key: u16,
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
    fn key(&self) -> u16 {
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


pub struct TranspositionTable {
    generation: u8,
    table: Vec<Cluster>,
}


impl TranspositionTable {
    fn new_search(&mut self) {
        self.generation += 0b100;  // Lower 2 bits are used by bound type
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use super::Cluster;
    use basetypes::*;

    #[test]
    fn test_cluster_size() {
        assert_eq!(::std::mem::size_of::<Cluster>(), 32);
    }
}
