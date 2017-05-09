//! Implements `ZobristArrays`.


/// Tables for calculating Zobrist hashes.
///
/// Zobrist hashing is a technique to transform a board position into
/// a number of a fixed length, with an equal distribution over all
/// possible numbers, invented by Albert Zobrist. The key property of
/// this method is that two similar positions generate entirely
/// different hash numbers.
pub struct ZobristArrays {
    /// The constant with which the hash value should be XOR-ed when
    /// the side to move changes.
    pub to_move: u64,

    /// Constants with which the hash value should be XOR-ed when a
    /// piece of given color on a given square appears/disappears.
    pub pieces: [[[u64; 64]; 6]; 2],

    /// Constants with which the hash value should be XOR-ed, for the
    /// old and the new castling rights on each move.
    pub castling_rights: [u64; 16],

    /// Constants with which the hash value should be XOR-ed, for the
    /// old and the new en-passant file on each move (a value between
    /// 0 and 7). Indexes between 8 and 15 point to zeroes, and are
    /// for convenience and memory safety.
    pub enpassant_file: [u64; 16],

    /// Constants with which the hash value should be XOR-ed to
    /// reflect the number of half-moves played without capturing a
    /// piece or advancing a pawn.
    pub halfmove_clock: [u64; 100],
}


impl ZobristArrays {
    /// Creates and initializes a new instance.
    fn new() -> ZobristArrays {
        use rand::{Rng, SeedableRng};
        use rand::isaac::Isaac64Rng;

        let seed: &[_] = &[1, 2, 3, 4];
        let mut rng: Isaac64Rng = SeedableRng::from_seed(seed);

        let to_move = rng.gen();
        let mut pieces = [[[0; 64]; 6]; 2];
        let mut castling_rights = [0; 16];
        let mut enpassant_file = [0; 16];
        let mut halfmove_clock = [0; 100];

        for color in 0..2 {
            for piece in 0..6 {
                for square in 0..64 {
                    pieces[color][piece][square] = rng.gen();
                }
            }
        }

        for value in 0..16 {
            castling_rights[value] = rng.gen();
        }

        for file in 0..8 {
            enpassant_file[file] = rng.gen();
        }

        for n in 0..100 {
            halfmove_clock[n] = rng.gen();
        }

        ZobristArrays {
            to_move: to_move,
            pieces: pieces,
            castling_rights: castling_rights,
            enpassant_file: enpassant_file,
            halfmove_clock: halfmove_clock,
        }
    }

    /// Returns a reference to an initialized `ZobristArrays` object.
    ///
    /// The object is created only during the first call. All next
    /// calls will return a reference to the same object. This is done
    /// in a thread-safe manner.
    pub fn get() -> &'static ZobristArrays {
        use std::sync::{Once, ONCE_INIT};
        static INIT_ARRAYS: Once = ONCE_INIT;
        static mut ARRAYS: Option<ZobristArrays> = None;
        unsafe {
            INIT_ARRAYS.call_once(|| { ARRAYS = Some(ZobristArrays::new()); });
            ARRAYS.as_ref().unwrap()
        }
    }
}
