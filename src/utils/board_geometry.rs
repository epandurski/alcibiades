//! Implements `BoardGeometry`.

use rand::{Rng, thread_rng};
use chesstypes::*;
use super::bitsets::*;


/// Tables and methods useful for move generation and position
/// evaluation.
pub struct BoardGeometry {
    /// Contains bitboards with all squares lying at the line
    /// determined by two squares.
    ///
    /// # Examples:
    ///
    /// ```text
    /// g.squares_at_line[B2][F6]
    /// . . . . . . . 1
    /// . . . . . . 1 .
    /// . . . . . 1 . .
    /// . . . . 1 . . .
    /// . . . 1 . . . .
    /// . . 1 . . . . .
    /// . 1 . . . . . .
    /// 1 . . . . . . .
    /// ```
    pub squares_at_line: [[Bitboard; 64]; 64],

    /// Contains bitboards with all squares lying between two squares
    /// including the two squares themselves.
    ///
    /// # Examples:
    ///
    /// ```text
    /// g.squares_between_including[B2][F6]
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . 1 . .
    /// . . . . 1 . . .
    /// . . . 1 . . . .
    /// . . 1 . . . . .
    /// . 1 . . . . . .
    /// . . . . . . . .
    /// ```
    pub squares_between_including: [[Bitboard; 64]; 64],

    /// Contains bitboards with all squares hidden behind a blocker
    /// from attacker's position.
    ///
    /// # Examples:
    /// 
    /// ```text
    /// g.squares_behind_blocker[B2][F6]
    /// . . . . . . . 1
    /// . . . . . . 1 .
    /// . . . . . B . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . A . . . . . .
    /// . . . . . . . .
    /// ```
    pub squares_behind_blocker: [[Bitboard; 64]; 64],

    /// Contains bitboards with the squares attacked by a pawn from a
    /// given square.
    ///
    /// # Examples:
    ///
    /// ```text
    /// g.pawn_attacks[WHITE][F6]
    /// . . . . . . . .
    /// . . . . 1 . 1 .
    /// . . . . . P . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . . . .
    ///
    /// g.pawn_attacks[BLACK][H8]
    /// . . . . . . . p
    /// . . . . . . 1 .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// . . . . . . . .
    /// ```
    pub pawn_attacks: [[Bitboard; 64]; 2],
}


impl BoardGeometry {
    /// Creates and initializes a new instance.
    fn new() -> BoardGeometry {
        let mut bg = BoardGeometry {
            squares_at_line: [[0; 64]; 64],
            squares_between_including: [[0; 64]; 64],
            squares_behind_blocker: [[0; 64]; 64],
            pawn_attacks: [[0; 64]; 2],
        };

        // Fill `bg.squares_at_line`.
        for a in 0..64 {
            let lines = [bb_file(a), bb_rank(a), bb_diag(a), bb_anti_diag(a)];
            for b in a + 1..64 {
                for line in lines.iter() {
                    if *line & (1 << b) != 0 {
                        bg.squares_at_line[a][b] = *line;
                        bg.squares_at_line[b][a] = *line;
                        break;
                    }
                }
            }
        }

        // Fill `bg.squares_behind_blocker`.
        for a in 0..64 {
            for b in 0..64 {
                let queen_attacks_from_a = calc_rook_attacks(a, 1 << a | 1 << b) |
                                           calc_bishop_attacks(a, 1 << a | 1 << b);
                bg.squares_behind_blocker[a][b] = bg.squares_at_line[a][b] & !(1 << a) &
                                                  !queen_attacks_from_a;
            }
        }

        // Fill `bg.squares_between_including`.
        for a in 0..64 {
            for b in 0..64 {
                bg.squares_between_including[a][b] = bg.squares_at_line[a][b] &
                                                     !bg.squares_behind_blocker[a][b] &
                                                     !bg.squares_behind_blocker[b][a];
            }
        }

        // Fill `bg.pawn_attacks`.
        const SHIFTS: [[isize; 2]; 2] = [[7, 9], [-9, -7]];
        for us in 0..2 {
            for a in 0..64 {
                bg.pawn_attacks[us][a] = (gen_shift(1 << a, SHIFTS[us][0]) & !BB_FILE_H) |
                                         (gen_shift(1 << a, SHIFTS[us][1]) & !BB_FILE_A);
            }
        }

        // Initialize the global attack tables.
        //
        // For every chess engine it is very important to be able to
        // very quickly find the attacking sets for all pieces, from
        // all possible origin squares, and all possible board
        // occupations. We use the "magic bitboards" technique to
        // access pre-calculated attacking sets of the sliding pieces
        // (bishop, rook, queen). The "magic bitboards" technique
        // consists of four steps:
        //
        // 1. Mask the relevant occupancy bits to form a key. For
        //    example if you had a rook on a1, the relevant occupancy
        //    bits will be from A2-A7 and B1-G1.
        //
        // 2. Multiply the key by a "magic number" to obtain an index
        //    mapping. This magic number can be generated by
        //    brute-force trial and error quite easily although it
        //    isn't 100% certain that the magic number is the best
        //    possible (see step 3).
        //
        // 3. Right shift the index mapping by `64 - n` bits to create
        //    an index, where `n` is the number of bits in the
        //    index. A better magic number will have less bits
        //    required in the index.
        //
        // 4. Use the index to reference a preinitialized attacks
        //    database.
        //
        // The following illustration should give an impression, how
        // magic bitboards work:
        //
        // ```
        //                                         any consecutive
        // relevant occupancy                      combination of
        // bishop B1, 5 bits                       the masked bits
        // . . . . . . . .     . . . . . . . .     . . .[C D E F G]
        // . . . . . . . .     . 1 . . . . . .     . . . . . . . .
        // . . . . . . G .     . 1 . . . . . .     . . . . . . . .
        // . . . . . F . .     . 1 . . . . . .     . . . . . . . .
        // . . . . E . . .  *  . 1 . . . . . .  =  . . garbage . .    >> (64- 5)
        // . . . D . . . .     . 1 . . . . . .     . . . . . . . .
        // . . C . . . . .     . . . . . . . .     . . . . . . . .
        // . . . . . . . .     . . . . . . . .     . . . . . . . .
        //
        //                                         any consecutive
        // relevant occupancy                      combination of
        // rook D4, 10 bits                        the masked bits
        // . . . . . . . .     . . . . . . . .     4 5 6 B C E F G]
        // . . . 6 . . . .     . . .some . . .     . . . . . .[1 2
        // . . . 5 . . . .     . . . . . . . .     . . . . . . . .
        // . . . 4 . . . .     . . .magic. . .     . . . . . . . .
        // . B C . E F G .  *  . . . . . . . .  =  . . garbage . .    >> (64-10)
        // . . . 2 . . . .     . . .bits . . .     . . . . . . . .
        // . . . 1 . . . .     . . . . . . . .     . . . . . . . .
        // . . . . . . . .     . . . . . . . .     . . . . . . . .
        // ```
        //
        // The above illustration is correct for the B1 bishop, since
        // it has only one ray and one bit per file and works
        // kindergarten like. In general a one to one mapping of N
        // scattered occupied bits to N consecutive bits is not always
        // possible. It requires one or two gaps inside the
        // consecutive N bits, to avoid collisions, blowing up the
        // table size.
        unsafe {
            init_king_attacks();
            init_knight_attacks();
            let bishop_attacks_size = init_slider_map(BISHOP, &mut BISHOP_MAP, 0, false);
            let total_size = init_slider_map(ROOK, &mut ROOK_MAP, bishop_attacks_size, false);
            assert!(total_size == SLIDER_ATTACKS_SIZE);
        }

        bg
    }

    /// Returns a reference to an initialized `BoardGeometry` object.
    ///
    /// The object is created only during the first call. All next
    /// calls will return a reference to the same object. This is done
    /// in a thread-safe manner.
    pub fn get() -> &'static BoardGeometry {
        use std::sync::{Once, ONCE_INIT};
        static INIT_GEOMETRY: Once = ONCE_INIT;
        static mut geometry: Option<BoardGeometry> = None;
        unsafe {
            INIT_GEOMETRY.call_once(|| {
                geometry = Some(BoardGeometry::new());
            });
            geometry.as_ref().unwrap()
        }
    }

    /// Returns the set of squares that are attacked by a piece from a
    /// given square.
    ///
    /// This function returns the set of squares that are attacked by
    /// a piece of type `piece` from the square `from_square`, on a
    /// board which is occupied with pieces according to the
    /// `occupied` bitboard. `piece` **must not** be `PAWN`. It does
    /// not matter if `from_square` is occupied or not.
    #[inline(always)]
    pub fn attacks_from(&self,
                        piece: PieceType,
                        from_square: Square,
                        occupied: Bitboard)
                        -> Bitboard {
        debug_assert!(piece < PAWN);
        debug_assert!(from_square <= 63);
        unsafe {
            match piece {
                QUEEN => {
                    BISHOP_MAP[from_square].attacks(occupied) |
                    ROOK_MAP[from_square].attacks(occupied)
                }
                ROOK => ROOK_MAP[from_square].attacks(occupied),
                BISHOP => BISHOP_MAP[from_square].attacks(occupied),
                KING => KING_ATTACKS[from_square],
                _ => KNIGHT_ATTACKS[from_square],
            }
        }
    }
}


// Global attack tables (uninitialized).
const SLIDER_ATTACKS_SIZE: usize = 107648;
static mut SLIDER_ATTACKS: [Bitboard; SLIDER_ATTACKS_SIZE] = [0; SLIDER_ATTACKS_SIZE];
static mut KING_ATTACKS: [Bitboard; 64] = [0; 64];
static mut KNIGHT_ATTACKS: [Bitboard; 64] = [0; 64];


// Global slider maps (uninitialized).
static mut BISHOP_MAP: [AttacksMagic; 64] = [AttacksMagic {
    offset: 0,
    mask: 0,
    magic: 0,
    shift: 0,
}; 64];
static mut ROOK_MAP: [AttacksMagic; 64] = [AttacksMagic {
    offset: 0,
    mask: 0,
    magic: 0,
    shift: 0,
}; 64];


/// An object that for a particular slider (bishop or rook) at a
/// particular square, can "magically" find the corresponding attack
/// set, for all possible board occupations.
#[derive(Copy, Clone)]
struct AttacksMagic {
    pub offset: usize,
    pub mask: Bitboard,
    pub magic: u64,
    pub shift: u32,
}


impl AttacksMagic {
    /// Returns the attack set for given board occupation.
    #[inline(always)]
    pub unsafe fn attacks(&self, occupied: Bitboard) -> Bitboard {
        let index = (self.magic.wrapping_mul(occupied & self.mask)) >> self.shift;
        *SLIDER_ATTACKS.get_unchecked(self.offset.wrapping_add(index as usize))
    }
}


/// A helper function for `init_magics`. It initializes knight's
/// attacks table.
unsafe fn init_knight_attacks() {
    let offsets = vec![(-1, -2), (-2, -1), (-2, 1), (-1, 2), (1, -2), (2, -1), (2, 1), (1, 2)];

    for (i, attacks) in KNIGHT_ATTACKS.iter_mut().enumerate() {
        let (r, c) = ((i / 8) as isize, (i % 8) as isize);

        for &(dr, dc) in &offsets {
            if r + dr >= 0 && c + dc >= 0 && r + dr < 8 && c + dc < 8 {
                *attacks |= 1 << ((r + dr) * 8 + c + dc);
            }
        }
    }
}


/// A helper function for `init_magics`. It initializes king's attacks
/// table.
unsafe fn init_king_attacks() {
    let offsets = vec![(1, -1), (1, 0), (1, 1), (0, -1), (0, 1), (-1, -1), (-1, 0), (-1, 1)];

    for (i, attacks) in KING_ATTACKS.iter_mut().enumerate() {
        let (r, c) = ((i / 8) as isize, (i % 8) as isize);

        for &(dr, dc) in &offsets {
            if r + dr >= 0 && c + dc >= 0 && r + dr < 8 && c + dc < 8 {
                *attacks |= 1 << ((r + dr) * 8 + c + dc);
            }
        }
    }
}


/// A helper function for `init_magics`. It initializes the look-up
/// tables for a particular slider (bishop or rook).
unsafe fn init_slider_map(piece: PieceType,
                          piece_map: &mut [AttacksMagic; 64],
                          mut offset: usize,
                          from_scratch: bool)
                          -> usize {
    assert!(piece == BISHOP || piece == ROOK);
    let mut rng = thread_rng();

    for (sq, entry) in piece_map.iter_mut().enumerate() {
        let attacks: fn(Square, Bitboard) -> Bitboard = if piece == BISHOP {
            calc_bishop_attacks
        } else {
            calc_rook_attacks
        };
        let edges = ((BB_RANK_1 | BB_RANK_8) & !bb_rank(sq)) |
                    ((BB_FILE_A | BB_FILE_H) & !bb_file(sq));
        let mask = attacks(sq, 1 << sq) & !edges;
        let num_ones = mask.count_ones();
        let shift = 64 - num_ones;

        let mut occupancy = vec![0; 1 << num_ones];
        let mut reference = vec![0; 1 << num_ones];
        let mut size = 0;
        let mut occ = 0;
        loop {
            occupancy[size] = occ;
            reference[size] = attacks(sq, occ | (1 << sq));
            size += 1;
            occ = occ.wrapping_sub(mask) & mask;
            if occ == 0 {
                // We have tried all relevant values for `occ`.
                break;
            }
        }

        let mut magic = if piece == BISHOP {
            BISHOP_MAGICS[sq]
        } else {
            ROOK_MAGICS[sq]
        };

        'outer: loop {
            if from_scratch {
                // Generate a new random magic from scratch.
                loop {
                    magic = rng.gen::<u64>() & rng.gen::<u64>() & rng.gen::<u64>();
                    if ((magic * mask) >> 56).count_ones() >= 6 {
                        break;
                    }
                }
            }

            let mut attacks = vec![0; size];
            for i in 0..size {
                let index = magic.wrapping_mul(occupancy[i]) >> shift;
                let attack = &mut attacks[index as usize];
                if *attack != 0 && *attack != reference[i] {
                    assert!(from_scratch,
                            "Error: Precalculated magic is incorrect. Square {}, for {} magic",
                            sq,
                            if piece == BISHOP {
                                "bishop"
                            } else {
                                "rook"
                            });
                    continue 'outer;
                }
                *attack = reference[i];
            }

            *entry = AttacksMagic {
                offset: offset,
                mask: mask,
                magic: magic,
                shift: shift,
            };
            for (i, &att) in attacks.iter().enumerate() {
                SLIDER_ATTACKS[offset + i] = att;
            }
            offset += size;
            break;
        }
    }
    offset
}


/// Pre-calculated bishop magic constants.
const BISHOP_MAGICS: [u64; 64] = [306397059236266368,
                                  6638343277122827280,
                                  10377420549504106496,
                                  9193021019258913,
                                  2306408226914042898,
                                  10379110636817760276,
                                  27167319028441088,
                                  7566153073497751552,
                                  1513227076520969216,
                                  301917653126479936,
                                  72075465430409232,
                                  2343002121441460228,
                                  36033212782477344,
                                  9223373154083475456,
                                  6935629192638251008,
                                  72621648200664064,
                                  2310506081245267984,
                                  2533291987569153,
                                  146934404644733024,
                                  1838417834950912,
                                  579856052833622016,
                                  1729946448243595776,
                                  705208029025040,
                                  2886877732040869888,
                                  10092575566416331020,
                                  5635409948247040,
                                  738739924278198804,
                                  4648849515743289408,
                                  9233786889293807616,
                                  1155253577929753088,
                                  435164712050360592,
                                  3026700562025580641,
                                  4612284839965491969,
                                  10448650511900137472,
                                  571823356120080,
                                  40569782189687936,
                                  148620986995048708,
                                  4901113822871308288,
                                  4612077461748908288,
                                  10204585674276944,
                                  2534512027246592,
                                  5766297627561820676,
                                  13809969191200768,
                                  1153062656578422784,
                                  9318235838682899712,
                                  11533824475839595776,
                                  433770548762247233,
                                  92326036501692936,
                                  9227053213059129360,
                                  577024872779350852,
                                  108087561569959936,
                                  582151826703646856,
                                  81404176367767,
                                  316415319130374273,
                                  9113856212762624,
                                  145453328103440392,
                                  441392350330618400,
                                  1126492748710916,
                                  2309220790581891072,
                                  3026423624667006980,
                                  18019391702696464,
                                  4516931289817600,
                                  1450317422841301124,
                                  9246488805123342592];


/// Pre-calculated rook magic constants.
const ROOK_MAGICS: [u64; 64] = [36028867955671040,
                                2395917338224361536,
                                936757656041832464,
                                648535942831284356,
                                36037595259731970,
                                13943151043426386048,
                                432349966580056576,
                                4683745813775001856,
                                1191624314978336800,
                                4611756662317916160,
                                4625338105090543616,
                                140806208356480,
                                1688987371057664,
                                9288708641522688,
                                153403870897537280,
                                281550411726850,
                                2401883155071024,
                                1206964838111645696,
                                166705754384925184,
                                36039792408011264,
                                10376580514281768960,
                                9148486532465664,
                                578787319189340418,
                                398007816633254020,
                                2341872150903791616,
                                2314850762536009728,
                                297238127310798880,
                                2251868801728768,
                                2594082183614301184,
                                820222482337235456,
                                37717655469424904,
                                577596144088011012,
                                1152991874030502016,
                                3171026856472219648,
                                20415869351890944,
                                4611844348286345472,
                                2455605323386324224,
                                140754676613632,
                                1740713828645089416,
                                58361257132164,
                                70370893791232,
                                9227880322828615684,
                                72092778695295040,
                                577023839834341392,
                                4723150143565660416,
                                563087661073408,
                                651083773116450,
                                72128789630550047,
                                153192758223054976,
                                869194865525653568,
                                4972009250306933248,
                                1031325449119138048,
                                1297041090863464576,
                                580401419157405824,
                                1657992643584,
                                306245066729521664,
                                15206439601351819394,
                                14143290885479661953,
                                1688988407201810,
                                18065251325837538,
                                1152927311403745429,
                                162411078742050817,
                                334255838724676,
                                27323018585852550];


#[cfg(test)]
mod tests {
    use super::*;
    use chesstypes::*;

    #[test]
    fn test_line_sets() {
        let g = BoardGeometry::new();
        assert_eq!(g.squares_at_line[B1][G1], 0b11111111);
        assert_eq!(g.squares_at_line[G8][B8], 0b11111111 << 56);
        assert_eq!(g.squares_between_including[B1][G1], 0b01111110);
        assert_eq!(g.squares_between_including[G8][B8], 0b01111110 << 56);
        assert_eq!(g.squares_behind_blocker[B1][G1], 1 << H1);
        assert_eq!(g.squares_behind_blocker[G8][B8], 1 << A8);
        assert_eq!(g.squares_behind_blocker[A1][G7], 1 << H8);
        assert_eq!(g.squares_behind_blocker[H1][B7], 1 << A8);
        assert_eq!(g.squares_behind_blocker[B7][G2], 1 << H1);
        assert_eq!(g.squares_behind_blocker[G7][B2], 1 << A1);
        assert_eq!(g.squares_behind_blocker[D7][D7], 0);
        assert_eq!(g.squares_behind_blocker[D7][F8], 0);
        assert_eq!(g.squares_between_including[A1][A4] | g.squares_behind_blocker[A1][A4],
                   g.squares_at_line[A1][A4]);
    }

    #[test]
    fn test_attacks_from() {
        let g = BoardGeometry::new();
        for piece in KING..PAWN {
            for square in 0..64 {
                assert_eq!(g.attacks_from(piece, square, 0),
                           g.attacks_from(piece, square, 1 << square));
                assert_eq!(g.attacks_from(piece, square, 1 << D4),
                           g.attacks_from(piece, square, 1 << D4 | 1 << square));
            }
        }
    }
}
