extern crate socrates;

#[test]
fn test_attack_sets() {
    use socrates::utils::{generate_attack_and_blockers_arrays};
    let (att_sets, bl_sets) = generate_attack_and_blockers_arrays();
    assert_eq!(att_sets[0][0], 0b11<<8 | 0b10);
    assert_eq!(bl_sets[0][0], 0);
    assert_eq!(att_sets[2][0], 0b11111110 | 1<<8 | 1<<16 | 1<<24 | 1<<32 | 1<<40 | 1<<48 | 1<<56);
    assert_eq!(bl_sets[2][0], 0b01111110 | 1<<8 | 1<<16 | 1<<24 | 1<<32 | 1<<40 | 1<<48 | 0<<56);
}

#[test]
fn test_fen_parsing() {
    use socrates::utils::*;
    assert!(Board::from_fen("nbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_err());
    assert!(Board::from_fen("rnbqkbnr1/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_err());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBN b KQkq e3 0 1").is_err());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR/ b KQkq e3 0 1").is_err());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNRR b KQkq e3 0 1").is_err());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP01PPP/RNBQKBNR b KQkq e3 0 1").is_err());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP91PPP/RNBQKBNR b KQkq e3 0 1").is_err());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPP*1PPP/RNBQKBNR b KQkq e3 0 1").is_err());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 * 1").is_err());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 *").is_err());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b - e3 0 1").is_ok());
    assert!(Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").is_ok());
}

