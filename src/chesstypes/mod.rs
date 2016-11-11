//! Defines basic types and constants like squares, pieces, moves,
//! evaluation.

pub mod board_representation;
pub mod evaluation;
pub mod castling;
pub mod moves;

pub use self::board_representation::*;
pub use self::castling::{CastlingRights, CastlingSide, QUEENSIDE, KINGSIDE};
pub use self::evaluation::*;
pub use self::moves::{Move, MoveDigest};
pub use self::moves::{MoveType, MOVE_ENPASSANT, MOVE_PROMOTION, MOVE_CASTLING, MOVE_NORMAL};
