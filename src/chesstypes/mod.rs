//! Defines basic types and constants like squares, pieces, moves,
//! value.

mod board;
mod castling;
pub mod squares;
pub mod files;
pub mod ranks;

pub use self::board::*;
pub use self::castling::*;


/// `MOVE_ENPASSANT`, `MOVE_PROMOTION`, `MOVE_CASTLING`, or
/// `MOVE_NORMAL`.
pub type MoveType = usize;

pub const MOVE_ENPASSANT: MoveType = 0;
pub const MOVE_PROMOTION: MoveType = 1;
pub const MOVE_CASTLING: MoveType = 2;
pub const MOVE_NORMAL: MoveType = 3;
