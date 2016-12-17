//! Defines basic types and constants like squares, pieces, moves,
//! value.

pub mod board_representation;
pub mod castling;
pub mod moves;

pub use self::board_representation::*;
pub use self::castling::*;
pub use self::moves::*;


/// Represents a notation error.
pub struct NotationError;
