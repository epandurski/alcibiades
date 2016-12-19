//! Defines basic types and constants like squares, pieces, moves,
//! value.

mod board;
mod castling;
mod moves;
pub mod squares;
pub mod files;
pub mod ranks;

pub use self::board::*;
pub use self::castling::*;
pub use self::moves::*;


/// Represents a notation error.
pub struct NotationError;
