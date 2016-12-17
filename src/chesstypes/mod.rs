//! Defines basic types and constants like squares, pieces, moves,
//! value.

pub mod board_representation;
pub mod castling;
pub mod moves;
pub mod depth;
pub mod notation;

pub use self::board_representation::*;
pub use self::castling::*;
pub use self::moves::*;
pub use self::depth::*;
pub use self::notation::*;
