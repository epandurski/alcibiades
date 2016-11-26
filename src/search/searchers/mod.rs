//! Implementations of the `SearchExecutor` trait.

mod deepening;
mod alphabeta;

pub use self::deepening::Deepening; 
pub use self::alphabeta::Alphabeta; 
