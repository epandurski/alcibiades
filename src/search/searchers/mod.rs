//! Implementations of `SearchExecutor` trait.

mod deepening;
mod standard;

pub use self::deepening::Deepening;
pub use self::standard::StandardSrch;
