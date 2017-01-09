//! Defines the `TimeManager` trait.

use uci::SetOption;
use hash_table::Variation;
use search_executor::*;


/// Describes the remaining time on the clocks.
pub struct RemainingTime {
    /// The remaining time in milliseconds for white.
    pub white_millis: u64,

    /// The remaining time in milliseconds for black.
    pub black_millis: u64,

    /// The number of milliseconds with which white's remaining time
    /// is incremented on each move.
    pub winc_millis: u64,

    /// The number of milliseconds with which black's remaining time
    /// is incremented on each move.
    pub binc_millis: u64,

    /// The number of moves to the next time control.
    ///
    /// Can not be zero.
    pub movestogo: Option<u64>,
}


/// A trait for deciding when the search must be terminated and the
/// best move played.
pub trait TimeManager<T: SearchExecutor<ReportData = Vec<Variation>>>: SetOption {
    /// Creates a new instance.
    ///
    /// * `position` gives the current position.
    ///
    /// * `time` gives the remaining time on the clocks.
    fn new(position: &T::SearchNode, time: &RemainingTime) -> Self;

    /// Registers a new search progress report with the time manager.
    fn update(&mut self, report: &SearchReport<Vec<Variation>>);

    /// Decides whether the search must be terminated and the best
    /// move played.
    fn must_play(&self) -> bool;
}
