//! Defines the `TimeManager` trait.

use uci::SetOption;
use search_executor::*;
use pv::*;


/// A trait for deciding when the search must be terminated and the
/// best move played.
pub trait TimeManager<T: SearchExecutor<ReportData = Vec<Variation>>>: SetOption {
    /// Creates a new instance.
    ///
    /// * `position` gives the current position.
    ///
    /// * `wtime_millis` specify the remaining time in milliseconds
    ///   for white.
    ///
    /// * `btime_millis` specify the remaining time in milliseconds
    ///   for black.
    ///
    /// * `winc_millis` specify the number of milliseconds with which
    ///   white's remaining time is incremented on each move.
    ///
    /// * `binc_millis` specify the number of milliseconds with which
    ///   black's remaining time is incremented on each move.
    ///
    /// * `movestogo` specifies the number of moves to the next time
    ///   control.
    fn new(position: &T::SearchNode,
           wtime_millis: Option<u64>,
           btime_millis: Option<u64>,
           winc_millis: Option<u64>,
           binc_millis: Option<u64>,
           movestogo: Option<u64>)
           -> Self;

    /// Registers a new search report with the time manager.
    fn update(&mut self, report: &SearchReport<Vec<Variation>>);

    /// Decides whether the search must be terminated and the best
    /// move played.
    fn must_play(&self, search: &T) -> bool;
}
