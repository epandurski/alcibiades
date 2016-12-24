//! Defines the `TimeManager` trait.

use uci::SetOption;
use board::*;
use search_executor::*;
use pv::*;


/// A trait for deciding when the search must be terminated and the
/// best move played.
pub trait TimeManager: SetOption {
    /// Creates a new instance.
    ///
    /// * `board` gives the current position.
    ///
    /// * `wtime_millis` and `btime_millis` specify the remaining time
    ///   in milliseconds for white and black.
    ///
    /// * `winc_millis` and `binc_millis` specify the number of
    ///   milliseconds with which the remaining time will be
    ///   incremented on each move for white and black.
    ///
    /// * `movestogo` specifies the number of moves to the next time
    ///   control.
    fn new(board: &Board,
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
    fn must_play(&self) -> bool;
}
