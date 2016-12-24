use uci::SetOption;
use board::*;
use search_executor::*;
use pv::*;


pub trait TimeManager: SetOption {
    /// Creates a new instance.
    ///
    /// `position` gives the current position. `wtime_millis`,
    /// `btime_millis`, `winc_millis`, and `binc_millis` specify the
    /// remaining time in milliseconds, and the number of milliseconds
    /// with which the remaining time will be incremented on each move
    /// (for black and white). `movestogo` specifies the number of
    /// moves to the next time control.
    fn new(board: &Board,
           wtime_millis: Option<u64>,
           btime_millis: Option<u64>,
           winc_millis: Option<u64>,
           binc_millis: Option<u64>,
           movestogo: Option<u64>)
           -> Self;

    /// Registers a new search report with the time manager.
    fn update(&mut self, report: &SearchReport<Vec<Variation>>);

    /// Decides if the search must be terminated.
    fn must_play(&self) -> bool;
}
