use std::cmp::min;
use std::time::SystemTime;
use chesstypes::*;
use search::SearchNode;
use super::Variation;


/// Contains information about the current progress of a search.
pub struct SearchStatus {
    /// `true` if the search is done, `false` otherwise.
    pub done: bool,

    /// The search depth completed so far.
    pub depth: u8,

    /// The number of different first moves that are being considered
    /// (from the root position).
    pub searchmoves_count: usize,

    /// The best variations found so far, sorted by descending first
    /// move strength. The first move in each variation will be
    /// different.
    pub variations: Vec<Variation>,

    /// The starting time for the search.
    pub started_at: SystemTime,

    /// The duration of the search in milliseconds.
    pub duration_millis: u64,

    /// The number of analyzed nodes.
    pub searched_nodes: u64,

    /// Average number of analyzed nodes per second.
    pub nps: u64,
}


/// Decides when the search must be terminated.
pub struct TimeManager {
    move_time_millis: u64, // move time in milliseconds
    must_play: bool,
}

impl TimeManager {
    /// Creates a new instance.
    ///
    /// `position` gives the current position. `pondering_is_allowed`
    /// tells if the engine is allowed to use opponent's time for
    /// thinking. `wtime_millis`, `btime_millis`, `winc_millis`, and
    /// `binc_millis` specify the remaining time in milliseconds, and
    /// the number of milliseconds with which the remaining time will
    /// be incremented on each move (for black and white). `movestogo`
    /// specifies the number of moves to the next time control.
    #[allow(unused_variables)]
    pub fn new(position: &SearchNode,
               pondering_is_allowed: bool,
               wtime_millis: Option<u64>,
               btime_millis: Option<u64>,
               winc_millis: Option<u64>,
               binc_millis: Option<u64>,
               movestogo: Option<u64>)
               -> TimeManager {
        // TODO: We ignore "pondering_is_allowed".

        let (time, inc) = if position.to_move() == WHITE {
            (wtime_millis, winc_millis.unwrap_or(0))
        } else {
            (btime_millis, binc_millis.unwrap_or(0))
        };
        let time = time.unwrap_or(0);
        let movestogo = movestogo.unwrap_or(40);
        let movetime = (time + inc * movestogo) / movestogo;
        TimeManager {
            move_time_millis: min(movetime, time / 2),
            must_play: false,
        }
    }

    /// Registers the current search status with the time manager.
    pub fn update_status(&mut self, new_status: &SearchStatus) {
        // TODO: Implement smarter time management.
        self.must_play = new_status.duration_millis >= self.move_time_millis;
    }

    /// Decides if the search must be terminated.
    #[inline]
    pub fn must_play(&self) -> bool {
        self.must_play
    }
}
