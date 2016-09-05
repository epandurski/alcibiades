use std::cmp::min;
use basetypes::*;
use search::SearchStatus;


pub struct TimeManager {
    move_time_millis: u64, // move time in milliseconds
}


impl TimeManager {
    #[allow(unused_variables)]
    pub fn new(us: Color,
               pondering_is_allowed: bool,
               wtime: Option<u64>,
               btime: Option<u64>,
               winc: Option<u64>,
               binc: Option<u64>,
               movestogo: Option<u64>)
               -> TimeManager {
        // TODO: We ignore "pondering_is_allowed".

        let (time, inc) = if us == WHITE {
            (wtime, winc.unwrap_or(0))
        } else {
            (btime, binc.unwrap_or(0))
        };
        let time = time.unwrap_or(0);
        let movestogo = movestogo.unwrap_or(40);
        let movetime = (time + inc * movestogo) / movestogo;
        TimeManager { move_time_millis: min(movetime, time / 2) }
    }

    pub fn must_play(&self, search_status: &SearchStatus) -> bool {
        // TODO: Implement smarter time management.
        search_status.duration_millis >= self.move_time_millis
    }
}


