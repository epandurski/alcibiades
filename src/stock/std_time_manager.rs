use std::sync::RwLock;
use std::cmp::min;
use std::time::SystemTime;
use board::*;
use search_executor::*;
use pv::*;
use time_manager::TimeManager;
use uci::{SetOption, OptionDescription};


/// Decides when the search must be terminated.
pub struct StdTimeManager {
    started_at: SystemTime,
    move_time_millis: u64, // move time in milliseconds
    must_play: bool,
}


impl<S> TimeManager<S> for StdTimeManager
    where S: SearchExecutor<ReportData = Vec<Variation>>
{
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
    fn new(board: &Board,
           halfmove_clock: u8,
           wtime_millis: Option<u64>,
           btime_millis: Option<u64>,
           winc_millis: Option<u64>,
           binc_millis: Option<u64>,
           movestogo: Option<u64>)
           -> StdTimeManager {
        // TODO: We ignore "PONDER".

        let (time, inc) = if board.to_move == WHITE {
            (wtime_millis, winc_millis.unwrap_or(0))
        } else {
            (btime_millis, binc_millis.unwrap_or(0))
        };
        let time = time.unwrap_or(0);
        let movestogo = movestogo.unwrap_or(40);
        let movetime = (time + inc * movestogo) / movestogo;
        StdTimeManager {
            started_at: SystemTime::now(),
            move_time_millis: min(movetime, time / 2),
            must_play: false,
        }
    }

    /// Registers the current search status with the time manager.
    #[allow(unused_variables)]
    fn update(&mut self, report: &SearchReport<Vec<Variation>>) {
        // TODO: Implement smarter time management.
        let duration = self.started_at.elapsed().unwrap();
        let duration_millis = 1000 * duration.as_secs() + duration.subsec_nanos() as u64 / 1000000;
        self.must_play = duration_millis >= self.move_time_millis;
    }

    /// Decides if the search must be terminated.
    #[inline]
    fn must_play(&self) -> bool {
        self.must_play
    }
}


impl SetOption for StdTimeManager {
    fn options() -> Vec<(String, OptionDescription)> {
        vec![("Ponder".to_string(), OptionDescription::Check { default: false })]
    }

    fn set_option(name: &str, value: &str) {
        if name == "Ponder" {
            match value {
                "true" => *PONDER.write().unwrap() = true,
                "false" => *PONDER.write().unwrap() = false,
                _ => (),
            }
        }
    }
}


lazy_static! {
    static ref PONDER: RwLock<bool> = RwLock::new(false);
}
