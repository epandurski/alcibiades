use std::sync::RwLock;
use std::cmp::min;
use std::time::SystemTime;
use board::*;
use search_executor::*;
use hash_table::Variation;
use search_node::SearchNode;
use time_manager::{TimeManager, RemainingTime};
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
    #[allow(unused_variables)]
    fn new(position: &S::SearchNode, time: RemainingTime) -> StdTimeManager {
        // TODO: We ignore "PONDER".

        let (t, inc) = if position.board().to_move == WHITE {
            (time.white_millis, time.winc_millis)
        } else {
            (time.black_millis, time.binc_millis)
        };
        let movestogo = time.movestogo.unwrap_or(40);
        let movetime = (t + inc * movestogo) / movestogo;
        StdTimeManager {
            started_at: SystemTime::now(),
            move_time_millis: min(movetime, t / 2),
            must_play: false,
        }
    }

    #[allow(unused_variables)]
    fn update(&mut self, report: &SearchReport<Vec<Variation>>) {
        // TODO: Implement smarter time management.
        let duration = self.started_at.elapsed().unwrap();
        let duration_millis = 1000 * duration.as_secs() + duration.subsec_nanos() as u64 / 1000000;
        self.must_play = duration_millis >= self.move_time_millis;
    }

    #[allow(unused_variables)]
    fn must_play(&self, search: &S) -> bool {
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
