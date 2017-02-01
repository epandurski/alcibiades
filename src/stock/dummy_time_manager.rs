//! Implements `DummyTimeManager`.

use search_executor::*;
use hash_table::Variation;
use time_manager::{TimeManager, RemainingTime};
use uci::SetOption;


/// Implements a time manager that never terminates the search.
///
/// This is useful when the search executor implements its own time
/// management logic.
pub struct DummyTimeManager;


impl<T> TimeManager<T> for DummyTimeManager
    where T: SearchExecutor<ReportData = Vec<Variation>>
{
    fn new(_: &T::SearchNode, _: &RemainingTime) -> DummyTimeManager {
        DummyTimeManager
    }

    fn must_play(&mut self, _: &mut T, _: Option<&SearchReport<Vec<Variation>>>) -> bool {
        false
    }
}


impl SetOption for DummyTimeManager {}
