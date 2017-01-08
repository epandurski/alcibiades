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


impl<S> TimeManager<S> for DummyTimeManager
    where S: SearchExecutor<ReportData = Vec<Variation>>
{
    fn new(_: &S::SearchNode, _: &RemainingTime) -> DummyTimeManager {
        DummyTimeManager
    }

    fn update(&mut self, _: &SearchReport<Vec<Variation>>) {}

    /// Returns `false`.
    fn must_play(&self, _: &S) -> bool {
        false
    }
}


impl SetOption for DummyTimeManager {}
