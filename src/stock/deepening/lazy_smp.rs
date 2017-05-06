//! Implements `Multipv`.

use super::{bogus_params, contains_dups};
use super::multipv::Multipv;
use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use uci::{SetOption, OptionDescription};
use moves::Move;
use value::*;
use depth::*;
use hash_table::*;
use search::{SearchParams, SearchReport};

// In this module we use the `DeepeningSearch` trait for depth-first
// searches too, so we rename it to avoid confusion.
use search::DeepeningSearch as SearchExecutor;


/// Executes mulit-PV searches with aspiration windows, complying with
/// `searchmoves`.
///
/// The auxiliary data field of searches' progress reports will
/// contain either an empty vector of moves, or the `searchmoves`
/// vector sorted by descending move strength. This allows the
/// iterative deepening routine to improve `searchmoves`' order on
/// each iteration.
pub struct LazySmp<T: SearchExecutor> {
    params: SearchParams<T::SearchNode>,
    // search_is_terminated: bool,
    // previously_searched_nodes: u64,

    // The real work will be handed over to `searcher`.
    searcher: Multipv<T>,
}


impl<T: SearchExecutor> SearchExecutor for LazySmp<T> {
    type HashTable = T::HashTable;

    type SearchNode = T::SearchNode;

    type ReportData = Vec<Move>;

    fn new(tt: Arc<Self::HashTable>) -> LazySmp<T> {
        LazySmp {
            params: bogus_params(),
            // search_is_terminated: false,
            // previously_searched_nodes: 0,
            searcher: Multipv::new(tt),
        }
    }

    fn start_search(&mut self, params: SearchParams<T::SearchNode>) {
        // debug_assert!(params.depth > 0);
        // debug_assert!(params.depth <= DEPTH_MAX);
        // debug_assert!(params.lower_bound >= VALUE_MIN);
        // debug_assert!(params.upper_bound <= VALUE_MAX);
        // debug_assert!(params.lower_bound < params.upper_bound);
        // debug_assert!(!contains_dups(&params.searchmoves));

        self.params = params;
        // self.search_is_terminated = false;
        // self.previously_searched_nodes = 0;
        self.searcher.start_search(self.params.clone());
    }

    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError> {
        self.searcher.try_recv_report()
    }

    fn wait_report(&self, duration: Duration) {
        self.searcher.wait_report(duration);
    }

    fn send_message(&mut self, message: &str) {
        // if message == "TERMINATE" {
        //     self.search_is_terminated = true;
        // }
        self.searcher.send_message(message);
    }
}


impl<T: SearchExecutor> SetOption for LazySmp<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        // let mut options = vec![("MultiPV".to_string(),
        //                         OptionDescription::Spin {
        //                             min: 1,
        //                             max: 500,
        //                             default: 1,
        //                         })];
        // options.extend(Multipv::<T>::options());
        // options
        Multipv::<T>::options()
    }

    fn set_option(name: &str, value: &str) {
        // if name == "MultiPV" {
        //     *VARIATION_COUNT.write().unwrap() = max(value.parse::<usize>().unwrap_or(0), 1);
        // }
        Multipv::<T>::set_option(name, value)
    }
}


impl<T: SearchExecutor> LazySmp<T> {
    /// Returns the best lines of play so far.
    pub fn extract_variations(&mut self) -> Vec<Variation> {
        self.searcher.extract_variations()
    }
}
