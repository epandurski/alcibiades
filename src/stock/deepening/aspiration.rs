//! Implements `Aspiration`.

use super::{bogus_params, contains_dups};
use std::cmp::{min, max};
use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use uci::{SetOption, OptionDescription};
use moves::Move;
use value::*;
use depth::*;
use hash_table::*;
use search_node::SearchNode;
use search::{SearchParams, SearchReport};

// In this module we use the `DeepeningSearch` trait for depth-first
// searches too, so we rename it to avoid confusion.
use search::DeepeningSearch as SearchExecutor;


/// Half-width of the initial aspiration window (centipawns).
const INITIAL_ASPIRATION_WINDOW: isize = 16;


/// Executes searches with aspiration windows.
pub struct Aspiration<T: SearchExecutor> {
    tt: Arc<T::HashTable>,
    params: SearchParams<T::SearchNode>,
    search_is_terminated: bool,
    previously_searched_nodes: u64,

    // The real work will be handed over to `searcher`.
    searcher: T,

    // The value for the root position so far.
    value: Value,

    // The lower bound of the aspiration window.
    alpha: Value,

    // The upper bound of the aspiration window.
    beta: Value,

    // The aspiration window will be widened by this value if the
    // aspirated search fails. (We use `isize` to avoid overflows.)
    delta: isize,

    // Indicates that the aspirated search will most probably fail high.
    expected_to_fail_high: bool,

    /// `Aspiration` has a special mode which can be used to implement
    /// late move reductions.
    pub lmr_mode: bool,
}


impl<T: SearchExecutor> SearchExecutor for Aspiration<T> {
    type HashTable = T::HashTable;

    type SearchNode = T::SearchNode;

    type ReportData = Vec<Move>;

    fn new(tt: Arc<Self::HashTable>) -> Aspiration<T> {
        Aspiration {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            lmr_mode: false,
            searcher: T::new(tt),
            value: VALUE_UNKNOWN,
            alpha: VALUE_MIN,
            beta: VALUE_MAX,
            delta: 0,
            expected_to_fail_high: false,
        }
    }

    fn start_search(&mut self, params: SearchParams<T::SearchNode>) {
        debug_assert!(params.depth >= 0);
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound >= VALUE_MIN);
        debug_assert!(params.upper_bound <= VALUE_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(!contains_dups(&params.searchmoves));
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.value = VALUE_UNKNOWN;
        self.calc_initial_aspiration_window();
        self.start_aspirated_search();
    }

    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError> {
        let SearchReport { searched_nodes, depth, value, done, .. } = try!(self.searcher
                                                                               .try_recv_report());
        let mut report = SearchReport {
            search_id: self.params.search_id,
            searched_nodes: self.previously_searched_nodes + searched_nodes,
            depth: 0,
            value: self.value,
            data: vec![],
            done: done,
        };
        if done && !self.search_is_terminated {
            self.previously_searched_nodes = report.searched_nodes;
            self.value = value;
            if self.widen_aspiration_window(value) {
                self.start_aspirated_search();
                report.done = false;
            } else {
                report.depth = depth;
            }
            report.value = value;
        }
        Ok(report)
    }

    fn wait_report(&self, duration: Duration) {
        self.searcher.wait_report(duration);
    }

    fn send_message(&mut self, message: &str) {
        if message == "TERMINATE" {
            self.search_is_terminated = true;
        }
        self.searcher.send_message(message);
    }
}


impl<T: SearchExecutor> SetOption for Aspiration<T> {
    fn options() -> Vec<(&'static str, OptionDescription)> {
        T::options()
    }

    fn set_option(name: &str, value: &str) {
        T::set_option(name, value)
    }
}


impl<T: SearchExecutor> Aspiration<T> {
    fn start_aspirated_search(&mut self) {
        let depth = if self.lmr_mode && self.expected_to_fail_high && self.params.depth > 0 {
            self.params.depth - 1
        } else {
            self.params.depth
        };
        self.searcher.start_search(SearchParams {
            search_id: 0,
            depth: depth,
            lower_bound: self.alpha,
            upper_bound: self.beta,
            ..self.params.clone()
        });
    }

    fn calc_initial_aspiration_window(&mut self) {
        self.delta = INITIAL_ASPIRATION_WINDOW;
        self.expected_to_fail_high = false;
        let SearchParams { lower_bound, upper_bound, .. } = self.params;
        let (mut a, mut b) = (VALUE_MIN, VALUE_MAX);
        if let Some(e) = self.tt.probe(self.params.position.hash()) {
            if e.depth() >= 4 && e.depth() + 2 >= self.params.depth {
                let v = e.value() as isize;
                if e.bound() & BOUND_LOWER != 0 {
                    a = max(v - self.delta, VALUE_MIN as isize) as Value;
                }
                if e.bound() & BOUND_UPPER != 0 {
                    b = min(v + self.delta, VALUE_MAX as isize) as Value;
                }
                debug_assert!(a < b);
                if a >= upper_bound {
                    a = upper_bound - 1;
                    self.delta = v - a as isize;
                    self.expected_to_fail_high = true;
                }
                if b <= lower_bound {
                    b = lower_bound + 1;
                    self.delta = b as isize - v;
                }
            }
        }
        self.alpha = max(a, lower_bound);
        self.beta = min(b, upper_bound);
        debug_assert!(self.alpha < self.beta);
    }

    fn widen_aspiration_window(&mut self, v: Value) -> bool {
        debug_assert!(self.delta > 0);
        let SearchParams { lower_bound, upper_bound, .. } = self.params;
        if lower_bound < self.alpha && lower_bound < v && v <= self.alpha ||
           self.lmr_mode && self.expected_to_fail_high && v < upper_bound {
            // Failed low -- reduce alpha.
            self.alpha = max(v as isize - self.delta, lower_bound as isize) as Value;
        } else if self.beta < upper_bound && self.beta <= v && v < upper_bound {
            // Failed high -- raise beta.
            self.beta = min(v as isize + self.delta, upper_bound as isize) as Value;
        } else {
            return false;
        }
        self.expected_to_fail_high = false;
        self.increase_delta();
        true
    }

    fn increase_delta(&mut self) {
        self.delta += 3 * self.delta / 8;
        if self.delta > 64 * INITIAL_ASPIRATION_WINDOW {
            self.delta = 1_000_000;
        }
    }
}
