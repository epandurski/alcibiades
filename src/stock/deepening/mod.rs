//! Implements iterative deepening, aspiration windows, multi-PV,
//! "searchmoves".

mod aspiration;
mod multipv;
mod thread_executor;

use self::multipv::Multipv;
use self::thread_executor::ThreadExecutor;
use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use regex::Regex;
use uci::{SetOption, OptionDescription};
use moves::Move;
use value::*;
use depth::*;
use hash_table::*;
use search_node::SearchNode;
use search::{Search, SearchParams, SearchReport};

// In this module we use the `DeepeningSearch` trait for depth-first
// searches too, so rename it to avoid confusion.
use search::DeepeningSearch as SearchExecutor;



/// Executes searches with iterative deepening, aspiration windows,
/// multi-PV, and "searchmoves".
///
/// *Iterative deepening* works as follows: A depth-first search is
/// executed with a depth of one ply, then the depth is incremented
/// and another search is executed. This process is repeated until the
/// search is terminated or the requested search depth is reached. In
/// case of a terminated search, the engine can always fall back to
/// the move selected in the last iteration of the search.
///
/// *Aspiration windows* are a way to reduce the search space in the
/// search. The way it works is that we get the value from the last
/// search iteration, calculate a window around it, and use this as
/// alpha-beta bounds for the next search. Because the window is
/// narrower, more beta cutoffs are achieved, and the search takes a
/// shorter time. The drawback is that if the true score is outside
/// this window, then a costly re-search must be made.
///
/// In *multi-PV* mode the engine calculates several principal
/// variations (PV), each one starting with a different first
/// move. This mode is very useful for chess analysis, but can make
/// the search slower.
///
/// *"searchmoves"* is a feature in the UCI protocol, which makes
/// possible to restrict the analysis to a subset of moves
/// only. Again, this is very useful for chess analysis.
///
/// # Usage
///
/// If `T` is a depth-first searcher, instantiate `Deepening<T>` to
/// turn it into a deepening searcher with aspiration windows,
/// multi-PV, and "searchmoves" support.
///
/// **Important note:** `Deepening` requires a proper transposition
/// table to do its work. It can not work with `DummyHashTable`.
pub struct Deepening<T: Search> {
    params: SearchParams<T::SearchNode>,
    search_is_terminated: bool,
    previously_searched_nodes: u64,

    // The real work will be handed over to `multipv`.
    multipv: Multipv<ThreadExecutor<T>>,

    // The search depth completed so far.
    depth: Depth,

    // The value for the root position so far.
    value: Value,

    // The depth at which the search are likely to be terminated.
    depth_target: Depth,
}


impl<T: Search> SearchExecutor for Deepening<T> {
    type HashTable = T::HashTable;

    type SearchNode = T::SearchNode;

    type ReportData = Vec<Variation>;

    fn new(tt: Arc<Self::HashTable>) -> Deepening<T> {
        Deepening {
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            multipv: Multipv::new(tt),
            depth: 0,
            value: VALUE_UNKNOWN,
            depth_target: DEPTH_MAX,
        }
    }

    fn start_search(&mut self, params: SearchParams<T::SearchNode>) {
        assert!(params.depth > 0, "For deepening, depth must be at least 1.");
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound >= VALUE_MIN);
        debug_assert!(params.upper_bound <= VALUE_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(!contains_dups(&params.searchmoves));
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.depth = 0;
        self.value = VALUE_UNKNOWN;
        self.depth_target = DEPTH_MAX;
        self.search_next_depth();
    }

    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError> {
        let SearchReport { searched_nodes, depth, value, data, done, .. } =
            try!(self.multipv.try_recv_report());
        if value != VALUE_UNKNOWN {
            self.value = value;
        }
        if !data.is_empty() {
            debug_assert!(contains_same_moves(&self.params.searchmoves, &data));
            self.params.searchmoves = data.clone();
        }
        let mut report = SearchReport {
            search_id: self.params.search_id,
            searched_nodes: self.previously_searched_nodes + searched_nodes,
            depth: self.depth,
            value: self.value,
            data: vec![],
            done: done,
        };
        if done && !self.search_is_terminated {
            debug_assert_eq!(depth, self.depth + 1);
            report.depth = depth;
            report.data.extend(self.multipv.extract_variations());
            self.previously_searched_nodes = report.searched_nodes;
            self.depth = depth;
            if depth < self.params.depth {
                self.search_next_depth();
                report.done = false;
            }
        }
        Ok(report)
    }

    fn wait_report(&self, duration: Duration) {
        self.multipv.wait_report(duration);
    }

    fn send_message(&mut self, message: &str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^TARGET_DEPTH=([-+]?\d+)$").unwrap();
        }
        if let Some(captures) = RE.captures(message) {
            self.depth_target = captures.get(1).unwrap().as_str().parse::<Depth>().unwrap();
        } else {
            if message == "TERMINATE" {
                self.search_is_terminated = true;
            }
            self.multipv.send_message(message);
        }
    }
}


impl<T: Search> SetOption for Deepening<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        Multipv::<ThreadExecutor<T>>::options()
    }

    fn set_option(name: &str, value: &str) {
        Multipv::<ThreadExecutor<T>>::set_option(name, value)
    }
}


impl<T: Search> Deepening<T> {
    fn search_next_depth(&mut self) {
        self.multipv.start_search(SearchParams {
            search_id: 0,
            depth: self.depth + 1,
            ..self.params.clone()
        });
    }
}


/// A helper function. It returns bogus search parameters.
fn bogus_params<T: SearchNode>() -> SearchParams<T> {
    const FEN: &'static str = "7k/8/8/8/8/8/8/7K w - - 0 1";
    SearchParams {
        search_id: 0,
        position: T::from_history(FEN, &mut vec![].into_iter()).ok().unwrap(),
        depth: 1,
        lower_bound: VALUE_MIN,
        upper_bound: VALUE_MAX,
        searchmoves: vec![Move::invalid()],
    }
}


/// A helper function. It checks if there are moves in the supplied
/// list that occur more than once.
fn contains_dups(list: &Vec<Move>) -> bool {
    let mut l = list.clone();
    l.sort();
    l.dedup();
    l.len() < list.len()
}


/// A helper function. It checks if the two supplied lists of moves
/// contain the same moves, possibly in different order.
fn contains_same_moves(list1: &Vec<Move>, list2: &Vec<Move>) -> bool {
    let mut list1 = list1.clone();
    let mut list2 = list2.clone();
    list1.sort();
    list2.sort();
    list1 == list2
}
