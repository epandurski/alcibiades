//! Implements iterative deepening, aspiration windows, multi-PV,
//! "searchmoves".

mod aspiration;
mod multipv;

use self::multipv::Multipv;
use std::thread;
use std::time::Duration;
use std::cell::RefCell;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver, TryRecvError};
use regex::Regex;
use uci::{SetOption, OptionDescription};
use moves::Move;
use value::*;
use depth::*;
use ttable::*;
use search_node::SearchNode;
use search::{Search, SearchParams, SearchReport};

// In this module we use the `DeepeningSearch` trait for depth-first
// searches too, so we rename it to avoid confusion.
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
/// table to do its work. It can not work with `DummyTtable`.
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
    type Ttable = T::Ttable;

    type SearchNode = T::SearchNode;

    type ReportData = Vec<Variation>;

    fn new(tt: Arc<Self::Ttable>) -> Deepening<T> {
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
        let SearchReport {
            searched_nodes,
            depth,
            value,
            data,
            done,
            ..
        } = try!(self.multipv.try_recv_report());
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
            self.depth_target = captures
                .get(1)
                .unwrap()
                .as_str()
                .parse::<Depth>()
                .unwrap();
        } else {
            if message == "TERMINATE" {
                self.search_is_terminated = true;
            }
            self.multipv.send_message(message);
        }
    }
}


impl<T: Search> SetOption for Deepening<T> {
    fn options() -> Vec<(&'static str, OptionDescription)> {
        Multipv::<ThreadExecutor<T>>::options()
    }

    fn set_option(name: &str, value: &str) {
        Multipv::<ThreadExecutor<T>>::set_option(name, value)
    }
}


impl<T: Search> Deepening<T> {
    fn search_next_depth(&mut self) {
        self.multipv
            .start_search(SearchParams {
                              search_id: 0,
                              depth: self.depth + 1,
                              ..self.params.clone()
                          });
    }
}


/// A helper type. It turns a `Search` into `SearchExecutor`.
struct ThreadExecutor<T: Search> {
    tt: Arc<T::Ttable>,
    messages_tx: Sender<String>,
    reports_rx: Receiver<SearchReport<T::ReportData>>,
    reports_tx: Sender<SearchReport<T::ReportData>>,
    pending_report: RefCell<Option<SearchReport<T::ReportData>>>,
    handle: Option<thread::JoinHandle<Value>>,
}

impl<T: Search> SearchExecutor for ThreadExecutor<T> {
    type Ttable = T::Ttable;

    type SearchNode = T::SearchNode;

    type ReportData = T::ReportData;

    fn new(tt: Arc<Self::Ttable>) -> Self {
        let (reports_tx, reports_rx) = channel();
        Self {
            tt: tt,
            messages_tx: channel().0,
            reports_rx: reports_rx,
            reports_tx: reports_tx,
            pending_report: RefCell::new(None),
            handle: None,
        }
    }

    fn start_search(&mut self, params: SearchParams<Self::SearchNode>) {
        let (messages_tx, messages_rx) = channel();
        self.messages_tx = messages_tx;
        self.handle.take().and_then(|h| h.join().ok());
        self.handle = Some(T::spawn(params,
                                    self.tt.clone(),
                                    self.reports_tx.clone(),
                                    messages_rx));
    }

    fn wait_report(&self, timeout_after: Duration) {
        let mut report = self.pending_report.borrow_mut();
        if report.is_none() {
            *report = self.reports_rx.recv_timeout(timeout_after).ok();
        }
    }

    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError> {
        self.pending_report
            .borrow_mut()
            .take()
            .ok_or(TryRecvError::Empty)
            .or_else(|_| self.reports_rx.try_recv())
    }

    fn send_message(&mut self, msg: &str) {
        self.messages_tx.send(msg.to_string()).ok();
    }
}

impl<T: Search> SetOption for ThreadExecutor<T> {
    fn options() -> Vec<(&'static str, OptionDescription)> {
        T::options()
    }

    fn set_option(name: &str, value: &str) {
        T::set_option(name, value);
    }
}


/// A helper function. It returns bogus search parameters.
fn bogus_params<T: SearchNode>() -> SearchParams<T> {
    const FEN: &'static str = "7k/8/8/8/8/8/8/7K w - - 0 1";
    SearchParams {
        search_id: 0,
        position: T::from_history(FEN, &mut vec![].into_iter())
            .ok()
            .unwrap(),
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
