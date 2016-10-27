//! Implements alpha-beta searching, iterative deepening, aspiration
//! windows, multi-PV.
//!
//! The alpha-beta algorithm is an enhancement to the minimax search
//! algorithm. It maintains two values, alpha and beta. They represent
//! the minimum score that the maximizing player is assured of (lower
//! bound) and the maximum score that the minimizing player is assured
//! of (upper bound) respectively.
//!
//! Iterative deepening works as follows: the program starts with a
//! one ply search, then increments the search depth and does another
//! search. This process is repeated until the time allocated for the
//! search is exhausted or the maximum search depth is reached. In
//! case of an unfinished search, the program can always fall back to
//! the move selected in the last iteration of the search.
//!
//! Aspiration windows are a way to reduce the search space in the
//! search. The way it works is that we get the value from the last
//! search iteration, calculate a window around it, and use this as
//! alpha-beta bounds for the next search. Because the window is
//! narrower, more beta cutoffs are achieved, and the search takes a
//! shorter time. The drawback is that if the true score is outside
//! this window, then a costly re-search must be made. But then most
//! probably the re-search will be much faster, because many positions
//! will be remembered from the transposition table.
//!
//! In multi-PV mode the engine calculates and sends to the GUI
//! several principal variations (PV), each one starting with a
//! different first move. This mode makes the search slower, but is
//! very useful for chess analysis.

pub mod alpha_beta;
pub mod threading;

use std::cmp::{min, max};
use std::time::Duration;
use std::thread;
use std::sync::{Arc, Mutex, Condvar};
use std::sync::mpsc::{channel, Sender, Receiver, TryRecvError};
use basetypes::*;
use moves::*;
use tt::*;
use position::*;
use self::threading::*;


/// The maximum search depth in half-moves.
pub const MAX_DEPTH: u8 = 63; // Should be less than 127.


/// Parameters describing a new search.
#[derive(Clone)]
pub struct SearchParams {
    /// A number identifying the new search.
    pub search_id: usize,

    /// The root position for the new search.
    pub position: Position,

    /// The requested search depth.
    pub depth: u8,

    /// The lower bound for the new search (alpha).
    pub lower_bound: Value,

    /// The upper bound for the new search (beta).
    pub upper_bound: Value,

    /// Restricts the analysis to the supplied list of moves only.
    ///
    /// All passed moves must be legal and unique. The behavior of the
    /// search is undefined if `searchmoves` is empty, but the
    /// supplied root position is not final.
    pub searchmoves: Vec<Move>,

    /// Specifies how many best lines of play to calculate (for the
    /// multi-PV mode).
    ///
    /// Must be greater than zero.
    pub variation_count: usize,
}


/// A progress report from a search.
#[derive(Clone)]
pub struct Report {
    /// The ID assigned to search.
    pub search_id: usize,

    /// The number of positions searched so far.
    pub searched_nodes: NodeCount,

    /// The search depth completed so far.
    pub depth: u8,

    /// The evaluation of the root position so far, or `VALUE_UNKNOWN`
    /// if not available.
    pub value: Value,

    /// The `searchmoves` list sorted by descending move strength (see
    /// `SearchParams`), or an empty list.
    pub sorted_moves: Vec<Move>,

    /// `true` if the search is done, `false` otherwise.
    pub done: bool,
}


/// The `SearchExecutor` trait is used to execute consecutive searches
/// in different starting positions.
pub trait SearchExecutor {
    /// Creates a new instance.
    fn new(tt: Arc<Tt>) -> Self;

    /// Starts a new search.
    ///
    /// After calling `start_search`, `try_recv_report` must be called
    /// periodically until the returned report indicates that the
    /// search is done. A new search must not be started until the
    /// previous search is done.
    fn start_search(&mut self, params: SearchParams);

    /// Attempts to return a search progress report without blocking.
    fn try_recv_report(&mut self) -> Result<Report, TryRecvError>;

    /// Waits until a search progress report is available, timing out
    /// after a specified duration or earlier.
    fn wait_report(&self, duration: Duration);

    /// Requests the termination of the current search.
    ///
    /// After calling `terminate`, `try_recv_report` must continue to
    /// be called periodically until the returned report indicates
    /// that the search is done.
    fn terminate_search(&mut self);
}


/// Executes alpha-beta searches.
///
/// **Important note:** `AlphabetaSearcher` ignores the `searchmoves`
/// search parameter. It always analyses all legal moves in the root
/// position, and always supplies an empty list of `sorted_moves` in
/// its progress reports.
pub struct AlphabetaSearcher {
    thread_join_handle: Option<thread::JoinHandle<()>>,
    thread_commands: Sender<Command>,
    thread_reports: Receiver<Report>,
    has_reports_condition: Arc<(Mutex<bool>, Condvar)>,
}

impl SearchExecutor for AlphabetaSearcher {
    fn new(tt: Arc<Tt>) -> AlphabetaSearcher {
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        let has_reports_condition = Arc::new((Mutex::new(false), Condvar::new()));
        AlphabetaSearcher {
            thread_commands: commands_tx,
            thread_reports: reports_rx,
            has_reports_condition: has_reports_condition.clone(),

            // Spawn a thread that will do the real work.
            thread_join_handle: Some(thread::spawn(move || {
                serve_simple(tt, commands_rx, reports_tx, has_reports_condition);
            })),
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(params.variation_count != 0);
        self.thread_commands.send(Command::Start(params)).unwrap();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        let mut has_reports = self.has_reports_condition.0.lock().unwrap();
        let result = self.thread_reports.try_recv();
        if result.is_err() {
            *has_reports = false;
        }
        result
    }

    fn wait_report(&self, duration: Duration) {
        let &(ref has_reports, ref condition) = &*self.has_reports_condition;
        let has_reports = has_reports.lock().unwrap();
        if !*has_reports {
            condition.wait_timeout(has_reports, duration).unwrap();
        }
    }

    fn terminate_search(&mut self) {
        self.thread_commands.send(Command::Terminate).unwrap();
    }
}

impl Drop for AlphabetaSearcher {
    fn drop(&mut self) {
        self.thread_commands.send(Command::Exit).unwrap();
        self.thread_join_handle.take().unwrap().join().unwrap();
    }
}


/// Executes searches with iterative deepening.
pub struct DeepeningSearcher<T: SearchExecutor> {
    params: SearchParams,
    search_is_terminated: bool,
    previously_searched_nodes: NodeCount,
    value: Value,

    // The real work will be handed over to `searcher`.
    searcher: T,

    // The depth of the currently executing search.
    depth: u8,
}

impl<T: SearchExecutor> DeepeningSearcher<T> {
    fn start_deeper_search(&mut self) {
        self.depth += 1;
        self.searcher.start_search(SearchParams {
            search_id: 0,
            depth: self.depth,
            ..self.params.clone()
        });
    }
}

impl<T: SearchExecutor> SearchExecutor for DeepeningSearcher<T> {
    fn new(tt: Arc<Tt>) -> DeepeningSearcher<T> {
        DeepeningSearcher {
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            value: VALUE_UNKNOWN,
            searcher: T::new(tt),
            depth: 0,
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(params.variation_count != 0);
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.value = VALUE_UNKNOWN;
        self.depth = 0;
        self.start_deeper_search();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        let Report { searched_nodes, depth, value, sorted_moves, done, .. } =
            try!(self.searcher.try_recv_report());
        if !sorted_moves.is_empty() {
            debug_assert!(contains_same_moves(&self.params.searchmoves, &sorted_moves));
            self.params.searchmoves = sorted_moves.clone();
        }
        let mut report = Report {
            search_id: self.params.search_id,
            searched_nodes: self.previously_searched_nodes + searched_nodes,
            depth: self.depth - 1,
            value: self.value,
            sorted_moves: sorted_moves,
            done: done,
        };
        if done && !self.search_is_terminated {
            debug_assert_eq!(depth, self.depth);
            self.previously_searched_nodes = report.searched_nodes;
            self.value = value;
            if self.depth < self.params.depth {
                self.start_deeper_search();
                report.done = false;
            }
            report.depth = depth;
        }
        Ok(report)
    }

    fn wait_report(&self, duration: Duration) {
        self.searcher.wait_report(duration);
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.searcher.terminate_search();
    }
}


/// Executes searches with aspiration windows.
pub struct AspirationSearcher<T: SearchExecutor> {
    tt: Arc<Tt>,
    params: SearchParams,
    search_is_terminated: bool,
    previously_searched_nodes: NodeCount,
    lmr_mode: bool,
    expected_to_fail_low: bool,

    // The real work will be handed over to `searcher`.
    searcher: T,

    // The aspiration window will be widened by this value if the
    // search fails. (We use `isize` to avoid overflows.)
    delta: isize,

    // The lower bound of the aspiration window.
    alpha: Value,

    // The upper bound of the aspiration window.
    beta: Value,
}

impl<T: SearchExecutor> AspirationSearcher<T> {
    fn lmr_mode(mut self) -> AspirationSearcher<T> {
        self.lmr_mode = true;
        self
    }

    fn start_aspirated_search(&mut self) {
        let depth = if self.lmr_mode && self.expected_to_fail_low && self.params.depth > 0 {
            // `MultipvSearcher` implements late move reductions by
            // using `AspirationSearcher` in a special mode.
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
        self.delta = 17; // TODO: make this `16`?
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
                }
                if b <= lower_bound {
                    b = lower_bound + 1;
                    self.delta = b as isize - v;
                    self.expected_to_fail_low = true;
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
        if self.beta < upper_bound && self.beta <= v && v < upper_bound ||
           self.lmr_mode && self.expected_to_fail_low && self.alpha < v {
            // The search failed high.
            self.beta = min(v as isize + self.delta, upper_bound as isize) as Value;
        } else if lower_bound < self.alpha && lower_bound < v && v <= self.alpha {
            // The search failed low.
            self.alpha = max(v as isize - self.delta, lower_bound as isize) as Value;
        } else {
            return false;
        }
        self.expected_to_fail_low = false;
        self.increase_delta();
        true
    }

    fn increase_delta(&mut self) {
        self.delta += 3 * self.delta / 8;
        if self.delta > 1500 {
            self.delta = 1_000_000;
        }
    }
}

impl<T: SearchExecutor> SearchExecutor for AspirationSearcher<T> {
    fn new(tt: Arc<Tt>) -> AspirationSearcher<T> {
        AspirationSearcher {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            lmr_mode: false,
            expected_to_fail_low: false,
            searcher: T::new(tt),
            delta: 0,
            alpha: VALUE_MIN,
            beta: VALUE_MAX,
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(params.variation_count != 0);
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.expected_to_fail_low = false;
        self.calc_initial_aspiration_window();
        self.start_aspirated_search();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        let Report { searched_nodes, depth, value, sorted_moves, done, .. } =
            try!(self.searcher.try_recv_report());
        if !sorted_moves.is_empty() {
            debug_assert!(contains_same_moves(&self.params.searchmoves, &sorted_moves));
            self.params.searchmoves = sorted_moves.clone();
        }
        let mut report = Report {
            search_id: self.params.search_id,
            searched_nodes: self.previously_searched_nodes + searched_nodes,
            depth: 0,
            value: VALUE_UNKNOWN,
            sorted_moves: sorted_moves,
            done: done,
        };
        if done && !self.search_is_terminated {
            self.previously_searched_nodes = report.searched_nodes;
            if self.widen_aspiration_window(value) {
                // A re-search is necessary.
                self.start_aspirated_search();
                report.done = false;
            } else {
                // The search is done.
                report.depth = depth;
                report.value = value;
            }
        }
        Ok(report)
    }

    fn wait_report(&self, duration: Duration) {
        self.searcher.wait_report(duration);
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.searcher.terminate_search();
    }
}


/// Executes mulit-PV searches.
pub struct MultipvSearcher<T: SearchExecutor> {
    tt: Arc<Tt>,
    params: SearchParams,
    search_is_terminated: bool,
    previously_searched_nodes: NodeCount,

    // The real work will be handed over to `searcher`.
    searcher: AspirationSearcher<T>,

    // The index (in `self.params.searchmoves`) of the currently
    // analyzed move.
    current_move_index: usize,

    // The index (in `self.params.searchmoves`) of the move that will
    // be analyzed next.
    next_move_index: usize,

    // The values for the corresponding moves in `self.params.searchmoves`.
    values: Vec<Value>,
}

impl<T: SearchExecutor> MultipvSearcher<T> {
    fn search_next_move(&mut self) -> bool {
        if self.next_move_index < self.params.searchmoves.len() {
            // Start a search for the next move in `searchmoves`.
            self.current_move_index = self.next_move_index;
            self.next_move_index += 1;
            let variation_count = min(self.params.variation_count, self.params.searchmoves.len());
            let alpha = self.values[variation_count - 1];
            if alpha < self.params.upper_bound {
                assert!(self.params
                            .position
                            .do_move(self.params.searchmoves[self.current_move_index]));
                self.searcher.start_search(SearchParams {
                    search_id: 0,
                    depth: self.params.depth - 1,
                    lower_bound: -self.params.upper_bound,
                    upper_bound: -max(alpha, self.params.lower_bound),
                    searchmoves: vec![],
                    ..self.params.clone()
                });
                return true;
            }
        }

        // No more moves to search -- write the result to the TT.
        if !self.params.searchmoves.is_empty() {
            let all_moves_were_considered = self.params.searchmoves.len() ==
                                            self.params.position.legal_moves().len();
            let best_move = self.params.searchmoves[0];
            let value = self.values[0];
            let bound = match value {
                v if v <= self.params.lower_bound && !all_moves_were_considered => BOUND_NONE,
                v if v <= self.params.lower_bound => BOUND_UPPER,
                v if v >= self.params.upper_bound || !all_moves_were_considered => BOUND_LOWER,
                _ => BOUND_EXACT,
            };
            self.tt.store(self.params.position.hash(),
                          TtEntry::new(value,
                                       bound,
                                       self.params.depth,
                                       best_move.digest(),
                                       self.params.position.evaluate_static()));
        }
        false
    }

    fn update_current_move_value(&mut self, v: Value) {
        debug_assert!(v >= self.values[self.current_move_index]);
        let i = &mut self.current_move_index;
        self.values[*i] = v;

        // Make sure that `self.values` remains sorted.
        while *i > 0 && v > self.values[*i - 1] {
            self.values.swap(*i, *i - 1);
            self.params.searchmoves.swap(*i, *i - 1);
            *i -= 1;
        }
    }
}

impl<T: SearchExecutor> SearchExecutor for MultipvSearcher<T> {
    fn new(tt: Arc<Tt>) -> MultipvSearcher<T> {
        MultipvSearcher {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            searcher: AspirationSearcher::new(tt).lmr_mode(),
            current_move_index: 0,
            next_move_index: 0,
            values: vec![],
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        assert!(params.depth > 0);
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(params.variation_count != 0);
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.values = vec![VALUE_MIN; self.params.searchmoves.len()];
        self.next_move_index = 0;
        self.search_next_move();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        if self.params.searchmoves.is_empty() {
            // `searchmoves` is empty -- we assume that the root
            // position is final. (We also update `searchmoves` so
            // that next calls to `try_recv_report` will return
            // `Err`.)
            self.params.searchmoves = vec![Move::invalid()];
            Ok(Report {
                search_id: self.params.search_id,
                searched_nodes: 0,
                depth: self.params.depth,
                value: self.params.position.evaluate_final(),
                sorted_moves: vec![],
                done: true,
            })
        } else {
            // `searchmoves` is not empty.
            let Report { searched_nodes, value, done, .. } = try!(self.searcher.try_recv_report());
            let mut report = Report {
                search_id: self.params.search_id,
                searched_nodes: self.previously_searched_nodes + searched_nodes,
                depth: 0,
                value: VALUE_UNKNOWN,
                sorted_moves: vec![],
                done: done,
            };
            if done && !self.search_is_terminated {
                self.previously_searched_nodes = report.searched_nodes;
                self.update_current_move_value(-value);
                self.params.position.undo_move();
                if self.search_next_move() {
                    report.done = false;
                } else {
                    // The search is done.
                    report.depth = self.params.depth;
                    report.value = self.values[0];
                    report.sorted_moves = self.params.searchmoves.clone();
                }
            }
            Ok(report)
        }
    }

    fn wait_report(&self, duration: Duration) {
        self.searcher.wait_report(duration);
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.searcher.terminate_search();
    }
}


/// A helper function. It returns bogus search parameters.
fn bogus_params() -> SearchParams {
    SearchParams {
        search_id: 0,
        position: Position::from_fen(START_POSITION_FEN).ok().unwrap(),
        depth: 0,
        lower_bound: VALUE_MIN,
        upper_bound: VALUE_MAX,
        searchmoves: vec![],
        variation_count: 1,
    }
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
