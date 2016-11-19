//! Implements iterative deepening, aspiration windows, multi-PV.
//!
//! # Usage
//!
//! To turn `StandardSearcher` into a deepening searcher, instantiate
//! one of the following types:
//!
//! * `Deepening<StandardSearcher>`
//! * `Deepening<Aspiration<StandardSearcher>>`
//! * `Deepening<Multipv<StandardSearcher>>`
use std::cmp::{min, max};
use std::time::Duration;
use std::sync::{Arc, RwLock};
use std::sync::mpsc::TryRecvError;
use chesstypes::*;
use tt::*;
use uci::{SetOption, OptionDescription};
use super::*;
use super::{contains_dups, contains_same_moves};

/// Executes searches with iterative deepening.
///
/// Iterative deepening works as follows: the program starts with a
/// one ply search, then increments the search depth and does another
/// search. This process is repeated until the time allocated for the
/// search is exhausted or the maximum search depth is reached. In
/// case of an unfinished search, the program can always fall back to
/// the move selected in the last iteration of the search.
pub struct Deepening<S: SearchExecutor> {
    params: SearchParams,
    search_is_terminated: bool,
    previously_searched_nodes: u64,

    // The real work will be handed over to `searcher`.
    searcher: S,

    // The search depth completed so far.
    depth: u8,

    // The value for the root position so far.
    value: Value,
}

impl<S: SearchExecutor> Deepening<S> {
    fn search_next_depth(&mut self) {
        self.searcher.start_search(SearchParams {
            search_id: 0,
            depth: self.depth + 1,
            ..self.params.clone()
        });
    }
}

impl<S: SearchExecutor> SetOption for Deepening<S> {
    fn options() -> Vec<(String, OptionDescription)> {
        S::options()
    }

    fn set_option(name: &str, value: &str) {
        S::set_option(name, value)
    }
}

impl<S: SearchExecutor> SearchExecutor for Deepening<S> {
    fn new(tt: Arc<Tt>) -> Deepening<S> {
        Deepening {
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            searcher: S::new(tt),
            depth: 0,
            value: VALUE_UNKNOWN,
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        assert!(params.depth > 0);
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(!contains_dups(&params.searchmoves));
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.depth = 0;
        self.value = VALUE_UNKNOWN;
        self.search_next_depth();
    }

    fn try_recv_report(&mut self) -> Result<SearchReport, TryRecvError> {
        let SearchReport { searched_nodes, depth, value, sorted_moves, done, .. } =
            try!(self.searcher.try_recv_report());
        if !sorted_moves.is_empty() {
            debug_assert!(contains_same_moves(&self.params.searchmoves, &sorted_moves));
            self.params.searchmoves = sorted_moves.clone();
        }
        let mut report = SearchReport {
            search_id: self.params.search_id,
            searched_nodes: self.previously_searched_nodes + searched_nodes,
            depth: self.depth,
            value: self.value,
            sorted_moves: sorted_moves,
            done: done,
        };
        if done && !self.search_is_terminated {
            debug_assert_eq!(depth, self.depth + 1);
            self.previously_searched_nodes = report.searched_nodes;
            self.depth = depth;
            self.value = value;
            if depth < self.params.depth {
                self.search_next_depth();
                report.done = false;
            }
            report.depth = depth;
            report.value = value;
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
///
/// Aspiration windows are a way to reduce the search space in the
/// search. The way it works is that we get the value from the last
/// search iteration, calculate a window around it, and use this as
/// alpha-beta bounds for the next search. Because the window is
/// narrower, more beta cutoffs are achieved, and the search takes a
/// shorter time. The drawback is that if the true score is outside
/// this window, then a costly re-search must be made.
pub struct Aspiration<S: SearchExecutor> {
    tt: Arc<Tt>,
    params: SearchParams,
    search_is_terminated: bool,
    previously_searched_nodes: u64,
    lmr_mode: bool,

    // The real work will be handed over to `searcher`.
    searcher: S,

    // The lower bound of the aspiration window.
    alpha: Value,

    // The upper bound of the aspiration window.
    beta: Value,

    // The aspiration window will be widened by this value if the
    // aspirated search fails. (We use `isize` to avoid overflows.)
    delta: isize,

    // Indicates that the aspirated search will most probably fail high.
    expected_to_fail_high: bool,
}

impl<S: SearchExecutor> Aspiration<S> {
    fn lmr_mode(mut self) -> Aspiration<S> {
        self.lmr_mode = true;
        self
    }

    fn start_aspirated_search(&mut self) {
        let depth = if self.lmr_mode && self.expected_to_fail_high && self.params.depth > 0 {
            // `Multipv` implements late move reductions by using
            // `Aspiration` in a special mode.
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
        self.delta = 16;
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
        if self.delta > 1500 {
            self.delta = 1_000_000;
        }
    }
}

impl<S: SearchExecutor> SetOption for Aspiration<S> {
    fn options() -> Vec<(String, OptionDescription)> {
        S::options()
    }

    fn set_option(name: &str, value: &str) {
        S::set_option(name, value)
    }
}

impl<S: SearchExecutor> SearchExecutor for Aspiration<S> {
    fn new(tt: Arc<Tt>) -> Aspiration<S> {
        Aspiration {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            lmr_mode: false,
            searcher: S::new(tt),
            alpha: VALUE_MIN,
            beta: VALUE_MAX,
            delta: 0,
            expected_to_fail_high: false,
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(!contains_dups(&params.searchmoves));
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.calc_initial_aspiration_window();
        self.start_aspirated_search();
    }

    fn try_recv_report(&mut self) -> Result<SearchReport, TryRecvError> {
        let SearchReport { searched_nodes, depth, value, sorted_moves, done, .. } =
            try!(self.searcher.try_recv_report());
        if !sorted_moves.is_empty() {
            debug_assert!(contains_same_moves(&self.params.searchmoves, &sorted_moves));
            self.params.searchmoves = sorted_moves.clone();
        }
        let mut report = SearchReport {
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
                self.start_aspirated_search();
                report.done = false;
            } else {
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
///
/// In multi-PV mode the engine calculates and sends to the GUI
/// several principal variations (PV), each one starting with a
/// different first move. This mode makes the search slower, but is
/// very useful for chess analysis.
pub struct Multipv<S: SearchExecutor> {
    tt: Arc<Tt>,
    params: SearchParams,
    search_is_terminated: bool,
    previously_searched_nodes: u64,

    // The real work will be handed over to `searcher`.
    searcher: Aspiration<S>,

    // How many best lines of play to calculate.
    variation_count: usize,

    // The index in `self.params.searchmoves` of the currently
    // considered move.
    current_move_index: usize,

    // The values for the corresponding moves in `self.params.searchmoves`.
    values: Vec<Value>,
}

lazy_static! {
    static ref VARIATION_COUNT: RwLock<usize> = RwLock::new(1);
}

impl<S: SearchExecutor> Multipv<S> {
    fn search_current_move(&mut self) -> bool {
        if self.current_move_index < self.params.searchmoves.len() {
            let alpha = self.values[self.variation_count - 1];
            if alpha < self.params.upper_bound {
                assert!(self.params
                            .position
                            .do_move(self.params.searchmoves[self.current_move_index]));
                self.previously_searched_nodes += 1;
                self.searcher.start_search(SearchParams {
                    search_id: 0,
                    depth: self.params.depth - 1,
                    lower_bound: -self.params.upper_bound,
                    upper_bound: -max(alpha, self.params.lower_bound),
                    searchmoves: self.params.position.legal_moves(),
                    ..self.params.clone()
                });
                return true;
            }
        }
        self.write_reslut_to_tt();
        false
    }

    fn write_reslut_to_tt(&self) {
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
    }

    fn change_current_move(&mut self, v: Value) {
        debug_assert!(v >= self.values[self.current_move_index]);
        let mut i = self.current_move_index;
        self.values[i] = v;
        self.current_move_index += 1;

        // Make sure that `self.values` remains sorted.
        while i > 0 && v > self.values[i - 1] {
            self.values.swap(i, i - 1);
            self.params.searchmoves.swap(i, i - 1);
            i -= 1;
        }
    }
}

impl<S: SearchExecutor> SetOption for Multipv<S> {
    fn options() -> Vec<(String, OptionDescription)> {
        // Add up all suported options.
        let mut options = vec![
            ("MultiPV".to_string(), OptionDescription::Spin { min: 1, max: 500, default: 1 }),
        ];
        options.extend(Aspiration::<S>::options());
        options
    }

    fn set_option(name: &str, value: &str) {
        if name == "MultiPV" {
            *VARIATION_COUNT.write().unwrap() = max(value.parse::<usize>().unwrap_or(0), 1);
        }
        Aspiration::<S>::set_option(name, value)
    }
}

impl<S: SearchExecutor> SearchExecutor for Multipv<S> {
    fn new(tt: Arc<Tt>) -> Multipv<S> {
        Multipv {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            searcher: Aspiration::new(tt).lmr_mode(),
            variation_count: 1,
            current_move_index: 0,
            values: vec![VALUE_MIN],
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(!contains_dups(&params.searchmoves));
        self.params = params;
        if self.params.depth == 0 {
            // If this happens, we probably have two nested Multipv
            // searchers, which is an awful idea. Nevertheless, we OK
            // this by applying a small cheat.
            self.params.searchmoves = vec![];
        }
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.variation_count = min(*VARIATION_COUNT.read().unwrap(),
                                   self.params.searchmoves.len());
        self.current_move_index = 0;
        self.values = vec![VALUE_MIN; self.params.searchmoves.len()];
        self.search_current_move();
    }

    fn try_recv_report(&mut self) -> Result<SearchReport, TryRecvError> {
        if self.params.searchmoves.is_empty() {
            // `searchmoves` is empty -- we assume that the root
            // position is final. (We also update `searchmoves` so
            // that other calls to `try_recv_report` will return
            // `Err`.)
            self.params.searchmoves = vec![Move::invalid()];
            Ok(SearchReport {
                search_id: self.params.search_id,
                searched_nodes: 0,
                depth: self.params.depth,
                value: self.params.position.evaluate_final(),
                sorted_moves: vec![],
                done: true,
            })
        } else {
            // `searchmoves` is not empty.
            let SearchReport { searched_nodes, value, done, .. } = try!(self.searcher
                                                                            .try_recv_report());
            let mut report = SearchReport {
                search_id: self.params.search_id,
                searched_nodes: self.previously_searched_nodes + searched_nodes,
                depth: 0,
                value: VALUE_UNKNOWN,
                sorted_moves: vec![],
                done: done,
            };
            if done && !self.search_is_terminated {
                self.previously_searched_nodes = report.searched_nodes;
                self.params.position.undo_move();
                self.change_current_move(-value);
                if self.search_current_move() {
                    report.done = false;
                } else {
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
    use board::Position;
    use board::evaluators::RandomEvaluator;
    SearchParams {
        search_id: 0,
        position: Box::new(Position::<RandomEvaluator>::create("7k/8/8/8/8/8/8/7K w - - 0 1",
                                                               &mut vec![].into_iter())
                               .ok()
                               .unwrap()),
        depth: 1,
        lower_bound: VALUE_MIN,
        upper_bound: VALUE_MAX,
        searchmoves: vec![Move::invalid()],
    }
}
