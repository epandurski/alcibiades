//! Implements iterative deepening, aspiration windows, multi-PV,
//! "searchmoves".

use std::cmp::{min, max};
use std::time::Duration;
use std::sync::{Arc, RwLock};
use std::sync::mpsc::TryRecvError;
use std::ops::Deref;
use uci::{SetOption, OptionDescription};
use chesstypes::*;
use board::BoardEvaluator;
use search::*;


/// Executes searches with iterative deepening, aspiration windows,
/// multi-PV, and "searchmoves".
///
/// *Iterative deepening* works as follows: A depth-first search is
/// executed with a depth of one ply, then the depth is incremented
/// and another search is executed. This process is repeated until the
/// search is terminated or the requested search depth is reached. In
/// case of an unfinished search, the engine can always fall back to
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
/// multi-PV, and "searchmoves" support. (`T` do not need to support
/// "searchmoves".)
pub struct Deepening<T: SearchExecutor> {
    tt: Arc<T::HashTable>,
    params: SearchParams<T::SearchNode>,
    search_is_terminated: bool,
    previously_searched_nodes: u64,

    // The real work will be handed over to `multipv`.
    multipv: Multipv<T>,

    // The search depth completed so far.
    depth: u8,

    // The value for the root position so far.
    value: Value,
}

impl<T: SearchExecutor> SearchExecutor for Deepening<T> {
    type HashTable = T::HashTable;

    type SearchNode = T::SearchNode;

    type ReportData = Vec<Variation>;

    fn new(tt: Arc<Self::HashTable>) -> Deepening<T> {
        Deepening {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            multipv: Multipv::new(tt),
            depth: 0,
            value: VALUE_UNKNOWN,
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
        self.search_next_depth();
    }

    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError> {
        let SearchReport { searched_nodes, depth, value, data, done, .. } =
            try!(self.multipv.try_recv_report());
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
            report.value = value;
            report.data.extend(self.extract_variations(data));
            self.previously_searched_nodes = report.searched_nodes;
            self.depth = depth;
            self.value = value;
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

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.multipv.terminate_search();
    }
}

impl<T: SearchExecutor> SetOption for Deepening<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        Multipv::<T>::options()
    }

    fn set_option(name: &str, value: &str) {
        Multipv::<T>::set_option(name, value)
    }
}

impl<T: SearchExecutor> Deepening<T> {
    fn search_next_depth(&mut self) {
        self.multipv.start_search(SearchParams {
            search_id: 0,
            depth: self.depth + 1,
            ..self.params.clone()
        });
    }

    fn extract_variations(&mut self, moves: Vec<Move>) -> Vec<Variation> {
        let mut variations = vec![];
        if self.multipv.searcher.lmr_mode {
            // Multi-PV with aspiration.
            for m in moves.iter().take(self.multipv.variation_count) {
                assert!(self.params.position.do_move(*m));
                let mut v = extract_pv(self.tt.deref(), &self.params.position, self.depth);
                self.params.position.undo_move();
                v.moves.insert(0, *m);
                v.value = -v.value;
                v.bound = match v.bound {
                    BOUND_LOWER => BOUND_UPPER,
                    BOUND_UPPER => BOUND_LOWER,
                    x => x,
                };
                variations.push(v);
            }
        } else if self.multipv.variation_count != 0 {
            // Aspiration only.
            debug_assert_eq!(self.multipv.variation_count, 1);
            variations.push(extract_pv(self.tt.deref(), &self.params.position, self.depth + 1));
        }
        variations
    }
}


/// Executes mulit-PV searches with aspiration windows.
struct Multipv<T: SearchExecutor> {
    tt: Arc<T::HashTable>,
    params: SearchParams<T::SearchNode>,
    search_is_terminated: bool,
    previously_searched_nodes: u64,

    // The real work will be handed over to `searcher`.
    searcher: Aspiration<T>,

    // How many best lines of play to calculate.
    variation_count: usize,

    // Whether all legal moves in the current position are considered.
    all_moves_are_considered: bool,

    // The index in `self.params.searchmoves` of the currently
    // considered move.
    current_move_index: usize,

    // The values for the corresponding moves in `self.params.searchmoves`.
    values: Vec<Value>,
}

lazy_static! {
    static ref VARIATION_COUNT: RwLock<usize> = RwLock::new(1);
}

impl<T: SearchExecutor> SearchExecutor for Multipv<T> {
    type HashTable = T::HashTable;

    type SearchNode = T::SearchNode;

    // Reports' auxiliary data will contain `searchmoves` sorted by
    // descending move strength, or an empty list.
    type ReportData = Vec<Move>;

    fn new(tt: Arc<Self::HashTable>) -> Multipv<T> {
        Multipv {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            searcher: Aspiration::new(tt),
            variation_count: 1,
            all_moves_are_considered: true,
            current_move_index: 0,
            values: vec![VALUE_MIN],
        }
    }

    fn start_search(&mut self, params: SearchParams<T::SearchNode>) {
        debug_assert!(params.depth > 0);
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound >= VALUE_MIN);
        debug_assert!(params.upper_bound <= VALUE_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(!contains_dups(&params.searchmoves));

        let n = params.searchmoves.len();
        self.variation_count = min(n, *VARIATION_COUNT.read().unwrap());
        self.all_moves_are_considered = n == params.position.legal_moves().len();
        if n == 0 || self.variation_count == 1 && self.all_moves_are_considered {
            // Aspiration only.
            debug_assert!(self.variation_count <= 1);
            self.searcher.lmr_mode = false;
            self.searcher.start_search(params);
        } else {
            // Multi-PV with aspiration.
            debug_assert!(self.variation_count >= 1);
            self.searcher.lmr_mode = true;
            self.params = params;
            self.search_is_terminated = false;
            self.previously_searched_nodes = 0;
            self.current_move_index = 0;
            self.values = vec![VALUE_MIN; n];
            self.search_current_move();
        }
    }

    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError> {
        if self.searcher.lmr_mode {
            // Multi-PV with aspiration.
            let SearchReport { searched_nodes, value, done, .. } = try!(self.searcher
                                                                            .try_recv_report());
            let mut report = SearchReport {
                search_id: self.params.search_id,
                searched_nodes: self.previously_searched_nodes + searched_nodes,
                depth: 0,
                value: VALUE_UNKNOWN,
                data: vec![],
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
                    report.data = self.params.searchmoves.clone();
                }
            }
            Ok(report)
        } else {
            // Aspiration only.
            self.searcher.try_recv_report()
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

impl<T: SearchExecutor> SetOption for Multipv<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        // Add up all suported options.
        let mut options = vec![
            ("MultiPV".to_string(), OptionDescription::Spin { min: 1, max: 500, default: 1 }),
        ];
        options.extend(Aspiration::<T>::options());
        options
    }

    fn set_option(name: &str, value: &str) {
        if name == "MultiPV" {
            *VARIATION_COUNT.write().unwrap() = max(value.parse::<usize>().unwrap_or(0), 1);
        }
        Aspiration::<T>::set_option(name, value)
    }
}

impl<T: SearchExecutor> Multipv<T> {
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
        let best_move = self.params.searchmoves[0];
        let value = self.values[0];
        let bound = match value {
            v if v <= self.params.lower_bound && !self.all_moves_are_considered => BOUND_NONE,
            v if v <= self.params.lower_bound => BOUND_UPPER,
            v if v >= self.params.upper_bound || !self.all_moves_are_considered => BOUND_LOWER,
            _ => BOUND_EXACT,
        };
        let p = &self.params.position;
        let eval_value = p.evaluator().evaluate(p.board(), p.halfmove_clock());
        self.tt.store(p.hash(),
                      <T::HashTable as HashTable>::Entry::new(value,
                                                              bound,
                                                              self.params.depth,
                                                              best_move.digest(),
                                                              eval_value));
    }

    fn change_current_move(&mut self, v: Value) {
        debug_assert!(v >= self.values[self.current_move_index]);
        let mut i = self.current_move_index;
        self.current_move_index += 1;

        // Update `self.values`, making sure that it remains sorted.
        self.values[i] = v;
        while i > 0 && v > self.values[i - 1] {
            self.values.swap(i, i - 1);
            self.params.searchmoves.swap(i, i - 1);
            i -= 1;
        }
    }
}


/// Executes searches with aspiration windows.
struct Aspiration<T: SearchExecutor> {
    tt: Arc<T::HashTable>,
    params: SearchParams<T::SearchNode>,
    search_is_terminated: bool,
    previously_searched_nodes: u64,
    lmr_mode: bool,

    // The real work will be handed over to `searcher`.
    searcher: T,

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

impl<T: SearchExecutor> SearchExecutor for Aspiration<T> {
    type HashTable = T::HashTable;

    type SearchNode = T::SearchNode;

    // Reports' auxiliary data will always contain an empty move list.
    type ReportData = Vec<Move>;

    fn new(tt: Arc<Self::HashTable>) -> Aspiration<T> {
        Aspiration {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            lmr_mode: false,
            searcher: T::new(tt),
            alpha: VALUE_MIN,
            beta: VALUE_MAX,
            delta: 0,
            expected_to_fail_high: false,
        }
    }

    fn start_search(&mut self, params: SearchParams<T::SearchNode>) {
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound >= VALUE_MIN);
        debug_assert!(params.upper_bound <= VALUE_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(!contains_dups(&params.searchmoves));
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
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
            value: VALUE_UNKNOWN,
            data: vec![],
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

impl<T: SearchExecutor> SetOption for Aspiration<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        T::options()
    }

    fn set_option(name: &str, value: &str) {
        T::set_option(name, value)
    }
}

impl<T: SearchExecutor> Aspiration<T> {
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
