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
//!
//! # Usage
//!
//! To execute a game search, instantiate one of the following types:
//!
//! * `DeepeningSearcher<AlphabetaSearcher>`
//! * `DeepeningSearcher<AspirationSearcher<AlphabetaSearcher>>`
//! * `DeepeningSearcher<MultipvSearcher<AlphabetaSearcher>>`
//!
//! then:
//!
//! 1. Call the `start_search` method.
//!
//! 2. Continue calling `wait_report` and `try_recv_report` methods
//! periodically, until the search is done.
//!
//! 3. Usually, when the search is done (or at least partially
//! completed), `extract_pv` will be called to obtain the primary
//! variation from the transposition table.
//!
//! # Example:
//! ```rust
//! use std::time::Duration;
//! use tt::*;
//! use search::*;
//! use position::*;
//!
//! let mut tt = Tt::new();
//! tt.resize(16);
//! let tt = Arc::new(tt);
//! let fen = "8/8/8/8/8/7k/7q/7K w - - 0 1";
//! let position = Box::new(Position::from_fen(fen).ok().unwrap());
//! let mut searcher: DeepeningSearcher<AspirationSearcher<AlphabetaSearcher>> =
//!     DeepeningSearcher::new(tt.clone());
//! searcher.start_search(SearchParams {
//!     search_id: 0,
//!     position: position.copy(),
//!     depth: 10,
//!     lower_bound: VALUE_MIN,
//!     upper_bound: VALUE_MAX,
//!     searchmoves: position.legal_moves(),
//!     variation_count: 1,
//! });
//! loop {
//!     searcher.wait_report(Duration::from_millis(20));
//!     if let Ok(report) = searcher.try_recv_report() {
//!         // Process the report here!
//!         if report.done {
//!             break;
//!         }
//!     }
//!     // Do something else here!
//! }
//! let pv = extract_pv(&tt, position.as_ref(), 10);
//! ```
pub mod alpha_beta;
mod threading;

use std::cmp::{min, max, Ordering};
use std::time::Duration;
use std::thread;
use std::sync::{Arc, Mutex, Condvar};
use std::sync::mpsc::{channel, Sender, Receiver, TryRecvError};
use basetypes::*;
use moves::*;
use tt::*;
use self::threading::*;


/// The maximum search depth in half-moves.
pub const MAX_DEPTH: u8 = 63; // Should be less than 127.


/// Parameters describing a new search.
pub struct SearchParams {
    /// A number identifying the new search.
    pub search_id: usize,

    /// The root position for the new search.
    pub position: Box<SearchNode>,

    /// The requested search depth.
    pub depth: u8,

    /// The lower bound for the new search (alpha).
    pub lower_bound: Value,

    /// The upper bound for the new search (beta).
    pub upper_bound: Value,

    /// Restricts the analysis to the supplied list of moves only.
    ///
    /// The same move should not occur more than once, and all
    /// supplied moves should be legal. The behavior of the new search
    /// is undefined if `searchmoves` is empty, but the supplied root
    /// position is not final.
    pub searchmoves: Vec<Move>,

    /// Specifies how many best lines of play to calculate (for the
    /// multi-PV mode).
    ///
    /// Must be greater than zero.
    pub variation_count: usize,
}

impl Clone for SearchParams {
    fn clone(&self) -> Self {
        SearchParams {
            position: self.position.copy(),
            searchmoves: self.searchmoves.clone(),
            ..*self
        }
    }
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


/// A trait for executing consecutive searches in different starting
/// positions.
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


/// A trait for interacting with chess positions.
pub trait SearchNode: Send {
    /// Returns an almost unique hash value for the position.
    ///
    /// **Important notes:** 1) Two positions that differ in their
    /// sets of previously repeated, still reachable boards will have
    /// different hashes. 2) Two positions that differ only in their
    /// number of played moves without capturing piece or advancing a
    /// pawn will have equal hashes, as long as they both are far from
    /// the rule-50 limit.
    fn hash(&self) -> u64;

    /// Returns if the side to move is in check.
    fn is_check(&self) -> bool;

    /// Returns if the side to move is unlikely to be in zugzwang.
    ///
    /// In many endgame positions there is a relatively high
    /// probability of zugzwang occurring. For such positions, this
    /// method will return `false`. For all "normal" positions it will
    /// return `true`. This is useful when deciding if it is safe to
    /// try a "null move".
    fn is_zugzwang_unlikely(&self) -> bool;

    /// Evaluates a final position.
    ///
    /// In final positions this method will return the correct value
    /// of the position (`0` for a draw, `VALUE_MIN` for a
    /// checkmate). A position is guaranteed to be final if
    /// `generate_moves` method generates no legal moves. (It may
    /// generate some pseudo-legal moves, but if none of them is
    /// legal, then the position is final.)
    ///
    /// **Important note:** Repeated and rule-50 positions are
    /// considered final (a draw).
    fn evaluate_final(&self) -> Value;

    /// Statically evaluates the position.
    ///
    /// This method considers only static material and positional
    /// properties of the position. If the position is dynamic, with
    /// pending tactical threats, this function will return a grossly
    /// incorrect evaluation. The returned value will be between
    /// `VALUE_EVAL_MIN` and `VALUE_EVAL_MAX`. For repeated and
    /// rule-50 positions `0` is returned.
    fn evaluate_static(&self) -> Value;

    /// Performs a "quiescence search" and returns an evaluation.
    ///
    /// The "quiescence search" is a restricted search which considers
    /// only a limited set of moves (winning captures, pawn promotions
    /// to queen, check evasions). The goal is to statically evaluate
    /// only "quiet" positions (positions where there are no winning
    /// tactical moves to be made). Although this search can cheaply
    /// and correctly resolve many tactical issues, it is blind to
    /// other simple tactical threats like most kinds of forks,
    /// checks, even a checkmate in one move.
    ///
    /// `lower_bound` and `upper_bound` together give the interval
    /// within which an as precise as possible evaluation is
    /// required. If during the calculation is determined that the
    /// exact evaluation is outside of this interval, this method may
    /// return a value that is closer to the the interval bounds than
    /// the exact evaluation, but always staying on the correct side
    /// of the interval. `static_evaluation` should be the value
    /// returned by `self.evaluate_static()`, or `VALUE_UNKNOWN`. The
    /// returned value will be between `VALUE_EVAL_MIN` and
    /// `VALUE_EVAL_MAX`. For repeated and rule-50 positions `0` is
    /// returned.
    ///
    /// **Note:** This method will return a reliable result even when
    /// the side to move is in check. In this case it will try all
    /// possible check evasions. (It will will never use the static
    /// evaluation value when in check.)
    fn evaluate_quiescence(&self,
                           lower_bound: Value,
                           upper_bound: Value,
                           static_evaluation: Value)
                           -> (Value, NodeCount);

    /// Returns the likely evaluation change (material) to be lost or
    /// gained as a result of a given move.
    ///
    /// This method performs static exchange evaluation (SEE). It
    /// examines the consequence of a series of exchanges on the
    /// destination square after a given move. A positive returned
    /// value indicates a "winning" move. For example, "PxQ" will
    /// always be a win, since the pawn side can choose to stop the
    /// exchange after its pawn is recaptured, and still be ahead. SEE
    /// is just an evaluation calculated without actually trying moves
    /// on the board, and therefore the returned value might be
    /// incorrect.
    ///
    /// The move passed to this method **must** have been generated by
    /// `generate_moves`, `try_move_digest`, or `null_move` methods
    /// for the current position on the board.
    fn evaluate_move(&self, m: Move) -> Value;

    /// Generates pseudo-legal moves.
    ///
    /// A pseudo-legal move is a move that is otherwise legal, except
    /// it might leave the king in check. Every legal move is a
    /// pseudo-legal move, but not every pseudo-legal move is legal.
    /// The generated moves will be pushed to `move_stack`. If all of
    /// the moves generated by this methods are illegal (this means
    /// that `do_move(m)` returns `false` for all of them), then the
    /// position is final, and `evaluate_final()` will return its
    /// correct value.
    ///
    /// **Important note:** Repeated and rule-50 positions are
    /// considered final (and therefore, this method generates no
    /// moves).
    fn generate_moves(&self, move_stack: &mut MoveStack);

    /// Returns a null move.
    ///
    /// "Null move" is a pseudo-move that changes only the side to
    /// move. It is sometimes useful to include a speculative null
    /// move in the search tree so as to achieve more aggressive
    /// pruning.
    fn null_move(&self) -> Move;

    /// Checks if `move_digest` represents a pseudo-legal move.
    ///
    /// If a move `m` exists that would be generated by
    /// `generate_moves` if called for the current position, and for
    /// that move `m.digest() == move_digest`, this method will
    /// return `Some(m)`. Otherwise it will return `None`. This is
    /// useful when playing moves from the transposition table,
    /// without calling `generate_moves`.
    fn try_move_digest(&self, move_digest: MoveDigest) -> Option<Move>;

    /// Plays a move on the board.
    ///
    /// It verifies if the move is legal. If the move is legal, the
    /// board is updated and `true` is returned. If the move is
    /// illegal, `false` is returned without updating the board. The
    /// move passed to this method **must** have been generated by
    /// `generate_moves`, `try_move_digest`, or `null_move` methods
    /// for the current position on the board.
    ///
    /// Moves generated by the `null_move` method are exceptions. For
    /// them `do_move(m)` will return `false` only if the king is in
    /// check or the position is a draw due to repetition or rule-50.
    fn do_move(&mut self, m: Move) -> bool;

    /// Takes back the last played move.
    fn undo_move(&mut self);

    /// Returns all legal moves in the position.
    ///
    /// **Important note:** This method is slower than
    /// `generate_moves` because it ensures that all returned moves
    /// are legal. No moves are returned for repeated and rule-50
    /// positions.
    fn legal_moves(&self) -> Vec<Move>;

    /// Returns an exact copy of the position.
    fn copy(&self) -> Box<SearchNode>;
}


/// Executes alpha-beta searches.
///
/// **Important note:** `AlphabetaSearcher` ignores the `searchmoves`
/// search parameter. It always analyses all legal moves in the root
/// position, and always gives an empty list of `sorted_moves` in its
/// progress reports.
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
        debug_assert!(!contains_dups(&params.searchmoves));
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

    // The real work will be handed over to `searcher`.
    searcher: T,

    // The search depth completed so far.
    depth: u8,

    // The value for the root position so far.
    value: Value,
}

impl<T: SearchExecutor> DeepeningSearcher<T> {
    fn search_next_depth(&mut self) {
        self.searcher.start_search(SearchParams {
            search_id: 0,
            depth: self.depth + 1,
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
            searcher: T::new(tt),
            depth: 0,
            value: VALUE_UNKNOWN,
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        assert!(params.depth > 0);
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(!contains_dups(&params.searchmoves));
        debug_assert!(params.variation_count != 0);
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.depth = 0;
        self.value = VALUE_UNKNOWN;
        self.search_next_depth();
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
pub struct AspirationSearcher<T: SearchExecutor> {
    tt: Arc<Tt>,
    params: SearchParams,
    search_is_terminated: bool,
    previously_searched_nodes: NodeCount,
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

impl<T: SearchExecutor> AspirationSearcher<T> {
    fn lmr_mode(mut self) -> AspirationSearcher<T> {
        self.lmr_mode = true;
        self
    }

    fn start_aspirated_search(&mut self) {
        let depth = if self.lmr_mode && self.expected_to_fail_high && self.params.depth > 0 {
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

impl<T: SearchExecutor> SearchExecutor for AspirationSearcher<T> {
    fn new(tt: Arc<Tt>) -> AspirationSearcher<T> {
        AspirationSearcher {
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

    fn start_search(&mut self, params: SearchParams) {
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(!contains_dups(&params.searchmoves));
        debug_assert!(params.variation_count != 0);
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
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
pub struct MultipvSearcher<T: SearchExecutor> {
    tt: Arc<Tt>,
    params: SearchParams,
    search_is_terminated: bool,
    previously_searched_nodes: NodeCount,

    // The real work will be handed over to `searcher`.
    searcher: AspirationSearcher<T>,

    // The index in `self.params.searchmoves` of the currently
    // considered move.
    current_move_index: usize,

    // The values for the corresponding moves in `self.params.searchmoves`.
    values: Vec<Value>,
}

impl<T: SearchExecutor> MultipvSearcher<T> {
    fn search_current_move(&mut self) -> bool {
        if self.current_move_index < self.params.searchmoves.len() {
            let variation_count = min(self.params.variation_count, self.params.searchmoves.len());
            let alpha = self.values[variation_count - 1];
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
                    searchmoves: vec![],
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

impl<T: SearchExecutor> SearchExecutor for MultipvSearcher<T> {
    fn new(tt: Arc<Tt>) -> MultipvSearcher<T> {
        MultipvSearcher {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            searcher: AspirationSearcher::new(tt).lmr_mode(),
            current_move_index: 0,
            values: vec![VALUE_MIN],
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        assert!(params.depth > 0);
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(!contains_dups(&params.searchmoves));
        debug_assert!(params.variation_count != 0);
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.values = vec![VALUE_MIN; self.params.searchmoves.len()];
        self.current_move_index = 0;
        self.search_current_move();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        if self.params.searchmoves.is_empty() {
            // `searchmoves` is empty -- we assume that the root
            // position is final. (We also update `searchmoves` so
            // that other calls to `try_recv_report` will return
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


/// A sequence of moves from some starting position, together with the
/// value assigned to the final position.
pub struct Variation {
    /// A sequence of moves from some starting position.
    pub moves: Vec<Move>,

    /// The value assigned to the final position.
    /// 
    /// **Important note:** Values under `-9999` or over `9999` may be
    /// chopped, because they often look ugly in GUIs.
    pub value: Value,

    /// The accuracy of the assigned value.
    pub bound: BoundType,
}


/// Extracts the primary variation for a given position from the
/// transposition table and returns it.
pub fn extract_pv(tt: &Tt, position: &SearchNode, depth: u8) -> Variation {
    let mut p = position.copy();
    let mut our_turn = true;
    let mut root_value = VALUE_UNKNOWN;
    let mut leaf_value = -9999;
    let mut leaf_bound = BOUND_LOWER;
    let mut pv_moves = Vec::new();

    'move_extraction: while let Some(entry) = tt.probe(p.hash()) {
        if entry.bound() != BOUND_NONE {
            // Get the next value and the bound type. (Note that in
            // half of the cases the value stored in `entry` is from
            // other side's perspective. Also, note that we chop
            // values under -9999 or over 9999.)
            if our_turn {
                leaf_value = entry.value();
                leaf_bound = entry.bound();
            } else {
                leaf_value = -entry.value();
                leaf_bound = match entry.bound() {
                    BOUND_UPPER => BOUND_LOWER,
                    BOUND_LOWER => BOUND_UPPER,
                    x => x,
                };
            }
            debug_assert!(leaf_value != VALUE_UNKNOWN);
            if leaf_value <= -9999 {
                leaf_value = -9999;
                if leaf_bound == BOUND_UPPER {
                    leaf_bound = BOUND_EXACT
                }
            } else if leaf_value >= 9999 {
                leaf_value = 9999;
                if leaf_bound == BOUND_LOWER {
                    leaf_bound = BOUND_EXACT
                }
            }
            if root_value == VALUE_UNKNOWN {
                root_value = leaf_value;
            }

            // Continue the move extraction cycle until `depth` is
            // reached or `leaf_value` has diverged from `root_value`.
            if pv_moves.len() < depth as usize && leaf_value == root_value {
                if let Some(m) = p.try_move_digest(entry.move16()) {
                    if p.do_move(m) {
                        pv_moves.push(m);
                        if leaf_bound == BOUND_EXACT {
                            our_turn = !our_turn;
                            continue 'move_extraction;
                        }
                    }
                }
            }
        }
        break 'move_extraction;
    }

    Variation {
        value: if root_value != VALUE_UNKNOWN {
            root_value
        } else {
            leaf_value
        },
        bound: match leaf_value.cmp(&root_value) {
            Ordering::Greater => BOUND_LOWER,
            Ordering::Less => BOUND_UPPER,
            Ordering::Equal => leaf_bound,
        },
        moves: pv_moves,
    }
}


/// A helper function. It returns bogus search parameters.
fn bogus_params() -> SearchParams {
    use position::*;
    use position::evaluation::RandomEvaluator;
    SearchParams {
        search_id: 0,
        position: Box::new(Position::<RandomEvaluator>::from_fen(START_POSITION_FEN).ok().unwrap()),
        depth: 1,
        lower_bound: VALUE_MIN,
        upper_bound: VALUE_MAX,
        searchmoves: vec![Move::invalid()],
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


/// A helper function. It checks if there are moves in the supplied
/// list that occur more than once.
fn contains_dups(list: &Vec<Move>) -> bool {
    let mut l = list.clone();
    l.sort();
    l.dedup();
    l.len() < list.len()
}
