//! Implements the searching of the game tree -- iterative deepening,
//! aspiration windows, multi-PV.
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
//! different first move. This mode makes the search slower, but it is
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
use position::{Position, START_POSITION_FEN};
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

    /// The lower bound for the new search.
    pub lower_bound: Value,

    /// The upper bound for the new search.
    pub upper_bound: Value,

    /// The evaluation of the root position so far, or `VALUE_UNKNOWN`
    /// if not available.
    pub value: Value,

    /// Restricts the analysis of the root position to the supplied
    /// list of moves only. No restrictions if the supplied list is
    /// empty.
    pub searchmoves: Vec<Move>,

    /// Specifies how many best lines of play to calculate. (The first
    /// move in each line will be different.)
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

    /// The best moves found so far (sorted by descending strength),
    /// or an empty list if not available.
    pub best_moves: Vec<Move>,

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
/// `SimpleSearcher::new` will spawn a separate thread to do the
/// computational heavy lifting.
struct SimpleSearcher {
    thread_join_handle: Option<thread::JoinHandle<()>>,
    thread_commands: Sender<Command>,
    thread_reports: Receiver<Report>,
    has_reports_condition: Arc<(Mutex<bool>, Condvar)>,
}

impl SearchExecutor for SimpleSearcher {
    fn new(tt: Arc<Tt>) -> SimpleSearcher {
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        let has_reports_condition = Arc::new((Mutex::new(false), Condvar::new()));
        SimpleSearcher {
            thread_commands: commands_tx,
            thread_reports: reports_rx,
            has_reports_condition: has_reports_condition.clone(),
            thread_join_handle: Some(thread::spawn(move || {
                serve_simple(tt, commands_rx, reports_tx, has_reports_condition);
            })),
        }
    }

    #[allow(unused_variables)]
    fn start_search(&mut self, params: SearchParams) {
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(params.searchmoves.is_empty());
        debug_assert_eq!(params.variation_count, 1);
        self.thread_commands.send(Command::Start(params)).unwrap();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        let result = self.thread_reports.try_recv();
        if result.is_err() {
            *self.has_reports_condition.0.lock().unwrap() = false;
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

impl Drop for SimpleSearcher {
    fn drop(&mut self) {
        self.thread_commands.send(Command::Exit).unwrap();
        self.thread_join_handle.take().unwrap().join().unwrap();
    }
}


/// Executes multi-PV searches.
///
/// `MultipvSearcher::new` will spawn a separate thread to do the
/// computational heavy lifting.
pub struct MultipvSearcher;



/// Executes multi-PV searches with aspiration windows.
///
/// `AspirationSearcher::new` will spawn a separate thread to do the
/// computational heavy lifting.
pub struct AspirationSearcher {
    params: SearchParams,

    /// `true` if the current search has been terminated.
    search_is_terminated: bool,

    /// The number of positions analyzed during previous failed searches.
    previously_searched_nodes: NodeCount,

    /// The evaluation of the root position so far.
    value: Value,

    /// The aspiration window will be widened by this value if the
    /// search fails. (We use `isize` to avoid overflows.)
    delta: isize,

    /// The lower bound of the aspiration window.     
    alpha: Value,

    /// The upper bound of the aspiration window.
    beta: Value,

    /// The real work will be handed over to `SimpleSearcher`.
    searcher: SimpleSearcher, // TODO: should be `MultipvSearcher`
}

impl AspirationSearcher {
    /// A helper method. It tells the multi-PV searcher to run a new
    /// search.
    fn start_aspirated_search(&mut self) {
        self.searcher.start_search(SearchParams {
            search_id: 0,
            lower_bound: self.alpha,
            upper_bound: self.beta,
            value: self.value,
            searchmoves: vec![], // TODO: should be `self.searchmoves.clone(),`
            variation_count: 1, // TODO: should be  `self.variation_count,`
            ..self.params.clone()
        });
    }

    /// A helper method. It multiplies `self.delta` by a constant.
    fn increase_delta(&mut self) {
        self.delta += 3 * self.delta / 8;
        if self.delta > 1500 {
            self.delta = 1_000_000;
        }
    }

    /// A helper method. It widens the aspiration window if necessary.
    fn widen_aspiration_window(&mut self) -> bool {
        let SearchParams { lower_bound, upper_bound, .. } = self.params;
        if lower_bound < self.alpha && lower_bound < self.value && self.value <= self.alpha {
            // Set smaller `self.alpha`.
            self.alpha = max(self.value as isize - self.delta, lower_bound as isize) as Value;
            self.increase_delta();
            return true;
        } else if self.beta < upper_bound && self.beta <= self.value && self.value < upper_bound {
            // Set bigger `self.beta`.
            self.beta = min(self.value as isize + self.delta, upper_bound as isize) as Value;
            self.increase_delta();
            return true;
        }
        false
    }
}

impl SearchExecutor for AspirationSearcher {
    fn new(tt: Arc<Tt>) -> AspirationSearcher {
        AspirationSearcher {
            params: SearchParams {
                search_id: 0,
                position: Position::from_fen(START_POSITION_FEN).ok().unwrap(),
                depth: 0,
                lower_bound: VALUE_MIN,
                upper_bound: VALUE_MAX,
                value: VALUE_UNKNOWN,
                searchmoves: vec![],
                variation_count: 1,
            },
            search_is_terminated: false,
            previously_searched_nodes: 0,
            value: VALUE_UNKNOWN,
            delta: 0,
            alpha: VALUE_MIN,
            beta: VALUE_MAX,
            searcher: SimpleSearcher::new(tt),
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.value = self.params.value;

        // The half-width of the initial aspiration window.
        self.delta = 17; // TODO: make this `16`?

        // Set the initial aspiration window (`self.alpha`, `self.beta`).
        let SearchParams { value, lower_bound, upper_bound, .. } = self.params;
        let (a, b) = if value == VALUE_UNKNOWN || value < lower_bound || value > upper_bound {
            (lower_bound, upper_bound)
        } else {
            let v = value as isize;
            (max(v - self.delta, lower_bound as isize) as Value,
             min(v + self.delta, upper_bound as isize) as Value)
        };
        self.alpha = a;
        self.beta = b;

        self.start_aspirated_search();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        let Report { searched_nodes, depth, value, best_moves, done, .. } =
            try!(self.searcher.try_recv_report());
        self.value = value;
        let searched_nodes = self.previously_searched_nodes + searched_nodes;
        let depth = if done && !self.search_is_terminated {
            self.previously_searched_nodes = searched_nodes;
            if self.widen_aspiration_window() {
                // Start a re-search.
                self.start_aspirated_search();
                return Err(TryRecvError::Empty);
            }
            depth
        } else {
            0
        };

        return Ok(Report {
            search_id: self.params.search_id,
            searched_nodes: searched_nodes,
            depth: depth,
            value: self.value,
            best_moves: best_moves,
            done: done,
        });
    }

    fn wait_report(&self, duration: Duration) {
        self.searcher.wait_report(duration);
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.searcher.terminate_search();
    }
}


/// Executes multi-PV searches with aspiration windows and iterative
/// deepening.
///
/// `DeepeningSearcher::new` will spawn a separate thread to do the
/// computational heavy lifting.
pub struct DeepeningSearcher {
    params: SearchParams,

    /// `true` if the current search has been terminated.
    search_is_terminated: bool,

    /// The number of positions analyzed during previous (shallower)
    /// searches.
    previously_searched_nodes: NodeCount,

    /// The evaluation of the root position so far.
    value: Value,

    /// The depth of the currently executing search.
    depth: u8,

    /// The real work will be handed over to `AspirationSearcher`.
    searcher: AspirationSearcher,
}

impl DeepeningSearcher {
    /// A helper method. It tells the aspiration searcher to run a new
    /// search.
    fn start_deeper_search(&mut self) {
        self.depth += 1;
        let value = if self.depth < 5 {
            // During the first few iterations the evaluation is
            // unreliable, and therefore we should not pass it to the
            // aspiration searcher.
            VALUE_UNKNOWN
        } else {
            self.value
        };
        self.searcher.start_search(SearchParams {
            search_id: 0,
            depth: self.depth,
            value: value,
            ..self.params.clone()
        });
    }
}

impl SearchExecutor for DeepeningSearcher {
    fn new(tt: Arc<Tt>) -> DeepeningSearcher {
        DeepeningSearcher {
            params: SearchParams {
                search_id: 0,
                position: Position::from_fen(START_POSITION_FEN).ok().unwrap(),
                depth: 0,
                lower_bound: VALUE_MIN,
                upper_bound: VALUE_MAX,
                value: VALUE_UNKNOWN,
                searchmoves: vec![],
                variation_count: 1,
            },
            search_is_terminated: false,
            previously_searched_nodes: 0,
            value: VALUE_UNKNOWN,
            depth: 0,
            searcher: AspirationSearcher::new(tt),
        }
    }

    fn start_search(&mut self, params: SearchParams) {
        debug_assert!(params.depth <= MAX_DEPTH);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.value = self.params.value;
        self.depth = 0;

        self.start_deeper_search();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        let Report { searched_nodes, depth, value, best_moves, done, .. } =
            try!(self.searcher.try_recv_report());
        if value != VALUE_UNKNOWN {
            self.value = value;
        }
        let searched_nodes = self.previously_searched_nodes + searched_nodes;
        let depth = if done && !self.search_is_terminated {
            debug_assert_eq!(depth, self.depth);
            self.previously_searched_nodes = searched_nodes;
            if self.depth < self.params.depth {
                self.start_deeper_search();
                return Err(TryRecvError::Empty);
            }
            self.depth
        } else {
            self.depth - 1
        };

        return Ok(Report {
            search_id: self.params.search_id,
            searched_nodes: searched_nodes,
            depth: depth,
            value: self.value,
            best_moves: best_moves,
            done: done,
        });
    }

    fn wait_report(&self, duration: Duration) {
        self.searcher.wait_report(duration);
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.searcher.terminate_search();
    }
}
