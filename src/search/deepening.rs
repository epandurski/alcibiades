//! Implements iterative deepening, aspiration windows, multi-PV.
//!
//! Iterative deepening works as follows: the program starts with a
//! one ply search, then increments the search depth and does another
//! search. This process is repeated until the time allocated for the
//! search is exhausted. In case of an unfinished search, the program
//! always has the option to fall back to the move selected in the
//! last iteration of the search.
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
//! different first move.

use std::cmp::{min, max};
use std::mem;
use std::thread;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver, TryRecvError};
use basetypes::*;
use moves::*;
use tt::*;
use position::Position;
use search::MAX_DEPTH;
use search::threading::{Command, Report, serve_simple, serve_deepening};


/// The `SearchExecutor` trait is used to execute consecutive searches
/// in different starting positions.
pub trait SearchExecutor {
    /// Creates a new instance.
    fn new(tt: Arc<Tt>) -> Self;

    /// Starts a new search.
    ///
    /// * `search_id`: a number identifying the new search;
    /// 
    /// * `position`: the root position;
    /// 
    /// * `depth`: the requested search depth;
    /// 
    /// * `lower_bound`: the lower bound for the new search;
    /// 
    /// * `upper_bound`: the upper bound for the new search;
    /// 
    /// * `value`: the evaluation of the root position so far, or
    ///   `VALUE_UNKNOWN` if not available.
    ///
    /// * `searchmoves`: restricts the analysis to the supplied list
    ///   of moves only (no restrictions if the suppied list is
    ///   empty);
    ///
    /// * `variation_count`: specifies how many best lines of play to
    ///   calculate (the first move in each line will be different).
    /// 
    /// After calling `start_search`, `try_recv_report` must be called
    /// periodically until the returned report indicates that the
    /// search is done. A new search must not be started until the
    /// previous search is done.
    fn start_search(&mut self,
                    search_id: usize,
                    position: Position,
                    depth: u8,
                    lower_bound: Value,
                    upper_bound: Value,
                    value: Value,
                    searchmoves: Vec<Move>,
                    variation_count: usize);

    /// Attempts to return a search report without blocking.
    fn try_recv_report(&mut self) -> Result<Report, TryRecvError>;

    /// Requests the termination of the current search.
    ///
    /// After calling `terminate`, `try_recv_report` must continue to
    /// be called periodically until the returned report indicates
    /// that the search is done.
    fn terminate_search(&mut self);
}



/// Executes bare alpha-beta searches to a fixed depth.
///
/// `SimpleSearcher::new` will spawn a separate thread to do the
/// computational heavy lifting.
pub struct SimpleSearcher {
    thread_join_handle: Option<thread::JoinHandle<()>>,
    thread_commands: Sender<Command>,
    thread_reports: Receiver<Report>,
}

impl SearchExecutor for SimpleSearcher {
    fn new(tt: Arc<Tt>) -> SimpleSearcher {
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        SimpleSearcher {
            thread_join_handle: Some(thread::spawn(move || {
                serve_simple(tt, commands_rx, reports_tx);
            })),
            thread_commands: commands_tx,
            thread_reports: reports_rx,
        }
    }

    #[allow(unused_variables)]
    fn start_search(&mut self,
                    search_id: usize,
                    position: Position,
                    depth: u8,
                    lower_bound: Value,
                    upper_bound: Value,
                    value: Value,
                    searchmoves: Vec<Move>,
                    variation_count: usize) {
        debug_assert!(depth <= MAX_DEPTH);
        debug_assert!(lower_bound < upper_bound);
        debug_assert!(lower_bound != VALUE_UNKNOWN);
        debug_assert!(searchmoves.is_empty());
        self.thread_commands
            .send(Command::Search {
                search_id: search_id,
                position: position,
                depth: depth,
                lower_bound: lower_bound,
                upper_bound: upper_bound,
            })
            .unwrap();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        self.thread_reports.try_recv()
    }

    fn terminate_search(&mut self) {
        self.thread_commands.send(Command::Stop).unwrap();
    }
}

impl Drop for SimpleSearcher {
    fn drop(&mut self) {
        self.thread_commands.send(Command::Exit).unwrap();
        self.thread_join_handle.take().unwrap().join().unwrap();
    }
}



/// Executes multi-PV searches to a fixed depth.
///
/// `MultipvSearcher::new` will spawn a separate thread to do the
/// computational heavy lifting.
pub struct MultipvSearcher;



/// Executes multi-PV searches to a fixed depth with aspiration.
///
/// `AspirationSearcher::new` will spawn a separate thread to do the
/// computational heavy lifting.
pub struct AspirationSearcher {
    search_id: usize,
    position: Position,
    depth: u8,
    lower_bound: Value,
    upper_bound: Value,
    value: Value,
    searchmoves: Vec<Move>,
    variation_count: usize,

    /// `true` if the current search has been terminated.
    search_is_terminated: bool,

    /// The number of positions analyzed during previous (failed) searches.
    previously_searched_nodes: NodeCount,

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
    /// A helper method. It tells `self.searcher` to run a new search.
    fn start_aspirated_search(&mut self) {
        self.searcher.start_search(0,
                                   self.position.clone(),
                                   self.depth,
                                   self.alpha,
                                   self.beta,
                                   self.value,
                                   vec![], // TODO: should be `self.searchmoves.clone(),`
                                   self.variation_count);
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
        let v = self.value as isize;
        if self.value <= self.alpha && self.lower_bound < self.alpha {
            // Set smaller `self.alpha`.
            self.alpha = max(v - self.delta, self.lower_bound as isize) as Value;
            self.increase_delta();
            return true;
        } else if self.value >= self.beta && self.upper_bound > self.beta {
            // Set bigger `self.beta`.
            self.beta = min(v + self.delta, self.upper_bound as isize) as Value;
            self.increase_delta();
            return true;
        }
        false
    }
}

impl SearchExecutor for AspirationSearcher {
    fn new(tt: Arc<Tt>) -> AspirationSearcher {
        let mut this: AspirationSearcher = unsafe { mem::uninitialized() };
        this.searcher = SimpleSearcher::new(tt);
        this
    }

    fn start_search(&mut self,
                    search_id: usize,
                    position: Position,
                    depth: u8,
                    lower_bound: Value,
                    upper_bound: Value,
                    value: Value,
                    searchmoves: Vec<Move>,
                    variation_count: usize) {
        debug_assert!(depth <= MAX_DEPTH);
        debug_assert!(lower_bound < upper_bound);
        debug_assert!(lower_bound != VALUE_UNKNOWN);

        self.search_id = search_id;
        self.position = position;
        self.depth = depth;
        self.lower_bound = lower_bound;
        self.upper_bound = upper_bound;
        self.value = value;
        self.searchmoves = searchmoves;
        self.variation_count = variation_count;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;

        // The half-width of the initial aspiration window.
        self.delta = 17; // TODO: make this `16`?

        // Set the initial aspiration window (`self.alpha`, `self.beta`).
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
        let searched_nodes = self.previously_searched_nodes + searched_nodes;
        let depth = if done && !self.search_is_terminated {
            self.previously_searched_nodes = searched_nodes;
            self.value = value;
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
            search_id: self.search_id,
            searched_nodes: searched_nodes,
            depth: depth,
            value: self.value,
            best_moves: best_moves,
            done: done,
        });
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.searcher.terminate_search();
    }
}



/// Executes deepeing multi-PV searches with aspiration.
///
/// `DeepeningSearcher::new` will spawn a separate thread to do the
/// computational heavy lifting.
pub struct DeepeningSearcher {
    thread: Option<thread::JoinHandle<()>>,
    thread_commands: Sender<Command>,
    thread_reports: Receiver<Report>,
}

impl SearchExecutor for DeepeningSearcher {
    fn new(tt: Arc<Tt>) -> DeepeningSearcher {
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        DeepeningSearcher {
            thread: Some(thread::spawn(move || {
                serve_deepening(tt, commands_rx, reports_tx);
            })),
            thread_commands: commands_tx,
            thread_reports: reports_rx,
        }
    }

    #[allow(unused_variables)]
    fn start_search(&mut self,
                    search_id: usize,
                    position: Position,
                    depth: u8,
                    lower_bound: Value,
                    upper_bound: Value,
                    value: Value,
                    searchmoves: Vec<Move>,
                    variation_count: usize) {
        debug_assert!(depth <= MAX_DEPTH);
        debug_assert!(lower_bound < upper_bound);
        debug_assert!(lower_bound != VALUE_UNKNOWN);
        self.thread_commands
            .send(Command::Search {
                search_id: search_id,
                position: position,
                depth: depth,
                lower_bound: lower_bound,
                upper_bound: upper_bound,
            })
            .unwrap();
    }

    fn try_recv_report(&mut self) -> Result<Report, TryRecvError> {
        self.thread_reports.try_recv()
    }

    fn terminate_search(&mut self) {
        self.thread_commands.send(Command::Stop).unwrap();
    }
}

impl Drop for DeepeningSearcher {
    fn drop(&mut self) {
        self.thread_commands.send(Command::Exit).unwrap();
        self.thread.take().unwrap().join().unwrap();
    }
}
