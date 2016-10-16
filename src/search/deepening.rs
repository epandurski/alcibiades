use std::cmp::{min, max};
use std::thread;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver};
use basetypes::*;
use moves::*;
use tt::*;
use position::Position;
use search::threading::{Command, Report, serve_simple};
use search::MAX_DEPTH;


/// The half-with of the initial aspiration window.
pub const INITIAL_ASPIRATION_WINDOW: Value = 17; // 16;


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
    /// After calling `start_search`, `wait_for_report` must be called
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

    /// Blocks and waits for a search report.
    fn wait_for_report(&mut self) -> Report;

    /// Requests the termination of the current search.
    ///
    /// After calling `terminate`, `wait_for_report` must continue to
    /// be called periodically until the returned report indicates
    /// that the search is done.
    fn terminate_search(&mut self);
}


/// Executes searches to a fixed depth.
pub struct SimpleSearcher {
    thread: Option<thread::JoinHandle<()>>,
    thread_commands: Sender<Command>,
    thread_reports: Receiver<Report>,
}

impl SearchExecutor for SimpleSearcher {
    fn new(tt: Arc<Tt>) -> SimpleSearcher {
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        SimpleSearcher {
            thread: Some(thread::spawn(move || {
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

    fn wait_for_report(&mut self) -> Report {
        self.thread_reports.recv().unwrap()
    }

    fn terminate_search(&mut self) {
        self.thread_commands.send(Command::Stop).unwrap();
    }
}

impl Drop for SimpleSearcher {
    fn drop(&mut self) {
        self.thread_commands.send(Command::Exit).unwrap();
        self.thread.take().unwrap().join().unwrap();
    }
}


/// Executes searches to a fixed depth with intelligently reduced
/// bounds (aspiration window).
/// 
/// Aspiration windows are a way to reduce the search space in the
/// search. The way it works is that we get the value from the last
/// search iteration (the `value` parameter to the `start_search`
/// method), calculate a window around it, and use this as alpha-beta
/// bounds for the search. Because the window is narrower, more beta
/// cutoffs are achieved, and the search takes a shorter time. The
/// drawback is that if the true score is outside this window, then a
/// costly re-search must be made. But then most probably the
/// re-search will be much faster, because many positions will be
/// remembered from the TT.
pub struct AspirationSearcher {
    search_id: usize,
    position: Position,
    depth: u8,
    lower_bound: Value,
    upper_bound: Value,
    value: Value,

    search_is_terminated: bool,

    /// The number of positions searched during previous searches.
    searched_nodes: NodeCount,

    /// The aspiration window will be widened by this value if the
    /// aspirated search fails. We use `isize` to avoid overflows.
    delta: isize,

    /// The lower bound of the aspiration window.     
    alpha: Value,

    /// The upper bound of the aspiration window.
    beta: Value,

    /// `AspirationSearcher` will hand over the real work to
    /// `SimpleSearcher`.
    simple_searcher: SimpleSearcher,
}

impl AspirationSearcher {
    /// A helper mehtod. It commands the slave thread to run a new search.
    fn start_aspirated_search(&mut self) {
        self.simple_searcher.start_search(0,
                                          self.position.clone(),
                                          self.depth,
                                          self.alpha,
                                          self.beta,
                                          self.value,
                                          vec![],
                                          1);
    }

    /// A helper method. It increases `self.delta` exponentially.
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
        AspirationSearcher {
            search_id: 0,
            position: Position::from_fen(::STARTING_POSITION).ok().unwrap(),
            depth: 0,
            lower_bound: VALUE_MIN,
            upper_bound: VALUE_MAX,
            value: VALUE_UNKNOWN,
            search_is_terminated: false,
            searched_nodes: 0,
            delta: INITIAL_ASPIRATION_WINDOW as isize,
            alpha: VALUE_MIN,
            beta: VALUE_MAX,
            simple_searcher: SimpleSearcher::new(tt),
        }
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
        debug_assert!(variation_count > 0);
        self.search_id = search_id;
        self.position = position;
        self.depth = depth;
        self.lower_bound = lower_bound;
        self.upper_bound = upper_bound;
        self.value = value;
        self.search_is_terminated = false;
        self.searched_nodes = 0;
        self.delta = INITIAL_ASPIRATION_WINDOW as isize;

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

    fn wait_for_report(&mut self) -> Report {
        loop {
            let Report { searched_nodes, depth, value, done, .. } = self.simple_searcher
                                                                        .wait_for_report();
            let searched_nodes = self.searched_nodes + searched_nodes;
            let depth = if done && !self.search_is_terminated {
                self.searched_nodes = searched_nodes;
                self.value = value;
                if self.widen_aspiration_window() {
                    // `value` is outside the aspiration window and we
                    // must start another search.
                    self.start_aspirated_search();
                    continue;
                }
                depth
            } else {
                0
            };

            return Report {
                search_id: self.search_id,
                searched_nodes: searched_nodes,
                depth: depth,
                value: self.value,
                best_moves: vec![],
                done: done,
            };
        }
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.simple_searcher.terminate_search();
    }
}
