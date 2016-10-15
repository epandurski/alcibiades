//! Implements search parallelization facilities.

use std::thread;
use std::cmp::{min, max};
use std::cell::UnsafeCell;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver, RecvError};
use basetypes::*;
use moves::*;
use position::Position;
use tt::*;
use search::INITIAL_ASPIRATION_WINDOW;
use search::alpha_beta::Search;


/// Represents a command to a search thread.
pub enum Command {
    /// Requests a new search.
    Search {
        /// A number identifying the new search.
        search_id: usize,

        /// The root position.
        position: Position,

        /// The requested search depth.
        depth: u8,

        /// The lower bound for the new search.
        lower_bound: Value,

        /// The upper bound for the new search.
        upper_bound: Value,
    },

    /// Stops the currently running search.
    Stop,

    /// Stops the currently running search and exits the search
    /// thread.
    Exit,
}


/// Represents a progress report from a search.
pub struct Report {
    /// The ID passed with the search command.
    pub search_id: usize,

    /// The number of positions searched so far.
    pub searched_nodes: NodeCount,

    /// The search depth completed so far.
    pub depth: u8,

    /// The evaluation of the root position so far, or `VALUE_UNKNOWN`
    /// if not available.
    pub value: Value,

    /// `true` if the search is done, `false` otherwise.
    pub done: bool,
}


/// Listens for commands, executes simple searches, sends reports
/// back.
///
/// This function will block and wait to receive commands on the
/// `commands` channel to start, stop, or exit searches. It is
/// intended to be called in a separate thread. While the search is
/// executed, regular `Report` messages will be send back to the
/// master thread via the `reports` channel. When the search is done,
/// the final `Report` message will have its `done` field set to
/// `true`.
///
/// # Example:
///
/// ```rust
/// // Spawn a slave thread:
/// let tt = Arc::new(tt);
/// let (commands_tx, commands_rx) = channel();
/// let (reports_tx, reports_rx) = channel();
/// thread::spawn(move || {
///     serve_simple(tt, commands_rx, reports_tx);
/// });
///
/// // Send a command to start a new search:
/// commands_tx.send(Command::Search {
///     search_id: 0,
///     position: Position::form_fen("8/8/8/8/8/8/7P/5k1K b - - 0 99"),
///     depth: 5,
///     lower_bound: -29999,
///     upper_bound: 29999,
/// }).unwrap();
/// ```
///
/// This function executes sequential (non-parallel) search to a fixed
/// depth.
pub fn serve_simple(tt: Arc<Tt>, commands: Receiver<Command>, reports: Sender<Report>) {
    thread_local!(
        static MOVE_STACK: UnsafeCell<MoveStack> = UnsafeCell::new(MoveStack::new())
    );
    MOVE_STACK.with(|s| {
        let mut move_stack = unsafe { &mut *s.get() };
        let mut pending_command = None;
        loop {
            // If there is a pending command, we take it, otherwise we
            // block and wait to receive a command.
            let command = match pending_command.take() {
                Some(cmd) => cmd,
                None => commands.recv().or::<RecvError>(Ok(Command::Exit)).unwrap(),
            };

            match command {
                Command::Search { search_id, position, depth, lower_bound, upper_bound } => {
                    debug_assert!(lower_bound < upper_bound);
                    let mut report = |searched_nodes| {
                        reports.send(Report {
                                   search_id: search_id,
                                   searched_nodes: searched_nodes,
                                   depth: 0,
                                   value: VALUE_UNKNOWN,
                                   done: false,
                               })
                               .ok();
                        if let Ok(cmd) = commands.try_recv() {
                            pending_command = Some(cmd);
                            true
                        } else {
                            false
                        }
                    };
                    let mut search = Search::new(position, &tt, move_stack, &mut report);
                    let value = search.run(lower_bound, upper_bound, depth, Move::invalid())
                                      .unwrap_or(VALUE_UNKNOWN);
                    reports.send(Report {
                               search_id: search_id,
                               searched_nodes: search.node_count(),
                               depth: if value == VALUE_UNKNOWN {
                                   0
                               } else {
                                   depth
                               },
                               value: value,
                               done: true,
                           })
                           .ok();
                    search.reset();
                }

                Command::Stop => continue,

                Command::Exit => break,
            }
        }
    })
}


/// Listens for commands, executes deepening searches, sends reports
/// back.
///
/// This function will block and wait to receive commands on the
/// `commands` channel to start, stop, or exit searches. It is
/// intended to be called in a separate thread. While the search is
/// executed, regular `Report` messages will be send back to the
/// master thread via the `reports` channel. When the search is done,
/// the final `Report` message will have its `done` field set to
/// `true`.
///
/// # Example:
///
/// ```rust
/// // Spawn a slave thread:
/// let tt = Arc::new(tt);
/// let (commands_tx, commands_rx) = channel();
/// let (reports_tx, reports_rx) = channel();
/// thread::spawn(move || {
///     serve_deepening(tt, commands_rx, reports_tx);
/// });
///
/// // Send a command to start a new search:
/// commands_tx.send(Command::Search {
///     search_id: 0,
///     position: Position::form_fen("8/8/8/8/8/8/7P/5k1K b - - 0 99"),
///     depth: 5,
///     lower_bound: -29999,
///     upper_bound: 29999,
/// }).unwrap();
/// ```
///
/// This function executes a deepening search with aspiration
/// window. It starts at depth `1` and consequently increases it until
/// the specified final depth is reached.
pub fn serve_deepening(tt: Arc<Tt>, commands: Receiver<Command>, reports: Sender<Report>) {
    // Start a slave thread that will be commanded to run searches
    // with increasing depths (search deepening).
    let (slave_commands_tx, slave_commands_rx) = channel();
    let (slave_reports_tx, slave_reports_rx) = channel();
    let slave = thread::spawn(move || {
        serve_simple(tt, slave_commands_rx, slave_reports_tx);
    });

    let mut pending_command = None;
    loop {
        // If there is a pending command, we take it, otherwise we
        // block and wait to receive a new one.
        let command = match pending_command.take() {
            Some(cmd) => cmd,
            None => commands.recv().or::<RecvError>(Ok(Command::Exit)).unwrap(),
        };

        match command {
            Command::Search { search_id, position, depth, lower_bound, upper_bound } => {
                debug_assert!(lower_bound < upper_bound);
                let mut current_searched_nodes = 0;
                let mut current_value = VALUE_UNKNOWN;
                let mut current_depth = 1;

                'depthloop: while current_depth <= depth {
                    // First we set up the aspiration window. Aspiration windows are a way
                    // to reduce the search space in the search. We use `current_value`
                    // from the last iteration of `depth`, and calculate a window around
                    // this as the alpha-beta bounds. Because the window is narrower, more
                    // beta cutoffs are achieved, and the search takes a shorter time. The
                    // drawback is that if the true score is outside this window, then a
                    // costly re-search must be made. But then most probably the re-search
                    // will be much faster, because many positions will be remembered from
                    // the TT.
                    //
                    // Here `delta` is the initial half-width of the window, that will be
                    // exponentially increased each time the search failed. We use `isize`
                    // type to avoid overflows.
                    let mut delta = INITIAL_ASPIRATION_WINDOW as isize;
                    let (mut alpha, mut beta) = if current_depth < 5 {
                        (lower_bound, upper_bound)
                    } else {
                        let v = current_value as isize;
                        (max(lower_bound as isize, v - delta) as Value,
                         min(v + delta, upper_bound as isize) as Value)
                    };

                    'aspiration: loop {
                        if alpha >= beta {
                            // This may happen if (v - delta, v + delta) and
                            // (lower_bound, upper_bound) do not intersect.
                            alpha = lower_bound;
                            beta = upper_bound;
                        }

                        // Command the slave thread to run a search.
                        slave_commands_tx.send(Command::Search {
                                             search_id: current_depth as usize,
                                             position: position.clone(),
                                             depth: current_depth,
                                             lower_bound: alpha,
                                             upper_bound: beta,
                                         })
                                         .unwrap();

                        'report: loop {
                            // In this loop we process the reports coming from the slave
                            // thread, but we also constantly check if there is a new
                            // pending command for us, in which case we have to terminate
                            // the search.
                            let Report { searched_nodes, value, done, .. } =
                                slave_reports_rx.recv().unwrap();
                            reports.send(Report {
                                       search_id: search_id,
                                       searched_nodes: current_searched_nodes + searched_nodes,
                                       depth: current_depth - 1,
                                       value: current_value,
                                       done: false,
                                   })
                                   .ok();
                            if pending_command.is_none() {
                                if done {
                                    current_searched_nodes += searched_nodes;
                                    current_value = value;
                                    break 'report;
                                }
                                if let Ok(cmd) = commands.try_recv() {
                                    slave_commands_tx.send(Command::Stop).unwrap();
                                    pending_command = Some(cmd);
                                }
                            }
                            if done {
                                current_searched_nodes += searched_nodes;
                                break 'depthloop;
                            }
                        } // end of 'report

                        // Check if the `current_value` is within the aspiration window
                        // (alpha, beta). If not so, we must consider running a re-search.
                        let v = current_value as isize;
                        if current_value <= alpha && lower_bound < alpha {
                            alpha = max(lower_bound as isize, v - delta) as Value;
                        } else if current_value >= beta && upper_bound > beta {
                            beta = min(v + delta, upper_bound as isize) as Value;
                        } else {
                            break 'aspiration;
                        }

                        // Increase the half-width of the aspiration window.
                        delta += 3 * delta / 8;
                        if delta > 1500 {
                            delta = 1_000_000;
                        }

                    } // end of 'aspiration

                    // Send a progress report with `current_value` for
                    // every completed depth.
                    reports.send(Report {
                               search_id: search_id,
                               searched_nodes: current_searched_nodes,
                               depth: current_depth,
                               value: current_value,
                               done: false,
                           })
                           .ok();
                    current_depth += 1;

                } // end of 'depthloop

                // The search is done -- send a final report.
                reports.send(Report {
                           search_id: search_id,
                           searched_nodes: current_searched_nodes,
                           depth: current_depth - 1,
                           value: current_value,
                           done: true,
                       })
                       .ok();
            }

            Command::Stop => {
                slave_commands_tx.send(Command::Stop).unwrap();
                continue;
            }

            Command::Exit => {
                slave_commands_tx.send(Command::Exit).unwrap();
                break;
            }
        }
    }
    slave.join().unwrap();
}





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
    /// * `searchmoves`: may restrict the analysis to the supplied
    ///   subset of moves only;
    ///
    /// * `variation_count`: specifies how many best lines to
    ///   calculate (the first move in each best line will be
    ///   different).
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
                    searchmoves: Option<Vec<Move>>,
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
                    searchmoves: Option<Vec<Move>>,
                    variation_count: usize) {
        debug_assert!(lower_bound < upper_bound);
        assert!(searchmoves.is_none(), "searchmoves is not supported");
        assert!(variation_count == 1, "variation_count is not supported");
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
                                          None,
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
            lower_bound: -29999,
            upper_bound: 29999,
            value: VALUE_UNKNOWN,
            search_is_terminated: false,
            searched_nodes: 0,
            delta: INITIAL_ASPIRATION_WINDOW as isize,
            alpha: -29999,
            beta: 29999,
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
                    searchmoves: Option<Vec<Move>>,
                    variation_count: usize) {
        debug_assert!(lower_bound < upper_bound);
        assert!(searchmoves.is_none(), "searchmoves is not supported");
        assert!(variation_count == 1, "variation_count is not supported");
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
                done: done,
            };
        }
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.simple_searcher.terminate_search();
    }
}
