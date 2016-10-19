//! Implements search parallelization.

use std::cell::UnsafeCell;
use std::sync::{Arc, Mutex, Condvar};
use std::sync::mpsc::{Sender, Receiver, RecvError};
use basetypes::*;
use moves::*;
use tt::*;
use position::Position;
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
///     lower_bound: VALUE_MIN,
///     upper_bound: VALUE_MAX,
/// }).unwrap();
/// ```
///
/// This function executes sequential (non-parallel) search to a fixed
/// depth.
pub fn serve_simple(tt: Arc<Tt>,
                    commands: Receiver<Command>,
                    reports: Sender<Report>,
                    has_reports_condition: Arc<(Mutex<bool>, Condvar)>) {
    thread_local!(
        static MOVE_STACK: UnsafeCell<MoveStack> = UnsafeCell::new(MoveStack::new())
    );
    MOVE_STACK.with(|s| {
        let &(ref has_reports, ref condition) = &*has_reports_condition;
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
                                   best_moves: vec![],
                                   done: false,
                               })
                               .ok();
                        let mut has_reports = has_reports.lock().unwrap();
                        *has_reports = true;
                        condition.notify_one();

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
                               best_moves: vec![],
                               done: true,
                           })
                           .ok();
                    let mut has_reports = has_reports.lock().unwrap();
                    *has_reports = true;
                    condition.notify_one();

                    search.reset();
                }

                Command::Stop => continue,

                Command::Exit => break,
            }
        }
    })
}
