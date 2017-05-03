//! Implements `SimpleSearchExecutor`.

use std::mem;
use std::cmp::max;
use std::thread;
use std::cell::UnsafeCell;
use std::sync::{Arc, Mutex, Condvar};
use std::sync::mpsc::{channel, Sender, Receiver, TryRecvError, RecvError};
use std::time::Duration;
use std::marker::PhantomData;
use std::ops::Deref;
use uci::{SetOption, OptionDescription};
use value::*;
use depth::*;
use board::*;
use moves::*;
use hash_table::*;
use search::{SearchParams, SearchReport, SearchExecutor};
use search_node::SearchNode;
use evaluator::Evaluator;
use qsearch::QsearchResult;
use utils::MoveStack;


/// Executes depth-first alpha-beta searches with null move pruning
/// and late move reductions.
///
/// *The alpha-beta algorithm* is an enhancement to the minimax search
/// algorithm. It maintains two values, alpha and beta. They represent
/// the minimum score that the maximizing player is assured of (lower
/// bound) and the maximum score that the minimizing player is assured
/// of (upper bound) respectively.
///
/// *Null move pruning* is a method to reduce the search space by
/// trying a "null" or "passing" move, then seeing if the score of the
/// subtree search is still high enough to cause a beta cutoff. Nodes
/// are saved by reducing the depth of the subtree under the null
/// move.
///
/// *Late move reductions* save search space by reducing the search
/// depth for moves that are ordered closer to the end (likely
/// fail-low nodes).
///
/// **Important note:** `SimpleSearchExecutor` ignores the
/// `searchmoves` search parameter. It always analyses all legal moves
/// in the root position.
pub struct SimpleSearchExecutor<T: HashTable, N: SearchNode> {
    phantom: PhantomData<T>,
    thread_join_handle: Option<thread::JoinHandle<()>>,
    thread_commands: Sender<Command<N>>,
    thread_reports: Receiver<SearchReport<()>>,
    has_reports_condition: Arc<(Mutex<bool>, Condvar)>,
}

impl<T, N> SearchExecutor for SimpleSearchExecutor<T, N>
    where T: HashTable,
          N: SearchNode
{
    type HashTable = T;

    type SearchNode = N;

    type ReportData = ();

    fn new(tt: Arc<T>) -> SimpleSearchExecutor<T, N> {
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        let has_reports_condition = Arc::new((Mutex::new(false), Condvar::new()));
        SimpleSearchExecutor {
            phantom: PhantomData,
            thread_commands: commands_tx,
            thread_reports: reports_rx,
            has_reports_condition: has_reports_condition.clone(),

            // Spawn a thread that will do the real work.
            thread_join_handle: Some(thread::spawn(move || {
                serve_simple(tt, commands_rx, reports_tx, has_reports_condition);
            })),
        }
    }

    fn start_search(&mut self, params: SearchParams<N>) {
        assert!(params.depth >= 0, "depth must be at least 0.");
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(params.searchmoves.is_empty() ||
                      contains_same_moves(&params.searchmoves, &params.position.legal_moves()),
                      "SimpleSearchExecutor ignores searchmoves");
        self.thread_commands.send(Command::Start(params)).unwrap();
    }

    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError> {
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

    fn send_message(&mut self, message: &str) {
        if message == "TERMINATE" {
            self.thread_commands.send(Command::Terminate).unwrap();
        }
    }
}

impl<T: HashTable, N: SearchNode> SetOption for SimpleSearchExecutor<T, N> {
    fn options() -> Vec<(String, OptionDescription)> {
        N::options()
    }

    fn set_option(name: &str, value: &str) {
        N::set_option(name, value);
    }
}

impl<T: HashTable, N: SearchNode> Drop for SimpleSearchExecutor<T, N> {
    fn drop(&mut self) {
        self.thread_commands.send(Command::Exit).unwrap();
        self.thread_join_handle.take().unwrap().join().unwrap();
    }
}


/// Represents a command to a search thread.
enum Command<N: SearchNode> {
    /// Starts a new search.
    Start(SearchParams<N>),

    /// Terminates the currently running search.
    Terminate,

    /// Terminates the currently running search and exits the search
    /// thread.
    Exit,
}


/// A helper function. It listens for commands, executes simple
/// searches, sends reports back.
///
/// This function will block and wait to receive commands on the
/// `commands` channel to start, stop, or exit searches. It is
/// intended to be called in a separate thread. While the search is
/// executed, regular `SearchReport` messages will be send back to the
/// master thread via the `reports` channel. When the search is done,
/// the final `SearchReport` message will have its `done` field set to
/// `true`.
fn serve_simple<T, N>(tt: Arc<T>,
                      commands: Receiver<Command<N>>,
                      reports: Sender<SearchReport<()>>,
                      has_reports_condition: Arc<(Mutex<bool>, Condvar)>)
    where T: HashTable,
          N: SearchNode
{
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
                Command::Start(SearchParams { search_id,
                                              position,
                                              depth,
                                              lower_bound,
                                              upper_bound,
                                              .. }) => {
                    debug_assert!(lower_bound < upper_bound);
                    let mut report = |searched_nodes| {
                        reports.send(SearchReport {
                                   search_id: search_id,
                                   searched_nodes: searched_nodes,
                                   depth: 0,
                                   value: VALUE_UNKNOWN,
                                   data: (),
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
                    let mut search = Search::new(position, tt.deref(), move_stack, &mut report);
                    let (depth, value) = if let Ok(v) = search.run(lower_bound,
                                                                   upper_bound,
                                                                   depth,
                                                                   Move::invalid()) {
                        (depth, v)
                    } else {
                        (0, VALUE_UNKNOWN)
                    };

                    reports.send(SearchReport {
                               search_id: search_id,
                               searched_nodes: search.node_count(),
                               depth: depth,
                               value: value,
                               data: (),
                               done: true,
                           })
                           .ok();
                    let mut has_reports = has_reports.lock().unwrap();
                    *has_reports = true;
                    condition.notify_one();

                    search.reset();
                }

                Command::Terminate => continue,

                Command::Exit => break,
            }
        }
    })
}


/// Represents a terminated search condition.
struct TerminatedSearch;


/// Represents a game tree search.        
struct Search<'a, T, N>
    where T: HashTable + 'a,
          N: SearchNode
{
    tt: &'a T,
    killers: KillerTable,
    position: N,
    moves: &'a mut MoveStack,
    moves_starting_ply: usize,
    state_stack: Vec<NodeState>,
    reported_nodes: u64,
    unreported_nodes: u64,
    report_function: &'a mut FnMut(u64) -> bool,
}

impl<'a, T, N> Search<'a, T, N>
    where T: HashTable + 'a,
          N: SearchNode
{
    /// Creates a new instance.
    ///
    /// `report_function` should be a function that registers the
    /// search progress. It will be called with the number of searched
    /// positions from the beginning of the search to this moment. The
    /// function should return `true` if the search should be
    /// terminated, otherwise it should return `false`.
    pub fn new(root: N,
               tt: &'a T,
               move_stack: &'a mut MoveStack,
               report_function: &'a mut FnMut(u64) -> bool)
               -> Search<'a, T, N> {
        let moves_starting_ply = move_stack.ply();
        Search {
            tt: tt,
            killers: KillerTable::new(),
            position: root,
            moves: move_stack,
            moves_starting_ply: moves_starting_ply,
            state_stack: Vec::with_capacity(32),
            reported_nodes: 0,
            unreported_nodes: 0,
            report_function: report_function,
        }
    }

    /// Performs a game tree search and returns the result.
    ///
    /// `alpha` and `beta` together give the interval within which an
    /// as precise as possible evaluation is required. If during the
    /// search is determined that the exact evaluation is outside of
    /// this interval, this method may return a value that is closer
    /// to the the interval bounds than the exact evaluation, but
    /// always staying on the correct side of the interval. `depth` is
    /// the desired search depth in half-moves. `last_move` should be
    /// the move that led to the current position, or `Move::invalid()`
    /// if the last move is unknown.
    ///
    /// **Important note**: This method may leave un-restored move
    /// lists in `move_stack` (see the parametes passed to
    /// `Search::new`). Call the `reset` method if you want the move
    /// stack to be restored to the state it had when the search
    /// instance was created.
    pub fn run(&mut self,
               mut alpha: Value, // lower bound
               beta: Value, // upper bound
               depth: Depth,
               last_move: Move)
               -> Result<Value, TerminatedSearch> {
        // This implementation performs a modified alpha-beta search.
        // It uses zero window searches with reduced depth for late
        // moves.
        //
        // The classical alpha-beta search is a huge enhancement to
        // the minimax search algorithm, that eliminates the need to
        // search large portions of the game tree by applying a
        // branch-and-bound technique. It does this without any
        // potential of overlooking a better move. If one already has
        // found a quite good move and searches for alternatives, one
        // refutation is enough to avoid it -- no need to look for
        // even stronger refutations. The algorithm maintains two
        // values, alpha and beta. They represent the minimum score
        // that the maximizing player is assured of and the maximum
        // score that the minimizing player is assured of
        // respectively.

        debug_assert!(alpha < beta);
        let mut value = VALUE_UNKNOWN;

        if let Some(v) = try!(self.node_begin(alpha, beta, depth, last_move)) {
            // We already have the final result.
            value = v;

        } else {
            // Initial guests.
            debug_assert!(depth > 0);
            let mut bound = BOUND_EXACT;
            let mut best_move = Move::invalid();

            // Try moves.
            while let Some(m) = self.do_move() {
                try!(self.report_progress(1));

                // Make a recursive call.
                let mut v = if m.score() > REDUCTION_THRESHOLD {
                    // The moves that have good chances to cause a
                    // beta cut-off we analyze with a full depth and
                    // fully open window (alpha, beta). We hope that
                    // at least one of these moves will raise `alpha`.
                    -try!(self.run(-beta, -alpha, depth - 1, m))
                } else {
                    // For the rest of the moves we first try to prove
                    // that they are not better than our current best
                    // move. For this purpose we search them with a
                    // reduced depth and a null window (alpha, alpha +
                    // 1). Only if it seems that the move is better
                    // than our current best move, we do a full-depth,
                    // full-window search.
                    match -try!(self.run(-alpha - 1, -alpha, depth - 2, m)) {
                        v if v <= alpha => v,
                        _ => -try!(self.run(-beta, -alpha, depth - 1, m)),
                    }
                };
                self.undo_move();
                debug_assert!(v > VALUE_UNKNOWN);

                // Increase/decrease the value for a checkmate by one
                // on every half-move. This way the engine will seek
                // for the fastest checkmate possible.
                if v < VALUE_EVAL_MIN - 1 {
                    v += 1;
                } else if v > VALUE_EVAL_MAX + 1 {
                    v -= 1;
                }

                // See how good this move was.
                if v >= beta {
                    // This move is so good, that the opponent will
                    // probably not allow this line of play to happen.
                    // Therefore we should not lose any more time on
                    // this position (beta cut-off).
                    best_move = m;
                    value = v;
                    bound = BOUND_LOWER;
                    self.register_killer_move(m);
                    break;
                }
                if v > value {
                    // We found a new best move.
                    best_move = m;
                    value = v;
                    bound = if v > alpha {
                        alpha = v;
                        BOUND_EXACT
                    } else {
                        BOUND_UPPER
                    };
                }
            }

            // Check if we are in a final position (no legal moves).
            if value == VALUE_UNKNOWN {
                value = self.position.evaluate_final();
                debug_assert_eq!(bound, BOUND_EXACT);
            }

            // Store the result to the transposition table.
            self.store(value, bound, depth, best_move);
        }

        self.node_end();
        Ok(value)
    }

    /// Returns the number of searched positions.
    #[inline]
    pub fn node_count(&self) -> u64 {
        self.reported_nodes + self.unreported_nodes
    }

    /// Resets the instance to the state it had when it was created.
    #[inline]
    pub fn reset(&mut self) {
        while self.moves.ply() > self.moves_starting_ply {
            self.moves.restore();
        }
        self.state_stack.clear();
        self.reported_nodes = 0;
        self.unreported_nodes = 0;
        self.killers.forget_all();
    }

    /// A helper method for `run`. Each call to `run` begins with a
    /// call to `node_begin`.
    ///
    /// This method tries to calculate and return the value for the
    /// node. It basically does 3 things:
    ///
    /// 1. Checks if the transposition table has the result.
    /// 2. On leaf nodes, performs quiescence search.
    /// 3. Performs null move pruning if possible.
    fn node_begin(&mut self,
                  alpha: Value,
                  beta: Value,
                  depth: Depth,
                  last_move: Move)
                  -> Result<Option<Value>, TerminatedSearch> {
        // Probe the transposition table.
        let hash = self.position.hash();
        let (entry, static_eval) = if let Some(e) = self.tt.probe(hash) {
            match e.static_eval() {
                VALUE_UNKNOWN => {
                    (e,
                     self.position
                         .evaluator()
                         .evaluate(self.position.board()))
                }
                v => (e, v),
            }
        } else {
            let v = self.position
                        .evaluator()
                        .evaluate(self.position.board());
            (T::Entry::new(0, BOUND_NONE, 0).set_static_eval(v), v)
        };
        self.state_stack.push(NodeState {
            phase: NodePhase::Pristine,
            hash_move_digest: entry.move_digest(),
            static_eval: static_eval,
            is_check: unsafe { mem::uninitialized() }, // We will initialize this soon!
            killer: None,
        });

        // Check if the TT entry gives the result.
        if entry.depth() >= depth {
            let value = entry.value();
            let bound = entry.bound();
            if (value >= beta && bound & BOUND_LOWER != 0) ||
               (value <= alpha && bound & BOUND_UPPER != 0) ||
               (bound == BOUND_EXACT) {
                return Ok(Some(value));
            };
        };

        // On leaf nodes, do quiescence search.
        if depth <= 0 {
            let result = self.position.qsearch(depth, alpha, beta, static_eval);
            try!(self.report_progress(result.searched_nodes()));
            let bound = if result.value() >= beta {
                BOUND_LOWER
            } else if result.value() <= alpha {
                BOUND_UPPER
            } else {
                BOUND_EXACT
            };
            self.tt.store(hash,
                          T::Entry::new(result.value(), bound, depth).set_static_eval(static_eval));
            return Ok(Some(result.value()));
        }

        // Save the current move list and other info that we will need
        // at later phases.
        {
            self.moves.save();
            let state = self.state_stack.last_mut().unwrap();
            state.phase = NodePhase::ConsideredNullMove;
            state.is_check = self.position.is_check();
        }

        // Consider null move pruning. In positions that are not prone
        // to zugzwang, we attempt to reduce the search space by
        // trying a "null" or "passing" move, then seeing if the score
        // of the sub-tree search is still high enough to cause a beta
        // cutoff. Nodes are saved by reducing the depth of the
        // sub-tree under the null move.
        if !last_move.is_null() && static_eval >= beta &&
           {
            let p = &self.position;
            !p.evaluator().is_zugzwangy(p.board())
        } {
            // Calculate the reduced depth.
            let reduced_depth = if depth > 7 {
                depth - NULL_MOVE_REDUCTION - 1
            } else {
                depth - NULL_MOVE_REDUCTION
            };

            // Check if the TT indicates that trying a null move is
            // futile. We rely on the fact that if no normal move can
            // reach `beta`, a null move will not do it either.
            if entry.depth() >= max(0, reduced_depth) && entry.value() < beta &&
               entry.bound() & BOUND_UPPER != 0 {
                return Ok(None);
            }

            // Play a null move and search.
            let m = self.position.null_move();
            if self.position.do_move(m) {
                let value = -try!(self.run(-beta, -alpha, max(0, reduced_depth - 1), m));
                self.position.undo_last_move();
                if value >= beta {
                    // The result we are about to return is more or
                    // less a lie (because of the depth reduction),
                    // and therefore we better tell a smaller lie and
                    // return `beta` here instead of `value`.
                    self.tt.store(hash,
                                  T::Entry::new(beta, BOUND_LOWER, depth)
                                      .set_static_eval(static_eval));
                    return Ok(Some(beta));
                }
            }
        }

        // Well, we do not know the value yet.
        Ok(None)
    }

    /// A helper method for `run`. Each call to `run` ends with a call
    /// to `node_end`.
    #[inline]
    fn node_end(&mut self) {
        // Restore the move list from the previous ply (half-move) and
        // pop the state stack.
        if let NodePhase::Pristine = self.state_stack.last().unwrap().phase {
            // For pristine nodes we have not saved a new move list,
            // so we should not call `restore`.
        } else {
            self.moves.restore();
        }
        self.state_stack.pop();

        // Killer moves for distant plys are gradually becoming
        // outdated, so we should downgrade them.
        let downgraded_ply = self.state_stack.len() + KILLERS_DOWNGRADE_DISTANCE;
        if downgraded_ply < DEPTH_MAX as usize {
            self.killers.downgrade(downgraded_ply);
        }
    }

    /// A helper method for `run`. It plays the next legal move in the
    /// current position and returns it.
    ///
    /// Each call to `do_move` for the same position will play and
    /// return a different move. When all legal moves has been played,
    /// `None` will be returned. `do_move` will do whatever it can to
    /// play the best moves first, and the worst last. It will also
    /// try to be efficient, for example it will generate the list of
    /// all pseudo-legal moves at the last possible moment.
    #[inline]
    fn do_move(&mut self) -> Option<Move> {
        debug_assert!(self.state_stack.len() > 0);
        let ply = self.state_stack.len() - 1;
        let state = &mut self.state_stack[ply];
        debug_assert!(if let NodePhase::Pristine = state.phase {
            false
        } else {
            true
        });
        debug_assert!(ply < DEPTH_MAX as usize);

        // Try the hash move.
        if let NodePhase::ConsideredNullMove = state.phase {
            state.phase = NodePhase::TriedHashMove;
            if let Some(mut m) = self.position.try_move_digest(state.hash_move_digest) {
                if self.position.do_move(m) {
                    m.set_score(MOVE_SCORE_MAX);
                    return Some(m);
                }
            }
        }

        // Generate all pseudo-legal moves.
        if let NodePhase::TriedHashMove = state.phase {
            state.phase = NodePhase::GeneratedMoves;
            self.position.generate_moves(self.moves);

            // Remove the already tried hash move from the list.
            if state.hash_move_digest != MoveDigest::invalid() {
                self.moves.pull_move(state.hash_move_digest);
            }

            // Set move scores to captures and pawn promotions to
            // queen according to their static exchange evaluation.
            for m in self.moves.list_mut().iter_mut() {
                let move_score = if m.move_type() == MOVE_PROMOTION {
                    if m.aux_data() == 0 {
                        MOVE_SCORE_MAX - 1
                    } else {
                        0
                    }
                } else if m.captured_piece() < PIECE_NONE {
                    match self.position.evaluate_move(*m) {
                        see if see > 0 => MOVE_SCORE_MAX - 1,
                        see if see == 0 => MOVE_SCORE_MAX - 2,
                        _ => 0,
                    }
                } else {
                    0
                };
                m.set_score(move_score);
            }
        }

        // Try the generated moves.
        while let Some(mut m) = if let NodePhase::TriedLosingCaptures = state.phase {
            // After we have tried the losing captures, we try the
            // rest of the moves in the order in which they reside in
            // the move stack, because at this stage the probability
            // of cut-off is low, so the move ordering is not
            // important.
            self.moves.pop()
        } else {
            self.moves.pull_best()
        } {
            // First -- the winning and even captures and promotions
            // to queen.
            if let NodePhase::GeneratedMoves = state.phase {
                if m.score() > REDUCTION_THRESHOLD {
                    if self.position.do_move(m) {
                        return Some(m);
                    }
                    continue;
                }
                state.phase = NodePhase::TriedWinningMoves;
            }

            // Second -- the killer moves. We try two killer moves in
            // two sequential iterations of the loop. `state.killer`
            // remembers where we are.
            if let NodePhase::TriedWinningMoves = state.phase {
                self.moves.add_move(m);
                let killer = if let Some(k2) = state.killer {
                    state.phase = NodePhase::TriedKillerMoves;
                    k2
                } else {
                    let (k1, k2) = self.killers.get(ply);
                    state.killer = Some(k2);
                    k1
                };
                if killer != MoveDigest::invalid() {
                    if let Some(mut m) = self.moves.pull_move(killer) {
                        if self.position.do_move(m) {
                            m.set_score(MOVE_SCORE_MAX);
                            return Some(m);
                        }
                    }
                }
                continue;
            }

            // Third -- the losing captures.
            if let NodePhase::TriedKillerMoves = state.phase {
                if m.captured_piece() < PIECE_NONE {
                    if self.position.do_move(m) {
                        m.set_score(MOVE_SCORE_MAX);
                        return Some(m);
                    }
                    continue;
                }
                state.phase = NodePhase::TriedLosingCaptures;
                self.moves.add_move(m);

                // TODO: Pull selected quiet moves to the top of the
                // move stack here, using the history
                // heuristics.
                continue;
            }

            // Fourth -- the remaining quiet moves.
            if self.position.do_move(m) {
                if state.is_check || self.position.is_check() || m.move_type() == MOVE_PROMOTION {
                    // When evading check, giving check, or promoting
                    // a pawn -- set a high move score to avoid search
                    // depth reductions.
                    m.set_score(MOVE_SCORE_MAX);
                }
                return Some(m);
            }
        }
        None
    }

    /// A helper method for `run`. It takes back the last move played
    /// by `do_move`.
    #[inline]
    fn undo_move(&mut self) {
        self.position.undo_last_move();
    }

    /// A helper method for `run`. It stores the updated node
    /// information in the transposition table.
    #[inline]
    fn store(&mut self, value: Value, bound: BoundType, depth: Depth, best_move: Move) {
        self.tt.store(self.position.hash(),
                      T::Entry::new(value, bound, depth)
                          .set_move_digest(best_move.digest())
                          .set_static_eval(self.state_stack.last().unwrap().static_eval));
    }

    /// A helper method for `run`. It reports search progress.
    ///
    /// From time to time, we should report how many nodes has been
    /// searched since the beginning of the search. This also gives an
    /// opportunity for the search to be terminated.
    #[inline]
    fn report_progress(&mut self, new_nodes: u64) -> Result<(), TerminatedSearch> {
        let node_count_report_interval = if cfg!(debug_assertions) {
            NODE_COUNT_REPORT_INTERVAL / 100
        } else {
            NODE_COUNT_REPORT_INTERVAL
        };
        self.unreported_nodes += new_nodes;
        if self.unreported_nodes >= node_count_report_interval {
            self.reported_nodes += self.unreported_nodes;
            self.unreported_nodes = 0;
            if (*self.report_function)(self.reported_nodes) {
                return Err(TerminatedSearch);
            }
        }
        Ok(())
    }

    /// A helper method for `run`. It registers that the move `m`
    /// caused a beta cut-off (a killer move).
    #[inline]
    fn register_killer_move(&mut self, m: Move) {
        self.killers.register(self.state_stack.len() - 1, m);
    }
}


/// The highest possible move score.
const MOVE_SCORE_MAX: u32 = ::std::u32::MAX;


/// The number of nodes that will be searched without reporting search
/// progress.
///
/// If this value is too small the engine may become slow, if this
/// value is too big the engine may become unresponsive.
const NODE_COUNT_REPORT_INTERVAL: u64 = 15000;


/// The number of half-moves with which the search depth will be
/// reduced when trying null moves.
const NULL_MOVE_REDUCTION: i8 = 3;


/// Moves with move scores higher than this number will be searched at
/// full depth. Moves with move scores lesser or equal to this number
/// will be searched at reduced depth.
const REDUCTION_THRESHOLD: u32 = 0;


/// When this distance in half-moves is reached, the old killer moves
/// will be downgraded. This affects for how long the successful old
/// killer moves are kept.
const KILLERS_DOWNGRADE_DISTANCE: usize = 3;


/// Tells where we are in the move generation sequence.
enum NodePhase {
    Pristine,
    ConsideredNullMove,
    TriedHashMove,
    GeneratedMoves,
    TriedWinningMoves,
    TriedKillerMoves,
    TriedLosingCaptures,
}


/// Holds information about the state of a node in the search tree.
struct NodeState {
    phase: NodePhase,
    hash_move_digest: MoveDigest,
    static_eval: Value,
    is_check: bool,
    killer: Option<MoveDigest>,
}


/// Holds two killer moves with their hit counters for every
/// half-move.
///
/// "Killer move" is a move which caused beta cut-off in a sibling
/// node, or any other earlier branch in the search tree with the same
/// distance to the root position. The idea is to try that move early
/// -- directly after the hash move and the winning captures.
struct KillerTable {
    array: [KillerPair; DEPTH_MAX as usize],
}

impl KillerTable {
    /// Creates a new instance.
    #[inline]
    pub fn new() -> KillerTable {
        KillerTable { array: [Default::default(); DEPTH_MAX as usize] }
    }

    /// Registers a new killer move for the specified `half_move`.
    #[inline]
    pub fn register(&mut self, half_move: usize, m: Move) {
        debug_assert!(half_move < self.array.len());
        if m.captured_piece() < PIECE_NONE || m.move_type() == MOVE_PROMOTION {
            // We do not want to waste our precious killer-slots on
            // captures and promotions.
            return;
        }
        let pair = &mut self.array[half_move];
        let minor = &mut pair.minor;
        let major = &mut pair.major;
        let digest = m.digest();
        debug_assert!(digest != MoveDigest::invalid());

        // Register the move in one of the slots.
        if major.digest == digest {
            major.hits = major.hits.wrapping_add(1);
            return;
        } else if minor.digest == digest {
            minor.hits = minor.hits.wrapping_add(1);
        } else {
            *minor = Killer {
                digest: digest,
                hits: 1,
            };
        }

        // Swap the slots if the minor killer has got at least as much
        // hits as the major killer.
        if minor.hits >= major.hits {
            mem::swap(minor, major);
        }
    }

    /// Returns the two killer moves for the specified `half_move`.
    ///
    /// This method will not return capture and promotion moves as
    /// killers, because those are tried early anyway. The move
    /// returned in the first slot should be treated as the better one
    /// of the two. If no killer move is available for one or both of
    /// the slots -- `0` is returned instead.
    #[inline]
    pub fn get(&self, half_move: usize) -> (MoveDigest, MoveDigest) {
        debug_assert!(half_move < self.array.len());
        let pair = &self.array[half_move];
        (pair.major.digest, pair.minor.digest)
    }

    /// Reduces the hit counters for the specified `half_move` by a
    /// factor of two.
    #[inline]
    pub fn downgrade(&mut self, half_move: usize) {
        debug_assert!(half_move < self.array.len());
        let pair = &mut self.array[half_move];
        pair.minor.hits >>= 1;
        pair.major.hits >>= 1;
    }

    /// Forgets all registered killer moves.
    #[inline]
    pub fn forget_all(&mut self) {
        for pair in self.array.iter_mut() {
            *pair = Default::default();
        }
    }
}


/// A killer move with its hit counter.
#[derive(Clone, Copy)]
struct Killer {
    pub digest: MoveDigest,
    pub hits: u16,
}


/// A pair of two killer moves.
///
/// The `major` killer is always the more important one. The killers
/// are swapped if at some moment the minor killer has got at least as
/// much hits as the major killer.
#[derive(Clone, Copy)]
struct KillerPair {
    pub minor: Killer,
    pub major: Killer,
}

impl Default for KillerPair {
    fn default() -> KillerPair {
        KillerPair {
            minor: Killer {
                digest: MoveDigest::invalid(),
                hits: 0,
            },
            major: Killer {
                digest: MoveDigest::invalid(),
                hits: 0,
            },
        }
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


#[cfg(test)]
mod tests {
    use super::{Search, KillerTable};
    use value::*;
    use board::*;
    use search_node::*;
    use moves::*;
    use hash_table::*;
    use stock::{StdHashTable, StdHashTableEntry, StdSearchNode, StdQsearch, StdMoveGenerator,
                SimpleEvaluator};
    use utils::MoveStack;

    type P = StdSearchNode<StdQsearch<StdMoveGenerator<SimpleEvaluator>>>;

    #[test]
    fn search() {
        let p = P::from_history("8/8/8/8/3q3k/7n/6PP/2Q2R1K b - - 0 1",
                                &mut vec![].into_iter())
                    .ok()
                    .unwrap();
        let tt = StdHashTable::<StdHashTableEntry>::new(None);
        let mut moves = MoveStack::new();
        let mut report = |_| false;
        let mut search = Search::new(p, &tt, &mut moves, &mut report);
        let value = search.run(VALUE_MIN, VALUE_MAX, 1, Move::invalid()).ok().unwrap();
        assert!(value < -300);
        search.reset();
        let value = search.run(VALUE_MIN, VALUE_MAX, 8, Move::invalid()).ok().unwrap();
        assert!(value > VALUE_EVAL_MAX);
    }

    #[test]
    fn killers() {
        let mut killers = KillerTable::new();
        let mut p = P::from_history("5r2/8/8/4q1p1/3P4/k3P1P1/P2b1R1B/K4R2 w - - 0 1",
                                    &mut vec![].into_iter())
                        .ok()
                        .unwrap();
        let mut v = MoveStack::new();
        p.generate_moves(&mut v);
        let mut i = 1;
        let mut previous_move_digest = MoveDigest::invalid();
        while let Some(m) = v.pop() {
            if m.captured_piece() == PIECE_NONE && p.do_move(m) {
                for _ in 0..i {
                    killers.register(0, m);
                }
                i += 1;
                p.undo_last_move();
                let (killer1, killer2) = killers.get(0);
                assert!(killer1 == m.digest());
                assert!(killer2 == previous_move_digest);
                previous_move_digest = m.digest();
            }
        }
        assert!(killers.get(1) == (MoveDigest::invalid(), MoveDigest::invalid()));
    }
}
