//! Implements higher-level facilities.

mod uci;
pub mod time_manager;

use std::collections::VecDeque;
use std::sync::Arc;
use std::time::{SystemTime, Duration};
use std::cmp::{max, Ordering};
use std::ops::Deref;
use std::io;
use chesstypes::*;
use search::*;
use self::time_manager::*;
use self::uci::{UciEngine, EngineReply, InfoItem, GoParams, run_server};
pub use self::uci::OptionDescription;


/// A trait for announcing and changing configuration options.
pub trait SetOption {
    /// Returns a list of supported configuration options (name and
    /// description).
    fn options() -> Vec<(String, OptionDescription)> {
        vec![]
    }

    /// Sets a new value for a given configuration option.
    ///
    /// Does nothing if called with unsupported option name.
    #[allow(unused_variables)]
    fn set_option(name: &str, value: &str) {}
}


/// The chess starting position in Forsyth–Edwards notation (FEN).
const START_POSITION_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1";

/// The version of the program.
const VERSION: &'static str = "0.1";

/// The name of the program.
const NAME: &'static str = "Alcibiades";

/// The author of the program.
const AUTHOR: &'static str = "Evgeni Pandurski";


/// Represents a condition for terminating the search.
enum PlayWhen {
    TimeManagement(TimeManager), // Stop when the time manager says.
    MoveTime(u64), // Stop after the given number of milliseconds.
    Nodes(u64), // Stop when the given number of nodes has been searched.
    Depth(u8), // Stop when the given search depth has been reached.
    Never, // An infinite search.
}


/// Implements `UciEngine` trait.
struct Engine<S: SearchExecutor> {
    tt: Arc<S::HashTable>,
    position: S::SearchNode,
    current_depth: u8,

    // `Engine` will hand over the real work to `SearchThread`.
    search_thread: SearchThread<S>,

    // Tells the engine when it must stop thinking and play the best
    // move it has found.
    play_when: PlayWhen,

    // Tells the engine how many best lines of play to calculate and
    // send to the GUI (the first move in each line will be
    // different). This is the so called "MultiPV" mode.
    variation_count: usize,

    // Tells the engine if it will be allowed to ponder. This option
    // is needed because the engine might change its time management
    // algorithm when pondering is allowed.
    pondering_is_allowed: bool,

    // `true` if the engine is thinking in pondering mode at the
    // moment.
    is_pondering: bool,

    // This helps the engine decide when to send periodic progress
    // reports.
    silent_since: SystemTime,

    // A queue for the messages send by the engine to the GUI.
    queue: VecDeque<EngineReply>,
}


impl<S: SearchExecutor> Engine<S> {
    /// A helper method. It it adds a progress report message to the
    /// queue.
    fn queue_progress_report(&mut self) {
        let &SearchStatus { depth, searched_nodes, nps, .. } = self.search_thread.status();
        self.queue.push_back(EngineReply::Info(vec![
            InfoItem { info_type: "depth".to_string(), data: format!("{}", depth) },
            InfoItem { info_type: "nodes".to_string(), data: format!("{}", searched_nodes) },
            InfoItem { info_type: "nps".to_string(), data: format!("{}", nps) },
        ]));
        self.silent_since = SystemTime::now();
    }

    /// A helper method. It it adds a message containing the current
    /// (multi)PV to the queue.
    fn queue_pv(&mut self) {
        let &SearchStatus { depth, ref variations, searched_nodes, duration_millis, nps, .. } =
            self.search_thread.status();
        for (i, &Variation { ref moves, value, bound }) in variations.iter().enumerate() {
            let bound_suffix = match bound {
                BOUND_EXACT => "",
                BOUND_UPPER => " upperbound",
                BOUND_LOWER => " lowerbound",
                _ => panic!("unexpected bound type"),
            };
            let mut moves_string = String::new();
            for m in moves {
                moves_string.push_str(&m.notation());
                moves_string.push(' ');
            }
            self.queue.push_back(EngineReply::Info(vec![
                InfoItem { info_type: "depth".to_string(), data: format!("{}", depth) },
                InfoItem { info_type: "multipv".to_string(), data: format!("{}", i + 1) },
                InfoItem { info_type: "score".to_string(), data: format!("cp {}{}", value, bound_suffix) },
                InfoItem { info_type: "time".to_string(), data: format!("{}", duration_millis) },
                InfoItem { info_type: "nodes".to_string(), data: format!("{}", searched_nodes) },
                InfoItem { info_type: "nps".to_string(), data: format!("{}", nps) },
                InfoItem { info_type: "pv".to_string(), data: moves_string },
            ]));
        }
        self.silent_since = SystemTime::now();
    }

    /// A helper method. It it adds a message containing the current
    /// best move to the queue.
    fn queue_best_move(&mut self) {
        let &SearchStatus { ref variations, .. } = self.search_thread.status();
        self.queue.push_back(EngineReply::BestMove {
            best_move: variations[0].moves.get(0).map_or("0000".to_string(), |m| m.notation()),
            ponder_move: variations[0].moves.get(1).map(|m| m.notation()),
        });
        self.silent_since = SystemTime::now();
    }
}


impl<S: SearchExecutor> UciEngine for Engine<S> {
    fn name() -> String {
        format!("{} {}", NAME, VERSION)
    }

    fn author() -> String {
        AUTHOR.to_string()
    }

    fn options() -> Vec<(String, OptionDescription)> {
        // Add up all suported options.
        let mut options = vec![
            ("Hash".to_string(), OptionDescription::Spin { min: 1, max: 64 * 1024, default: 16 }),
            ("Ponder".to_string(), OptionDescription::Check { default: false }),
        ];
        options.extend(S::options());
        options.extend(S::SearchNode::options());

        // Remove duplicated options.
        options.sort_by(|a, b| a.0.cmp(&b.0));
        let mut options_dedup = vec![];
        let mut prev_name = String::from("");
        for o in options.drain(..) {
            if o.0 == prev_name {
                continue;
            }
            prev_name = o.0.clone();
            options_dedup.push(o);
        }
        options_dedup
    }

    fn new(tt_size_mb: Option<usize>) -> Engine<S> {
        let tt = Arc::new(S::HashTable::new(tt_size_mb));
        Engine {
            tt: tt.clone(),
            position: S::SearchNode::create(START_POSITION_FEN, &mut vec![].into_iter())
                          .ok()
                          .unwrap(),
            current_depth: 0,
            search_thread: SearchThread::new(tt),
            play_when: PlayWhen::Never,
            variation_count: 1,
            pondering_is_allowed: false,
            is_pondering: false,
            silent_since: SystemTime::now(),
            queue: VecDeque::new(),
        }
    }

    fn set_option(&mut self, name: &str, value: &str) {
        match name {
            "Ponder" => {
                self.pondering_is_allowed = value == "true";
            }
            "MultiPV" => {
                self.variation_count = max(value.parse::<usize>().unwrap_or(0), 1);
            }
            "Hash" => {
                // We do not support re-sizing of the transposition
                // table once the engine has been started.
                return;
            }
            _ => (),
        }
        S::set_option(name, value);
        S::SearchNode::set_option(name, value);
    }

    fn new_game(&mut self) {
        self.tt.clear();
    }

    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if let Ok(p) = S::SearchNode::create(fen, moves) {
            self.position = p;
        }
    }

    fn go(&mut self, params: GoParams) {
        // Note: We ignore the "mate" field.
        let GoParams { searchmoves,
                       ponder,
                       wtime,
                       btime,
                       winc,
                       binc,
                       movestogo,
                       depth,
                       nodes,
                       movetime,
                       infinite,
                       .. } = params;

        self.search_thread.stop();
        self.tt.new_search();
        self.current_depth = 0;
        self.is_pondering = ponder;
        self.play_when = if infinite {
            PlayWhen::Never
        } else if movetime.is_some() {
            PlayWhen::MoveTime(movetime.unwrap())
        } else if nodes.is_some() {
            PlayWhen::Nodes(nodes.unwrap())
        } else if depth.is_some() {
            PlayWhen::Depth(depth.unwrap() as u8)
        } else {
            PlayWhen::TimeManagement(TimeManager::new(self.position.to_move(),
                                                      self.pondering_is_allowed,
                                                      wtime,
                                                      btime,
                                                      winc,
                                                      binc,
                                                      movestogo))
        };
        self.silent_since = SystemTime::now();
        self.search_thread.search(&self.position, searchmoves);
    }

    fn ponder_hit(&mut self) {
        if self.search_thread.status().done {
            self.queue_best_move();
        } else {
            self.is_pondering = false;
        }
    }

    fn stop(&mut self) {
        self.search_thread.stop();
        self.queue_best_move();
    }

    fn wait_for_reply(&mut self, duration: Duration) -> Option<EngineReply> {
        if self.queue.is_empty() {
            let done = self.search_thread.status().done;

            // Wait for `search_thread` to do some work, and hopefully
            // update its status. (We must do this even when the
            // search is done -- in that case the next line will just
            // yield the CPU to another process.)
            self.search_thread.wait_status_update(duration);

            if !done {
                let &SearchStatus { done, depth, searched_nodes, duration_millis, .. } =
                    self.search_thread.status();

                // Send the (multi)PV for each newly reached depth.
                if depth > self.current_depth {
                    self.current_depth = depth;
                    self.queue_pv();
                }

                // Send a progress report periodically.
                if self.silent_since.elapsed().unwrap().as_secs() > 10 {
                    self.queue_progress_report();
                }

                // Register the search status with the time manager.
                if let PlayWhen::TimeManagement(ref mut tm) = self.play_when {
                    tm.update_status(self.search_thread.status());
                }

                // Check if we must play now.
                if !self.is_pondering &&
                   match self.play_when {
                    PlayWhen::TimeManagement(ref tm) => done || tm.must_play(),
                    PlayWhen::MoveTime(t) => done || duration_millis >= t,
                    PlayWhen::Nodes(n) => done || searched_nodes >= n,
                    PlayWhen::Depth(d) => done || depth >= d,
                    PlayWhen::Never => false,
                } {
                    self.stop();
                }
            }
        }

        self.queue.pop_front()
    }

    fn exit(&mut self) {
        self.search_thread.stop();
    }
}


/// A thread that executes consecutive searches in different starting
/// positions.
struct SearchThread<S: SearchExecutor> {
    tt: Arc<S::HashTable>,
    position: S::SearchNode,
    status: SearchStatus,
    searcher: S,
}

impl<S: SearchExecutor> SearchThread<S> {
    /// Creates a new instance.
    pub fn new(tt: Arc<S::HashTable>) -> SearchThread<S> {
        SearchThread {
            tt: tt.clone(),
            position: S::SearchNode::create(START_POSITION_FEN, &mut vec![].into_iter())
                          .ok()
                          .unwrap(),
            status: SearchStatus {
                done: true,
                depth: 0,
                variations: vec![Variation {
                                     value: VALUE_MIN,
                                     bound: BOUND_LOWER,
                                     moves: vec![],
                                 }],
                searchmoves_count: 20,
                searched_nodes: 0,
                started_at: SystemTime::now(),
                duration_millis: 0,
                nps: 0,
            },
            searcher: S::new(tt),
        }
    }

    /// Terminates the currently running search (if any), starts a new
    /// search, updates the status.
    ///
    /// `position` is the starting position for the new
    /// search. `searchmoves` restricts the analysis to the supplied
    /// list of moves only (no restrictions if the suppied list is
    /// empty). The move format is long algebraic notation. Examples:
    /// e2e4, e7e5, e1g1 (white short castling), e7e8q (for
    /// promotion).
    pub fn search(&mut self, position: &S::SearchNode, mut searchmoves: Vec<String>) {
        self.position = position.clone();

        // Validate `searchmoves`.
        let mut moves = vec![];
        let legal_moves = position.legal_moves();
        if !searchmoves.is_empty() {
            searchmoves.sort();
            for m in legal_moves.iter() {
                if searchmoves.binary_search(&m.notation()).is_ok() {
                    moves.push(*m);
                }
            }
        };
        let searchmoves = if moves.is_empty() {
            legal_moves
        } else {
            moves
        };

        // Terminate the currently running search.
        self.stop();

        // Update the status.
        self.status = SearchStatus {
            done: false,
            depth: 0,
            searchmoves_count: searchmoves.len(),
            variations: vec![Variation {
                                 value: VALUE_MIN,
                                 bound: BOUND_LOWER,
                                 moves: searchmoves.first().map_or(vec![], |m| vec![*m]),
                             }],
            searched_nodes: 0,
            started_at: SystemTime::now(),
            duration_millis: 0,
            ..self.status
        };

        // Start a new search.
        self.searcher.start_search(SearchParams {
            search_id: 0,
            position: position.clone(),
            depth: DEPTH_MAX,
            lower_bound: VALUE_MIN,
            upper_bound: VALUE_MAX,
            searchmoves: searchmoves,
        });
    }

    /// Terminates the currently running search (if any), updates the
    /// status.
    pub fn stop(&mut self) {
        self.searcher.terminate_search();
        while !self.status().done {
            self.wait_status_update(Duration::from_millis(1000));
        }
    }

    /// Returns the status of the current search.
    ///
    /// **Important note:** Consecutive calls to this method will
    /// return the same unchanged result. Only after calling `search`,
    /// `stop`, or `wait_status_update`, the result returned by
    /// `status` may change.
    #[inline(always)]
    pub fn status(&self) -> &SearchStatus {
        &self.status
    }

    /// Waits for a search status update, timing out after a specified
    /// duration or earlier.
    pub fn wait_status_update(&mut self, duration: Duration) {
        self.searcher.wait_report(duration);
        while let Ok(r) = self.searcher.try_recv_report() {
            self.process_report(r)
        }
    }

    /// A helper method. It updates the current status according to
    /// the received report message.
    fn process_report(&mut self, report: SearchReport) {
        let duration = self.status.started_at.elapsed().unwrap();
        self.status.duration_millis = 1000 * duration.as_secs() +
                                      (duration.subsec_nanos() / 1000000) as u64;
        self.status.searched_nodes = report.searched_nodes;
        self.status.nps = 1000 * (self.status.nps + self.status.searched_nodes) /
                          (1000 + self.status.duration_millis);
        if self.status.depth < report.depth {
            self.status.depth = report.depth;
            self.status.variations = vec![extract_pv(self.tt.deref(),
                                                     &self.position,
                                                     report.depth)];
        }
        self.status.done = report.done;
    }
}


/// A sequence of moves from some starting position, together with the
/// value assigned to the final position.
pub struct Variation {
    /// A sequence of moves from some starting position.
    pub moves: Vec<Move>,

    /// The value assigned to the final position.
    pub value: Value,

    /// The accuracy of the assigned value.
    pub bound: BoundType,
}


/// Extracts the primary variation for a given position from the
/// transposition table and returns it.
/// 
/// **Important note:** Values under `-9999`, or over `9999` will be
/// chopped.
fn extract_pv<T: HashTable, N: SearchNode>(tt: &T, position: &N, depth: u8) -> Variation {
    let mut p = position.clone();
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
                if let Some(m) = p.try_move_digest(entry.move_digest()) {
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


/// Serves UCI commands until a "quit" command is received.
///
/// The current thread will block until the UCI session is closed.
///
/// Returns `Err` if the handshake was unsuccessful, or if an IO error
/// occurred.
pub fn run<S: SearchExecutor>() -> io::Result<()> {
    run_server::<Engine<S>>()
}
