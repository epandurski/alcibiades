//! Implements higher-level facilities.

use std::collections::VecDeque;
use std::cmp::min;
use std::sync::Arc;
use std::time::{SystemTime, Duration};
use basetypes::*;
use tt::*;
use uci::*;
use search::*;
use board::START_POSITION_FEN;
use board::rules::Position;
use board::evaluators::RandomEvaluator;


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
    Nodes(NodeCount), // Stop when the given number of nodes has been searched.
    Depth(u8), // Stop when the given search depth has been reached.
    Never, // An infinite search.
}


/// Implements `UciEngine` trait.
pub struct Engine {
    tt: Arc<Tt>,
    position: Position<RandomEvaluator>,
    current_depth: u8,

    // `Engine` will hand over the real work to `SearchThread`.
    search_thread: SearchThread,

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


impl Engine {
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


impl SetOption for Engine {
    fn options() -> Vec<(String, OptionDescription)> {
        vec![
            // TODO: Calculate a sane limit for the hash size.
            ("Hash".to_string(), OptionDescription::Spin { min: 1, max: 2048, default: 16 }),
            ("Ponder".to_string(), OptionDescription::Check { default: false }),
            ("MultiPV".to_string(), OptionDescription::Spin { min: 1, max: 500, default: 1 }),
        ]
    }

    fn set_option(&mut self, name: &str, value: &str) {
        match name {
            "Ponder" => {
                self.pondering_is_allowed = value == "true";
            }

            "MultiPV" => {
                self.variation_count = match value.parse::<usize>().unwrap_or(0) {
                    0 => 1,
                    n if n > 500 => 500,
                    n => n,
                };
            }

            // An invalid option. Notice that we do not support
            // re-sizing of the transposition table once the engine
            // had started.
            _ => (),
        }
    }
}


impl UciEngine for Engine {
    fn name() -> String {
        format!("{} {}", NAME, VERSION)
    }

    fn author() -> String {
        AUTHOR.to_string()
    }

    fn new(tt_size_mb: Option<usize>) -> Engine {
        let mut tt = Tt::new();
        if let Some(tt_size_mb) = tt_size_mb {
            tt.resize(tt_size_mb);
        }
        let tt = Arc::new(tt);

        Engine {
            tt: tt.clone(),
            position: Position::from_fen(START_POSITION_FEN).ok().unwrap(),
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

    fn new_game(&mut self) {
        self.tt.clear();
    }

    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if let Ok(p) = Position::from_history(fen, moves) {
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
            PlayWhen::TimeManagement(TimeManager::new(&self.position,
                                                      self.pondering_is_allowed,
                                                      wtime,
                                                      btime,
                                                      winc,
                                                      binc,
                                                      movestogo))
        };
        self.silent_since = SystemTime::now();
        self.search_thread.search(&self.position, self.variation_count, searchmoves);
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


/// Contains information about the current progress of a search.
struct SearchStatus {
    /// `true` if the search is done, `false` otherwise.
    pub done: bool,

    /// The search depth completed so far.
    pub depth: u8,

    /// The number of different first moves that are being considered
    /// (from the root position).
    pub searchmoves_count: usize,

    /// The best variations found so far, sorted by descending first
    /// move strength. The first move in each variation will be
    /// different.
    pub variations: Vec<Variation>,

    /// The starting time for the search.
    pub started_at: SystemTime,

    /// The duration of the search in milliseconds.
    pub duration_millis: u64,

    /// The number of analyzed nodes.
    pub searched_nodes: NodeCount,

    /// Average number of analyzed nodes per second.
    pub nps: NodeCount,
}


/// A thread that executes consecutive searches in different starting
/// positions.
struct SearchThread {
    tt: Arc<Tt>,
    position: Box<SearchNode>,
    status: SearchStatus,
    searcher: DeepeningSearcher<MultipvSearcher<StandardSearcher>>,
}

impl SearchThread {
    /// Creates a new instance.
    pub fn new(tt: Arc<Tt>) -> SearchThread {
        use board::evaluators::RandomEvaluator;
        SearchThread {
            tt: tt.clone(),
            position: Box::new(Position::<RandomEvaluator>::from_fen(START_POSITION_FEN)
                                   .ok()
                                   .unwrap()),
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
            searcher: DeepeningSearcher::new(tt),
        }
    }

    /// Terminates the currently running search (if any), starts a new
    /// search, updates the status.
    ///
    /// `position` is the starting position for the new
    /// search. `variation_count` specifies how many best lines of
    /// play to calculate (the first move in each line will be
    /// different). `searchmoves` restricts the analysis to the
    /// supplied list of moves only (no restrictions if the suppied
    /// list is empty). The move format is long algebraic
    /// notation. Examples: e2e4, e7e5, e1g1 (white short castling),
    /// e7e8q (for promotion).
    pub fn search(&mut self,
                  position: &SearchNode,
                  variation_count: usize,
                  mut searchmoves: Vec<String>) {
        self.position = position.copy();

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
            position: position.copy(),
            depth: DEPTH_MAX,
            lower_bound: VALUE_MIN,
            upper_bound: VALUE_MAX,
            searchmoves: searchmoves,
            variation_count: variation_count,
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
            self.status.variations = vec![extract_pv(&self.tt,
                                                     self.position.as_ref(),
                                                     report.depth)];
        }
        self.status.done = report.done;
    }
}


/// Decides when the search must be terminated.
struct TimeManager {
    move_time_millis: u64, // move time in milliseconds
    must_play: bool,
}

impl TimeManager {
    /// Creates a new instance.
    ///
    /// `position` gives the current position. `pondering_is_allowed`
    /// tells if the engine is allowed to use opponent's time for
    /// thinking. `wtime_millis`, `btime_millis`, `winc_millis`, and
    /// `binc_millis` specify the remaining time in milliseconds, and
    /// the number of milliseconds with which the remaining time will
    /// be incremented on each move (for black and white). `movestogo`
    /// specifies the number of moves to the next time control.
    #[allow(unused_variables)]
    pub fn new(position: &Position<RandomEvaluator>,
               pondering_is_allowed: bool,
               wtime_millis: Option<u64>,
               btime_millis: Option<u64>,
               winc_millis: Option<u64>,
               binc_millis: Option<u64>,
               movestogo: Option<u64>)
               -> TimeManager {
        // TODO: We ignore "pondering_is_allowed".

        let (time, inc) = if position.board().to_move() == WHITE {
            (wtime_millis, winc_millis.unwrap_or(0))
        } else {
            (btime_millis, binc_millis.unwrap_or(0))
        };
        let time = time.unwrap_or(0);
        let movestogo = movestogo.unwrap_or(40);
        let movetime = (time + inc * movestogo) / movestogo;
        TimeManager {
            move_time_millis: min(movetime, time / 2),
            must_play: false,
        }
    }

    /// Registers the current search status with the time manager.
    pub fn update_status(&mut self, new_status: &SearchStatus) {
        // TODO: Implement smarter time management.
        self.must_play = new_status.duration_millis >= self.move_time_millis;
    }

    /// Decides if the search must be terminated.
    #[inline]
    pub fn must_play(&self) -> bool {
        self.must_play
    }
}
