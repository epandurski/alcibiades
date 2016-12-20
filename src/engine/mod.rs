//! Facilities for implementing chess engines.

pub mod time_manager;

use std::collections::VecDeque;
use std::ops::Deref;
use std::sync::Arc;
use std::time::{SystemTime, Duration};
use std::cmp::min;
use std::io;
use uci::*;
use search::*;
use self::time_manager::*;


/// Serves UCI commands until a "quit" command is received.
///
/// The current thread will block until the UCI session is closed.
///
/// Returns `Err` if the handshake was unsuccessful, or if an IO error
/// occurred.
pub fn run_server<S: SearchExecutor<ReportData = Vec<Variation>>>() -> io::Result<()> {
    run_engine::<Engine<S>>()
}


/// The chess starting position in Forsyth–Edwards notation (FEN).
const START_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1";

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
    Depth(Depth), // Stop when the given search depth has been completed.
    Never, // An infinite search.
}


/// Implements `UciEngine` trait.
struct Engine<S: SearchExecutor<ReportData = Vec<Variation>>> {
    tt: Arc<S::HashTable>,
    position: S::SearchNode,
    searcher: S,

    // The starting time of the current/last search.
    started_at: SystemTime,

    // The status of the current/last search.
    status: SearchStatus,

    // Tells the engine when it must stop thinking and play the best
    // move it has found.
    play_when: PlayWhen,

    // Tells the engine if it will be allowed to ponder. This option
    // is needed because the engine might change its time management
    // algorithm when pondering is allowed.
    pondering_is_allowed: bool,

    // `true` if the engine is thinking in pondering mode at the moment.
    is_pondering: bool,

    // A queue for the messages send by the engine to the GUI.
    queue: VecDeque<EngineReply>,

    // Helps the engine decide when to send periodic progress reports.
    silent_since: SystemTime,
}


impl<S: SearchExecutor<ReportData = Vec<Variation>>> UciEngine for Engine<S> {
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
            ("Clear Hash".to_string(), OptionDescription::Button),
            ("Ponder".to_string(), OptionDescription::Check { default: false }),
        ];
        options.extend(S::options());

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
            position: S::SearchNode::from_history(START_FEN, &mut vec![].into_iter()).ok().unwrap(),
            searcher: S::new(tt),
            started_at: SystemTime::now(),
            status: SearchStatus {
                done: true,
                depth: 0,
                searched_nodes: 0,
                duration_millis: 0,
                nps: 0,
            },
            play_when: PlayWhen::Never,
            pondering_is_allowed: false,
            is_pondering: false,
            queue: VecDeque::new(),
            silent_since: SystemTime::now(),
        }
    }

    fn set_option(&mut self, name: &str, value: &str) {
        match name {
            "Ponder" => {
                // TODO: Can we remove this?
                self.pondering_is_allowed = value == "true";
            }
            "Hash" => {
                // We do not support re-sizing of the transposition
                // table once the engine has been started.
                return;
            }
            "Clear Hash" => {
                self.tt.clear();
            }
            _ => (),
        }
        S::set_option(name, value);
    }

    fn new_game(&mut self) {
        self.tt.clear();
    }

    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if let Ok(p) = S::SearchNode::from_history(fen, moves) {
            self.position = p;
        }
    }

    fn go(&mut self, params: GoParams) {
        // Note: We ignore the "mate" field.
        //
        // TODO: What should we do with "mate"?
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

        self.terminate();
        self.tt.new_search();
        self.is_pondering = ponder;
        self.play_when = if infinite {
            PlayWhen::Never
        } else if movetime.is_some() {
            PlayWhen::MoveTime(movetime.unwrap())
        } else if nodes.is_some() {
            PlayWhen::Nodes(nodes.unwrap())
        } else if depth.is_some() {
            PlayWhen::Depth(min(depth.unwrap(), DEPTH_MAX as u64) as Depth)
        } else {
            PlayWhen::TimeManagement(TimeManager::new(self.position.board().to_move,
                                                      self.pondering_is_allowed,
                                                      wtime,
                                                      btime,
                                                      winc,
                                                      binc,
                                                      movestogo))
        };
        self.silent_since = SystemTime::now();
        self.start(searchmoves);
    }

    fn ponder_hit(&mut self) {
        if self.status.done {
            self.queue_best_move();
        } else {
            self.is_pondering = false;
        }
    }

    fn stop(&mut self) {
        self.terminate();
        self.queue_best_move();
    }

    fn wait_for_reply(&mut self, duration: Duration) -> Option<EngineReply> {
        if self.queue.is_empty() {
            let done = self.status.done;

            // Wait for `search_thread` to do some work, and hopefully
            // update its status. (We must do this even when the
            // search is done -- in that case the next line will just
            // yield the CPU to another process.)
            self.wait_status_update(duration);

            if !done {
                let &SearchStatus { done, depth, searched_nodes, duration_millis, .. } =
                    &self.status;

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
        self.terminate();
    }
}


impl<S: SearchExecutor<ReportData = Vec<Variation>>> Engine<S> {
    /// Adds a progress report message to the queue.
    fn queue_progress_report(&mut self) {
        let SearchStatus { ref depth, ref searched_nodes, ref nps, .. } = self.status;
        self.queue.push_back(EngineReply::Info(vec![
            InfoItem { info_type: "depth".to_string(), data: format!("{}", depth) },
            InfoItem { info_type: "nodes".to_string(), data: format!("{}", searched_nodes) },
            InfoItem { info_type: "nps".to_string(), data: format!("{}", nps) },
        ]));
        self.silent_since = SystemTime::now();
    }

    /// Adds a message containing the current (multi)PV to the queue.
    fn queue_variations(&mut self, variations: &Vec<Variation>) {
        let SearchStatus { ref depth, ref searched_nodes, ref duration_millis, ref nps, .. } =
            self.status;
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

    /// Adds a message containing the current best move to the queue.
    fn queue_best_move(&mut self) {
        let pv = extract_pv(self.tt.deref(), &self.position, 2);
        let best_move = if let Some(m) = pv.moves.get(0) {
            m.notation()
        } else {
            // If the PV is empty, pick the first legal move.
            self.position.legal_moves().get(0).map_or("0000".to_string(), |m| m.notation())
        };
        self.queue.push_back(EngineReply::BestMove {
            best_move: best_move,
            ponder_move: pv.moves.get(1).map(|m| m.notation()),
        });
        self.silent_since = SystemTime::now();
    }

    /// Terminates the currently running search (if any), starts a new
    /// search, updates the status.
    fn start(&mut self, mut searchmoves: Vec<String>) {
        // Validate `searchmoves`.
        let mut moves = vec![];
        let legal_moves = self.position.legal_moves();
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
        self.terminate();

        // Update the status.
        self.status = SearchStatus {
            done: false,
            depth: 0,
            searched_nodes: 0,
            duration_millis: 0,
            ..self.status
        };

        // Start a new search.
        self.searcher.start_search(SearchParams {
            search_id: 0,
            position: self.position.clone(),
            depth: DEPTH_MAX,
            lower_bound: VALUE_MIN,
            upper_bound: VALUE_MAX,
            searchmoves: searchmoves,
        });
    }

    /// Terminates the currently running search (if any), updates the
    /// status.
    fn terminate(&mut self) {
        self.searcher.terminate_search();
        while !self.status.done {
            self.wait_status_update(Duration::from_millis(1000));
        }
    }

    /// Waits for a search status update, timing out after a specified
    /// duration or earlier.
    fn wait_status_update(&mut self, duration: Duration) {
        self.searcher.wait_report(duration);
        while let Ok(r) = self.searcher.try_recv_report() {
            self.process_report(r)
        }
    }

    /// A helper method. It updates the current status according to
    /// the received report message.
    fn process_report(&mut self, report: SearchReport<Vec<Variation>>) {
        // Register the search status with the time manager.
        if let PlayWhen::TimeManagement(ref mut tm) = self.play_when {
            tm.update_status(&report);
        }

        let duration = self.started_at.elapsed().unwrap();
        self.status.duration_millis = 1000 * duration.as_secs() +
                                      (duration.subsec_nanos() / 1000000) as u64;
        self.status.searched_nodes = report.searched_nodes;
        self.status.nps = 1000 * (self.status.nps + self.status.searched_nodes) /
                          (1000 + self.status.duration_millis);
        self.status.depth = report.depth;
        self.status.done = report.done;

        // Send the (multi)PV if available.
        if !report.data.is_empty() {
            self.queue_variations(&report.data);
        }

        // Send a progress report periodically.
        if self.silent_since.elapsed().unwrap().as_secs() > 10 {
            self.queue_progress_report();
        }
    }
}


/// Contains information about the current progress of a search.
struct SearchStatus {
    pub done: bool,
    pub depth: Depth,
    pub searched_nodes: u64,

    /// The duration of the search in milliseconds.
    pub duration_millis: u64,

    /// Average number of analyzed nodes per second.
    pub nps: u64,
}
