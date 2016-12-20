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


const NAME: &'static str = "Alcibiades";
const AUTHOR: &'static str = "Evgeni Pandurski";
const VERSION: &'static str = "0.1";
const START_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1";


struct SearchStatus {
    pub done: bool,
    pub depth: Depth,
    pub searched_nodes: u64,

    // The duration of the search in milliseconds.
    pub duration_millis: u64,
}

impl Default for SearchStatus {
    fn default() -> Self {
        SearchStatus {
            done: false,
            depth: 0,
            searched_nodes: 0,
            duration_millis: 0,
        }
    }
}


enum PlayWhen {
    TimeManagement(TimeManager), // Stop when the time manager says so.
    MoveTime(u64), // Stop after the given number of milliseconds.
    Nodes(u64), // Stop when the given number of nodes has been searched.
    Depth(Depth), // Stop when the given search depth has been completed.
    Never, // An infinite search.
}


struct Engine<S: SearchExecutor<ReportData = Vec<Variation>>> {
    tt: Arc<S::HashTable>,
    position: S::SearchNode,
    searcher: S,
    queue: VecDeque<EngineReply>,

    // The starting time of the current/last search.
    started_at: SystemTime,

    // The status of the current/last search.
    status: SearchStatus,

    // Nodes per second statistics.
    nps_stats: (u64, u64, u64),

    // Helps the engine decide when to show periodic progress reports.
    silent_since: SystemTime,

    // Whether the engine is thinking in pondering mode at the moment.
    is_pondering: bool,

    // Tells the engine when it must stop thinking and play the best move.
    play_when: PlayWhen,

    // Tells the engine if it will be allowed to ponder. This option
    // is needed because the engine might change its time management
    // algorithm when pondering is allowed.
    pondering_is_allowed: bool,
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
            queue: VecDeque::new(),
            started_at: SystemTime::now(),
            status: SearchStatus { done: true, ..Default::default() },
            nps_stats: (0, 0, 0),
            silent_since: SystemTime::now(),
            is_pondering: false,
            play_when: PlayWhen::Never,
            pondering_is_allowed: false,
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
        self.terminate();

        // TODO: What should we do with "mate"?
        let GoParams { mut searchmoves,
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

        // Validate `searchmoves`.
        let searchmoves = {
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
            if moves.is_empty() {
                legal_moves
            } else {
                moves
            }
        };

        // Start a new search.
        self.tt.new_search();
        self.started_at = SystemTime::now();
        self.status = Default::default();
        self.nps_stats = (self.nps_stats.0, 0, 0);
        self.silent_since = SystemTime::now();
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
        self.searcher.start_search(SearchParams {
            search_id: 0,
            position: self.position.clone(),
            depth: DEPTH_MAX,
            lower_bound: VALUE_MIN,
            upper_bound: VALUE_MAX,
            searchmoves: searchmoves,
        });
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

            // Wait for the search thread to do some work, and
            // hopefully update the status. (We must do this even when
            // the search is done -- in that case the next line will
            // just yield the CPU to another process.)
            self.wait_status_update(duration);

            // See if we must play now.
            if !done {
                let &SearchStatus { done, depth, searched_nodes, duration_millis, .. } =
                    &self.status;
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
    fn queue_progress_report(&mut self) {
        let SearchStatus { ref depth, ref searched_nodes, .. } = self.status;
        self.queue.push_back(EngineReply::Info(vec![
            InfoItem { info_type: "depth".to_string(), data: format!("{}", depth) },
            InfoItem { info_type: "nodes".to_string(), data: format!("{}", searched_nodes) },
            InfoItem { info_type: "nps".to_string(), data: format!("{}", self.nps_stats.0) },
        ]));
        self.silent_since = SystemTime::now();
    }

    fn queue_pv(&mut self, variations: &Vec<Variation>) {
        let SearchStatus { ref depth, ref searched_nodes, ref duration_millis, .. } = self.status;
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
                InfoItem { info_type: "nps".to_string(), data: format!("{}", self.nps_stats.0) },
                InfoItem { info_type: "pv".to_string(), data: moves_string },
            ]));
        }
        if !variations.is_empty() {
            self.silent_since = SystemTime::now();
        }
    }

    fn queue_best_move(&mut self) {
        let pv = extract_pv(self.tt.deref(), &self.position, 2);
        let best_move = if let Some(m) = pv.moves.get(0) {
            m.notation()
        } else {
            // If there is no best move, pick the first legal move.
            self.position.legal_moves().get(0).map_or("0000".to_string(), |m| m.notation())
        };
        self.queue.push_back(EngineReply::BestMove {
            best_move: best_move,
            ponder_move: pv.moves.get(1).map(|m| m.notation()),
        });
        self.silent_since = SystemTime::now();
    }

    fn terminate(&mut self) {
        self.searcher.terminate_search();
        while !self.status.done {
            self.wait_status_update(Duration::from_millis(1000));
        }
    }

    fn wait_status_update(&mut self, duration: Duration) {
        self.searcher.wait_report(duration);
        while let Ok(r) = self.searcher.try_recv_report() {
            self.process_report(r)
        }
    }

    fn process_report(&mut self, report: SearchReport<Vec<Variation>>) {
        let d = self.started_at.elapsed().unwrap();
        let duration_millis = 1000 * d.as_secs() + (d.subsec_nanos() / 1_000_000) as u64;
        self.status = SearchStatus {
            done: report.done,
            depth: report.depth,
            searched_nodes: report.searched_nodes,
            duration_millis: duration_millis,
        };

        // Update `self.nps_stats` each 1000 milliseconds.
        let elapsed_millis = duration_millis - self.nps_stats.2;
        if elapsed_millis >= 1000 {
            let nodes = report.searched_nodes - self.nps_stats.1;
            self.nps_stats = (1000 * nodes / elapsed_millis, report.searched_nodes, duration_millis)
        }

        // Inform the time manager.
        if let PlayWhen::TimeManagement(ref mut tm) = self.play_when {
            tm.update_status(&report);
        }

        // Show the primary variations provided with the report (if any).
        self.queue_pv(&report.data);

        // If nothing has happened for a while, show a progress report.
        if self.silent_since.elapsed().unwrap().as_secs() > 10 {
            self.queue_progress_report();
        }
    }
}


/// Serves UCI commands until a "quit" command is received.
///
/// The current thread will block until the UCI session is closed.
///
/// Returns `Err` if the handshake was unsuccessful, or if an IO error
/// occurred.
pub fn run_server<S: SearchExecutor<ReportData = Vec<Variation>>>() -> io::Result<()> {
    run_engine::<Engine<S>>()
}
