//! Facilities for implementing chess engines.

pub mod time_manager;

use std::cell::UnsafeCell;
use std::collections::VecDeque;
use std::ops::Deref;
use std::sync::Arc;
use std::time::{SystemTime, Duration};
use std::cmp::min;
use std::io;
use uci::*;
use search::*;
use self::time_manager::*;


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


struct Engine<T: SearchExecutor<ReportData = Vec<Variation>>> {
    tt: Arc<T::HashTable>,
    position: T::SearchNode,
    searcher: T,
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

impl<T: SearchExecutor<ReportData = Vec<Variation>>> UciEngine for Engine<T> {
    fn name() -> &'static str {
        ENGINE_IDENTITY.with(|e| unsafe { (&*e.get()).name })
    }

    fn author() -> &'static str {
        ENGINE_IDENTITY.with(|e| unsafe { (&*e.get()).author })
    }

    fn options() -> Vec<(String, OptionDescription)> {
        // Add up all suported options.
        let mut options = vec![
            ("Hash".to_string(), OptionDescription::Spin { min: 1, max: 64 * 1024, default: 16 }),
            ("Clear Hash".to_string(), OptionDescription::Button),
            ("Ponder".to_string(), OptionDescription::Check { default: false }),
        ];
        options.extend(T::options());

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

    fn new(tt_size_mb: Option<usize>) -> Engine<T> {
        const START_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1";
        let tt = Arc::new(T::HashTable::new(tt_size_mb));
        let started_at = SystemTime::now();
        Engine {
            tt: tt.clone(),
            position: T::SearchNode::from_history(START_FEN, &mut vec![].into_iter()).ok().unwrap(),
            searcher: T::new(tt),
            queue: VecDeque::new(),
            started_at: started_at,
            status: SearchStatus { done: true, ..Default::default() },
            nps_stats: (0, 0, 0),
            silent_since: started_at,
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
        T::set_option(name, value);
    }

    fn new_game(&mut self) {
        self.tt.clear();
    }

    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if let Ok(p) = T::SearchNode::from_history(fen, moves) {
            self.position = p;
        }
    }

    fn go(&mut self, params: &GoParams) {
        self.terminate();

        // Validate `params.searchmoves`.
        //
        // TODO: What should we do with "mate"?
        let searchmoves = {
            let mut moves = vec![];
            let legal_moves = self.position.legal_moves();
            if !params.searchmoves.is_empty() {
                let mut v = params.searchmoves.clone();
                v.sort();
                for m in legal_moves.iter() {
                    if v.binary_search(&m.notation()).is_ok() {
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
        self.silent_since = self.started_at;
        self.is_pondering = params.ponder;
        self.play_when = if params.infinite {
            PlayWhen::Never
        } else if params.movetime.is_some() {
            PlayWhen::MoveTime(params.movetime.unwrap())
        } else if params.nodes.is_some() {
            PlayWhen::Nodes(params.nodes.unwrap())
        } else if params.depth.is_some() {
            PlayWhen::Depth(min(params.depth.unwrap(), DEPTH_MAX as u64) as Depth)
        } else {
            PlayWhen::TimeManagement(TimeManager::new(self.position.board().to_move,
                                                      self.pondering_is_allowed,
                                                      params.wtime,
                                                      params.btime,
                                                      params.winc,
                                                      params.binc,
                                                      params.movestogo))
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
            let is_thinking = !self.status.done;

            // Wait for the search thread to do some work, and
            // hopefully update the status. (We must do this even when
            // the engine is not thinking -- in that case the next
            // line will just yield the CPU to another process.)
            self.wait_status_update(duration);

            // See if we must stop thinking and play.
            if is_thinking && !self.is_pondering &&
               match self.play_when {
                PlayWhen::TimeManagement(ref tm) => self.status.done || tm.must_play(),
                PlayWhen::MoveTime(t) => self.status.done || self.status.duration_millis >= t,
                PlayWhen::Nodes(n) => self.status.done || self.status.searched_nodes >= n,
                PlayWhen::Depth(d) => self.status.done || self.status.depth >= d,
                PlayWhen::Never => false,
            } {
                self.stop();
            }
        }

        self.queue.pop_front()
    }

    fn exit(&mut self) {
        self.terminate();
    }
}

impl<T: SearchExecutor<ReportData = Vec<Variation>>> Engine<T> {
    fn queue_progress_info(&mut self) {
        let SearchStatus { ref depth, ref searched_nodes, ref duration_millis, .. } = self.status;
        self.queue.push_back(EngineReply::Info(vec![
            InfoItem { info_type: "depth".to_string(), data: format!("{}", depth) },
            InfoItem { info_type: "time".to_string(), data: format!("{}", duration_millis) },
            InfoItem { info_type: "nodes".to_string(), data: format!("{}", searched_nodes) },
            InfoItem { info_type: "nps".to_string(), data: format!("{}", self.nps_stats.0) },
        ]));
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
            let mut pv = String::new();
            for m in moves {
                pv.push_str(&m.notation());
                pv.push(' ');
            }
            self.queue.push_back(EngineReply::Info(vec![
                InfoItem { info_type: "depth".to_string(), data: format!("{}", depth) },
                InfoItem { info_type: "multipv".to_string(), data: format!("{}", i + 1) },
                InfoItem { info_type: "score".to_string(), data: format!("cp {}{}", value, bound_suffix) },
                InfoItem { info_type: "time".to_string(), data: format!("{}", duration_millis) },
                InfoItem { info_type: "nodes".to_string(), data: format!("{}", searched_nodes) },
                InfoItem { info_type: "nps".to_string(), data: format!("{}", self.nps_stats.0) },
                InfoItem { info_type: "pv".to_string(), data: pv },
            ]));
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
            self.process_report(&r)
        }
    }

    fn process_report(&mut self, report: &SearchReport<Vec<Variation>>) {
        let duration_millis = {
            let d = self.started_at.elapsed().unwrap();
            1000 * d.as_secs() + (d.subsec_nanos() / 1_000_000) as u64
        };
        self.status = SearchStatus {
            done: report.done,
            depth: report.depth,
            searched_nodes: report.searched_nodes,
            duration_millis: duration_millis,
        };

        // Update `self.nps_stats` every 1000 milliseconds.
        let elapsed_millis = duration_millis - self.nps_stats.2;
        if elapsed_millis >= 1000 {
            let nodes = report.searched_nodes - self.nps_stats.1;
            self.nps_stats = (1000 * nodes / elapsed_millis, report.searched_nodes, duration_millis)
        }

        // Inform the time manager.
        if let PlayWhen::TimeManagement(ref mut tm) = self.play_when {
            tm.update_status(report);
        }

        // If primary variations are provided with the report, show them.
        if !report.data.is_empty() {
            self.queue_pv(&report.data);
            self.silent_since = SystemTime::now();
        }

        // If nothing has happened for a while, show progress info.
        if self.silent_since.elapsed().unwrap().as_secs() > 10 {
            self.queue_progress_info();
            self.silent_since = SystemTime::now();
        }
    }
}


/// Serves UCI commands until a "quit" command is received.
///
/// The current thread will block until the UCI session is closed.
///
/// Returns `Err` if the handshake was unsuccessful, or if an IO error
/// occurred.
pub fn run_server<T>(name: &'static str, author: &'static str) -> io::Result<()>
    where T: SearchExecutor<ReportData = Vec<Variation>>
{
    ENGINE_IDENTITY.with(|e| unsafe {
        let e = &mut *e.get();
        e.name = name;
        e.author = author;
    });
    run_engine::<Engine<T>>()
}


// Since `run_server` blocks the current thread, it can safely pass
// engine's name and author as thread-local statics. (This time,
// simplicity beats beauty.)
thread_local!(
    static ENGINE_IDENTITY: UnsafeCell<EngineIdentity> =
        UnsafeCell::new(EngineIdentity { name: "", author: ""})
);

struct EngineIdentity {
    name: &'static str,
    author: &'static str,
}
