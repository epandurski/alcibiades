//! Implements a generic chess engine.

use std::process;
use std::marker::PhantomData;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, Duration};
use std::cmp::{min, max};
use uci::*;
use value::*;
use depth::*;
use search::*;
use hash_table::*;
use moves::Move;
use search_node::SearchNode;
use time_manager::{TimeManager, RemainingTime};


struct SearchStatus {
    pub done: bool,
    pub depth: Depth,
    pub value: Value,
    pub searched_nodes: u64,

    // The duration of the search in milliseconds.
    pub duration_millis: u64,
}

impl Default for SearchStatus {
    fn default() -> Self {
        SearchStatus {
            done: false,
            depth: 0,
            value: VALUE_UNKNOWN,
            searched_nodes: 0,
            duration_millis: 0,
        }
    }
}


enum PlayWhen<S, T>
    where S: DeepeningSearch<ReportData = Vec<Variation>>,
          T: TimeManager<S>
{
    TimeManagement(T), // Stop when the time manager says so.
    MoveTime(u64), // Stop after the given number of milliseconds.
    Nodes(u64), // Stop when the given number of nodes has been searched.
    Depth(Depth), // Stop when the given search depth has been completed.
    Mate(i16), // Stop when a mate in the given number of moves is found.
    Never(PhantomData<S>), // An infinite search.
}


struct Engine<S, T>
    where S: DeepeningSearch<ReportData = Vec<Variation>>,
          T: TimeManager<S>
{
    tt: Arc<S::HashTable>,
    position: S::SearchNode,
    searcher: S,
    queue: VecDeque<EngineReply>,

    // The starting time of the current/last search.
    started_at: SystemTime,

    // The status of the current/last search.
    status: SearchStatus,

    // The current best line of play.
    best_line: Vec<Move>,

    // Nodes per second statistics.
    nps_stats: (u64, u64, u64),

    // Helps the engine decide when to show periodic progress reports.
    silent_since: SystemTime,

    // Whether the engine is thinking in pondering mode at the moment.
    is_pondering: bool,

    // Tells the engine when it must stop thinking and play the best move.
    play_when: PlayWhen<S, T>,
}

impl<S, T> UciEngine for Engine<S, T>
    where S: DeepeningSearch<ReportData = Vec<Variation>>,
          T: TimeManager<S>
{
    fn name() -> &'static str {
        ENGINE.lock().unwrap().as_ref().unwrap().name
    }

    fn author() -> &'static str {
        ENGINE.lock().unwrap().as_ref().unwrap().author
    }

    fn options() -> Vec<(&'static str, OptionDescription)> {
        // Add up all suported options.
        let mut options = vec![
            ("Hash", OptionDescription::Spin { min: 1, max: 64 * 1024, default: 16 }),
            ("Clear Hash", OptionDescription::Button),
        ];
        options.extend(S::options());
        options.extend(T::options());

        // Remove duplicated options.
        options.sort_by(|a, b| a.0.cmp(&b.0));
        let mut options_dedup = vec![];
        let mut prev_name = "";
        for o in options.drain(..) {
            if o.0 == prev_name {
                continue;
            }
            prev_name = o.0;
            options_dedup.push(o);
        }
        options_dedup
    }

    fn new(tt_size_mb: Option<usize>) -> Engine<S, T> {
        const START_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1";
        let tt = Arc::new(S::HashTable::new(tt_size_mb));
        let started_at = SystemTime::now();
        Engine {
            tt: tt.clone(),
            position: S::SearchNode::from_history(START_FEN, &mut vec![].into_iter()).ok().unwrap(),
            searcher: S::new(tt),
            queue: VecDeque::new(),
            started_at: started_at,
            status: SearchStatus { done: true, ..Default::default() },
            best_line: vec![],
            nps_stats: (0, 0, 0),
            silent_since: started_at,
            is_pondering: false,
            play_when: PlayWhen::Never(PhantomData),
        }
    }

    fn set_option(&mut self, name: &str, value: &str) {
        match name {
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
        T::set_option(name, value);
    }

    fn new_game(&mut self) {
        self.tt.clear();
    }

    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if let Ok(p) = S::SearchNode::from_history(fen, moves) {
            self.position = p;
        }
    }

    fn go(&mut self, params: &GoParams) {
        self.terminate();

        // Validate `params.searchmoves`.
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
        let depth = params.depth.map_or(DEPTH_MAX, |x| min(x, DEPTH_MAX as u64) as Depth);
        let remaining_time = RemainingTime {
            white_millis: params.wtime.unwrap_or(300_000),
            black_millis: params.btime.unwrap_or(300_000),
            winc_millis: params.winc.unwrap_or(0),
            binc_millis: params.binc.unwrap_or(0),
            movestogo: match params.movestogo {
                Some(0) => None, // Zero moves to go is a nonsense.
                x => x,
            },
        };
        self.tt.new_search();
        self.started_at = SystemTime::now();
        self.status = Default::default();
        self.best_line = vec![];
        self.nps_stats = (self.nps_stats.0, 0, 0);
        self.silent_since = self.started_at;
        self.is_pondering = params.ponder;
        self.play_when = if params.infinite {
            PlayWhen::Never(PhantomData)
        } else if params.movetime.is_some() {
            PlayWhen::MoveTime(params.movetime.unwrap())
        } else if params.nodes.is_some() {
            PlayWhen::Nodes(params.nodes.unwrap())
        } else if params.depth.is_some() {
            PlayWhen::Depth(depth)
        } else if params.mate.is_some() {
            PlayWhen::Mate(min(params.mate.unwrap(), (DEPTH_MAX + 1) as u64 / 2) as i16)
        } else {
            PlayWhen::TimeManagement(T::new(&self.position, &remaining_time))
        };
        self.searcher.start_search(SearchParams {
            search_id: 0,
            position: self.position.clone(),
            depth: depth,
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
                PlayWhen::TimeManagement(_) => self.status.done,
                PlayWhen::MoveTime(t) => self.status.done || self.status.duration_millis >= t,
                PlayWhen::Nodes(n) => self.status.done || self.status.searched_nodes >= n,
                PlayWhen::Depth(d) => self.status.done || self.status.depth >= d,
                PlayWhen::Mate(m) => self.status.done || self.status.value > VALUE_MAX - 2 * m,
                PlayWhen::Never(_) => false,
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

impl<S, T> Engine<S, T>
    where S: DeepeningSearch<ReportData = Vec<Variation>>,
          T: TimeManager<S>
{
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
        fn suffix(bound: BoundType) -> &'static str {
            match bound {
                BOUND_UPPER => " upperbound",
                BOUND_LOWER => " lowerbound",
                BOUND_EXACT => "",
                _ => panic!("unexpected bound type"),
            }
        }

        let SearchStatus { ref depth, ref searched_nodes, ref duration_millis, .. } = self.status;
        for (i, &Variation { ref moves, value, bound }) in variations.iter().enumerate() {
            let score = match value {
                v if bound & BOUND_UPPER != 0 && VALUE_MIN < v && v < VALUE_EVAL_MIN => {
                    format!("mate {}", (VALUE_MIN - v - 1) / 2)
                }
                v if bound & BOUND_LOWER != 0 && VALUE_EVAL_MAX < v && v < VALUE_MAX => {
                    format!("mate {}", (VALUE_MAX - v + 1) / 2)
                }
                v if v <= -9999 => format!("cp -9999{}", suffix(bound | BOUND_LOWER)),
                v if v >= 9999 => format!("cp 9999{}", suffix(bound | BOUND_UPPER)),
                v => format!("cp {}{}", v, suffix(bound)),
            };
            let mut pv = String::new();
            for m in moves.iter().take(max(0, *depth) as usize) {
                pv.push_str(&m.notation());
                pv.push(' ');
            }
            self.queue.push_back(EngineReply::Info(vec![
                InfoItem { info_type: "depth".to_string(), data: format!("{}", depth) },
                InfoItem { info_type: "multipv".to_string(), data: format!("{}", i + 1) },
                InfoItem { info_type: "score".to_string(), data: score },
                InfoItem { info_type: "time".to_string(), data: format!("{}", duration_millis) },
                InfoItem { info_type: "nodes".to_string(), data: format!("{}", searched_nodes) },
                InfoItem { info_type: "nps".to_string(), data: format!("{}", self.nps_stats.0) },
                InfoItem { info_type: "pv".to_string(), data: pv },
            ]));
        }
    }

    fn queue_best_move(&mut self) {
        let mut best_line = &self.tt.extract_pv(&self.position).moves;
        if best_line.is_empty() {
            // We prefer to get the best line of play directly from
            // the transposition table, but if for some reason it is
            // empty, we fall back to using the stored one.
            best_line = &self.best_line;
        };
        let best_move = if let Some(m) = best_line.get(0) {
            m.notation()
        } else {
            // If we still do not have a best move, we pick the first legal one.
            self.position.legal_moves().get(0).map_or("0000".to_string(), |m| m.notation())
        };
        self.queue.push_back(EngineReply::BestMove {
            best_move: best_move,
            ponder_move: best_line.get(1).map(|m| m.notation()),
        });
    }

    fn terminate(&mut self) {
        self.searcher.send_message("TERMINATE");
        while !self.status.done {
            self.wait_status_update(Duration::from_millis(1000));
        }
    }

    fn wait_status_update(&mut self, duration: Duration) {
        let mut received_report = false;
        self.searcher.wait_report(duration);
        while let Ok(r) = self.searcher.try_recv_report() {
            received_report = true;
            self.process_report(&r);
            self.inform_time_manager(Some(&r));
        }
        if !received_report && !self.status.done {
            self.inform_time_manager(None);
        }
    }

    fn inform_time_manager(&mut self, report: Option<&SearchReport<Vec<Variation>>>) {
        if let PlayWhen::TimeManagement(ref mut tm) = self.play_when {
            if tm.must_play(&mut self.searcher, report) && !self.is_pondering {
                self.searcher.send_message("TERMINATE");
            }
        }
    }

    fn process_report(&mut self, report: &SearchReport<Vec<Variation>>) {
        assert!(!self.status.done);
        assert!(report.depth >= self.status.depth);
        assert!(report.searched_nodes >= self.status.searched_nodes);
        let zero_millis = Duration::from_millis(0);
        let duration_millis = {
            let d = self.started_at.elapsed().unwrap_or(zero_millis);
            1000 * d.as_secs() + (d.subsec_nanos() / 1_000_000) as u64
        };
        self.status = SearchStatus {
            done: report.done,
            depth: report.depth,
            value: report.value,
            searched_nodes: report.searched_nodes,
            duration_millis: duration_millis,
        };

        // Update `self.nps_stats` every 1000 milliseconds.
        let elapsed_millis = duration_millis - self.nps_stats.2;
        if elapsed_millis >= 1000 {
            let nodes = report.searched_nodes - self.nps_stats.1;
            self.nps_stats = (1000 * nodes / elapsed_millis, report.searched_nodes, duration_millis)
        }

        // If principal variations are provided with the report, show them.
        if !report.data.is_empty() {
            self.best_line = report.data[0].moves.clone();
            self.queue_pv(&report.data);
            self.silent_since = SystemTime::now();
        }

        // If nothing has happened for a while, show progress info.
        if self.silent_since.elapsed().unwrap_or(zero_millis).as_secs() > 10 {
            self.queue_progress_info();
            self.silent_since = SystemTime::now();
        }
    }
}


/// Runs a UCI protocol server.
///
/// "Universal Chess Interface" (UCI) is an open protocol for chess
/// engines to communicate with other programs including Graphical
/// User Interfaces (GUI). The protocol is independent of the
/// operating system. For "Windows", the engine is a normal "exe"
/// file, either a console or "real" windows application. All
/// communication is done via standard input and output with text
/// commands.
///
/// # Parameters:
///
/// * `name` gives the name of the engine.
///
/// * `author` gives the name of the author.
///
/// # Type parameters:
///
/// * `S` implements game tree searching with iterative deepening. If
///   principal variations are included in the progress reports from
///   the search, they will be forwarded to the GUI, and eventually
///   used to determine the best move.
///
///   **Note:** Normally, principal variations (PV) should be sent
///   only when a new search depth is reached, and possibly when a new
///   best move is found. Therefore, the majority of the progress
///   reports will carry an empty `Vec<Variation>` instance. In
///   multi-PV mode the first slot of the vector is for the best
///   variation, the second slot is for the second-best variation, and
///   so forth.
///
/// * `T` is responsible for managing engine's thinking time.
pub fn run_uci<S, T>(name: &'static str, author: &'static str) -> !
    where S: DeepeningSearch<ReportData = Vec<Variation>>,
          T: TimeManager<S>
{
    // Set engine's identity.
    {
        let mut engine = ENGINE.lock().unwrap();
        assert!(engine.is_none(), "two engines can not run in parallel");
        *engine = Some(EngineIdentity {
            name: name,
            author: author,
        });
    }

    // Run the engine.
    process::exit(match run_engine::<Engine<S, T>>() {
        Ok(_) => 0,
        Err(_) => 1,
    });
}

struct EngineIdentity {
    name: &'static str,
    author: &'static str,
}

lazy_static! {
    static ref ENGINE: Mutex<Option<EngineIdentity>> = Mutex::new(None);
}
