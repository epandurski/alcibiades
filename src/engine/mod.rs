pub mod search;

use std::thread;
use std::cmp::min;
use std::collections::VecDeque;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver};
use std::time::SystemTime;
use std::num::Wrapping;
use basetypes::*;
use uci::{UciEngine, UciEngineFactory, EngineReply, OptionName, OptionDescription};
use position::Position;
use chess_move::*;
use tt::*;


const VERSION: &'static str = "0.1";
const MAX_DEPTH: u8 = 126;


/// Implements `UciEngine` trait.
pub struct Engine {
    tt: Arc<TranspositionTable>,
    reply_queue: VecDeque<EngineReply>,
    position: Position,
    is_thinking: bool,
    is_pondering: bool,

    // Tells the engine if it will be allowed to ponder. This option
    // is needed because the engine might change its time management
    // algorithm when pondering is allowed.
    pondering_is_allowed: bool,

    // A channel for sending commands to the search thread.
    commands: Sender<search::Command>,

    // A channel for receiving reports from the search thread.
    reports: Receiver<search::Report>,

    // Search status info
    search_id: usize,
    thinking_since: SystemTime,
    silent_since: SystemTime,
    current_depth: u8,
    searched_nodes: NodeCount,
    searched_time: u64, // milliseconds
    nps: u64, // nodes per second
    mangled_pv: bool, // `true` if the primary variation is imperfect
    last_pv: Vec<Move>,
    last_pv_value: Value,
    last_pv_bound: BoundType,
    stop_when: TimeManagement,
}


impl Engine {
    /// Creates a new instance.
    ///
    /// `tt_size_mb` is the preferred size of the transposition
    /// table in Mbytes.
    pub fn new(tt_size_mb: usize) -> Engine {
        let mut tt = TranspositionTable::new();
        tt.resize(tt_size_mb);
        let tt1 = Arc::new(tt);
        let tt2 = tt1.clone();
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        thread::spawn(move || {
            search::run_deepening(tt2, commands_rx, reports_tx);
        });

        Engine {
            tt: tt1,
            reply_queue: VecDeque::new(),
            position: Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 \
                                          1")
                          .ok()
                          .unwrap(),
            is_thinking: false,
            is_pondering: false,
            pondering_is_allowed: false,
            commands: commands_tx,
            reports: reports_rx,
            search_id: 0,
            thinking_since: SystemTime::now(),
            silent_since: SystemTime::now(),
            current_depth: 0,
            searched_nodes: 0,
            searched_time: 0,
            nps: 0,
            mangled_pv: false,
            last_pv: vec![],
            last_pv_value: -30000,
            last_pv_bound: BOUND_LOWER,
            stop_when: TimeManagement::Infinite,
        }
    }
}


impl UciEngine for Engine {
    fn set_option(&mut self, name: &str, value: &str) {
        match name {
            "Ponder" => {
                self.pondering_is_allowed = value == "true";
            }

            // An invalid option. Notice that we do not support
            // re-sizing of the transposition table once the engine
            // had started.
            _ => (),
        }
    }

    fn new_game(&mut self) {
        // Clearing the transposition table would not change anything,
        // so we do nothing.
    }

    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if !self.is_thinking {
            if let Ok(p) = Position::from_history(fen, moves) {
                self.position = p;
                self.tt.new_search();
            }
        }
    }

    #[allow(unused_variables)]
    fn go(&mut self,
          searchmoves: Option<Vec<String>>,
          ponder: bool,
          wtime: Option<u64>,
          btime: Option<u64>,
          winc: Option<u64>,
          binc: Option<u64>,
          movestogo: Option<u64>,
          depth: Option<u64>,
          nodes: Option<u64>,
          mate: Option<u64>,
          movetime: Option<u64>,
          infinite: bool) {
        if !self.is_thinking {
            // Note: We ignore "searchmoves" and "mate" parameters.

            self.is_thinking = true;
            self.is_pondering = ponder;
            self.search_id = (Wrapping(self.search_id) + Wrapping(1)).0;
            self.thinking_since = SystemTime::now();
            self.silent_since = SystemTime::now();
            self.current_depth = 0;
            self.searched_nodes = 0;
            self.searched_time = 0;
            self.mangled_pv = false;
            self.last_pv = vec![];
            self.last_pv_value = -30000;
            self.last_pv_bound = BOUND_LOWER;

            // Figure out when we should stop thinking.
            //
            // TODO: Implement smarter time management.
            let (time, inc) = if self.position.board().to_move() == WHITE {
                (wtime, winc.unwrap_or(0))
            } else {
                (btime, binc.unwrap_or(0))
            };
            self.stop_when = if infinite {
                TimeManagement::Infinite
            } else if movetime.is_some() {
                TimeManagement::MoveTime(movetime.unwrap())
            } else if nodes.is_some() {
                TimeManagement::Nodes(nodes.unwrap())
            } else if depth.is_some() {
                TimeManagement::Depth(depth.unwrap() as u8)
            } else {
                let time = time.unwrap_or(0);
                let movestogo = movestogo.unwrap_or(40);
                let movetime = (time + inc * movestogo) / movestogo;
                TimeManagement::MoveTimeHint(min(movetime, time / 2))
            };

            // Start deepening search to the maximum possible depth.
            self.commands
                .send(search::Command::Search {
                    search_id: self.search_id,
                    position: self.position.clone(),
                    depth: MAX_DEPTH,
                    lower_bound: -30000,
                    upper_bound: 30000,
                })
                .unwrap();
        }
    }

    fn ponder_hit(&mut self) {
        if self.is_thinking && self.is_pondering {
            self.is_pondering = false;
        }
    }

    fn stop(&mut self) {
        if self.is_thinking {
            self.commands.send(search::Command::Stop).unwrap();
            let mut p = self.position.clone();
            let (best_move, ponder_move) = if let Some(m) = self.do_best_move(&mut p) {
                (m.notation(), self.do_best_move(&mut p).map(|m| m.notation()))
            } else {
                ("0000".to_string(), None)
            };
            self.reply_queue.push_back(EngineReply::BestMove {
                best_move: best_move,
                ponder_move: ponder_move,
            });
            self.is_thinking = false;
        }
    }

    fn is_thinking(&self) -> bool {
        self.is_thinking
    }

    fn get_reply(&mut self) -> Option<EngineReply> {
        // Check if we should stop thinking.
        if self.is_thinking && !self.is_pondering {
            if match self.stop_when {
                TimeManagement::MoveTime(t) => self.searched_time >= t,
                TimeManagement::MoveTimeHint(t) => self.searched_time >= t,
                TimeManagement::Nodes(n) => self.searched_nodes >= n,
                TimeManagement::Depth(d) => self.current_depth > d,
                TimeManagement::Infinite => false,
            } {
                self.stop();
            }
        }

        // Check the reports queue.
        while let Ok(report) = self.reports.try_recv() {
            if self.is_thinking {
                match report {
                    search::Report::Progress { search_id, depth, searched_nodes }
                        if search_id == self.search_id => {
                        // Register search progress.
                        self.register_progress(depth, searched_nodes);
                        if self.silent_since.elapsed().unwrap().as_secs() > 20 {
                            if self.mangled_pv {
                                self.report_pv(depth - 1);
                            } else {
                                self.report_progress(depth);
                            }
                        }
                    }
                    search::Report::Done { search_id, .. } if search_id == self.search_id => {
                        // Terminate the search if not infinite.
                        self.stop_when = match self.stop_when {
                            TimeManagement::Infinite => TimeManagement::Infinite,
                            _ => TimeManagement::MoveTime(0),
                        };
                    }
                    // We may still receive stale reports from already
                    // stopped searches.
                    _ => (),
                }
            }
        }

        self.reply_queue.pop_front()
    }
}


impl Engine {
    // A helper method. It updates the search status info and makes
    // sure that a new PV is sent to the GUI for every newly reached
    // depth.
    fn register_progress(&mut self, depth: u8, searched_nodes: NodeCount) {
        let thinking_duration = self.thinking_since.elapsed().unwrap();
        self.searched_time = 1000 * thinking_duration.as_secs() +
                             (thinking_duration.subsec_nanos() / 1000000) as u64;
        self.searched_nodes = searched_nodes;
        self.nps = 1000 * (self.nps + self.searched_nodes) / (1000 + self.searched_time);
        if self.current_depth < depth {
            self.current_depth = depth;
            self.report_pv(depth - 1);
            if !self.mangled_pv {
                self.report_progress(depth);
            }
        }
    }

    // A helper method. It extracts the primary variation (PV) from
    // the transposition table (TT) and sends it to the GUI.
    fn report_pv(&mut self, depth: u8) {
        let mut prev_move = None;
        let mut p = self.position.clone();

        // Extract the PV, the value, and the bound from the TT.
        let mut pv = Vec::new();
        let mut value = -30000;
        let mut bound = BOUND_LOWER;
        while let Some(entry) = self.tt.probe(p.hash()) {
            if pv.len() < depth as usize && entry.bound() != BOUND_NONE {
                if let Some(m) = prev_move {
                    pv.push(m);
                }
                if pv.len() & 1 == 0 {
                    value = entry.value();
                    bound = entry.bound();
                } else {
                    // In this case the value stored in `entry` is
                    // from other side's perspective.
                    value = -entry.value();
                    bound = match entry.bound() {
                        BOUND_UPPER => BOUND_LOWER,
                        BOUND_LOWER => BOUND_UPPER,
                        x => x,
                    };
                }
                if bound == BOUND_EXACT {
                    if let Some(m) = p.try_move_digest(entry.move16()) {
                        if p.do_move(m) && !p.is_repeated() {
                            // Try to extend the PV.
                            prev_move = Some(m);
                            continue;
                        }
                    }
                }
            }
            break;
        }

        // Check if the extracted PV is imperfect.
        self.mangled_pv = bound != BOUND_EXACT || pv.len() < depth as usize;

        if self.last_pv == pv && self.last_pv_value == value && self.last_pv_bound == bound {
            // The newly extracted PV is the same as the last
            // one. There is no point in sending it again, so we send
            // a progress report instead.
            self.report_progress(depth);
        } else {
            // Send the newly extracted PV to the GUI.
            let value_suffix = match bound {
                BOUND_EXACT => "",
                BOUND_UPPER => " upperbound",
                BOUND_LOWER => " lowerbound",
                _ => panic!("unexpected bound type"),
            };
            let mut pv_string = String::new();
            for m in &pv {
                pv_string.push(' ');
                pv_string.push_str(&m.notation());
            }
            self.reply_queue.push_back(EngineReply::Info(vec![
                ("depth".to_string(), format!("{}", depth)),
                ("score".to_string(), format!("cp {}{}", value, value_suffix)),
                ("time".to_string(), format!("{}", self.searched_time)),
                ("nodes".to_string(), format!("{}", self.searched_nodes)),
                ("nps".to_string(), format!("{}", self.nps)),
                ("pv".to_string(), pv_string),
            ]));
            self.last_pv = pv;
            self.last_pv_value = value;
            self.last_pv_bound = bound;
        }
    }

    // A helper method. It reports the current depth, the searched
    // node count, and the nodes per second to the GUI.
    fn report_progress(&mut self, depth: u8) {
        self.reply_queue.push_back(EngineReply::Info(vec![
            ("depth".to_string(), format!("{}", depth)),
            ("nodes".to_string(), format!("{}", self.searched_nodes)),
            ("nps".to_string(), format!("{}", self.nps)),
        ]));
        self.silent_since = SystemTime::now();
    }

    // A helper method for `Engine::stop`. It probes the TT for the
    // best move in the position `p`, plays it, and returns it. If the
    // TT gives no move this function will play and return the first
    // legal move. `None` is returned only when there are no legal
    // moves.
    fn do_best_move(&self, p: &mut Position) -> Option<Move> {
        // Try to get best move from the TT.
        let move16 = self.tt.probe(p.hash()).map_or(0, |entry| entry.move16());
        if let Some(m) = p.try_move_digest(move16) {
            if p.do_move(m) {
                return Some(m);
            }
        }

        // Otherwise, pick the first legal move.
        let mut s = MoveStack::new();
        p.generate_moves(&mut s);
        while let Some(m) = s.pop() {
            if p.do_move(m) {
                return Some(m);
            }
        }

        // No legal moves.
        None
    }
}


/// Implements `UciEngineFactory` trait.
pub struct EngineFactory;


impl UciEngineFactory<Engine> for EngineFactory {
    fn name(&self) -> String {
        format!("Alcibiades {}", VERSION)
    }

    fn author(&self) -> String {
        "Evgeni Pandurski".to_string()
    }

    fn options(&self) -> Vec<(OptionName, OptionDescription)> {
        vec![
            // TODO: Calculate a sane limit for the hash size.
            ("Hash".to_string(), OptionDescription::Spin { min: 1, max: 2048, default: 16 }),
            ("Ponder".to_string(), OptionDescription::Check { default: false }),
        ]
    }

    fn create(&self, hash_size_mb: Option<usize>) -> Engine {
        Engine::new(hash_size_mb.unwrap_or(16))
    }
}


enum TimeManagement {
    MoveTime(u64),
    MoveTimeHint(u64),
    Nodes(NodeCount),
    Depth(u8),
    Infinite,
}
