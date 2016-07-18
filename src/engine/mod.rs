pub mod search;

use std::thread;
use std::cmp::min;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver};
use std::time::SystemTime;
use std::num::Wrapping;
use basetypes::*;
use uci::{UciEngine, UciEngineFactory, EngineReply, OptionName, OptionDescription};
use position::Position;
use chess_move::*;
use tt::*;

// use rand;
// use rand::distributions::{Sample, Range};
// let mut rng = rand::thread_rng();
// let mut between = Range::new(0, 10);
// let x = between.sample(&mut rng);


const VERSION: &'static str = "0.1";
const MAX_DEPTH: u8 = 126;

/// Implements `UciEngine` trait.
pub struct Engine {
    position: Position,
    tt: Arc<TranspositionTable>,
    commands: Sender<search::Command>,
    reports: Receiver<search::Report>,
    replies: Vec<EngineReply>,
    
    // Tells the engine if it will be allowed to ponder. This option
    // is needed because the engine might change its time management
    // algorithm when pondering is allowed.
    pondering_is_allowed: bool,
    
    is_thinking: bool,
    is_pondering: bool,

    // Search status info
    search_id: usize,
    thinking_since: SystemTime,
    no_reports_since: SystemTime,
    current_depth: u8,
    searched_nodes: NodeCount,
    searched_time: u64, // milliseconds
    nps: u64,  // nodes per second
    mangled_pv: bool, // `true` if the primary variation is imperfect
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
        let tt = Arc::new(tt);
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        let tt2 = tt.clone();
        thread::spawn(move || {
            search::run_deepening(tt2, commands_rx, reports_tx);
        });

        Engine {
            position: Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 \
                                          1")
                          .ok()
                          .unwrap(),
            tt: tt,
            commands: commands_tx,
            reports: reports_rx,
            replies: vec![],
            pondering_is_allowed: false,
            is_thinking: false,
            is_pondering: false,
            search_id: 0,
            thinking_since: SystemTime::now(),
            no_reports_since: SystemTime::now(),
            current_depth: 0,
            searched_nodes: 0,
            searched_time: 0,
            nps: 0,
            mangled_pv: false,
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
            self.no_reports_since = SystemTime::now();
            self.current_depth = 0;
            self.searched_nodes = 0;
            self.searched_time = 0;
            self.mangled_pv = false;

            // Figure out when we should stop thinking.
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
                    lower_bound: -20000,
                    upper_bound: 20000,
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
            let best_move = self.get_best_move();
            self.replies.push(EngineReply::BestMove {
                best_move: best_move,
                ponder_move: None,
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
                    // Search progress.
                    search::Report::Progress { search_id, searched_nodes, depth }
                        if search_id == self.search_id => {

                        self.register_progress(depth, searched_nodes);
                        if self.no_reports_since.elapsed().unwrap().as_secs() > 20 {
                            if self.mangled_pv {
                                self.report_pv(depth - 1);
                            } else {
                                self.report_progress();
                            }
                        }
                    }

                    // Terminate the search (unless pondering of infinite).
                    search::Report::Done { search_id, .. } if search_id == self.search_id => {
                        if !self.is_pondering {
                            self.stop_when = if let TimeManagement::Infinite = self.stop_when {
                                TimeManagement::Infinite
                            } else {
                                TimeManagement::MoveTime(0)
                            };
                        }
                    }

                    // Stale reports from stopped searches.
                    _ => (),
                }
            }
        }

        if self.replies.len() > 0 {
            Some(self.replies.remove(0))
        } else {
            None
        }
    }
}


impl Engine {
    // A helper method.
    fn register_progress(&mut self, depth: u8, searched_nodes: NodeCount) {
        let thinking_duration = self.thinking_since.elapsed().unwrap();
        self.searched_nodes = searched_nodes;
        self.searched_time = 1000 * thinking_duration.as_secs() +
                             (thinking_duration.subsec_nanos() / 1000000) as u64;
        self.nps = 1000 * (self.nps + self.searched_nodes) / (1000 + self.searched_time);
        if self.current_depth < depth {
            self.current_depth = depth;
            self.report_pv(depth - 1);
            if !self.mangled_pv {
                self.report_progress();
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
        let mut value = -20000;
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
        self.mangled_pv = bound != BOUND_EXACT || pv.len() < depth as usize;

        // Send the extracted info to the GUI.
        let value_suffix = match bound {
            BOUND_EXACT => "",
            BOUND_UPPER => " upperbound",
            BOUND_LOWER => " lowerbound",
            _ => panic!("unexpected bound type"),
        };
        let mut pv_string = String::new();
        for m in pv {
            pv_string.push(' ');
            pv_string.push_str(&m.notation());
        }
        self.replies.push(EngineReply::Info(vec![
            ("depth".to_string(), format!("{}", depth)),
            ("score".to_string(), format!("cp {}{}", value, value_suffix)),
            ("time".to_string(), format!("{}", self.searched_time)),
            ("nodes".to_string(), format!("{}", self.searched_nodes)),
            ("nps".to_string(), format!("{}", self.nps)),
            ("pv".to_string(), pv_string),
        ]));
        self.no_reports_since = SystemTime::now();
    }

    // A helper method. It reports the depth, the node count, and
    // nodes per second to the GUI.
    fn report_progress(&mut self) {
        self.replies.push(EngineReply::Info(vec![
            ("depth".to_string(), format!("{}", self.current_depth)),
            ("nodes".to_string(), format!("{}", self.searched_nodes)),
            ("nps".to_string(), format!("{}", self.nps)),
        ]));
        self.no_reports_since = SystemTime::now();
    }

    fn get_best_move(&mut self) -> String {
        let mut m = match self.tt.probe(self.position.hash()) {
            Some(entry) => entry.move16(),
            None => 0,
        };
        if m == 0 {
            // Pick the first legal move.
            let mut first_legal_move = Move::invalid();
            let mut v = MoveStack::new();
            self.position.generate_moves(&mut v);
            while let Some(x) = v.pop() {
                if self.position.do_move(x) {
                    self.position.undo_move();
                    first_legal_move = x;
                    break;
                }
            }
            m = first_legal_move.digest();
        }
        if m != 0 {
            let move_type = move_type(m);
            let orig_square = orig_square(m);
            let dest_square = dest_square(m);
            let promoted_piece = match aux_data(m) {
                0 => "q",
                1 => "r",
                2 => "b",
                3 => "n",
                _ => panic!("invalid promoted piece code"),
            };
            format!("{}{}{}",
                    notation(orig_square),
                    notation(dest_square),
                    if move_type == MOVE_PROMOTION {
                        promoted_piece
                    } else {
                        ""
                    })

        } else {
            "0000".to_string()
        }
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
