pub mod search;

use std::thread;
use std::cmp::min;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver};
use std::time::SystemTime;
use basetypes::*;
use uci::{UciEngine, UciEngineFactory, EngineReply, OptionName, OptionDescription};
use position::Position;
use chess_move::*;
use tt::TranspositionTable;

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
    pondering_is_allowed: bool,
    is_thinking: bool,
    is_pondering: bool,
    stop_when: TimeManagement,
    replies: Vec<EngineReply>,
    tt: Arc<TranspositionTable>,
    commands: Sender<search::Command>,
    reports: Receiver<search::Report>,
    started_thinking_at: SystemTime,
    searched_nodes: NodeCount,
    searched_time: u64,
    searched_depth: u8,
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
            pondering_is_allowed: false,
            is_thinking: false,
            is_pondering: false,
            stop_when: TimeManagement::Infinite,
            replies: vec![],
            tt: tt,
            commands: commands_tx,
            reports: reports_rx,
            started_thinking_at: SystemTime::now(),
            searched_nodes: 0,
            searched_time: 0,
            searched_depth: 0,
        }
    }
}


impl UciEngine for Engine {
    fn set_option(&mut self, name: &str, value: &str) {
        match name {
            // Tells the engine that it will be allowed to ponder.
            // This option is needed because the engine might change
            // its time management algorithm when pondering is
            // allowed.
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
            // Note: We do not support the "searchmoves" parameter.

            let (time, inc) = if self.position.board().to_move() == WHITE {
                (wtime, winc.unwrap_or(0))
            } else {
                (btime, binc.unwrap_or(0))
            };
            self.is_thinking = true;
            self.is_pondering = ponder;
            self.stop_when = if infinite {
                TimeManagement::Infinite
            } else if movetime.is_some() {
                TimeManagement::MoveTime(movetime.unwrap())
            } else if nodes.is_some() {
                TimeManagement::Nodes(nodes.unwrap())
            } else if depth.is_some() {
                TimeManagement::Depth(min(depth.unwrap(), MAX_DEPTH as u64) as u8)
            } else {
                // TODO: Robert Hyatt proposes this:
                //
                // nMoves =  min( numberOfMovesOutOfBook, 10 )
                // factor = 2 - nMoves / 10
                // target = timeLeft / numberOfMovesUntilNextTimeControl
                // time   = factor * target
                let time = time.unwrap_or(0);
                let movestogo = movestogo.unwrap_or(40);
                let movetime = (time + inc * movestogo) / movestogo;
                TimeManagement::MoveTimeHint(min(movetime, time / 2))
            };
            self.started_thinking_at = SystemTime::now();
            self.searched_nodes = 0;
            self.searched_time = 0;
            self.searched_depth = 0;

            self.commands
                .send(search::Command::Search {
                    search_id: 0,
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
        // TODO: implement panic mode for MoveTimeHint management.
        if self.is_thinking && !self.is_pondering {
            if match self.stop_when {
                TimeManagement::MoveTime(t) => self.searched_time >= t,
                TimeManagement::MoveTimeHint(t) => self.searched_time >= t,
                TimeManagement::Nodes(n) => self.searched_nodes >= n,
                TimeManagement::Depth(d) => self.searched_depth >= d,
                TimeManagement::Infinite => false,
            } {
                self.stop();
            }
        }

        // Empty the reports queue.
        while let Ok(report) = self.reports.try_recv() {
            // let rep_type = match report {
            //     search::Report::Progress { searched_nodes, .. } => format!("progress {}", searched_nodes),
            //     search::Report::Done { .. } => "done".to_string(),
            // };
            // self.replies.push(EngineReply::Info(vec![
            //     ("string".to_string(), rep_type),
            // ]));
            if self.is_thinking {
                match report {
                    search::Report::Progress { searched_nodes, depth, .. } => {
                        self.searched_nodes = searched_nodes;
                        let duration = self.started_thinking_at.elapsed().unwrap();
                        self.searched_time = 1000 * duration.as_secs() +
                                             (duration.subsec_nanos() / 1000000) as u64;
                        if self.searched_depth < depth {
                            self.searched_depth = depth;
                            let mut p = self.position.clone();
                            let mut v = MoveStack::new();
                            let mut pv = String::from("");
                            let mut pv_length = 0;
                            let mut value = None;
                            'depthloop: for _ in 0..depth {
                                let m = match self.tt.probe(p.hash()) {
                                    Some(entry) => {
                                        if value.is_none() {
                                            value = Some(entry.value());
                                        }
                                        entry.move16()
                                    }
                                    None => 0,
                                };
                                if m != 0 {
                                    p.generate_moves(&mut v);
                                    while let Some(x) = v.pop() {
                                        if x.digest() == m && p.do_move(x) {
                                            pv_length += 1;
                                            pv.push_str(&x.notation());
                                            pv.push(' ');
                                            v.clear();
                                            continue 'depthloop;
                                        }
                                    }
                                }
                                break;
                            }
                            if searched_nodes > 0 && pv_length >= depth {
                                self.replies.push(EngineReply::Info(vec![
                                    ("depth".to_string(), format!("{}", depth)),
                                    ("score".to_string(), format!("cp {}", value.unwrap_or(666))),
                                    ("nodes".to_string(), format!("{}", searched_nodes)),
                                    ("time".to_string(), format!("{}", self.searched_time)),
                                    ("pv".to_string(), format!("{}", pv)),
                                ]));
                            }
                        }
                    }
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
