#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod basetypes;
pub mod chess_move;
pub mod position;
pub mod search;
pub mod uci;
pub mod tt;
pub mod time_manager;

use std::process::exit;
use std::sync::Arc;
use std::time::SystemTime;
use std::collections::VecDeque;
use basetypes::NodeCount;
use tt::{Tt, BOUND_EXACT, BOUND_UPPER, BOUND_LOWER};
use position::Position;
use uci::{UciEngine, UciEngineFactory};
use search::{Variation, SearchStatus, SearchThread};
use time_manager::TimeManager;


/// The version of the program.
pub const VERSION: &'static str = "0.1";

// The name of the program.
const NAME: &'static str = "Alcibiades";

// The author of the program.
const AUTHOR: &'static str = "Evgeni Pandurski";

// The starting position in Forsyth–Edwards notation (FEN).
const STARTING_POSITION: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1";


// Represents a condition for terminating the search.
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
    position: Position,
    current_depth: u8,

    // `Engine` will hand over the real work to `SearchThread`.
    search_thread: SearchThread,

    // Tells the engine when it must stop thinking and play the best
    // move it has found.
    play_when: PlayWhen,

    // Tells the engine how many best lines to calculate and send to
    // the GUI (the first move in each best line is different). This
    // is the so called "MultiPV" mode.
    variation_count: usize,

    // Tells the engine if it will be allowed to ponder. This option
    // is needed because the engine might change its time management
    // algorithm when pondering is allowed.
    pondering_is_allowed: bool,

    // `true` if the engine is thinking in pondering mode.
    is_pondering: bool,

    // Marks the last time when a message was sent by the engine to
    // the GUI, or if no message has been sent yet -- the time when
    // the search stared.
    silent_since: SystemTime,

    // A queue for the messages send by the engine to the GUI.
    queue: VecDeque<uci::EngineReply>,
}


impl Engine {
    /// Creates a new instance.
    ///
    /// `tt_size_mb` is the preferred size of the transposition
    /// table in Mbytes.
    pub fn new(tt_size_mb: usize) -> Engine {
        let mut tt = Tt::new();
        tt.resize(tt_size_mb);
        let tt = Arc::new(tt);

        Engine {
            tt: tt.clone(),
            position: Position::from_fen(STARTING_POSITION).ok().unwrap(),
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

    // A helper method. It it adds a progress report message to
    // `self.queue`.
    fn queue_progress_report(&mut self) {
        let &SearchStatus { depth, searched_nodes, nps, .. } = self.search_thread.status();
        self.queue.push_back(uci::EngineReply::Info(vec![
            ("depth".to_string(), format!("{}", depth)),
            ("nodes".to_string(), format!("{}", searched_nodes)),
            ("nps".to_string(), format!("{}", nps)),
        ]));
        self.silent_since = SystemTime::now();
    }

    // A helper method. It it adds a message containing the current
    // (multi)PV to `self.queue`.
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
            self.queue.push_back(uci::EngineReply::Info(vec![
            ("depth".to_string(), format!("{}", depth)),
            ("multipv".to_string(), format!("{}", i + 1)),
            ("score".to_string(), format!("cp {}{}", value, bound_suffix)),
            ("time".to_string(), format!("{}", duration_millis)),
            ("nodes".to_string(), format!("{}", searched_nodes)),
            ("nps".to_string(), format!("{}", nps)),
            ("pv".to_string(), moves_string),
        ]));
        }
        self.silent_since = SystemTime::now();
    }

    // A helper method. It it adds a message containing the current
    // best move to `self.queue`.
    fn queue_best_move(&mut self) {
        let &SearchStatus { ref variations, .. } = self.search_thread.status();
        self.queue.push_back(uci::EngineReply::BestMove {
            best_move: variations[0].moves.get(0).map_or("0000".to_string(), |m| m.notation()),
            ponder_move: variations[0].moves.get(1).map(|m| m.notation()),
        });
        self.silent_since = SystemTime::now();
    }
}


impl UciEngine for Engine {
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

    fn new_game(&mut self) {
        self.tt.clear();
    }

    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if let Ok(p) = Position::from_history(fen, moves) {
            self.position = p;
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
        // Note: We ignore the "mate" parameter.

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

    fn get_reply(&mut self) -> Option<uci::EngineReply> {
        if !self.search_thread.status().done {
            // Update the search status.
            self.search_thread.update_status();
            let &SearchStatus { done, depth, searched_nodes, duration_millis, .. } =
                self.search_thread.status();
            
            // Send the (multi)PV when changed.
            if depth > self.current_depth {
                self.current_depth = depth;
                self.queue_pv();
            }

            // Send periodic progress reports.
            if self.silent_since.elapsed().unwrap().as_secs() > 10 {
                self.queue_progress_report();
            }

            // Register the updated search status with the time manager.
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

        self.queue.pop_front()
    }

    fn exit(&mut self) {
        self.search_thread.join();
    }
}


/// Implements `UciEngineFactory` trait.
pub struct EngineFactory;


impl UciEngineFactory<Engine> for EngineFactory {
    fn name(&self) -> String {
        format!("{} {}", NAME, VERSION)
    }

    fn author(&self) -> String {
        AUTHOR.to_string()
    }

    fn options(&self) -> Vec<(uci::OptionName, uci::OptionDescription)> {
        vec![
            // TODO: Calculate a sane limit for the hash size.
            ("Hash".to_string(), uci::OptionDescription::Spin { min: 1, max: 2048, default: 16 }),
            ("Ponder".to_string(), uci::OptionDescription::Check { default: false }),
            ("MultiPV".to_string(), uci::OptionDescription::Spin { min: 1, max: 500, default: 1 }),
        ]
    }

    fn create(&self, hash_size_mb: Option<usize>) -> Engine {
        Engine::new(hash_size_mb.unwrap_or(16))
    }
}


fn main() {
    if let Ok(mut uci_loop) = uci::Server::wait_for_hanshake(EngineFactory) {
        match uci_loop.serve() {
            Ok(_) => {
                exit(0);
            }
            Err(_) => {
                exit(1);
            }
        }
    }
    exit(2);
}
