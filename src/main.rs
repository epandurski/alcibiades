#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod basetypes;
pub mod chess_move;
pub mod position;
pub mod search;
pub mod uci;

use std::process::exit;
use std::sync::Arc;
use std::time::SystemTime;
use std::collections::VecDeque;
use basetypes::*;
use position::Position;
use uci::{UciEngine, UciEngineFactory, EngineReply, OptionName, OptionDescription};
use search::{TimeManagement, SearchStatus, MultipvSearch};
use search::tt::*;


// The version of the program.
const VERSION: &'static str = "0.1";

// The name of the program.
const NAME: &'static str = "Alcibiades";

// The author of the program.
const AUTHOR: &'static str = "Evgeni Pandurski";

// The starting position in Forsyth–Edwards notation (FEN).
const STARTING_POSITION: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1";

// Represents a condition for terminating the search.
enum PlayWhen {
    TimeManagement(TimeManagement), // Stop when `TimeManagement` says.
    MoveTime(u64), // Stop after the given number of milliseconds.
    Nodes(NodeCount), // Stop when the given number of nodes has been searched.
    Depth(u8), // Stop when the given search depth has been reached.
    Never, // An infinite search.
}


/// Implements `UciEngine` trait.
pub struct Engine {
    tt: Arc<TranspositionTable>,
    position: Position,
    current_depth: u8,
    current_value: Value,

    // `Engine` will hand over the real work to `MultipvSearch`.
    search: MultipvSearch,

    // Tells the engine when it must stop thinking and play the best
    // move it has found.
    play_when: PlayWhen,

    // Tells the engine how many best lines to calculate and send to
    // the GUI (the first move in each line is different). This is the
    // so called "MultiPV" mode.
    pv_count: usize,
    
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
    queue: VecDeque<EngineReply>,
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

        Engine {
            tt: tt.clone(),
            position: Position::from_fen(STARTING_POSITION).ok().unwrap(),
            current_depth: 0,
            current_value: VALUE_UNKNOWN,
            search: MultipvSearch::new(tt),
            play_when: PlayWhen::Never,
            pv_count: 1,
            pondering_is_allowed: false,
            is_pondering: false,
            silent_since: SystemTime::now(),
            queue: VecDeque::new(),
        }
    }

    // A helper method. It it adds a progress report message to
    // `self.queue`.
    fn queue_progress_report(&mut self) {
        let SearchStatus { depth, searched_nodes, nps, .. } = *self.search.status();
        self.queue.push_back(EngineReply::Info(vec![
            ("depth".to_string(), format!("{}", depth)),
            ("nodes".to_string(), format!("{}", searched_nodes)),
            ("nps".to_string(), format!("{}", nps)),
        ]));
        self.silent_since = SystemTime::now();
    }

    // A helper method. It it adds a message containing the current PV
    // to `self.queue`.
    fn queue_pv(&mut self) {
        let SearchStatus { depth, value, bound, ref pv, searched_time, searched_nodes, nps, .. } =
            *self.search.status();
        let score_suffix = match bound {
            BOUND_EXACT => "",
            BOUND_UPPER => " upperbound",
            BOUND_LOWER => " lowerbound",
            _ => panic!("unexpected bound type"),
        };
        let mut pv_string = String::new();
        for m in pv {
            pv_string.push_str(&m.notation());
            pv_string.push(' ');
        }
        self.queue.push_back(EngineReply::Info(vec![
            ("depth".to_string(), format!("{}", depth)),
            ("score".to_string(), format!("cp {}{}", value, score_suffix)),
            ("time".to_string(), format!("{}", searched_time)),
            ("nodes".to_string(), format!("{}", searched_nodes)),
            ("nps".to_string(), format!("{}", nps)),
            ("pv".to_string(), pv_string),
        ]));
        self.silent_since = SystemTime::now();
    }

    // A helper method. It it adds a message containing the current
    // best move to `self.queue`.
    fn queue_best_move(&mut self) {
        let SearchStatus { ref pv, .. } = *self.search.status();
        self.queue.push_back(EngineReply::BestMove {
            best_move: pv.get(0).map_or("0000".to_string(), |m| m.notation()),
            ponder_move: pv.get(1).map(|m| m.notation()),
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
                self.pv_count = value.parse::<usize>().unwrap_or(1);
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
        // NOTE: We ignore the "mate" parameter.

        self.search.stop();
        self.tt.new_search();
        self.current_depth = 0;
        self.current_value = VALUE_UNKNOWN;
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
            PlayWhen::TimeManagement(TimeManagement::new(self.position.board().to_move(),
                                                         self.pondering_is_allowed,
                                                         wtime,
                                                         btime,
                                                         winc,
                                                         binc,
                                                         movestogo))
        };
        self.silent_since = SystemTime::now();
        self.search.start(&self.position, searchmoves, self.pv_count);
    }

    fn ponder_hit(&mut self) {
        if self.search.status().done {
            self.queue_best_move();
        } else {
            self.is_pondering = false;
        }
    }

    fn stop(&mut self) {
        self.search.stop();
        self.queue_best_move();
    }

    fn get_reply(&mut self) -> Option<EngineReply> {
        if !self.search.status().done {
            let SearchStatus { done, depth, value, searched_nodes, searched_time, .. } =
                *self.search.update_status();

            // Send the PV when changed.
            if depth > 0 && (depth > self.current_depth || value != self.current_value) {
                self.current_depth = depth;
                self.current_value = value;
                self.queue_pv();
            }

            // Send periodic progress reports.
            if self.silent_since.elapsed().unwrap().as_secs() > 10 {
                self.queue_progress_report();
            }

            // Check if we must play now.
            if !self.is_pondering &&
               match self.play_when {
                PlayWhen::TimeManagement(ref tm) => done || tm.must_play(self.search.status()),
                PlayWhen::MoveTime(t) => done || searched_time >= t,
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
        self.search.exit();
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

    fn options(&self) -> Vec<(OptionName, OptionDescription)> {
        vec![
            // TODO: Calculate a sane limit for the hash size.
            ("Hash".to_string(), OptionDescription::Spin { min: 1, max: 2048, default: 16 }),
            ("Ponder".to_string(), OptionDescription::Check { default: false }),
            ("MultiPV".to_string(), OptionDescription::Spin { min: 1, max: 500, default: 1 }),
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
