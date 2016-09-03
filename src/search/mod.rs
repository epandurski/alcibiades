//! Implements the principal game engine functionality.

pub mod tt;
pub mod alpha_beta;
pub mod threading;

use std::thread;
use std::cmp::min;
use std::collections::VecDeque;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver};
use std::time::SystemTime;
use basetypes::*;
use chess_move::*;
use position::Position;
use uci::{UciEngine, UciEngineFactory, EngineReply, OptionName, OptionDescription};
use self::tt::*;
use self::threading::*;


/// The maximum search depth in half-moves.
pub const MAX_DEPTH: u8 = 63; // Should be less than 127.

/// The half-with of the initial aspiration window in centipawns.
pub const INITIAL_ASPIRATION_WINDOW: Value = 17; // 16;

/// The number of nodes that will be searched without reporting search
/// progress.
///
/// If this value is too small the engine may become slow, if this
/// value is too big the engine may become unresponsive.
pub const NODE_COUNT_REPORT_INTERVAL: NodeCount = 10000;


struct TimeManagement {
    move_time: u64, // milliseconds
}

impl TimeManagement {
    #[allow(unused_variables)]
    pub fn new(us: Color,
               pondering_is_allowed: bool,
               wtime: Option<u64>,
               btime: Option<u64>,
               winc: Option<u64>,
               binc: Option<u64>,
               movestogo: Option<u64>)
               -> TimeManagement {
        // TODO: We ignore "pondering_is_allowed".

        let (time, inc) = if us == WHITE {
            (wtime, winc.unwrap_or(0))
        } else {
            (btime, binc.unwrap_or(0))
        };
        let time = time.unwrap_or(0);
        let movestogo = movestogo.unwrap_or(40);
        let movetime = (time + inc * movestogo) / movestogo;
        TimeManagement { move_time: min(movetime, time / 2) }
    }

    pub fn must_play(&self, search_status: &SearchStatus) -> bool {
        // TODO: Implement smarter time management.
        search_status.searched_time >= self.move_time
    }
}


struct SearchStatus {
    pub started_at: Option<SystemTime>,
    pub done: bool,
    pub depth: u8,
    pub value: Value,
    pub bound: BoundType,
    pub pv: Vec<Move>,
    pub searched_nodes: NodeCount,
    pub searched_time: u64, // milliseconds
    pub nps: u64, // nodes per second
}


/// Implements `UciEngine` trait.
pub struct MultipvSearch {
    tt: Arc<TranspositionTable>,
    position: Position,
    status: SearchStatus,

    // A handle to the search thread.
    search_thread: Option<thread::JoinHandle<()>>,

    // A channel for sending commands to the search thread.
    commands: Sender<Command>,

    // A channel for receiving reports from the search thread.
    reports: Receiver<Report>,
}


impl MultipvSearch {
    /// Creates a new instance.
    pub fn new(tt: Arc<TranspositionTable>) -> MultipvSearch {

        // Spawn the search thread.
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        let tt_clone = tt.clone();
        let search_thread = thread::spawn(move || {
            serve_deepening(tt_clone, commands_rx, reports_tx);
        });

        MultipvSearch {
            tt: tt,
            search_thread: Some(search_thread),
            position: Position::from_fen("k7/8/8/8/8/8/8/7K w - - 0 1").ok().unwrap(),
            commands: commands_tx,
            reports: reports_rx,
            status: SearchStatus {
                started_at: None,
                done: true,
                depth: 0,
                value: VALUE_UNKNOWN,
                bound: BOUND_NONE,
                pv: vec![],
                searched_nodes: 0,
                searched_time: 0,
                nps: 0,
            },
        }
    }

    /// Starts a new search.
    pub fn start(&mut self, p: &Position) {
        self.stop();
        self.position = p.clone();
        self.status = SearchStatus {
            started_at: Some(SystemTime::now()),
            done: false,
            depth: 0,
            value: VALUE_UNKNOWN,
            bound: BOUND_NONE,
            pv: vec![],
            searched_nodes: 0,
            searched_time: 0,
            ..self.status
        };
        self.commands
            .send(Command::Search {
                search_id: 0,
                position: p.clone(),
                depth: MAX_DEPTH,
                lower_bound: -29999,
                upper_bound: 29999,
            })
            .unwrap();
    }

    /// Stops the current search.
    pub fn stop(&mut self) {
        if !self.status.done {
            self.commands.send(Command::Stop).unwrap();
            loop {
                if let Ok(Report::Done { .. }) = self.reports.recv() {
                    break;
                }
            }
            self.status.done = true;
        }
    }

    /// Stops the current search and joins the search thread.
    ///
    /// After calling `exit`, no other methods on this instance should
    /// be called.
    fn exit(&mut self) {
        self.commands.send(Command::Exit).unwrap();
        self.search_thread.take().unwrap().join().unwrap();
    }

    #[inline(always)]
    fn status(&self) -> &SearchStatus {
        &self.status
    }

    fn update_status(&mut self) -> &SearchStatus {
        while let Ok(report) = self.reports.try_recv() {
            match report {
                Report::Progress { searched_depth, searched_nodes, value, .. } => {
                    self.register_progress(searched_depth, searched_nodes, value);
                }
                Report::Done { .. } => {
                    self.status.done = true;
                }
            }
        }
        &self.status
    }

    // A helper method. It updates the search status info and makes
    // sure that a new PV is sent to the GUI for each newly reached
    // depth.
    fn register_progress(&mut self, depth: u8, searched_nodes: NodeCount, value: Value) {
        let thinking_duration = self.status.started_at.unwrap().elapsed().unwrap();
        self.status.searched_time = 1000 * thinking_duration.as_secs() +
                                    (thinking_duration.subsec_nanos() / 1000000) as u64;
        self.status.searched_nodes = searched_nodes;
        self.status.nps = 1000 * (self.status.nps + self.status.searched_nodes) /
                          (1000 + self.status.searched_time);
        if self.status.depth < depth || self.status.value != value {
            self.status.depth = depth;
            self.status.value = value;
            self.extract_pv(depth);
        }
    }

    // A helper method. It extracts the primary variation (PV) from
    // the transposition table (TT) and updates `status.current_pv`.
    //
    // **Note:** Because the PV is a moving target (the search
    // continues to run in parallel), imperfections in the reported
    // PVs are unavoidable. To deal with this, we turn a blind eye if
    // the value at the root of the PV differs from the value at the
    // leaf by no more than `EPSILON`.
    fn extract_pv(&mut self, depth: u8) {
        if depth == 0 {
            return;
        }

        // First: Extract the PV, the leaf value, the root value, and
        // the bound type from the TT.
        let mut p = self.position.clone();
        let mut our_turn = true;
        let mut prev_move = None;
        let mut pv = Vec::new();
        let mut leaf_value = -19999;
        let mut root_value = leaf_value;
        let mut bound = BOUND_LOWER;
        while let Some(entry) = self.tt.peek(p.hash()) {
            if entry.bound() != BOUND_NONE {
                // Get the value and the bound type. In half of the
                // cases the value stored in `entry` is from other
                // side's perspective.
                if our_turn {
                    leaf_value = entry.value();
                    bound = entry.bound();
                } else {
                    leaf_value = -entry.value();
                    bound = match entry.bound() {
                        BOUND_UPPER => BOUND_LOWER,
                        BOUND_LOWER => BOUND_UPPER,
                        x => x,
                    };
                }

                // The values under -19999 and over 19999 carry
                // information about in how many moves is the
                // inevitable checkmate. However, do not show this to
                // the user, because it is sometimes incorrect.
                if leaf_value >= 20000 {
                    leaf_value = 19999;
                    if bound == BOUND_LOWER {
                        bound = BOUND_EXACT
                    }
                }
                if leaf_value <= -20000 {
                    leaf_value = -19999;
                    if bound == BOUND_UPPER {
                        bound = BOUND_EXACT
                    }
                }

                if let Some(m) = prev_move {
                    // Extend the PV with the previous move.
                    pv.push(m);
                } else {
                    // We are at the root -- set the root value.
                    root_value = leaf_value;
                }

                if pv.len() < depth as usize && (leaf_value - root_value).abs() <= EPSILON {
                    if let Some(m) = p.try_move_digest(entry.move16()) {
                        if p.do_move(m) {
                            if bound == BOUND_EXACT {
                                // Extend the PV with one more move.
                                prev_move = Some(m);
                                our_turn = !our_turn;
                                continue;
                            } else {
                                // This is the last move in the PV.
                                pv.push(m);
                            }
                        }
                    }
                }
            }
            break;
        }

        // Second: Change the bound type if the leaf value in the PV
        // differs too much from the root value.
        bound = match leaf_value - root_value {
            x if x > EPSILON && bound != BOUND_UPPER => BOUND_LOWER,
            x if x < -EPSILON && bound != BOUND_LOWER => BOUND_UPPER,
            _ => bound,
        };

        // Third: Update `status`.
        self.status.value = root_value;
        self.status.bound = bound;
        self.status.pv = pv;
    }
}


/// Implements `UciEngine` trait.
pub struct Engine {
    tt: Arc<TranspositionTable>,
    search: MultipvSearch,
    queue: VecDeque<EngineReply>,
    position: Position,
    is_pondering: bool,
    play_when: PlayWhen,
    silent_since: SystemTime,

    current_depth: u8,
    current_value: Value,

    // Tells the engine if it will be allowed to ponder. This option
    // is needed because the engine might change its time management
    // algorithm when pondering is allowed.
    pondering_is_allowed: bool,
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
            search: MultipvSearch::new(tt),
            queue: VecDeque::new(),
            position: Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 \
                                          1")
                          .ok()
                          .unwrap(),
            is_pondering: false,
            play_when: PlayWhen::Never,
            current_depth: 0,
            current_value: VALUE_UNKNOWN,
            pondering_is_allowed: false,
            silent_since: SystemTime::now(),
        }
    }

    fn queue_progress_report(&mut self) {
        let SearchStatus { depth, searched_nodes, nps, .. } = *self.search.status();
        self.queue.push_back(EngineReply::Info(vec![
            ("depth".to_string(), format!("{}", depth)),
            ("nodes".to_string(), format!("{}", searched_nodes)),
            ("nps".to_string(), format!("{}", nps)),
        ]));
        self.silent_since = SystemTime::now();
    }

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

    fn queue_best_move(&mut self) {
        // TODO: Use `self.status.best_move`.
        let pv = &self.search.status().pv;
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

            // An invalid option. Notice that we do not support
            // re-sizing of the transposition table once the engine
            // had started.
            _ => (),
        }
    }

    fn new_game(&mut self) {
        if !self.is_thinking() {
            self.tt.clear();
        }
    }

    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if !self.is_thinking() {
            if let Ok(p) = Position::from_history(fen, moves) {
                self.position = p;
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
        if !self.is_thinking() {
            // TODO: We ignore "searchmoves" and "mate" parameters.

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
            self.search.start(&self.position);
        }
    }

    fn ponder_hit(&mut self) {
        if self.is_thinking() {
            self.is_pondering = false;
        }
    }

    fn stop(&mut self) {
        if self.is_thinking() {
            self.search.stop();
            self.queue_best_move();
        }
    }

    #[inline]
    fn is_thinking(&self) -> bool {
        !self.search.status().done
    }

    fn get_reply(&mut self) -> Option<EngineReply> {
        if self.is_thinking() {
            if self.search.update_status().done {
                // Unless this happens to be an infinite search,
                // terminate it as soon as possible.
                self.play_when = if let PlayWhen::Never = self.play_when {
                    PlayWhen::Never
                } else {
                    PlayWhen::MoveTime(0)
                };
            } else {
                // Check if we have to play now.
                if !self.is_pondering &&
                   match self.play_when {
                    PlayWhen::TimeManagement(ref tm) => tm.must_play(self.search.status()),
                    PlayWhen::MoveTime(t) => self.search.status().searched_time >= t,
                    PlayWhen::Nodes(n) => self.search.status().searched_nodes >= n,
                    PlayWhen::Depth(d) => self.search.status().depth > d,
                    PlayWhen::Never => false,
                } {
                    self.stop();
                }
            }

            // Send the new PV if changed.
            let SearchStatus { depth, value, searched_nodes, .. } = *self.search.status();
            if depth > self.current_depth || value != self.current_value {
                self.current_depth = depth;
                self.current_value = value;
                if searched_nodes >= NODE_COUNT_REPORT_INTERVAL {
                    self.queue_pv();
                }
            }

            // Send periodic progress reports.
            if self.silent_since.elapsed().unwrap().as_secs() > 10 {
                self.queue_progress_report();
            }
        }

        self.queue.pop_front()
    }

    fn exit(&mut self) {
        self.search.exit();
    }
}


// A sufficiently small value (in centipawns).
const EPSILON: Value = 8;


enum PlayWhen {
    TimeManagement(TimeManagement),
    MoveTime(u64),
    Nodes(NodeCount),
    Depth(u8),
    Never,
}


/// Implements `UciEngineFactory` trait.
pub struct EngineFactory;


impl UciEngineFactory<Engine> for EngineFactory {
    fn name(&self) -> String {
        format!("Alcibiades {}", ::VERSION)
    }

    fn author(&self) -> String {
        ::AUTHOR.to_string()
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
