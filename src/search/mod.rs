//! Implements the searching of the game tree.

pub mod alpha_beta;
pub mod threading;

use std::thread;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver};
use std::time::SystemTime;
use basetypes::*;
use chess_move::*;
use position::Position;
use tt::*;
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


/// A sequence of moves from some starting position, together with the
/// value assigned to the final position.
pub struct Variation {
    /// A sequence of moves from some starting position.
    pub moves: Vec<Move>,

    /// The value assigned to the final position.
    pub value: Value,

    /// The accuracy of the assigned value.
    pub bound: BoundType,
}


/// Contains information about the current progress of a search.
pub struct SearchStatus {
    started_at: Option<SystemTime>,

    /// The starting position for the search.
    position: Position,

    /// `true` if the search is finished or has been stopped, `false`
    /// otherwise.
    pub done: bool,

    /// The reached search depth.
    pub depth: u8,

    pub variations: Vec<Variation>,

    /// Number of milliseconds since the beginning of the search.
    pub duration_millis: u64,

    /// Number of analyzed nodes since the beginning of the search.
    pub searched_nodes: NodeCount,

    /// Number of analyzed nodes per second.
    pub nps: NodeCount,
}


/// A thread that executes consecutive searches in different starting
/// positions.
pub struct SearchThread {
    tt: Arc<Tt>,
    status: SearchStatus,

    // A handle to the search thread.
    search_thread: Option<thread::JoinHandle<()>>,

    // A channel for sending commands to the search thread.
    commands: Sender<Command>,

    // A channel for receiving reports from the search thread.
    reports: Receiver<Report>,
}


impl SearchThread {
    /// Creates a new instance.
    pub fn new(tt: Arc<Tt>) -> SearchThread {

        // Spawn the search thread.
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        let tt_clone = tt.clone();
        let search_thread = thread::spawn(move || {
            serve_deepening(tt_clone, commands_rx, reports_tx);
        });

        SearchThread {
            tt: tt,
            search_thread: Some(search_thread),
            commands: commands_tx,
            reports: reports_rx,
            status: SearchStatus {
                started_at: None,
                position: Position::from_fen(::STARTING_POSITION).ok().unwrap(),
                done: true,
                depth: 0,
                variations: vec![Variation {
                                     value: -19999,
                                     bound: BOUND_LOWER,
                                     moves: vec![],
                                 }],
                searched_nodes: 0,
                duration_millis: 0,
                nps: 0,
            },
        }
    }

    /// Stops the current search, starts a new search, updates the
    /// status.
    ///
    /// `position` is the starting position for the new
    /// search. `variation_count` specifies how many best lines to
    /// calculate (the first move in each best line will be
    /// different). `searchmoves` may restrict the analysis to the
    /// supplied subset of moves only. The move format is long
    /// algebraic notation. Examples: e2e4, e7e5, e1g1 (white short
    /// castling), e7e8q (for promotion).
    #[allow(unused_variables)]
    pub fn search(&mut self,
                  position: &Position,
                  variation_count: usize,
                  searchmoves: Option<Vec<String>>) {
        // TODO: We ignore the "variation_count" parameter.

        // TODO: We ignore the "searchmoves" parameter.

        // TODO: Add `self.legal_moves_count` filed.

        self.stop();
        self.commands
            .send(Command::Search {
                search_id: 0,
                position: position.clone(),
                depth: MAX_DEPTH,
                lower_bound: -29999,
                upper_bound: 29999,
            })
            .unwrap();
        self.status = SearchStatus {
            started_at: Some(SystemTime::now()),
            position: position.clone(),
            done: false,
            depth: 0,
            variations: vec![Variation {
                                 value: -19999,
                                 bound: BOUND_LOWER,
                                 moves: vec![],
                             }], // TODO: Set good initial value (vec![best_move]).
            searched_nodes: 0,
            duration_millis: 0,
            ..self.status
        };
    }

    /// Stops the current search, updates the status.
    ///
    /// Does nothing if the current search is already stopped.
    pub fn stop(&mut self) {
        self.commands.send(Command::Stop).unwrap();
        while !self.status().done {
            let r = self.reports.recv().unwrap();
            self.process_report(r);
        }
    }

    /// Updates the status of the current search.
    pub fn update_status(&mut self) {
        while let Ok(r) = self.reports.try_recv() {
            self.process_report(r)
        }
    }

    /// Returns the status of the current search.
    ///
    /// **Important note:** Consecutive calls to this method will
    /// return the same unchanged result. Only after calling `search`,
    /// `stop`, or `update_status`, the result returned by `status`
    /// may change.
    #[inline(always)]
    pub fn status(&self) -> &SearchStatus {
        &self.status
    }

    /// Stops the current search and joins the search thread.
    ///
    /// After calling `join`, no other methods on this instance should
    /// be called.
    pub fn join(&mut self) {
        self.stop();
        self.commands.send(Command::Exit).unwrap();
        self.search_thread.take().unwrap().join().unwrap();
    }

    // A helper method. It updates the current status according to the
    // received report message.
    fn process_report(&mut self, report: Report) {
        let duration = self.status.started_at.unwrap().elapsed().unwrap();
        self.status.duration_millis = 1000 * duration.as_secs() +
                                      (duration.subsec_nanos() / 1000000) as u64;
        self.status.searched_nodes = report.searched_nodes;
        self.status.nps = 1000 * (self.status.nps + self.status.searched_nodes) /
                          (1000 + self.status.duration_millis);
        if self.status.depth < report.depth {
            self.status.depth = report.depth;
            self.status.variations = vec![extract_pv(&self.tt,
                                                     &self.status.position,
                                                     report.depth)];
        }
        self.status.done = report.done;
    }
}


// A helper function. It extracts the primary variation (PV) from the
// transposition table (TT) and returns it.
//
// **Note:** Because the PV is a moving target (the search
// continues to run in parallel), imperfections in the reported
// PVs are unavoidable. To deal with this, we turn a blind eye if
// the value at the root of the PV differs from the value at the
// leaf by no more than `EPSILON`.
fn extract_pv(tt: &Tt, position: &Position, depth: u8) -> Variation {
    const EPSILON: Value = 8; // A sufficiently small value (in centipawns).

    // Extract the PV, the leaf value, the root value, and the bound
    // type from the TT.
    let mut p = position.clone();
    let mut our_turn = true;
    let mut prev_move = None;
    let mut moves = Vec::new();
    let mut leaf_value = -19999;
    let mut root_value = leaf_value;
    let mut bound = BOUND_LOWER;
    while let Some(entry) = tt.peek(p.hash()) {
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
                moves.push(m);
            } else {
                // We are at the root -- set the root value.
                root_value = leaf_value;
            }

            if moves.len() < depth as usize && (leaf_value - root_value).abs() <= EPSILON {
                if let Some(m) = p.try_move_digest(entry.move16()) {
                    if p.do_move(m) {
                        if bound == BOUND_EXACT {
                            // Extend the PV with one more move.
                            prev_move = Some(m);
                            our_turn = !our_turn;
                            continue;
                        } else {
                            // This is the last move in the PV.
                            moves.push(m);
                        }
                    }
                }
            }
        }
        break;
    }

    // Change the bound type if the leaf value in the PV differs too
    // much from the root value.
    bound = match leaf_value - root_value {
        x if x > EPSILON && bound != BOUND_UPPER => BOUND_LOWER,
        x if x < -EPSILON && bound != BOUND_LOWER => BOUND_UPPER,
        _ => bound,
    };

    Variation {
        value: root_value,
        bound: bound,
        moves: moves,
    }
}
