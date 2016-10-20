//! Things we have not found a home for.

use std::sync::Arc;
use std::time::{SystemTime, Duration};
use basetypes::*;
use moves::*;
use tt::*;
use position::{Position, START_POSITION_FEN};
use search::{MAX_DEPTH, SearchParams, Report, SearchExecutor, DeepeningSearcher};


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
    /// The starting time for the search.
    pub started_at: SystemTime,

    /// The starting (root) position for the search.
    pub position: Position,

    /// `true` if the search has stopped, `false` otherwise.
    pub done: bool,

    /// The reached search depth.
    pub depth: u8,

    /// The number of legal moves in the starting (root) position.
    pub legal_moves_count: usize,

    /// The best variations found so far. The first move in each
    /// variation will be different.
    pub variations: Vec<Variation>,

    /// The duration of the search in milliseconds.
    pub duration_millis: u64,

    /// The number of analyzed nodes.
    pub searched_nodes: NodeCount,

    /// Average number of analyzed nodes per second.
    pub nps: NodeCount,
}


/// A thread that executes consecutive searches in different starting
/// positions.
pub struct SearchThread {
    tt: Arc<Tt>,
    status: SearchStatus,
    searcher: DeepeningSearcher,
}


impl SearchThread {
    /// Creates a new instance.
    pub fn new(tt: Arc<Tt>) -> SearchThread {
        SearchThread {
            tt: tt.clone(),
            status: SearchStatus {
                started_at: SystemTime::now(),
                position: Position::from_fen(START_POSITION_FEN).ok().unwrap(),
                done: true,
                depth: 0,
                variations: vec![Variation {
                                     value: VALUE_MIN,
                                     bound: BOUND_LOWER,
                                     moves: vec![],
                                 }],
                legal_moves_count: 20,
                searched_nodes: 0,
                duration_millis: 0,
                nps: 0,
            },
            searcher: DeepeningSearcher::new(tt),
        }
    }

    /// Stops the current search, starts a new search, updates the
    /// status.
    ///
    /// `position` is the starting position for the new
    /// search. `variation_count` specifies how many best lines of
    /// play to calculate (the first move in each line will be
    /// different). `searchmoves` restricts the analysis to the
    /// supplied list of moves only (no restrictions if the suppied
    /// list is empty). The move format is long algebraic
    /// notation. Examples: e2e4, e7e5, e1g1 (white short castling),
    /// e7e8q (for promotion).
    #[allow(unused_variables)]
    pub fn search(&mut self,
                  position: &Position,
                  variation_count: usize,
                  mut searchmoves: Vec<String>) {
        // Find all legal moves.
        let mut legal_moves = vec![];
        let mut move_stack = MoveStack::new();
        let mut p = position.clone();
        p.generate_moves(&mut move_stack);
        while let Some(m) = move_stack.pop() {
            if p.do_move(m) {
                legal_moves.push(m);
                p.undo_move();
            }
        }

        let legal_moves_count = legal_moves.len();

        // Remove all legal moves not allowed by `searchmoves`.
        if !searchmoves.is_empty() {
            searchmoves.sort();
            let mut moves = vec![];
            for m in legal_moves.iter() {
                if searchmoves.binary_search(&m.notation()).is_ok() {
                    moves.push(*m);
                }
            }
            if !moves.is_empty() {
                legal_moves = moves;
            }
        };

        self.stop();
        self.searcher.start_search(SearchParams {
            search_id: 0,
            position: position.clone(),
            depth: MAX_DEPTH,
            lower_bound: VALUE_MIN,
            upper_bound: VALUE_MAX,
            value: VALUE_UNKNOWN,
            searchmoves: legal_moves,
            variation_count: variation_count,
        });

        self.status = SearchStatus {
            started_at: SystemTime::now(),
            position: p,
            done: false,
            depth: 0,
            legal_moves_count: legal_moves_count,
            variations: vec![Variation {
                                 value: VALUE_MIN,
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
        self.searcher.terminate_search();
        while !self.status().done {
            self.wait_status_update(Duration::from_millis(1000));
        }
    }

    /// Returns the status of the current search.
    ///
    /// **Important note:** Consecutive calls to this method will
    /// return the same unchanged result. Only after calling `search`,
    /// `stop`, or `wait_status_update`, the result returned by
    /// `status` may change.
    #[inline(always)]
    pub fn status(&self) -> &SearchStatus {
        &self.status
    }

    /// Waits for a search status update, timing out after a specified
    /// duration or earlier.
    pub fn wait_status_update(&mut self, duration: Duration) {
        self.searcher.wait_report(duration);
        while let Ok(r) = self.searcher.try_recv_report() {
            self.process_report(r)
        }
    }

    /// A helper method. It updates the current status according to
    /// the received report message.
    fn process_report(&mut self, report: Report) {
        let duration = self.status.started_at.elapsed().unwrap();
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


/// A helper function. It extracts the primary variation (PV) from the
/// transposition table (TT) and returns it.
///
/// **Note:** Because the PV is a moving target (the search continues
/// to run in parallel), imperfections in the reported PVs are
/// unavoidable. To deal with this, we turn a blind eye if the value
/// at the root of the PV differs from the value at the leaf by no
/// more than `EPSILON`.
fn extract_pv(tt: &Tt, position: &Position, depth: u8) -> Variation {
    const EPSILON: Value = 8; // A sufficiently small value (in centipawns).

    // Extract the PV, the leaf value, the root value, and the bound
    // type from the TT.
    let mut p = position.clone();
    let mut our_turn = true;
    let mut prev_move = None;
    let mut moves = Vec::new();
    let mut leaf_value = -9999;
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

            // The values under -9999 and over 9999 may look ugly in
            // some GUIs, so we trim them.
            if leaf_value > 9999 {
                leaf_value = 9999;
                if bound == BOUND_LOWER {
                    bound = BOUND_EXACT
                }
            }
            if leaf_value < -9999 {
                leaf_value = 9999;
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
