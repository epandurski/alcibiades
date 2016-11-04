//! Implements some higher-level searching facilities.

use std::cmp::min;
use std::sync::Arc;
use std::time::{SystemTime, Duration};
use basetypes::*;
use tt::*;
use position::*;
use search::*;


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
    searcher: DeepeningSearcher<MultipvSearcher<AlphabetaSearcher>>,
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

    /// Terminates the currently running search (if any), starts a new
    /// search, updates the status.
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
        let mut legal_moves = position.legal_moves();
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
            searchmoves: legal_moves,
            variation_count: variation_count,
        });

        self.status = SearchStatus {
            started_at: SystemTime::now(),
            position: position.clone(),
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

    /// Terminates the currently running search (if any), updates the
    /// status.
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


/// Decides when the search must be terminated.
pub struct TimeManager {
    move_time_millis: u64, // move time in milliseconds
    must_play: bool,
}


impl TimeManager {
    /// Creates a new instance.
    ///
    /// `position` gives the current position. `pondering_is_allowed`
    /// tells if the engine is allowed to use opponent's time for
    /// thinking. `wtime_millis`, `btime_millis`, `winc_millis`, and
    /// `binc_millis` specify the remaining time in milliseconds, and
    /// the number of milliseconds with which the remaining time will
    /// be incremented on each move (for black and white). `movestogo`
    /// specifies the number of moves to the next time control.
    #[allow(unused_variables)]
    pub fn new(position: &Position,
               pondering_is_allowed: bool,
               wtime_millis: Option<u64>,
               btime_millis: Option<u64>,
               winc_millis: Option<u64>,
               binc_millis: Option<u64>,
               movestogo: Option<u64>)
               -> TimeManager {
        // TODO: We ignore "pondering_is_allowed".

        let (time, inc) = if position.to_move() == WHITE {
            (wtime_millis, winc_millis.unwrap_or(0))
        } else {
            (btime_millis, binc_millis.unwrap_or(0))
        };
        let time = time.unwrap_or(0);
        let movestogo = movestogo.unwrap_or(40);
        let movetime = (time + inc * movestogo) / movestogo;
        TimeManager {
            move_time_millis: min(movetime, time / 2),
            must_play: false,
        }
    }

    /// Registers the current search status with the time manager.
    pub fn update_status(&mut self, new_status: &SearchStatus) {
        // TODO: Implement smarter time management.
        self.must_play = new_status.duration_millis >= self.move_time_millis;
    }

    /// Decides if the search must be terminated.
    #[inline]
    pub fn must_play(&self) -> bool {
        self.must_play
    }
}
