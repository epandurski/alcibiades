//! Implements `Multipv`.

use std::cmp::{min, max};
use std::time::Duration;
use std::sync::{Arc, RwLock};
use std::sync::mpsc::TryRecvError;
use uci::{SetOption, OptionDescription};
use moves::Move;
use value::*;
use depth::*;
use hash_table::*;
use evaluator::Evaluator;
use search_node::SearchNode;
use search_executor::{SearchParams, SearchReport, SearchExecutor};
use time_manager::RemainingTime;
use super::{bogus_params, contains_dups};
use super::aspiration::Aspiration;


/// Executes mulit-PV searches with aspiration windows, complying with
/// `searchmoves`.
/// 
/// The auxiliary data field of searches' progress reports will
/// contain either an empty vector of moves, or the `searchmoves`
/// vector sorted by descending move strength. This allows the
/// iterative deepening routine to improve `searchmoves`' order on
/// each iteration.
pub struct Multipv<T: SearchExecutor> {
    tt: Arc<T::HashTable>,
    params: SearchParams<T::SearchNode>,
    search_is_terminated: bool,
    previously_searched_nodes: u64,

    // The real work will be handed over to `searcher`.
    searcher: Aspiration<T>,

    // The number of best lines of play that should be calculated.
    variation_count: usize,

    // Whether all legal moves in the root position are considered.
    all_moves_are_considered: bool,

    // The index in `self.params.searchmoves` of the currently
    // considered move.
    current_move_index: usize,

    // The values for the corresponding moves in `self.params.searchmoves`.
    values: Vec<Value>,
}


impl<T: SearchExecutor> SearchExecutor for Multipv<T> {
    type HashTable = T::HashTable;

    type SearchNode = T::SearchNode;

    type ReportData = Vec<Move>;

    fn new(tt: Arc<Self::HashTable>) -> Multipv<T> {
        Multipv {
            tt: tt.clone(),
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            searcher: Aspiration::new(tt),
            variation_count: 1,
            all_moves_are_considered: true,
            current_move_index: 0,
            values: vec![VALUE_MIN],
        }
    }

    fn start_search(&mut self, params: SearchParams<T::SearchNode>, _: Option<&RemainingTime>) {
        debug_assert!(params.depth > 0);
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound >= VALUE_MIN);
        debug_assert!(params.upper_bound <= VALUE_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(!contains_dups(&params.searchmoves));

        let n = params.searchmoves.len();
        self.all_moves_are_considered = n == params.position.legal_moves().len();
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.variation_count = min(n, *VARIATION_COUNT.read().unwrap());
        if n == 0 || self.variation_count == 1 && self.all_moves_are_considered {
            // A plain aspiration search.
            //
            // A search is not a genuine multi-PV search if all legal
            // moves in the root position are being considered, and
            // the number of best lines of play that should be
            // calculated is one or zero. In those cases we fall-back
            // to a plain aspiration search.
            debug_assert!(self.variation_count <= 1);
            self.searcher.lmr_mode = false;
            self.searcher.start_search(self.params.clone(), None);
        } else {
            // A genuine multi-PV search.
            debug_assert!(self.variation_count >= 1);
            self.searcher.lmr_mode = true;
            self.current_move_index = 0;
            self.values = vec![VALUE_MIN; n];
            self.search_current_move();
        }
    }

    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError> {
        if self.runs_genuine_multipv_search() {
            let SearchReport { searched_nodes, value, done, .. } = try!(self.searcher
                                                                            .try_recv_report());
            let mut report = SearchReport {
                search_id: self.params.search_id,
                searched_nodes: self.previously_searched_nodes + searched_nodes,
                depth: 0,
                value: VALUE_UNKNOWN,
                data: vec![],
                done: done,
            };
            if done && !self.search_is_terminated {
                self.previously_searched_nodes = report.searched_nodes;
                self.params.position.undo_last_move();
                self.advance_current_move(-value);
                if self.search_current_move() {
                    report.done = false;
                } else {
                    report.depth = self.params.depth;
                    report.value = self.values[0];
                    report.data = self.params.searchmoves.clone();
                }
            }
            Ok(report)
        } else {
            self.searcher.try_recv_report()
        }
    }

    fn wait_report(&self, duration: Duration) {
        self.searcher.wait_report(duration);
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.searcher.terminate_search();
    }
}


impl<T: SearchExecutor> SetOption for Multipv<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        let mut options = vec![
            ("MultiPV".to_string(), OptionDescription::Spin { min: 1, max: 500, default: 1 }),
        ];
        options.extend(Aspiration::<T>::options());
        options
    }

    fn set_option(name: &str, value: &str) {
        if name == "MultiPV" {
            *VARIATION_COUNT.write().unwrap() = max(value.parse::<usize>().unwrap_or(0), 1);
        }
        Aspiration::<T>::set_option(name, value)
    }
}


impl<T: SearchExecutor> Multipv<T> {
    /// Returns the best lines of play so far.
    pub fn extract_variations(&mut self) -> Vec<Variation> {
        let mut variations = vec![];
        if self.runs_genuine_multipv_search() {
            for m in self.params.searchmoves.iter().take(self.variation_count) {
                let p = &mut self.params.position;
                assert!(p.do_move(*m));
                let mut v = self.tt.extract_pv(p);
                p.undo_last_move();
                v.moves.insert(0, *m);
                v.value = -v.value;
                v.bound = match v.bound {
                    BOUND_LOWER => BOUND_UPPER,
                    BOUND_UPPER => BOUND_LOWER,
                    x => x,
                };
                variations.push(v);
            }
        } else if self.variation_count != 0 {
            debug_assert_eq!(self.variation_count, 1);
            variations.push(self.tt.extract_pv(&self.params.position));
        }
        variations
    }

    fn search_current_move(&mut self) -> bool {
        if self.current_move_index < self.params.searchmoves.len() {
            let alpha = self.values[self.variation_count - 1];
            if alpha < self.params.upper_bound {
                let m = self.params.searchmoves[self.current_move_index];
                assert!(self.params.position.do_move(m));
                self.previously_searched_nodes += 1;
                self.searcher.start_search(SearchParams {
                                               search_id: 0,
                                               depth: self.params.depth - 1,
                                               lower_bound: -self.params.upper_bound,
                                               upper_bound: -max(alpha, self.params.lower_bound),
                                               searchmoves: self.params.position.legal_moves(),
                                               ..self.params.clone()
                                           },
                                           None);
                return true;
            }
        }
        self.write_reslut_to_tt();
        false
    }

    fn write_reslut_to_tt(&self) {
        if self.all_moves_are_considered {
            let value = self.values[0];
            let bound = match value {
                v if v <= self.params.lower_bound => BOUND_UPPER,
                v if v >= self.params.upper_bound => BOUND_LOWER,
                _ => BOUND_EXACT,
            };
            let best_move = self.params.searchmoves[0];
            let p = &self.params.position;
            self.tt.store(p.hash(), <T::HashTable as HashTable>::Entry::with_static_eval(
                value, bound, self.params.depth, best_move.digest(), p.evaluator().evaluate(p.board())));
        }
    }

    fn advance_current_move(&mut self, v: Value) {
        debug_assert!(v >= self.values[self.current_move_index]);
        let mut i = self.current_move_index;
        self.current_move_index += 1;

        // Update `self.values` making sure that it remains sorted.
        self.values[i] = v;
        while i > 0 && v > self.values[i - 1] {
            self.values.swap(i, i - 1);
            self.params.searchmoves.swap(i, i - 1);
            i -= 1;
        }
    }

    #[inline]
    fn runs_genuine_multipv_search(&self) -> bool {
        self.searcher.lmr_mode
    }
}


lazy_static! {
    static ref VARIATION_COUNT: RwLock<usize> = RwLock::new(1);
}
