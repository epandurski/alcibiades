//! Implements iterative deepening.
use std::time::Duration;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use chesstypes::*;
use engine::{SetOption, OptionDescription};
use search::*;


/// Executes searches with iterative deepening.
///
/// Iterative deepening works as follows: the program starts with a
/// one ply search, then increments the search depth and does another
/// search. This process is repeated until the time allocated for the
/// search is exhausted or the maximum search depth is reached. In
/// case of an unfinished search, the program can always fall back to
/// the move selected in the last iteration of the search.
///
/// # Usage
///
/// To turn `StandardSearcher` into a deepening searcher, instantiate
/// `Deepening<StandardSearcher>`.
pub struct Deepening<T: SearchExecutor> {
    params: SearchParams<T::SearchNode>,
    search_is_terminated: bool,
    previously_searched_nodes: u64,

    // The real work will be handed over to `searcher`.
    searcher: T,

    // The search depth completed so far.
    depth: u8,

    // The value for the root position so far.
    value: Value,
}

impl<T: SearchExecutor> Deepening<T> {
    fn search_next_depth(&mut self) {
        self.searcher.start_search(SearchParams {
            search_id: 0,
            depth: self.depth + 1,
            ..self.params.clone()
        });
    }
}

impl<T: SearchExecutor> SetOption for Deepening<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        T::options()
    }

    fn set_option(name: &str, value: &str) {
        T::set_option(name, value)
    }
}

impl<T: SearchExecutor> SearchExecutor for Deepening<T> {
    type HashTable = T::HashTable;

    type SearchNode = T::SearchNode;

    fn new(tt: Arc<Self::HashTable>) -> Deepening<T> {
        Deepening {
            params: bogus_params(),
            search_is_terminated: false,
            previously_searched_nodes: 0,
            searcher: T::new(tt),
            depth: 0,
            value: VALUE_UNKNOWN,
        }
    }

    fn start_search(&mut self, params: SearchParams<T::SearchNode>) {
        assert!(params.depth > 0);
        debug_assert!(params.depth <= DEPTH_MAX);
        debug_assert!(params.lower_bound < params.upper_bound);
        debug_assert!(params.lower_bound != VALUE_UNKNOWN);
        debug_assert!(!contains_dups(&params.searchmoves));
        self.params = params;
        self.search_is_terminated = false;
        self.previously_searched_nodes = 0;
        self.depth = 0;
        self.value = VALUE_UNKNOWN;
        self.search_next_depth();
    }

    fn try_recv_report(&mut self) -> Result<SearchReport, TryRecvError> {
        let SearchReport { searched_nodes, depth, value, sorted_moves, done, .. } =
            try!(self.searcher.try_recv_report());
        if !sorted_moves.is_empty() {
            debug_assert!(contains_same_moves(&self.params.searchmoves, &sorted_moves));
            self.params.searchmoves = sorted_moves.clone();
        }
        let mut report = SearchReport {
            search_id: self.params.search_id,
            searched_nodes: self.previously_searched_nodes + searched_nodes,
            depth: self.depth,
            value: self.value,
            sorted_moves: sorted_moves,
            done: done,
        };
        if done && !self.search_is_terminated {
            debug_assert_eq!(depth, self.depth + 1);
            self.previously_searched_nodes = report.searched_nodes;
            self.depth = depth;
            self.value = value;
            if depth < self.params.depth {
                self.search_next_depth();
                report.done = false;
            }
            report.depth = depth;
            report.value = value;
        }
        Ok(report)
    }

    fn wait_report(&self, duration: Duration) {
        self.searcher.wait_report(duration);
    }

    fn terminate_search(&mut self) {
        self.search_is_terminated = true;
        self.searcher.terminate_search();
    }
}


/// A helper function. It returns bogus search parameters.
fn bogus_params<T: SearchNode>() -> SearchParams<T> {
    const FEN: &'static str = "7k/8/8/8/8/8/8/7K w - - 0 1";
    SearchParams {
        search_id: 0,
        position: T::from_history(FEN, &mut vec![].into_iter()).ok().unwrap(),
        depth: 1,
        lower_bound: VALUE_MIN,
        upper_bound: VALUE_MAX,
        searchmoves: vec![Move::invalid()],
    }
}


/// A helper function. It checks if there are moves in the supplied
/// list that occur more than once.
fn contains_dups(list: &Vec<Move>) -> bool {
    let mut l = list.clone();
    l.sort();
    l.dedup();
    l.len() < list.len()
}


/// A helper function. It checks if the two supplied lists of moves
/// contain the same moves, possibly in different order.
fn contains_same_moves(list1: &Vec<Move>, list2: &Vec<Move>) -> bool {
    let mut list1 = list1.clone();
    let mut list2 = list2.clone();
    list1.sort();
    list2.sort();
    list1 == list2
}
