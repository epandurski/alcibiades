//! Implements `StdTimeManager`.

use std::sync::RwLock;
use std::time::SystemTime;
use board::*;
use depth::*;
use value::*;
use search_executor::*;
use hash_table::Variation;
use search_node::SearchNode;
use time_manager::{TimeManager, RemainingTime};
use uci::{SetOption, OptionDescription};


/// Implements the `TimeManager` trait.
pub struct StdTimeManager {
    started_at: SystemTime,
    depth: Depth,
    value: Value,
    extrapolation_points: Vec<(f64, f64)>,
    hard_limit: f64,
    allotted_time: f64,
    must_play: bool,
}


impl<S> TimeManager<S> for StdTimeManager
    where S: SearchExecutor<ReportData = Vec<Variation>>
{
    fn new(position: &S::SearchNode, time: RemainingTime) -> StdTimeManager {
        // Get our remaining time and increment (in milliseconds).
        let (t, inc) = if position.board().to_move == WHITE {
            (time.white_millis as f64, time.winc_millis as f64)
        } else {
            (time.black_millis as f64, time.binc_millis as f64)
        };

        // Get the number of moves until the next time control, or if
        // not available, guess the number of moves to the end of the
        // game.
        let n = match time.movestogo.unwrap_or(0) {
            0 => 40,
            n => n,
        } as f64;

        // Calculate the total time we have.
        let time_heap = t + inc * (n - 1.0);

        // Set a hard limit for the time we will spend on this
        // move. Thinking longer that that would be reckless.
        let hard_limit = (t / n.sqrt() + inc).min(t - 1000.0);

        StdTimeManager {
            started_at: SystemTime::now(),
            depth: 0,
            value: VALUE_UNKNOWN,
            extrapolation_points: Vec::with_capacity(32),
            hard_limit: if position.legal_moves().len() > 1 {
                hard_limit
            } else {
                // When there is only one legal move, the engine is
                // allowed to think just a fraction of a second in
                // order to find a good ponder move.
                hard_limit.min(500.0)
            },
            allotted_time: if *PONDERING_IS_ALLOWED.read().unwrap() {
                // Statistically, the move we ponder will be played in
                // 50% of the cases. Therefore, in principal we should
                // add half of opponent's thinking time to our time
                // heap. In reality we do not know how opponent's time
                // will be spend, so we speculatively increase our
                // time heap by 50%.
                1.5 * time_heap / n
            } else {
                time_heap / n
            },
            must_play: false,
        }
    }

    fn update(&mut self, report: &SearchReport<Vec<Variation>>) {
        let &SearchReport { ref depth, ref value, ref searched_nodes, .. } = report;
        if *depth > self.depth {
            self.depth = *depth;
            if *searched_nodes < 100 {
                return; // We ignore the first few depths.
            }
            let t = elapsed_millis(&self.started_at);
            let depth = *depth as f64;
            let searched_nodes = *searched_nodes as f64;

            // We maintain a list of data points so as to be able to
            // intelligently guess how much time it will take for the
            // next search depth to complete. (We apply an exponential
            // regression over the last `M` points in the list.)
            const M: usize = 5;
            self.extrapolation_points.push((depth, searched_nodes.ln()));
            let expected_duration = match self.extrapolation_points.len() {
                n if n >= M => {
                    let last_m = &self.extrapolation_points[n - M..];
                    let factor = extrapolate(last_m, depth + 1.0).exp() / searched_nodes;

                    // Update `BRANCHING_FACTOR`. This is an average
                    // branching factor calculated over the last few
                    // moves.
                    if n == M {
                        let mut bf = BRANCHING_FACTOR.write().unwrap();
                        *bf = (*bf * 2.0 + factor) / 3.0;
                    }

                    factor * t
                }
                _ => *BRANCHING_FACTOR.read().unwrap() * t,
            };

            // We may need to revise the allotted time if position's
            // evaluation has changed a lot with the newly completed
            // depth.
            if (expected_duration > self.allotted_time) &&
               (self.value != VALUE_UNKNOWN && *value != VALUE_UNKNOWN) &&
               (self.value as isize - *value as isize).abs() >= 25 {
                self.allotted_time = expected_duration;
            }
            self.value = *value;

            // We try to stay as close as possible to the allotted
            // time, without crossing the hard limit.
            self.must_play = expected_duration > self.hard_limit ||
                             expected_duration > self.allotted_time &&
                             expected_duration - self.allotted_time > self.allotted_time - t;
        }
    }

    #[allow(unused_variables)]
    fn must_play(&self, search: &S) -> bool {
        self.must_play || elapsed_millis(&self.started_at) > self.hard_limit
    }
}


impl SetOption for StdTimeManager {
    fn options() -> Vec<(String, OptionDescription)> {
        vec![("Ponder".to_string(), OptionDescription::Check { default: false })]
    }

    fn set_option(name: &str, value: &str) {
        if name == "Ponder" {
            match value {
                "true" => *PONDERING_IS_ALLOWED.write().unwrap() = true,
                "false" => *PONDERING_IS_ALLOWED.write().unwrap() = false,
                _ => (),
            }
        }
    }
}


lazy_static! {
    static ref PONDERING_IS_ALLOWED: RwLock<bool> = RwLock::new(false);
    static ref BRANCHING_FACTOR: RwLock<f64> = RwLock::new(2.0);
}


/// A helper function. It calculates the elapsed milliseconds since a
/// given time.
fn elapsed_millis(since: &SystemTime) -> f64 {
    let d = since.elapsed().unwrap();
    (1000 * d.as_secs()) as f64 + (d.subsec_nanos() / 1_000_000) as f64
}


/// A helper function. It linearly extrapolates the value y(x) using
/// the (x, y) values in `points` as a reference.
fn extrapolate(points: &[(f64, f64)], x: f64) -> f64 {
    debug_assert!(points.len() > 1);
    let sum_x = points.iter().fold(0.0, |acc, &p| acc + p.0);
    let sum_y = points.iter().fold(0.0, |acc, &p| acc + p.1);
    let sum_xx = points.iter().fold(0.0, |acc, &p| acc + p.0 * p.0);
    let sum_xy = points.iter().fold(0.0, |acc, &p| acc + p.0 * p.1);
    let n = points.len() as f64;
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_xx - sum_x * sum_x);
    let intercept = (sum_y - slope * sum_x) / n;
    slope * x + intercept
}


#[cfg(test)]
mod tests {
    #[test]
    fn linear_regression() {
        use super::extrapolate;
        let points = vec![(21.0, 1.0), (22.0, 2.0), (23.0, 3.0), (24.0, 4.0)];
        let x = 25.0;
        let y = extrapolate(&points, x);
        assert!(4.99 < y && y < 5.01);
    }
}
