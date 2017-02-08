//! Implements `StdTimeManager`.

use std::sync::RwLock;
use std::time::{SystemTime, Duration};
use std::cmp::min;
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
    data_points: Vec<(f64, f64)>,
    hard_limit: f64,
    allotted_time: f64,
    must_play: bool,
}


impl<T> TimeManager<T> for StdTimeManager
    where T: SearchExecutor<ReportData = Vec<Variation>>
{
    fn new(position: &T::SearchNode, time: &RemainingTime) -> StdTimeManager {
        // Get our remaining time and time increment (in milliseconds).
        let (t, inc) = if position.board().to_move == WHITE {
            (time.white_millis as f64, time.winc_millis as f64)
        } else {
            (time.black_millis as f64, time.binc_millis as f64)
        };

        // Get the number of moves until the next time control, or if
        // not available, guess the number of moves to the end of the
        // game.
        let n = time.movestogo.unwrap_or(40) as f64;
        debug_assert!(n >= 1.0);

        // Calculate the total time we have.
        let time_heap = t + inc * (n - 1.0);

        // Set a hard limit for the time we will spend on this
        // move. Thinking longer that that would be reckless.
        let hard_limit = (t / n.sqrt() + inc).min(t - 1000.0);

        StdTimeManager {
            started_at: SystemTime::now(),
            depth: 0,
            value: VALUE_UNKNOWN,
            data_points: Vec::with_capacity(32),
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

    #[allow(unused_variables)]
    fn must_play(&mut self,
                 search_executor: &mut T,
                 report: Option<&SearchReport<Vec<Variation>>>)
                 -> bool {
        if !self.must_play {
            let mut is_finished = false;
            if let Some(r) = report {
                if r.depth > self.depth {
                    self.depth = r.depth;
                    let (target_depth, t_next) = self.target_depth(r);
                    let msg = format!("TARGET_DEPTH={}", target_depth);
                    search_executor.send_message(msg.as_str());
                    is_finished = r.depth >= target_depth || t_next > self.hard_limit
                }
            }
            self.must_play = is_finished || elapsed_millis(&self.started_at) > self.hard_limit;
        }
        self.must_play
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


impl StdTimeManager {
    /// Guesses what target depth we will be able to reach, and how
    /// much time (milliseconds) it will take for the next search
    /// depth to complete.
    fn target_depth(&mut self, report: &SearchReport<Vec<Variation>>) -> (Depth, f64) {
        let t = elapsed_millis(&self.started_at);

        // Ignore the first 1-2 depths.
        if t < 0.001 || report.searched_nodes < 100 {
            return (DEPTH_MAX, t);
        }

        // Add (x, y) to the list of data points.
        let x = report.depth as f64;
        let y = t.ln();
        self.data_points.push((x, y));

        // Do a linear extrapolation based on the last `M` data points.
        const M: usize = 5;
        let y_max = self.allotted_time.max(0.001).ln();
        let x_max;
        let t_next;
        match self.data_points.len() {
            n if n >= M => {
                let last_m = &self.data_points[n - M..];
                let (slope, intercept) = linear_regression(last_m);
                debug_assert!(slope >= 0.001);
                if n == M {
                    let mut s = AVG_SLOPE.write().unwrap();
                    *s = (*s * 2.0 + slope) / 3.0;
                }
                x_max = (y_max - intercept) / slope;
                t_next = (slope * (x + 1.0) + intercept).exp();
            }
            _ => {
                // There are not enough data points yet -- use `AVG_SLOPE`.
                let s = AVG_SLOPE.read().unwrap();
                x_max = x + (y_max - y) / *s;
                t_next = t * s.exp();
            }
        };

        // Set the target depth as close as possible to `x_max`.
        let mut target_depth = x_max.round()
                                    .min(DEPTH_MAX as f64)
                                    .max(DEPTH_MIN as f64) as Depth;

        // Search one ply deeper if the target depth is reached, but
        // position's evaluation has changed a lot.
        //
        // TODO: `25` must be bound to pawn's value.
        if target_depth == report.depth &&
           (self.value != VALUE_UNKNOWN && report.value != VALUE_UNKNOWN) &&
           (self.value as isize - report.value as isize).abs() >= 25 {
            target_depth = min(target_depth + 1, DEPTH_MAX);
        }
        self.value = report.value;

        (target_depth, 1.5 * t_next)
    }
}


lazy_static! {
    static ref PONDERING_IS_ALLOWED: RwLock<bool> = RwLock::new(false);
    static ref AVG_SLOPE: RwLock<f64> = RwLock::new(0.7);
}


/// Calculates elapsed milliseconds since a given time.
fn elapsed_millis(since: &SystemTime) -> f64 {
    let d = since.elapsed().unwrap_or(Duration::from_millis(0));
    (1000 * d.as_secs()) as f64 + (d.subsec_nanos() / 1_000_000) as f64
}


/// Calculates a regression line that approximates `points`.
fn linear_regression(points: &[(f64, f64)]) -> (f64, f64) {
    debug_assert!(points.len() > 1);
    let sum_x = points.iter().fold(0.0, |acc, &p| acc + p.0);
    let sum_y = points.iter().fold(0.0, |acc, &p| acc + p.1);
    let sum_xx = points.iter().fold(0.0, |acc, &p| acc + p.0 * p.0);
    let sum_xy = points.iter().fold(0.0, |acc, &p| acc + p.0 * p.1);
    let n = points.len() as f64;
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_xx - sum_x * sum_x);
    let intercept = (sum_y - slope * sum_x) / n;
    (slope.max(0.001), intercept)
}


#[cfg(test)]
mod tests {
    #[test]
    fn linear_regression() {
        use super::linear_regression;
        let points = vec![(21.0, 1.0), (22.0, 2.0), (23.0, 3.0), (24.0, 4.0)];
        let x = 25.0;
        let (slope, intercept) = linear_regression(&points);
        let y = slope * x + intercept;
        assert!(4.99 < y && y < 5.01);
    }
}
