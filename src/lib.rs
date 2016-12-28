//! This crate provides a framework for writing chess engines.
//!
//! # Why chess engines?
//!
//! Simple! Chess is the greatest thing humans have
//! invented. Computers follow closely ;)
//!
//! # Why a framework?
//!
//! There is lots of knowledge out there about how to write a chess
//! engine, and there is a lot of place for innovation too. Writing a
//! chess engine is fun, but even for the simplest engine there is a
//! lot of complex (and boring) things that have to be implemented
//! correctly: the UCI protocol communication, the rules, the static
//! exchange evaluation, and so forth. Thousands of programmers had
//! been re-implementing those things over and over again.
//!
//! So, if you want to write your own chess engine, you face an
//! unpleasant choice: You either roll up your sleeves and implement
//! all the hard stuff from scratch, or you take someone else's chess
//! engine and struggle to understand its cryptic, undocumented source
//! code, hoping that it is general enough to allow modification. This
//! unfortunate situation stifles innovation.
//!
//! # Usage
//!
//! Here is how simple it is to create a chess engine using the
//! framework:
//!
//! ```rust,no_run
//! extern crate alcibiades;
//!
//! use std::process::exit;
//! use alcibiades::stock::*;
//! use alcibiades::engine::run_uci;
//!
//! fn main() {
//!     type SearchNode = StdSearchNode<StdQsearch<StdMoveGenerator<SimpleEvaluator>>>;
//!     type SearchExecutor = Deepening<StdSearchExecutor<StdHashTable, SearchNode>>;
//!     exit(match run_uci::<SearchExecutor, StdTimeManager>("My engine", "John Doe") {
//!         Ok(_) => 0,
//!         Err(_) => 1,
//!     })
//! }
//! ```
//!
//! This engine is all assembled from the "in stock" implementations
//! of the different framework traits.
//!
//! In reality, you will probably want to write your own
//! implementations of at least some of the framework traits. While
//! doing so, you are not limited to implementing only the methods
//! required by the traits. You can add your own methods and use
//! Rust's incredible capabilities for generic programming to call
//! them. For example you may write your own static position evaluator
//! which have the `consult_endgame_table()` method. Then you will be
//! able to write search algorithm that makes use of this method.

#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod utils;
pub mod engine;
pub mod stock;
pub mod squares;
pub mod files;
pub mod ranks;
pub mod bitsets;
mod board;
mod moves;
mod value;
mod depth;
mod evaluator;
mod search_node;
mod search_executor;
mod hash_table;
mod move_generator;
mod qsearch;
mod time_manager;
mod uci;

pub use board::*;
pub use moves::*;
pub use value::*;
pub use depth::*;
pub use evaluator::*;
pub use search_node::*;
pub use search_executor::*;
pub use hash_table::*;
pub use move_generator::*;
pub use qsearch::*;
pub use time_manager::*;
pub use uci::{SetOption, OptionDescription};
