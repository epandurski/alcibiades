//! Implements move generation logic.
//!
//! Generation of moves is at the heart of every chess
//! engine. `StdMoveGenerator` provides a very fast move
//! generator. Writing a good move generator is not
//! easy. Nevertheless, if you decide to do so, you can define your
//! own type that implements the `MoveGenerator` trait.

mod position;

pub use self::position::Position;
