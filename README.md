# alcibiades

A framework for writing chess engines in Rust.

## Why chess engines?

Simple! Chess is the greatest thing humans have invented. Computers
follow closely ;)

## Why a framework?

There is lots of knowledge out there about how to write a chess
engine, and there is a lot of room for innovation also. Writing a
chess engine is fun, but even for the simplest engine there is a lot
of complex (and boring) things that have to be implemented first: the
UCI protocol communication, the rules, the static exchange evaluator,
and many more. Thousands of programmers have been re-implementing
those things over and over again.

So, if you want to write your own chess engine, you face an unpleasant
choice: You either roll up your sleeves and implement all the hard
stuff from scratch, or you take someone else's chess engine and
struggle to understand its cryptic, undocumented source code, hoping
that it will be general enough to allow modification. This unfortunate
situation stifles innovation.

## Usage

Here is how simple it is to create a chess engine using the framework:

```rust,no_run
extern crate alcibiades;
use alcibiades::stock::*;
use alcibiades::engine::run_uci;

fn main() {
    type HashTable = StdHashTable<StdHashTableEntry>;
    type SearchNode = StdSearchNode<StdQsearch<StdMoveGenerator<SimpleEvaluator>>>;
    type SearchExecutor = Deepening<SimpleSearchThread<HashTable, SearchNode>>;
    run_uci::<SearchExecutor, StdTimeManager>("My engine", "John Doe");
}
```

This engine is assembled from the "in stock" implementations of the
different framework traits.

In reality, you will probably want to write your own implementations
for some of the framework traits. Thanks to Rust's incredible generic
programming capabilities, you are not limited to implementing only the
methods required by the traits. For example you may write your own
static position evaluator which has a `consult_endgame_table`
method. Then you will be able to write a search algorithm that uses
this method.

Last but not least, the framework has an extensive
[documentation](https://epandurski.github.io/alcibiades).
