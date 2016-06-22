pub mod search;

use std::thread;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver, RecvError};
use basetypes::*;
use uci::{UciEngine, UciEngineFactory, EngineReply, OptionName, OptionDescription};
use position::Position;
use chess_move::*;
use tt::TranspositionTable;

// use rand;
// use rand::distributions::{Sample, Range};
// let mut rng = rand::thread_rng();
// let mut between = Range::new(0, 10);
// let x = between.sample(&mut rng);


const VERSION: &'static str = "0.1";


struct DeepeningSearcher {
    positions: Sender<Option<Position>>,
    master: thread::JoinHandle<Result<(), RecvError>>,
    slave: thread::JoinHandle<()>,
}


impl DeepeningSearcher {
    fn new(tt: Arc<TranspositionTable>) -> DeepeningSearcher {
        let (positions_tx, positions_rx) = channel();
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        DeepeningSearcher {
            positions: positions_tx,
            master: thread::spawn(move || {
                let mut pending_position = None;
                loop {
                    // If there is a pending position, we take it,
                    // otherwise we block and wait to receive a new
                    // one.
                    if let Some(p) = pending_position.take()
                                                     .unwrap_or(positions_rx.recv().unwrap()) {
                        'depthloop: for depth in 1.. {
                            commands_tx.send(search::Command::Search {
                                           search_id: 0,
                                           position: p.clone(),
                                           depth: depth,
                                           lower_bound: -20000,
                                           upper_bound: 20000,
                                       })
                                       .unwrap();
                            loop {
                                match reports_rx.recv().unwrap() {
                                    search::Report::Progress { .. } => {
                                        if let Ok(x) = positions_rx.try_recv() {
                                            // There is a new position pending -- we
                                            // should terminate the current search.
                                            pending_position = Some(x);
                                            break 'depthloop;
                                        }
                                    }
                                    search::Report::Done { .. } => break,
                                }
                            }
                        }
                    }
                }
            }),
            slave: thread::spawn(move || {
                search::run(&tt, commands_rx, reports_tx);
            }),
        }
    }

    fn start(&mut self, p: &Position) {
        self.positions.send(Some(p.clone())).unwrap();
    }

    fn stop(&mut self) {
        self.positions.send(None).unwrap();
    }
}


/// Implements `UciEngine` trait.
pub struct Engine {
    position: Position,
    pondering_is_allowed: bool,
    multi_pv: usize,
    is_thinking: bool,
    is_pondering: bool,
    replies: Vec<EngineReply>,
    infinite: bool,
    tt: Arc<TranspositionTable>,
    commands: Sender<search::Command>,
    reports: Receiver<search::Report>,
    searcher: thread::JoinHandle<()>,
}


impl Engine {
    /// Creates a new instance.
    ///
    /// `tt_size_mb` is the preferred size of the transposition
    /// table in Mbytes.
    pub fn new(tt_size_mb: usize) -> Engine {
        let (commands_tx, commands_rx) = channel();
        let (reports_tx, reports_rx) = channel();
        let mut tt = TranspositionTable::new();
        tt.resize(tt_size_mb);
        let tt = Arc::new(tt);
        Engine {
            position: Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 \
                                          1")
                          .ok()
                          .unwrap(),
            pondering_is_allowed: false,
            multi_pv: 1,
            is_thinking: false,
            is_pondering: false,
            replies: vec![],
            infinite: false,
            tt: tt.clone(),
            commands: commands_tx,
            reports: reports_rx,
            searcher: thread::spawn(move || {
                search::run(&tt, commands_rx, reports_tx);
            }),
        }
    }
}


impl UciEngine for Engine {
    fn set_option(&mut self, name: &str, value: &str) {
        match name {
            // We do not support re-sizing of the transposition table
            // once the engine had started, so we do nothing.
            "Hash" => (),

            // Tells the engine that it will be allowed to ponder.
            // This option is needed because the engine might change
            // its time management algorithm when pondering is
            // allowed.
            "Ponder" => {
                self.pondering_is_allowed = value == "true";
            }

            // Tells the engine to output multiple best lines.
            // the default value is `1`.)
            "MultiPV" => {
                self.multi_pv = value.parse::<usize>().ok().unwrap_or(1);
            }

            // An invalid option.
            _ => {
                self.replies.push(EngineReply::Info(vec![("string".to_string(),
                                                          format!("Invalid option \"{}\"", name))]));
            }
        }
    }

    fn new_game(&mut self) {
        // Clearing the transposition table would not change anything,
        // so we do nothing.
    }

    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if !self.is_thinking {
            if let Ok(p) = Position::from_history(fen, moves) {
                self.position = p;
            }
        }
    }

    #[allow(unused_variables)]
    fn go(&mut self,
          searchmoves: Option<Vec<String>>,
          ponder: bool,
          wtime: Option<u64>,
          btime: Option<u64>,
          winc: Option<u64>,
          binc: Option<u64>,
          movestogo: Option<u64>,
          depth: Option<u64>,
          nodes: Option<u64>,
          mate: Option<u64>,
          movetime: Option<u64>,
          infinite: bool) {
        if !self.is_thinking {
            self.commands
                .send(search::Command::Search {
                    search_id: 0,
                    position: self.position.clone(),
                    depth: 5,
                    lower_bound: -20000,
                    upper_bound: 20000,
                })
                .unwrap();
            self.is_thinking = true;
            self.is_pondering = ponder;
            self.infinite = infinite;
        }
    }

    fn ponder_hit(&mut self) {
        if self.is_thinking && self.is_pondering {
            self.is_pondering = false;
        }
    }

    fn stop(&mut self) {
        if self.is_thinking {
            self.commands.send(search::Command::Stop).unwrap();
            let best_move = self.get_best_move();
            self.replies.push(EngineReply::BestMove {
                best_move: best_move,
                ponder_move: None,
            });
            self.is_thinking = false;
        }
    }

    fn is_thinking(&self) -> bool {
        self.is_thinking
    }

    fn get_reply(&mut self) -> Option<EngineReply> {
        // TODO: do interactive deepening instead.
        if !self.is_pondering {
            if let Ok(search::Report::Done { .. }) = self.reports.try_recv() {
                self.stop();
            }
        }

        self.replies.pop()
    }
}


impl Engine {
    fn get_best_move(&mut self) -> String {
        let mut m = match self.tt.probe(self.position.hash()) {
            Some(entry) => entry.move16(),
            None => 0,
        };
        if m == 0 {
            // Pick the first legal move.
            let mut first_legal_move = Move::from_u32(0);
            let mut v = Vec::with_capacity(128);
            self.position.generate_moves(&mut v);
            while let Some(x) = v.pop() {
                if self.position.do_move(x) {
                    self.position.undo_move();
                    first_legal_move = x;
                    break;
                }
            }
            m = first_legal_move.move16();
        }
        if m != 0 {
            let move_type = ((m & 0b1100000000000000) >> 14) as MoveType;
            let orig_square = ((m & 0b0011111100000000) >> 8) as Square;
            let dest_square = ((m & 0b0000000011111100) >> 2) as Square;
            let promoted_piece = match m & 0b11 {
                0 => "q",
                1 => "r",
                2 => "b",
                3 => "n",
                _ => panic!("invalid promoted piece code"),
            };
            format!("{}{}{}",
                    notation(orig_square),
                    notation(dest_square),
                    if move_type == MOVE_PROMOTION {
                        promoted_piece
                    } else {
                        ""
                    })

        } else {
            "0000".to_string()
        }
    }
}


/// Implements `UciEngineFactory` trait.
pub struct EngineFactory;


impl UciEngineFactory<Engine> for EngineFactory {
    fn name(&self) -> String {
        format!("Alcibiades {}", VERSION)
    }

    fn author(&self) -> String {
        "Evgeni Pandurski".to_string()
    }

    fn options(&self) -> Vec<(OptionName, OptionDescription)> {
        vec![
            // TODO: Calculate a sane limit for the hash size.
            ("Hash".to_string(), OptionDescription::Spin { min: 1, max: 2048, default: 16 }),
            ("Ponder".to_string(), OptionDescription::Check { default: false }),
            ("MultiPV".to_string(), OptionDescription::Spin { min: 1, max: 32, default: 1 }),
        ]
    }

    fn create(&self, hash_size_mb: Option<usize>) -> Engine {
        Engine::new(hash_size_mb.unwrap_or(16))
    }
}
