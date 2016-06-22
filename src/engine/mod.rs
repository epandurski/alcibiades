pub mod search;


use std::thread;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver};
use basetypes::*;
use uci::{UciEngine, UciEngineFactory, EngineReply, OptionName, OptionDescription};
use position::Position;
use chess_move::*;
use tt::TranspositionTable;
use rand;
use rand::distributions::{Sample, Range};


const VERSION: &'static str = "0.1";


/// Implements `UciEngine` trait.
pub struct Engine {
    position: Position,
    pondering_is_allowed: bool,
    multi_pv: usize,
    replies: Vec<EngineReply>,
    is_thinking: bool,
    infinite: bool,
    ponder: bool,
    best_move: String,
    move_stack: Vec<Move>,
    tt: Arc<TranspositionTable>,
    commands: Sender<search::Command>,
    reports: Receiver<search::Progress>,
    results: Receiver<search::Done>,
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
        let (results_tx, results_rx) = channel();
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
            replies: vec![],
            infinite: false,
            ponder: false,
            best_move: "0000".to_string(),
            move_stack: Vec::new(),
            tt: tt.clone(),
            commands: commands_tx,
            reports: reports_rx,
            results: results_rx,
            searcher: thread::spawn(move || {
                search::run(&tt, commands_rx, reports_tx, results_tx);
            }),
        }
    }
}


impl UciEngine for Engine {
    #[allow(unused_variables)]
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

    #[allow(unused_variables)]
    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if self.is_thinking {
            return;
        }
        if let Ok(p) = Position::from_history(fen, moves) {
            self.position = p;
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
        if self.is_thinking {
            return;
        }

        // Get some legal move
        let s = &mut self.move_stack;
        let p = &mut self.position;
        p.generate_moves(s);
        let mut rng = rand::thread_rng();
        let mut between = Range::new(0, 10);
        let mut best_score = -20000;
        while let Some(m) = s.pop() {
            if p.do_move(m) {
                let score = -p.evaluate_quiescence(-20000, 20000).0 + between.sample(&mut rng);
                if score >= best_score {
                    best_score = score;
                    // self.replies.push(EngineReply::Info(vec![("info".to_string(),
                    //                                           format!("{} -> {}",
                    //                                                   m.notation(),
                    //                                                   score))]));
                    self.best_move = m.notation();
                }
                p.undo_move();
            }
        }

        // Start a new search
        self.commands
            .send(search::Command::Search(search::Parameters {
                id: 0,
                position: p.clone(),
                depth: 5,
                lower_bound: -20000,
                upper_bound: 20000,
            }))
            .unwrap();
        self.ponder = ponder;
        self.infinite = infinite;
        self.is_thinking = true;
    }

    fn ponder_hit(&mut self) {
        if self.is_thinking && self.ponder {
            self.ponder = false;
        }
    }

    fn stop(&mut self) {
        if self.is_thinking {
            self.commands.send(search::Command::Stop).unwrap();
            self.ponder = false;
            self.infinite = false;
        }
    }

    fn is_thinking(&self) -> bool {
        self.is_thinking
    }

    fn get_reply(&mut self) -> Option<EngineReply> {
        if !self.ponder {
            if let Ok(_) = self.results.try_recv() {
                self.try_to_get_best_move_from_tt();
                self.replies.push(EngineReply::BestMove {
                    best_move: self.best_move.clone(),
                    ponder_move: None,
                });
                self.best_move = "0000".to_string();
                self.is_thinking = false;
            }
        }
        self.replies.pop()
    }
}


impl Engine {
    fn try_to_get_best_move_from_tt(&mut self) {
        if let Some(entry) = self.tt.probe(self.position.hash()) {
            let m = entry.move16();
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
                self.best_move = format!("{}{}{}",
                                         notation(orig_square),
                                         notation(dest_square),
                                         if move_type == MOVE_PROMOTION {
                                             promoted_piece
                                         } else {
                                             ""
                                         })
            }
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
