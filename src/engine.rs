#![allow(unused_imports)]

use uci::{UciEngine, UciEngineFactory, EngineReply, OptionName, OptionDescription};
use position::Position;
use chess_move::*;
use rand;
use rand::distributions::{Sample, Range};


pub const VERSION: &'static str = "0.1";


pub struct DummyEngine {
    position: Position,
    replies: Vec<EngineReply>,
    is_thinking: bool,
    infinite: bool,
    ponder: bool,
    best_move: String,
    move_stack: Vec<Move>,
}


impl DummyEngine {
    pub fn new() -> DummyEngine {
        DummyEngine {
            position: Position::from_history("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk \
                                              - 0 1",
                                             &mut vec![].into_iter())
                          .ok()
                          .unwrap(),
            replies: vec![],
            is_thinking: false,
            infinite: false,
            ponder: false,
            best_move: "0000".to_string(),
            move_stack: Vec::new(),
        }
    }
}


impl UciEngine for DummyEngine {
    #[allow(unused_variables)]
    fn set_option(&mut self, name: &str, value: &str) {
        self.replies.push(EngineReply::Info(vec![("string".to_string(),
                                                  format!("{} -> {}", name, value))]));
    }

    fn new_game(&mut self) {}

    #[allow(unused_variables)]
    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>) {
        if self.is_thinking {
            return;
        }
        self.position = Position::from_history(fen, moves).ok().unwrap();
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
        let s = &mut self.move_stack;
        let p = &mut self.position;
        p.generate_moves(s);
        // let mut legal_moves = vec![];
        let mut best_score = -20000;
        while let Some(m) = s.pop() {
            if p.do_move(m) {
                let score = -p.evaluate_quiescence(-20000, 20000);
                if score > best_score {
                    best_score = score;
                    // self.replies.push(EngineReply::Info(vec![("info".to_string(),
                    //                                           format!("{} -> {}",
                    //                                                   m.notation(),
                    //                                                   score))]));
                    self.best_move = m.notation();
                }
                p.undo_move();
                // legal_moves.push(m.notation());
            }
        }
        // let mut rng = rand::thread_rng();
        // let mut between = Range::new(0, legal_moves.len());
        // self.best_move = legal_moves[between.sample(&mut rng)].clone();

        self.ponder = ponder;
        self.infinite = infinite;
        self.is_thinking = infinite | ponder;
        if !self.is_thinking {
            self.replies.push(EngineReply::BestMove {
                best_move: self.best_move.clone(),
                ponder_move: None,
            });
            self.best_move = "0000".to_string();
        }
    }

    fn ponder_hit(&mut self) {
        if self.is_thinking && self.ponder {
            self.ponder = false;
            if !self.infinite {
                self.stop()
            }
        }
    }

    fn stop(&mut self) {
        if !self.is_thinking {
            return;
        }
        self.replies.push(EngineReply::BestMove {
            best_move: self.best_move.clone(),
            ponder_move: None,
        });
        self.best_move = "0000".to_string();
        self.is_thinking = false;
    }

    fn is_thinking(&self) -> bool {
        self.is_thinking
    }

    fn get_reply(&mut self) -> Option<EngineReply> {
        self.replies.pop()
    }
}


pub struct DummyEngineFactory {
    name: String,
}


impl DummyEngineFactory {
    pub fn new() -> DummyEngineFactory {
        DummyEngineFactory { name: format!("Alcibiades {}", VERSION) }
    }
}


impl UciEngineFactory<DummyEngine> for DummyEngineFactory {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn author(&self) -> &str {
        "Evgeni Pandurski"
    }

    fn options(&self) -> Vec<(OptionName, OptionDescription)> {
        // vec![
        //     ("Nullmove".to_string(), OptionDescription::Check { default: true }),
        //     ("Selectivity".to_string(), OptionDescription::Spin { default: 2, min: 0, max: 4 }),
        //     ("NalimovPath".to_string(), OptionDescription::String { default: "c:\\".to_string() }),
        //     ("Clear Hash".to_string(), OptionDescription::Button),
        // ]

        vec![
            ("Clear Hash".to_string(), OptionDescription::Button),
            ("Style".to_string(),
             OptionDescription::Combo {
                 default: "Normal".to_string(),
                 list: vec![
                     "Solid".to_string(),
                     "Normal".to_string(),
                     "Risky".to_string()
                 ]
             }),
        ]
    }

    fn create(&self) -> DummyEngine {
        DummyEngine::new()
    }
}
