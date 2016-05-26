#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod position;
pub mod basetypes;
pub mod bitsets;
pub mod notation;
pub mod uci;

use std::process::exit;

fn main() {
    // use regex::Regex;
    // let c = [(true, true); 2];
    // use position::Position;
    // Position::from_fen("k7/8/8/8/7P/8/8/7K w - h3 0 1").is_ok();
    // println!("Board -> {}", 1);

    if let Ok(mut uci_loop) = uci::Server::wait_for_hanshake(&DummyEngineFactory) {
        match uci_loop.serve() {
            Ok(_) => {
                exit(0);
            }
            Err(_) => {
                exit(1);
            }
        }
    }
    exit(1);
}


struct DummyEngineFactory;


impl uci::EngineFactory<DummyEngine> for DummyEngineFactory {
    fn name(&self) -> &str {
        "Socrates"
    }
    fn author(&self) -> &str {
        "Evgeni Pandurski"
    }
    fn options(&self) -> Vec<(uci::OptionName, uci::ValueDescription)> {
        vec![
            ("Nullmove".to_string(), uci::ValueDescription::Check { default: true }),
            ("Selectivity".to_string(), uci::ValueDescription::Spin { default: 2, min: 0, max: 4 }),
            ("Style".to_string(),
             uci::ValueDescription::Combo {
                 default: "Normal".to_string(),
                 list: vec![
                     "Solid".to_string(),
                     "Normal".to_string(),
                     "Risky".to_string()
                 ]
             }),
            ("NalimovPath".to_string(), uci::ValueDescription::String { default: "c:\\".to_string() }),
            ("Clear Hash".to_string(), uci::ValueDescription::Button),
        ]
    }

    fn create(&self) -> DummyEngine {
        DummyEngine
    }
}


struct DummyEngine;


impl uci::Engine for DummyEngine {
    #[allow(unused_variables)]
    fn set_option(&mut self, name: &str, value: &str) {}

    fn new_game(&mut self) {}

    #[allow(unused_variables)]
    fn position(&mut self, fen: &str, moves: Vec<String>) {}

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
    }

    fn ponder_hit(&mut self) {}

    fn stop(&mut self) {}

    fn ponder_move(&self) -> Option<String> {
        None
    }

    fn is_thinking(&self) -> bool {
        false
    }

    fn get_reply(&mut self) -> Option<uci::EngineReply> {
        Some(uci::EngineReply::Info(vec![]))
    }
}
