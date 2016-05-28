use uci::{UciEngine, UciEngineFactory, EngineReply, OptionName, OptionDescription};


pub struct DummyEngine;


impl UciEngine for DummyEngine {
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

    fn get_reply(&mut self) -> Option<EngineReply> {
        // Some(uci::EngineReply::BestMove {
        //     best_move: "e2e4".to_string(),
        //     ponder_move: Some("e7e5".to_string()),
        // })
        Some(EngineReply::Info(vec![("depth".to_string(), "5".to_string()),
                                    ("time".to_string(), "2000".to_string())]))
    }
}


pub struct DummyEngineFactory;


impl UciEngineFactory<DummyEngine> for DummyEngineFactory {
    fn name(&self) -> &str {
        "Socrates"
    }
    fn author(&self) -> &str {
        "Evgeni Pandurski"
    }
    fn options(&self) -> Vec<(OptionName, OptionDescription)> {
        vec![
            ("Nullmove".to_string(), OptionDescription::Check { default: true }),
            ("Selectivity".to_string(), OptionDescription::Spin { default: 2, min: 0, max: 4 }),
            ("Style".to_string(),
             OptionDescription::Combo {
                 default: "Normal".to_string(),
                 list: vec![
                     "Solid".to_string(),
                     "Normal".to_string(),
                     "Risky".to_string()
                 ]
             }),
            ("NalimovPath".to_string(), OptionDescription::String { default: "c:\\".to_string() }),
            ("Clear Hash".to_string(), OptionDescription::Button),
        ]
    }

    fn create(&self) -> DummyEngine {
        DummyEngine
    }
}
