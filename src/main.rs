#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate rand;

pub mod position;
pub mod basetypes;
pub mod bitsets;
pub mod notation;
pub mod uci;

use std::io;
use std::process::exit;
use uci::{UciServingLoop, UciEngine, OptionDescription, ValueDescription};


fn main() {
    // use regex::Regex;
    // let c = [(true, true); 2];
    // use position::Position;
    // Position::from_fen("k7/8/8/8/7P/8/8/7K w - h3 0 1").is_ok();
    // println!("Board -> {}", 1);

    let engine = DummyEngine;
    if let Ok(mut uci_loop) = UciServingLoop::wait_for_hanshake(io::stdin(), io::stdout(), engine) {
        match uci_loop.run() {
            Ok(_) => { exit(0); }
            Err(_) => { exit(1); }
        }
    }
}


struct DummyEngine;

impl UciEngine for DummyEngine {
    fn name(&self) -> &str {
        "Socrates"
    }
    fn author(&self) -> &str {
        "Evgeni Pandurski"
    }
    fn options(&self) -> Vec<OptionDescription> {
        vec![
            OptionDescription {
                 name: "Nullmove".to_string(),
                 description: ValueDescription::Check { default: true },
            },
            OptionDescription {
                 name: "Selectivity".to_string(),
                 description: ValueDescription::Spin { default: 2, min: 0, max: 4 },
            },
            OptionDescription {
                name: "Style".to_string(),
                description: ValueDescription::Combo { default: "Normal".to_string(),
                                                       list: vec![
                                                           "Solid".to_string(),
                                                           "Normal".to_string(),
                                                           "Risky".to_string()
                                                       ]
                },
            },
            OptionDescription {
                 name: "NalimovPath".to_string(),
                 description: ValueDescription::String { default: "c:\\".to_string() },
            },
            OptionDescription {
                 name: "Clear Hash".to_string(),
                 description: ValueDescription::Button,
            },
        ]
    }
    fn set_option(&mut self, name: &str, value: &str) {}
    fn start(&mut self) {}
}
