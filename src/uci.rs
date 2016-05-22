//! This module handles the "Universal Chess Interface" protocol
//! communication.

use regex::Regex;
use std::io;
use std::io::{Read, Write, BufRead, BufReader, BufWriter, ErrorKind};


/// A command from the GUI to the engine.
pub enum UciCommand {
    Uci,
    Debug(bool),
    IsReady,
    SetOption(SetOptionParams),
    UciNewGame,
    Position(PositionParams),
    Go(GoParams),
    Stop,
    PonderHit,
    Quit,
}


pub struct SetOptionParams {
    name: String,
    value: String,
}


pub struct PositionParams {
    fen: String,
    moves: Vec<String>,
}


pub struct GoParams {
    searchmoves: Option<Vec<String>>,
    ponder: bool,
    wtime: Option<u64>,
    btime: Option<u64>,
    winc: Option<u64>,
    binc: Option<u64>,
    movestogo: Option<u32>,
    depth: Option<u32>,
    nodes: Option<u64>,
    mate: Option<u32>,
    movetime: Option<u64>,
    infinite: bool,
}


fn parse_uci_command(s: &str) -> Option<UciCommand> {
    lazy_static! {
        static ref COMMAND: Regex = Regex::new(
            r"\b(uci|debug|isready|setoption|register|ucinewgame|position|go|stop|ponderhit|quit)\b").unwrap();
    }

    if let Some((start, end)) = COMMAND.find(s) {
        let command = &s[start..end];
        let rest = &s[end..];
        match command {
            "uci" => Some(UciCommand::Uci),
            "debug" => {
                if let Some(params) = parse_debug_params(rest) {
                    Some(UciCommand::Debug(params))
                } else {
                    None
                }
            }
            "isready" => Some(UciCommand::IsReady),
            "setoption" => {
                if let Some(params) = parse_setoption_params(rest) {
                    Some(UciCommand::SetOption(params))
                } else {
                    None
                }
            }
            "ucinewgame" => Some(UciCommand::UciNewGame),
            "postition" => Some(UciCommand::Position(parse_position_params(rest))),
            "go" => Some(UciCommand::Go(parse_go_params(rest))),
            "stop" => Some(UciCommand::Stop),
            "ponderhit" => Some(UciCommand::PonderHit),
            "quit" => Some(UciCommand::Quit),
            _ => None,
        }
    } else {
        None
    }
}


fn parse_debug_params(params: &str) -> Option<bool> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"^\s+(on|off)\s*$").unwrap();
    }
    if let Some(captures) = RE.captures(params) {
        match captures.at(1) {
            Some("on") => Some(true),
            Some("off") => Some(false),
            _ => None,
        }
    } else {
        None
    }
}


fn parse_setoption_params(params: &str) -> Option<SetOptionParams> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"^\s+name\s+(\S.*?)(?:\s+value\s+(.*?))?\s*$").unwrap();
    }
    if let Some(captures) = RE.captures(params) {
        Some(SetOptionParams {
            name: captures.at(1).unwrap().to_string(),
            value: captures.at(2).unwrap_or("").to_string(),
        })
    } else {
        None
    }
}


fn parse_position_params(params: &str) -> PositionParams {
    PositionParams {
        fen: "String".to_string(),
        moves: vec!["move1".to_string(), "move2".to_string()],
    }
}


fn parse_go_params(params: &str) -> GoParams {
    const KEY: &'static str =
        r"searchmoves|ponder|wtime|btime|winc|binc|movestogo|depth|nodes|mate|movetime|infinite";
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(r"\s+({0})\s+(.*?)\s*(?:{0}|$)", KEY).as_str()
        ).unwrap();
    }
    let mut result = GoParams {
        searchmoves: None,
        ponder: false,
        wtime: None,
        btime: None,
        winc: None,
        binc: None,
        movestogo: None,
        depth: None,
        nodes: None,
        mate: None,
        movetime: None,
        infinite: false,
    };
    for captures in RE.captures_iter(params) {
        let key = captures.at(1).unwrap();
        let value = captures.at(2).unwrap();
        match key {
            "searchmoves" => {
                result.searchmoves = Some(value.split_whitespace()
                                               .map(|x| x.to_string())
                                               .collect());
            }
            "wtime" => {
                result.wtime = value.parse::<u64>().ok();
            }
            "btime" => {
                result.btime = value.parse::<u64>().ok();
            }
            "winc" => {
                result.winc = value.parse::<u64>().ok();
            }
            "binc" => {
                result.binc = value.parse::<u64>().ok();
            }
            "movestogo" => {
                result.movestogo = value.parse::<u32>().ok();
            }
            "depth" => {
                result.depth = value.parse::<u32>().ok();
            }
            "nodes" => {
                result.nodes = value.parse::<u64>().ok();
            }
            "mate" => {
                result.mate = value.parse::<u32>().ok();
            }
            "movetime" => {
                result.movetime = value.parse::<u64>().ok();
            }
            "infinite" => {
                result.infinite = value == "";
            }
            "ponder" => {
                result.ponder = value == "";
            }
            _ => {}
        }
    }
    result
}


/// A response from the engine to the GUI .
pub enum UciResponse {
    Id {
        attribute: String,
        value: String,
    },
    UciOk,
    ReadyOk,
    BestMove {
        best_move: String,
        ponder: Option<String>,
    },
    Info(String),
    Option(OptionDescription),
}


/// A description of a single configuration option (name and value)
/// supported by the engine.
///
/// The GUI may use this information to configure the engine. It may
/// also build dialog boxes according to the received option
/// descriptions so that GUI users can configure the engine.
pub struct OptionDescription {
    name: String,
    description: ValueDescription,
}


/// A description of a single configurable value.
pub enum ValueDescription {
    Check {
        default: bool,
    },
    Spin {
        min: i32,
        max: i32,
        default: i32,
    },
    Combo {
        list: Vec<String>,
        default: String,
    },
    String {
        default: String,
    },
    Button,
}


/// The main UCI protocol serving loop.
pub struct UciServingLoop<R: Read, W: Write> {
    reader: BufReader<R>,
    writer: BufWriter<W>,
    // engine: Box<UciEngine>,
    engine_is_started: bool,
    engine_is_thinking: bool,
}


impl<R: Read, W: Write> UciServingLoop<R, W> {
    fn wait_for_hanshake(in_stream: R, out_stream: W) -> io::Result<Self> {
        let mut reader = BufReader::new(in_stream);
        let mut writer = BufWriter::new(out_stream);
        let mut line = String::new();
        if try!(reader.read_line(&mut line)) == 0 {
            return Err(io::Error::new(ErrorKind::UnexpectedEof, "EOF"));
        }
        if line.as_str() != "uci" {
            return Err(io::Error::new(ErrorKind::Other, "unrecognized protocol"));
        }
        Ok(UciServingLoop {
            reader: reader,
            writer: writer,
            engine_is_started: false,
            engine_is_thinking: false,
        })
    }
}


pub trait UciEngine {
    fn options(&self) -> Vec<OptionDescription>;
}
