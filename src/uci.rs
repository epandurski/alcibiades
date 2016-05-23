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
    movestogo: Option<u64>,
    depth: Option<u64>,
    nodes: Option<u64>,
    mate: Option<u64>,
    movetime: Option<u64>,
    infinite: bool,
}


fn parse_uci_command(s: &str) -> Option<UciCommand> {
    const COMMAND: &'static str = "uci|debug|isready|setoption|register|\
                                   ucinewgame|position|go|stop|ponderhit|quit";
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(r"\b({})\s*(?:\s(.*)|$)", COMMAND).as_str()
        ).unwrap();
    }

    if let Some(captures) = RE.captures(s) {
        let command = captures.at(1).unwrap();
        let the_rest = captures.at(2).unwrap_or("");
        match command {
            "uci" => Some(UciCommand::Uci),
            "debug" => {
                if let Some(params) = parse_debug_params(the_rest) {
                    Some(UciCommand::Debug(params))
                } else {
                    None
                }
            }
            "isready" => Some(UciCommand::IsReady),
            "setoption" => {
                if let Some(params) = parse_setoption_params(the_rest) {
                    Some(UciCommand::SetOption(params))
                } else {
                    None
                }
            }
            "ucinewgame" => Some(UciCommand::UciNewGame),
            "postition" => Some(UciCommand::Position(parse_position_params(the_rest))),
            "go" => Some(UciCommand::Go(parse_go_params(the_rest))),
            "stop" => Some(UciCommand::Stop),
            "ponderhit" => Some(UciCommand::PonderHit),
            "quit" => Some(UciCommand::Quit),
            _ => None,
        }
    } else {
        None
    }
}


fn parse_debug_params(s: &str) -> Option<bool> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"^(on|off)\s*$").unwrap();
    }
    if let Some(captures) = RE.captures(s) {
        match captures.at(1) {
            Some("on") => Some(true),
            Some("off") => Some(false),
            _ => None,
        }
    } else {
        None
    }
}


fn parse_setoption_params(s: &str) -> Option<SetOptionParams> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"^name\s+(.*?)(?:\s+value\s+(.*?))?\s*$").unwrap();
    }
    if let Some(captures) = RE.captures(s) {
        Some(SetOptionParams {
            name: captures.at(1).unwrap().to_string(),
            value: captures.at(2).unwrap_or("").to_string(),
        })
    } else {
        None
    }
}


fn parse_position_params(s: &str) -> PositionParams {
    PositionParams {
        fen: "String".to_string(),
        moves: vec!["move1".to_string(), "move2".to_string()],
    }
}


fn parse_go_params(s: &str) -> GoParams {
    const MOVES: &'static str = r"(?:\s+[a-h][1-8][a-h][1-8][qrbn]?)+";
    const KEYWORD: &'static str = "wtime|btime|winc|binc|movestogo|depth|nodes|\
                                   mate|movetime|ponder|infinite|searchmoves";
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(
                r"\b(?P<keyword>{})(?:\s+(?P<number>\d+)|(?P<moves>{}))?(?:\s+|$)",
                KEYWORD,
                MOVES,
            ).as_str()
        ).unwrap();
    }

    let mut params = GoParams {
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

    for captures in RE.captures_iter(s) {
        let keyword = captures.name("keyword").unwrap();
        match keyword {
            "searchmoves" => {
                if let Some(moves) = captures.name("moves") {
                    params.searchmoves = Some(moves.split_whitespace()
                                                   .map(|x| x.to_string())
                                                   .collect());
                }
            }
            "infinite" => {
                params.infinite = true;
            }
            "ponder" => {
                params.ponder = true;
            }
            _ => {
                if let Some(number) = captures.name("number") {
                    let field = match keyword {
                        "wtime" => &mut params.wtime,
                        "btime" => &mut params.btime,
                        "winc" => &mut params.winc,
                        "binc" => &mut params.binc,
                        "movestogo" => &mut params.movestogo,
                        "depth" => &mut params.depth,
                        "nodes" => &mut params.nodes,
                        "mate" => &mut params.mate,
                        "movetime" => &mut params.movetime,
                        _ => panic!("invalid keyword"),
                    };
                    *field = number.parse::<u64>().ok();
                }
            }
        }
    }
    params
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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_go_params() {
        use super::parse_go_params;
        assert_eq!(parse_go_params(" wtime22000  ").wtime, None);
        assert_eq!(parse_go_params(" wtime    22000  ").wtime, Some(22000));
        assert_eq!(parse_go_params("wtime 22000").wtime, Some(22000));
        assert_eq!(parse_go_params("wtime 99999999999999998888888888999999999999999999").wtime, None);
        assert_eq!(parse_go_params("wtime 22000").infinite, false);
        assert_eq!(parse_go_params("searchmoves   e2e4  c7c8q  ").searchmoves,
                   Some(vec!["e2e4".to_string(), "c7c8q".to_string()]));
        assert_eq!(parse_go_params("searchmoves   e2e4  c7c8q,ponder  ").searchmoves,
                   Some(vec!["e2e4".to_string()]));
        assert_eq!(parse_go_params("searchmoves aabb").searchmoves, None);
        assert_eq!(parse_go_params("infinite wtime 22000").wtime, Some(22000));
        assert_eq!(parse_go_params("infinite wtime 22000").infinite, true);
        assert_eq!(parse_go_params("wtime 22000 infinite btime 11000").infinite, true);
        assert_eq!(parse_go_params("wtime fdfee / 22000 infinite btime 11000 fdfds").infinite, true);
        assert_eq!(parse_go_params("wtime 22000 infinite btime 11000 ponder").btime, Some(11000));
    }
}
