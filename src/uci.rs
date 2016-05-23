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


struct ParseError;


fn parse_uci_command(s: &str) -> Result<UciCommand, ParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(r"\b({})\s*(?:\s(.*)|$)",
                    "uci|debug|isready|setoption|register|\
                     ucinewgame|position|go|stop|ponderhit|quit",  // any UCI command
            ).as_str()
        ).unwrap();
    }
    if let Some(captures) = RE.captures(s) {
        let command_str = captures.at(1).unwrap();
        let params_str = captures.at(2).unwrap_or("");
        match command_str {
            "uci" if params_str == "" => Ok(UciCommand::Uci),
            "stop" if params_str == "" => Ok(UciCommand::Stop),
            "quit" if params_str == "" => Ok(UciCommand::Quit),
            "isready" if params_str == "" => Ok(UciCommand::IsReady),
            "ponderhit" if params_str == "" => Ok(UciCommand::PonderHit),
            "ucinewgame" if params_str == "" => Ok(UciCommand::UciNewGame),
            "debug" => Ok(UciCommand::Debug(try!(parse_debug_params(params_str)))),
            "setoption" => Ok(UciCommand::SetOption(try!(parse_setoption_params(params_str)))),
            "position" => Ok(UciCommand::Position(try!(parse_position_params(params_str)))),
            "go" => Ok(UciCommand::Go(parse_go_params(params_str))),
            _ => Err(ParseError),
        }
    } else {
        Err(ParseError)
    }
}


fn parse_debug_params(s: &str) -> Result<bool, ParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"^(on|off)\s*$").unwrap();
    }
    if let Some(captures) = RE.captures(s) {
        match captures.at(1).unwrap() {
            "on" => Ok(true),
            "off" => Ok(false),
            _ => Err(ParseError),
        }
    } else {
        Err(ParseError)
    }
}


fn parse_setoption_params(s: &str) -> Result<SetOptionParams, ParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"^name\s+(.*?)(?:\s+value\s+(.*?))?\s*$").unwrap();
    }
    if let Some(captures) = RE.captures(s) {
        Ok(SetOptionParams {
            name: captures.at(1).unwrap().to_string(),
            value: captures.at(2).unwrap_or("").to_string(),
        })
    } else {
        Err(ParseError)
    }
}


fn parse_position_params(s: &str) -> Result<PositionParams, ParseError> {
    const STARTPOS: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1";
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(
                r"^(?:fen\s+(?P<fen>.*?)|startpos)(?:\s+moves(?P<moves>{}))?\s*$",
                r"(?:\s+[a-h][1-8][a-h][1-8][qrbn]?)*",  // a possibly empty list of moves
            ).as_str()
        ).unwrap();
    }
    if let Some(captures) = RE.captures(s) {
        Ok(PositionParams {
            fen: if let Some(fen) = captures.name("fen") {
                fen.to_string()
            } else {
                STARTPOS.to_string()
            },
            moves: captures.name("moves")
                           .unwrap_or("")
                           .split_whitespace()
                           .map(|x| x.to_string())
                           .collect(),
        })
    } else {
        Err(ParseError)
    }
}


fn parse_go_params(s: &str) -> GoParams {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(
                r"\b(?P<keyword>{})(?:\s+(?P<number>\d+)|(?P<moves>{}))?(?:\s+|$)",
                "wtime|btime|winc|binc|movestogo|depth|\
                 nodes|mate|movetime|ponder|infinite|searchmoves",  // any keyword
                r"(?:\s+[a-h][1-8][a-h][1-8][qrbn]?)+",  // a non-empty list of moves
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
        assert_eq!(parse_go_params("wtime 99999999999999998888888888999999999999999999").wtime,
                   None);
        assert_eq!(parse_go_params("wtime 22000").infinite, false);
        assert_eq!(parse_go_params("searchmoves   e2e4  c7c8q  ").searchmoves,
                   Some(vec!["e2e4".to_string(), "c7c8q".to_string()]));
        assert_eq!(parse_go_params("searchmoves   e2e4  c7c8q,ponder  ").searchmoves,
                   Some(vec!["e2e4".to_string()]));
        assert_eq!(parse_go_params("searchmoves aabb").searchmoves, None);
        assert_eq!(parse_go_params("infinite wtime 22000").wtime, Some(22000));
        assert_eq!(parse_go_params("infinite wtime 22000").infinite, true);
        assert_eq!(parse_go_params("wtime 22000 infinite btime 11000").infinite,
                   true);
        assert_eq!(parse_go_params("wtime fdfee / 22000 infinite btime 11000 fdfds").infinite,
                   true);
        assert_eq!(parse_go_params("wtime 22000 infinite btime 11000 ponder").btime,
                   Some(11000));
    }

    #[test]
    fn test_parse_debug_params() {
        use super::parse_debug_params;
        assert_eq!(parse_debug_params("on").ok().unwrap(), true);
        assert!(parse_debug_params(" on").is_err());
        assert_eq!(parse_debug_params("off   ").ok().unwrap(), false);
        assert!(parse_debug_params(" off   ").is_err());
        assert!(parse_debug_params("    ").is_err());
        assert!(parse_debug_params(" sgssgs  ").is_err());
    }

    #[test]
    fn test_parse_setoption_params() {
        use super::parse_setoption_params;
        assert_eq!(parse_setoption_params("name   xxx  value   yyy  ").ok().unwrap().name,
                   "xxx".to_string());
        assert_eq!(parse_setoption_params("name xxx value yyy").ok().unwrap().value,
                   "yyy".to_string());
        assert_eq!(parse_setoption_params("name xxx   value  ").ok().unwrap().value,
                   "".to_string());
        assert_eq!(parse_setoption_params("name xxx    ").ok().unwrap().value,
                   "".to_string());
        assert_eq!(parse_setoption_params("name     ").ok().unwrap().name,
                   "".to_string());
        assert_eq!(parse_setoption_params("name     ").ok().unwrap().value,
                   "".to_string());
        assert!(parse_setoption_params("namexxx     ").is_err());
    }

    #[test]
    fn test_parse_position_params() {
        use super::parse_position_params;
        assert_eq!(parse_position_params("startpos  ").ok().unwrap().fen,
                   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1");
        assert_eq!(parse_position_params("startpos ").ok().unwrap().moves.len(),
                   0);
        assert_eq!(parse_position_params("startpos   moves  ").ok().unwrap().moves.len(),
                   0);
        assert_eq!(parse_position_params("startpos   moves   e2e4   d2d4 ")
                       .ok()
                       .unwrap()
                       .moves
                       .len(),
                   2);
        assert_eq!(parse_position_params("fen xxx moves e2e4").ok().unwrap().moves.len(),
                   1);
        assert_eq!(parse_position_params("fen xxx moves e2e4").ok().unwrap().fen,
                   "xxx".to_string());
        assert_eq!(parse_position_params("fen   xxx    moves e2e4").ok().unwrap().fen,
                   "xxx".to_string());
        assert_eq!(parse_position_params("fen   xxx    moves").ok().unwrap().fen,
                   "xxx".to_string());
        assert_eq!(parse_position_params("fen   xxx   ").ok().unwrap().fen,
                   "xxx".to_string());
    }

    #[test]
    fn test_parse_uci_command() {
        use super::parse_uci_command;
        assert!(match parse_uci_command("uci").ok().unwrap() {
            UciCommand::Uci => true,
            _ => false,
        });
        assert!(match parse_uci_command("  uci  ").ok().unwrap() {
            UciCommand::Uci => true,
            _ => false,
        });
        assert!(match parse_uci_command(" foo  uci  ").ok().unwrap() {
            UciCommand::Uci => true,
            _ => false,
        });
        assert!(match parse_uci_command("isready  ").ok().unwrap() {
            UciCommand::IsReady => true,
            _ => false,
        });
        assert!(parse_uci_command("isready xxx ").is_err());
        assert!(parse_uci_command("quit xxx ").is_err());
        assert!(parse_uci_command("stop xxx ").is_err());
        assert!(parse_uci_command("ponderhit xxx ").is_err());
        assert!(match parse_uci_command("ponderhit  ").ok().unwrap() {
            UciCommand::PonderHit => true,
            _ => false,
        });
        assert!(match parse_uci_command(" foo quit  ").ok().unwrap() {
            UciCommand::Quit => true,
            _ => false,
        });
        assert!(match parse_uci_command("  stop  ").ok().unwrap() {
            UciCommand::Stop => true,
            _ => false,
        });
        assert!(match parse_uci_command("ucinewgame").ok().unwrap() {
            UciCommand::UciNewGame => true,
            _ => false,
        });
        assert!(match parse_uci_command("position startpos").ok().unwrap() {
            UciCommand::Position(_) => true,
            _ => false,
        });
        assert!(match parse_uci_command("position fen k7/8/8/8/8/8/8/7K w - - 0 1")
                          .ok()
                          .unwrap() {
            UciCommand::Position(_) => true,
            _ => false,
        });
        assert!(match parse_uci_command("position fen k7/8/8/8/8/8/8/7K w - - 0 1 moves h1h2")
                          .ok()
                          .unwrap() {
            UciCommand::Position(_) => true,
            _ => false,
        });
        assert!(match parse_uci_command("setoption name x value y").ok().unwrap() {
            UciCommand::SetOption(_) => true,
            _ => false,
        });
        assert!(match parse_uci_command("debug on").ok().unwrap() {
            UciCommand::Debug(true) => true,
            _ => false,
        });
        assert!(match parse_uci_command("go infinite").ok().unwrap() {
            UciCommand::Go(_) => true,
            _ => false,
        });
    }
}
