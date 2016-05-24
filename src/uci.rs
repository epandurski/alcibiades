//! This module handles the "Universal Chess Interface" protocol
//! communication.

use regex::Regex;
use std::io;
use std::io::{Read, Write, BufRead, BufReader, BufWriter, ErrorKind};


/// A command from the GUI to the engine.
pub enum UciCommand {
    SetOption(SetOptionParams),
    IsReady,
    UciNewGame,
    Position(PositionParams),
    Go(GoParams),
    Stop,
    PonderHit,
    Quit,
}


/// Parameters for `UciCommand::SetOption`.
pub struct SetOptionParams {
    pub name: String,
    pub value: String,
}


/// Parameters for `UciCommand::Postion`.
pub struct PositionParams {
    pub fen: String,
    pub moves: Vec<String>,
}


/// Parameters for `UciCommand::Go`.
pub struct GoParams {
    pub searchmoves: Option<Vec<String>>,
    pub ponder: bool,
    pub wtime: Option<u64>,
    pub btime: Option<u64>,
    pub winc: Option<u64>,
    pub binc: Option<u64>,
    pub movestogo: Option<u64>,
    pub depth: Option<u64>,
    pub nodes: Option<u64>,
    pub mate: Option<u64>,
    pub movetime: Option<u64>,
    pub infinite: bool,
}


struct ParseError;


/// Tries to interpret a string as a UCI command.
fn parse_uci_command(s: &str) -> Result<UciCommand, ParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(r"\b({})\s*(?:\s(.*)|$)",
                    "setoption|isready|ucinewgame|\
                     position|go|stop|ponderhit|quit",  // UCI command
            ).as_str()
        ).unwrap();
    }
    if let Some(captures) = RE.captures(s) {
        let command_str = captures.at(1).unwrap();
        let params_str = captures.at(2).unwrap_or("");
        match command_str {
            "stop" => Ok(UciCommand::Stop),
            "quit" => Ok(UciCommand::Quit),
            "isready" => Ok(UciCommand::IsReady),
            "ponderhit" => Ok(UciCommand::PonderHit),
            "ucinewgame" => Ok(UciCommand::UciNewGame),
            "setoption" => Ok(UciCommand::SetOption(try!(parse_setoption_params(params_str)))),
            "position" => Ok(UciCommand::Position(try!(parse_position_params(params_str)))),
            "go" => Ok(UciCommand::Go(parse_go_params(params_str))),
            _ => Err(ParseError),
        }
    } else {
        Err(ParseError)
    }
}


// A helper function for `parse_uci_command`. It parses parameters for
// the "setoption" command.
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


// A helper function for `parse_uci_command`. It parses parameters for
// the "position" command.
fn parse_position_params(s: &str) -> Result<PositionParams, ParseError> {
    const STARTPOS: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1";
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(
                r"^(?:fen\s+(?P<fen>{})|startpos)(?:\s+moves(?P<moves>{}))?\s*$",
                r"[1-8KQRBNPkqrbnp/]+\s+[wb]\s+(?:[KQkq]{1,4}|-)\s+(?:[a-h][1-8]|-)\s+\d+\s+\d+",
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


// A helper function for `parse_uci_command`. It parses parameters for
// the "go" command.
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
/// descriptions so that GUI users can configure the engine too.
pub struct OptionDescription {
    pub name: String,
    pub description: ValueDescription,
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
pub struct UciServingLoop<R: Read, W: Write, E: UciEngine> {
    reader: BufReader<R>,
    writer: BufWriter<W>,
    engine: E,
    engine_is_started: bool,
    engine_is_thinking: bool,
}


impl<R: Read, W: Write, E: UciEngine> UciServingLoop<R, W, E> {
    pub fn wait_for_hanshake(in_stream: R, out_stream: W, engine: E) -> io::Result<Self> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"\buci(?:\s|$)").unwrap();
        }
        let mut reader = BufReader::new(in_stream);
        let mut writer = BufWriter::new(out_stream);
        let mut line = String::new();
        if try!(reader.read_line(&mut line)) == 0 {
            return Err(io::Error::new(ErrorKind::UnexpectedEof, "EOF"));
        }
        if !RE.is_match(line.as_str()) {
            return Err(io::Error::new(ErrorKind::Other, "unrecognized protocol"));
        }
        write!(writer, "id name {}\n", engine.name());
        write!(writer, "id author {}\n", engine.author());
        for opt in engine.options() {
            write!(writer,
                   "option name {} type {}\n",
                   opt.name,
                   match opt.description {
                       ValueDescription::Check { default } => format!("check defalut {}", default),
                       ValueDescription::Spin { default, min, max } => {
                           format!("spin defalut {} min {} max {}", default, min, max)
                       }
                       ValueDescription::Combo { default, list } => {
                           format!("combo default {}{}",
                                   default,
                                   list.into_iter().fold(String::new(), |mut acc, x| {
                                       acc.push_str(" var ");
                                       acc.push_str(x.as_str());
                                       acc
                                   }))
                       }
                       ValueDescription::String { default } => {
                           format!("string defalut {}", default)
                       }
                       ValueDescription::Button => "button".to_string(),
                   });
        }
        writer.flush();
        Ok(UciServingLoop {
            reader: reader,
            writer: writer,
            engine: engine,
            engine_is_started: false,
            engine_is_thinking: false,
        })
    }
}


pub trait UciEngine {
    fn name(&self) -> &str;
    fn author(&self) -> &str;
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
        assert_eq!(parse_position_params("fen 8/8/8/8/8/8/8/k6K w KQk e6 0 1 moves e2e4")
                       .ok()
                       .unwrap()
                       .moves
                       .len(),
                   1);
        assert_eq!(parse_position_params("fen   8/8/8/8/8/8/8/k6K w - - 0 1  moves e2e4")
                       .ok()
                       .unwrap()
                       .fen,
                   "8/8/8/8/8/8/8/k6K w - - 0 1".to_string());
        assert_eq!(parse_position_params("fen   8/8/8/8/8/8/8/k6K   w   -  -  0  1    moves e2e4")
                       .ok()
                       .unwrap()
                       .fen,
                   "8/8/8/8/8/8/8/k6K   w   -  -  0  1".to_string());
        assert_eq!(parse_position_params("fen   8/8/8/8/8/8/8/k6K w - - 0 1    moves")
                       .ok()
                       .unwrap()
                       .fen,
                   "8/8/8/8/8/8/8/k6K w - - 0 1".to_string());
        assert_eq!(parse_position_params("fen   8/8/8/8/8/8/8/k6K w - - 0 1   ").ok().unwrap().fen,
                   "8/8/8/8/8/8/8/k6K w - - 0 1".to_string());
    }

    #[test]
    fn test_parse_uci_command() {
        use super::parse_uci_command;
        assert!(match parse_uci_command("isready").ok().unwrap() {
            UciCommand::IsReady => true,
            _ => false,
        });
        assert!(match parse_uci_command("   isready  ").ok().unwrap() {
            UciCommand::IsReady => true,
            _ => false,
        });
        assert!(match parse_uci_command("isready  ").ok().unwrap() {
            UciCommand::IsReady => true,
            _ => false,
        });
        assert!(match parse_uci_command("isready xxx").ok().unwrap() {
            UciCommand::IsReady => true,
            _ => false,
        });
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
        assert!(parse_uci_command("position fen k7/8/8/8/8/8/8/7K w - - 0 1 moves h1h2 aabb")
                    .is_err());
        assert!(match parse_uci_command("setoption name x value y").ok().unwrap() {
            UciCommand::SetOption(_) => true,
            _ => false,
        });
        assert!(match parse_uci_command("go infinite").ok().unwrap() {
            UciCommand::Go(_) => true,
            _ => false,
        });
    }
}
