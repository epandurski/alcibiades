//! This module handles the "Universal Chess Interface" protocol
//! communication.

use regex::Regex;
use std::io;
use std::io::{Read, Write, BufRead, BufReader, BufWriter, ErrorKind};


/// A response from the engine to the GUI .
pub enum UciResponse {
    BestMove {
        best_move: String,
        ponder: Option<String>,
    },
    Info(String),
}


/// Description of a configuration option supported by the engine.
///
/// The GUI will use this information to configure the engine. Most
/// commonly it will build a dialog box according to the received
/// option descriptions so that GUI users can configure the engine
/// themselves.
pub struct OptionDescription {
    pub name: String,
    pub description: ValueDescription,
}


/// Description of a configurable value.
///
/// Configurable options can have several value types, depending on
/// their intended appearance in the GUI: check box, spin box, combo
/// box, string box, or button.
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


/// UCI protocol server.
pub struct Server<R, W, F, E>
    where R: Read,
          W: Write,
          F: EngineFactory<E>,
          E: Engine
{
    reader: BufReader<R>,
    writer: BufWriter<W>,
    engine_factory: F,
    engine: Option<E>,
}


impl<R, W, F, E> Server<R, W, F, E>
    where R: Read,
          W: Write,
          F: EngineFactory<E>,
          E: Engine
{
    /// Waits for a UCI handshake from the GUI and sends a proper
    /// response.
    ///
    /// Will return `Err` if the handshake was unsuccessful, or an IO
    /// error had occurred.
    pub fn wait_for_hanshake(in_stream: R, out_stream: W, engine_factory: F) -> io::Result<Self> {
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
        try!(write!(writer, "id name {}\n", engine_factory.name()));
        try!(write!(writer, "id author {}\n", engine_factory.author()));
        for opt in engine_factory.options() {
            try!(write!(writer,
                        "option name {} type {}\n",
                        opt.name,
                        match opt.description {
                            ValueDescription::Check { default } => {
                                format!("check defalut {}", default)
                            }
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
                        }));
        }
        try!(write!(writer, "uciok\n"));
        try!(writer.flush());
        Ok(Server {
            reader: reader,
            writer: writer,
            engine_factory: engine_factory,
            engine: None,
        })
    }

    /// Serves UCI commands until a "quit" command is received.
    ///
    /// May return `Err` if an IO error had occurred.
    pub fn serve(&mut self) -> io::Result<()> {
        let mut line = String::new();
        while try!(self.reader.read_line(&mut line)) > 0 {
            if let Ok(cmd) = parse_uci_command(line.as_str()) {
                if let UciCommand::Quit = cmd {
                    // "quit" command has been received from the GUI.
                    break;
                }
                if self.engine.is_none() {
                    // The UCI protocol requires that we do not
                    // initialize the engine before "isready",
                    // "setoption", or other non-"quit" command had
                    // been received.
                    self.engine = Some(self.engine_factory.create());
                }
                let engine = self.engine.as_mut().unwrap();

                match cmd {
                    UciCommand::IsReady => {
                        try!(write!(self.writer, "readyok\n"));
                        try!(self.writer.flush());
                    }
                    UciCommand::SetOption(SetOptionParams { name, value }) => {
                        engine.set_option(name.as_str(), value.as_str());
                    }
                    UciCommand::Position(PositionParams { fen, moves }) => {
                        engine.position(fen.as_str(), moves);
                    }
                    UciCommand::Stop => {
                        engine.stop();
                    }
                    UciCommand::UciNewGame => {
                        engine.new_game();
                    }
                    UciCommand::PonderHit => {
                        engine.ponder_hit();
                    }
                    UciCommand::Go(GoParams { searchmoves,
                                              ponder,
                                              wtime,
                                              btime,
                                              winc,
                                              binc,
                                              movestogo,
                                              depth,
                                              nodes,
                                              mate,
                                              movetime,
                                              infinite }) => {
                        engine.go(searchmoves,
                                  ponder,
                                  wtime,
                                  btime,
                                  winc,
                                  binc,
                                  movestogo,
                                  depth,
                                  nodes,
                                  mate,
                                  movetime,
                                  infinite);
                    }
                    UciCommand::Quit => panic!("This should not happen!"),
                }
            }
            line.clear();
        }
        Ok(())
    }
}


/// UCI-compatible chess engine factory.
pub trait EngineFactory<E: Engine> {
    /// Returns the name of the engine.
    fn name(&self) -> &str;

    /// Returns the author of the engine.
    fn author(&self) -> &str;

    /// Returns all configuration options supported by the engine.
    fn options(&self) -> Vec<OptionDescription>;

    /// Returns a fully initialized engine.
    fn create(&self) -> E;
}


/// UCI-compatible chess engine.
pub trait Engine {
    /// Sets a new value for a given configuration option.
    fn set_option(&mut self, name: &str, value: &str);

    /// Tells the engine that the next position will be from a
    /// different game.
    ///
    /// Does nothing if the engine is thinking at the moment.
    ///
    /// In practice, this method will clear the transposition tables.
    fn new_game(&mut self);

    /// Loads a new chess position.
    ///
    /// Does nothing if the engine is thinking at the moment.
    /// 
    /// `fen` will be the position represented in Forsyth–Edwards
    /// notation. `moves` is a list of moves played from the given
    /// position. The move format is in long algebraic
    /// notation. Examples: `e2e4`, `e7e5`, `e1g1` (white short
    /// castling), `e7e8q` (for promotion).
    fn position(&mut self, fen: &str, moves: Vec<String>);

    /// Tells the engine to start thinking.
    ///
    /// Does nothing if the engine is thinking at the moment.
    ///
    /// Engine's thinking can be influenced by many parameters:
    /// 
    /// * *searchmoves:* Restricts the search to a subset of moves
    /// only. The move format is in long algebraic notation. Examples:
    /// `e2e4`, `e7e5`, `e1g1` (white short castling), `e7e8q` (for
    /// promotion).
    /// 
    /// * *ponder:* Starts searching in pondering mode. The last move
    /// sent in in the position string is the ponder move. The engine
    /// can do what it wants to do, but after a `ponder_hit()` command
    /// it should execute the suggested move to ponder on. This means
    /// that the ponder move sent by the GUI can be interpreted as a
    /// recommendation about which move to ponder. However, if the
    /// engine decides to ponder on a different move, it should not
    /// display any mainlines as they are likely to be misinterpreted
    /// by the GUI because the GUI expects the engine to ponder on the
    /// suggested move.
    ///      
    /// * *wtime:* Milliseconds left on the white's clock.
    /// 
    /// * *btime:* Milliseconds left on the black's clock.
    /// 
    /// * *winc:* White increment per move in milliseconds.
    /// 
    /// * *binc:* Black increment per move in milliseconds.
    /// 
    /// * *movestogo:* The number of moves to the next time control.
    /// 
    /// * *depth:* Search to this depth (plies) only.
    /// 
    /// * *nodes:* Search that many nodes only.
    /// 
    /// * *mate:* Search for a mate in that many moves.
    /// 
    /// * *movetime:* Search for exactly that many milliseconds.
    /// 
    /// * *infinite:* Search until the `stop()` command. Do not exit
    /// the search without being told so in this mode!
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
          infinite: bool);

    /// Forces the engine to stop thinking and spit the best move it
    /// had found.
    ///
    /// Does nothing if the engine is not thinking at the moment.
    fn stop(&mut self);
    
    /// Returns the move on which the engine is pondering at the
    /// moment.
    ///
    /// Pondering is using the opponent's move time to consider likely
    /// opponent moves and thus gain a pre-processing advantage when
    /// it is our turn to move, this is also referred as "permanent
    /// brain".
    /// 
    /// The move format is in long algebraic notation. Examples:
    /// `e2e4`, `e7e5`, `e1g1` (white short castling), `e7e8q` (for
    /// promotion).  Returns `None` if the engine is not thinking in
    /// pondering mode at the moment.
    fn ponder_move(&self) -> Option<String>;
    
    /// Tells the engine that the move it is pondering on was played
    /// on the board.
    ///
    /// Does nothing if the engine is not thinking in pondering mode
    /// at the moment.
    fn ponder_hit(&mut self);

    /// Returns if the engine is thinking at the moment.
    fn is_thinking(&self) -> bool;
}


// A command from the GUI to the engine.
enum UciCommand {
    SetOption(SetOptionParams),
    IsReady,
    UciNewGame,
    Position(PositionParams),
    Go(GoParams),
    Stop,
    PonderHit,
    Quit,
}


// Parameters for `UciCommand::SetOption`.
struct SetOptionParams {
    name: String,
    value: String,
}


// Parameters for `UciCommand::Postion`.
struct PositionParams {
    fen: String,
    moves: Vec<String>,
}


// Parameters for `UciCommand::Go`.
struct GoParams {
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


// Represents a parse error.
struct ParseError;


// Tries to interpret a string as a UCI command.
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
            r"^name\s+(\S.*?)(?:\s+value\s+(.*?))?\s*$").unwrap();
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
        assert!(parse_setoption_params("name     ").is_err());
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
        use super::{parse_uci_command, UciCommand};
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
