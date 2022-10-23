//! Implements the "Universal Chess Interface" protocol communication.
//!
//! "Universal Chess Interface" (UCI) is an open communication
//! protocol for chess engines to play games automatically, that is to
//! communicate with other programs including Graphical User
//! Interfaces (GUI). UCI was designed and developed by Rudolf Huber
//! and Stefan Meyer-Kahlen, released in November 2000. The protocol
//! is independent of the operating system. For "Windows", the engine
//! is a normal "exe" file, either a console or "real" windows
//! application. All communication is done via standard input and
//! output with text commands.
//!
//! This module handles the low-level details of the UCI protocol. It
//! only requires the programmer to define a type that implements the
//! `UciEngine` trait. Then `run_engine` will handle the communication
//! with the GUI all by itself.

use std::default::Default;
use std::time::Duration;
use std::thread::{spawn, sleep};
use std::io;
use std::io::{Write, BufWriter, BufRead, ErrorKind};
use std::sync::mpsc::{channel, TryRecvError};
use regex::Regex;


/// A command from the GUI to the engine.
enum UciCommand {
    /// This is sent to the engine when the user wants to change the
    /// value of some configuration option supported by the engine.
    SetOption { name: String, value: String },

    /// This is used to synchronize the engine with the GUI.
    IsReady,

    /// This is sent to the engine when the next search (started with
    /// `UciCommand::Position` and `UciCommand::Go`) will be from a
    /// different game.
    UciNewGame,

    /// Set up the position described in `fen` and play the suppied
    /// `moves` on the internal chess board.
    Position { fen: String, moves: String },

    /// Start calculating on the current position set up with
    /// `UciCommand::Position`.
    Go(GoParams),

    ///	Stop calculating as soon as possible and send
    ///	`EngineReply::BestMove`.
    Stop,

    /// The user has played the expected move. This will be sent if
    /// the engine was told to ponder on the same move the user has
    /// played.
    PonderHit,

    /// Quit the program as soon as possible.
    Quit,
}


/// Parameters influencing engine's thinking.
#[derive(Default)]
pub struct GoParams {
    /// Restricts the search to a subset of moves only.
    ///
    /// No restrictions should be applied when the supplied list is
    /// empty. The move format is long algebraic notation. Examples:
    /// `e2e4`, `e7e5`, `e1g1` (white short castling), `e7e8q` (for
    /// promotion).
    pub searchmoves: Vec<String>,

    /// Whether to starts searching in pondering mode.
    ///
    /// When searching in pondering mode the last move sent in the
    /// "position" command is the ponder move. The engine can do what
    /// it wants to do, but after a "ponder hit" command it should
    /// execute the suggested move to ponder on. This means that the
    /// ponder move sent by the GUI can be interpreted as a
    /// recommendation about which move to ponder. However, if the
    /// engine decides to ponder on a different move, it should not
    /// display any mainlines as they are likely to be misinterpreted
    /// by the GUI because the GUI expects the engine to ponder on the
    /// suggested move.
    pub ponder: bool,

    /// Milliseconds left on white's clock.
    pub wtime: Option<u64>,

    /// Milliseconds left on black's clock.
    pub btime: Option<u64>,

    /// White increment per move in milliseconds.
    pub winc: Option<u64>,

    /// Black increment per move in milliseconds.
    pub binc: Option<u64>,

    /// The number of moves to the next time control.
    pub movestogo: Option<u64>,

    /// Search to this depth (plies) only.
    pub depth: Option<u64>,

    /// Search that many nodes only.
    pub nodes: Option<u64>,

    /// Search for a mate in that many moves.
    pub mate: Option<u64>,

    /// Search for exactly that many milliseconds.
    pub movetime: Option<u64>,

    /// Search until the "stop" command.
    ///
    /// The engine must not exit the search without being told so in
    /// this mode.
    pub infinite: bool,
}


/// An information item that the engine sends to the GUI.
///
/// The GUI requires the engine to send various types of information
/// during its working. Here are some standard ones:
///
/// * `"depth"`: The search depth in half-moves.
///
/// * `"time"`: The time searched in milliseconds, this should be sent
///   together with the PV.
///
/// * `"nodes"`: Nodes searched, the engine should send this info
///   regularly.
///
/// * `"pv"`: The best line found.
///
/// * `"multipv"`: For the multi PV mode.
///
/// * `"score"`: The score from the engine's point of view.
///
/// * `"nps"`: Nodes per second searched, the engine should send this
///   info regularly.
///
/// * `"string"`: Any string that will be displayed.
pub struct InfoItem {
    pub info_type: String,
    pub data: String,
}


/// A reply from the engine to the GUI.
///
/// The engine reply is either a best move found, or new/updated
/// information items. The move format is long algebraic
/// notation. Examples: `e2e4`, `e7e5`, `e1g1` (white short castling),
/// `e7e8q` (for promotion). If supplied, `ponder_move` is the
/// response on which the engine would like to ponder.
pub enum EngineReply {
    Info(Vec<InfoItem>),
    BestMove {
        best_move: String,
        ponder_move: Option<String>,
    },
}


/// Describes a configuration option.
///
/// Configurable options can be of several different types, depending
/// on their intended appearance in the GUI: check box, spin box,
/// combo box, string box, or button.
#[derive(Clone, Debug)]
pub enum OptionDescription {
    Check { default: bool },
    Spin { min: i32, max: i32, default: i32 },
    Combo { list: Vec<String>, default: String },
    String { default: String },
    Button,
}


/// A trait for announcing and changing configuration options.
pub trait SetOption {
    /// Returns a list of supported configuration options (name and
    /// description).
    fn options() -> Vec<(&'static str, OptionDescription)>
        where Self: Sized
    {
        vec![]
    }

    /// Updates the internal state to keep up with the new value for a
    /// given configuration option.
    ///
    /// * `name` gives the name of the configuration option.
    ///
    /// * `value` is the new value for the configuration option.
    ///
    /// Does nothing when called with unsupported configuration option
    /// name.
    #[allow(unused_variables)]
    fn set_option(name: &str, value: &str) where Self: Sized {}
}


/// A trait for UCI-compatible chess engines.
///
/// The methods in this trait, except the method `wait_for_reply`,
/// must not block the current thread.
pub trait UciEngine {
    /// Returns the name of the engine.
    fn name() -> &'static str;

    /// Returns the author of the engine.
    fn author() -> &'static str;

    /// Returns a list of supported configuration options (name and
    /// description).
    fn options() -> Vec<(&'static str, OptionDescription)>;

    /// Creates a new instance.
    ///
    /// `tt_size_mb` is the preferred size of the transposition table
    /// in Mbytes.
    fn new(tt_size_mb: Option<usize>) -> Self;

    /// Sets a new value for a given configuration option.
    fn set_option(&mut self, name: &str, value: &str);

    /// Tells the engine that the next position will be from a
    /// different game.
    ///
    /// In practice, this method will clear the transposition tables.
    fn new_game(&mut self);

    /// Loads a new chess position.
    ///
    /// `fen` will be the position represented in Forsyth–Edwards
    /// notation. `moves` is an iterator over the moves played from
    /// the given position. The move format is long algebraic
    /// notation. Examples: `e2e4`, `e7e5`, `e1g1` (white short
    /// castling), `e7e8q` (for promotion).
    fn position(&mut self, fen: &str, moves: &mut dyn Iterator<Item = &str>);

    /// Tells the engine to start thinking.
    fn go(&mut self, params: &GoParams);

    /// Forces the engine to stop thinking and reply with the best
    /// move it had found.
    fn stop(&mut self);

    /// Tells the engine that the move it is pondering on was played
    /// on the board.
    ///
    /// Pondering is using the opponent's move time to consider likely
    /// opponent moves and thus gain a pre-processing advantage when
    /// it is our turn to move.
    fn ponder_hit(&mut self);

    /// Waits for an engine reply, timing out after a specified
    /// duration or earlier.
    fn wait_for_reply(&mut self, duration: Duration) -> Option<EngineReply>;

    /// Terminates the engine permanently.
    ///
    /// After calling `exit`, no other methods on this instance should
    /// be called.
    fn exit(&mut self);
}


/// Serves UCI commands until a "quit" command is received.
///
/// The current thread will block until the UCI session is closed.
///
/// Returns `Err` if the handshake was unsuccessful, or if an IO error
/// occurred.
pub fn run_engine<E: UciEngine>() -> io::Result<()> {
    let mut server = Server::<E>::wait_for_hanshake()?;
    server.serve()
}


/// A UCI protocol server.
///
/// Connects the engine to the GUI.
struct Server<E: UciEngine> {
    engine: Option<E>,
}


impl<E: UciEngine> Server<E> {
    /// Waits for UCI handshake from the GUI.
    ///
    /// Will return `Err` if the handshake was unsuccessful, or if an
    /// IO error has occurred. The current thread will be blocked
    /// until the handshake is finalized.
    pub fn wait_for_hanshake() -> io::Result<Self> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"\buci(?:\s|$)").unwrap();
        }
        let stdin = io::stdin();
        let mut reader = stdin.lock();
        let mut writer = BufWriter::new(io::stdout());
        let mut line = String::new();
        if reader.read_line(&mut line)? == 0 {
            return Err(io::Error::new(ErrorKind::UnexpectedEof, "EOF"));
        }
        if !RE.is_match(line.as_str()) {
            return Err(io::Error::new(ErrorKind::Other, "unrecognized protocol"));
        }
        write!(writer, "id name {}\n", E::name())?;
        write!(writer, "id author {}\n", E::author())?;
        for (name, description) in E::options() {
            write!(writer,
                    "option name {} type {}\n",
                    name,
                    match description {
                        OptionDescription::Check { default } => {
                            format!("check default {}", default)
                        }
                        OptionDescription::Spin { default, min, max } => {
                            format!("spin default {} min {} max {}", default, min, max)
                        }
                        OptionDescription::Combo { default, list } => {
                            format!("combo default {}{}",
                                    default,
                                    list.into_iter()
                                        .fold(String::new(), |mut acc, x| {
                acc.push_str(" var ");
                acc.push_str(x.as_str());
                acc
            }))
                        }
                        OptionDescription::String { default } => {
                            format!("string default {}", default)
                        }
                        OptionDescription::Button => "button".to_string(),
                    })?;
        }
        write!(writer, "uciok\n")?;
        writer.flush()?;
        Ok(Server { engine: None })
    }

    /// Blocks the current thread and serves UCI commands until a
    /// "quit" command is received.
    ///
    /// Will return `Err` if an IO error has occurred.
    pub fn serve(&mut self) -> io::Result<()> {
        let mut writer = BufWriter::new(io::stdout());
        let (tx, rx) = channel();

        // Spawn a thread that reads from `stdin` and writes to `tx`.
        let read_thread = spawn(move || -> io::Result<()> {
            let stdin = io::stdin();
            let mut reader = stdin.lock();
            let mut line = String::new();
            loop {
                if let Ok(cmd) = match reader.read_line(&mut line)? {
                       0 => return Err(io::Error::new(ErrorKind::UnexpectedEof, "EOF")),
                       _ => parse_uci_command(line.as_str()),
                   } {
                    if let UciCommand::Quit = cmd {
                        return Ok(());
                    }
                    tx.send(cmd).unwrap();
                }
                line.clear();
            }
        });

        'mainloop: loop {
            // Try to receive commands from the GUI, pass them to the engine.
            'read_commands: while let Some(cmd) = match rx.try_recv() {
                                      Ok(cmd) => Some(cmd),
                                      Err(TryRecvError::Empty) => None,
                                      Err(TryRecvError::Disconnected) => break 'mainloop,
                                  } {
                let engine = if let Some(ref mut e) = self.engine {
                    e
                } else {
                    // Initialize the engine. (The UCI specification
                    // states that the "Hash" "setoption" command
                    // should be the first command passed to the
                    // engine.)
                    if let UciCommand::SetOption {
                               ref name,
                               ref value,
                           } = cmd {
                        if name == "Hash" {
                            let hash_size_mb = value.parse::<usize>().ok();
                            self.engine = Some(E::new(hash_size_mb));
                            continue 'read_commands;
                        }
                    }
                    self.engine = Some(E::new(None));
                    self.engine.as_mut().unwrap()
                };

                // Pass the received command to the engine.
                match cmd {
                    UciCommand::IsReady => {
                        write!(writer, "readyok\n")?;
                        writer.flush()?;
                    }
                    UciCommand::SetOption { name, value } => {
                        engine.set_option(name.as_str(), value.as_str());
                    }
                    UciCommand::Position { fen, moves } => {
                        engine.position(fen.as_str(), &mut moves.split_whitespace());
                    }
                    UciCommand::Stop => {
                        engine.stop();

                        // The "stop" command requires the engine to
                        // reply with a move immediately.
                        break 'read_commands;
                    }
                    UciCommand::UciNewGame => {
                        engine.new_game();
                    }
                    UciCommand::PonderHit => {
                        engine.ponder_hit();
                    }
                    UciCommand::Go(params) => {
                        engine.go(&params);
                    }
                    UciCommand::Quit => unreachable!(),
                }
            } // 'read_commands

            if let Some(ref mut engine) = self.engine {
                // Wait for engine replies, fetch them to `stdout`.
                let mut reply_count = 0;
                while let Some(reply) = engine.wait_for_reply(Duration::from_millis(25)) {
                    reply_count += 1;
                    match reply {
                        EngineReply::BestMove {
                            best_move,
                            ponder_move,
                        } => {
                            write!(writer,
                                        "bestmove {}{}",
                                        best_move,
                                        match ponder_move {
                                            None => "\n".to_string(),
                                            Some(m) => format!(" ponder {}\n", m),
                                        })?
                        }
                        EngineReply::Info(infos) => {
                            if infos.len() > 0 {
                                write!(writer, "info")?;
                                for InfoItem { info_type, data } in infos {
                                    write!(writer, " {} {}", info_type, data)?;
                                }
                                write!(writer, "\n")?;
                            }
                        }
                    }
                    if reply_count >= 40 {
                        // The engine is sending lots of replies, but
                        // we should not forget to process GUI
                        // commands as well.
                        break;
                    }
                }
                writer.flush()?;
            } else {
                // The engine is not initialized yet.
                sleep(Duration::from_millis(25));
            }
        } // 'mainloop

        // End the UCI session.
        if let Some(ref mut engine) = self.engine {
            engine.exit();
        }
        read_thread.join().unwrap()
    }
}


/// Represents a parse error.
struct ParseError;


fn parse_uci_command(s: &str) -> Result<UciCommand, ParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(r"\b({})\s*(?:\s(.*)|$)",
                    "setoption|isready|ucinewgame|\
                     position|go|stop|ponderhit|quit",
            ).as_str()
        ).unwrap();
    }
    if let Some(captures) = RE.captures(s) {
        let command_str = captures.get(1).unwrap().as_str();
        let params_str = captures.get(2).map_or("", |m| m.as_str());
        match command_str {
            "stop" => Ok(UciCommand::Stop),
            "quit" => Ok(UciCommand::Quit),
            "isready" => Ok(UciCommand::IsReady),
            "ponderhit" => Ok(UciCommand::PonderHit),
            "ucinewgame" => Ok(UciCommand::UciNewGame),
            "setoption" => parse_setoption_params(params_str),
            "position" => parse_position_params(params_str),
            "go" => parse_go_params(params_str),
            _ => Err(ParseError),
        }
    } else {
        Err(ParseError)
    }
}


fn parse_setoption_params(s: &str) -> Result<UciCommand, ParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"^name\s+(\S.*?)(?:\s+value\s+(.*?))?\s*$").unwrap();
    }
    if let Some(captures) = RE.captures(s) {
        Ok(UciCommand::SetOption {
               name: captures.get(1).unwrap().as_str().to_string(),
               value: captures.get(2).map_or("", |m| m.as_str()).to_string(),
           })
    } else {
        Err(ParseError)
    }
}


fn parse_position_params(s: &str) -> Result<UciCommand, ParseError> {
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
        Ok(UciCommand::Position {
               fen: if let Some(fen) = captures.name("fen") {
                   fen.as_str().to_string()
               } else {
                   STARTPOS.to_string()
               },
               moves: captures
                   .name("moves")
                   .map_or("", |m| m.as_str())
                   .to_string(),
           })
    } else {
        Err(ParseError)
    }
}


fn parse_go_params(s: &str) -> Result<UciCommand, ParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(
                r"\b(?P<keyword>{})(?:\s+(?P<number>\d+)|(?P<moves>{}))?(?:\s+|$)",
                "wtime|btime|winc|binc|movestogo|depth|\
                 nodes|mate|movetime|ponder|infinite|searchmoves",
                r"(?:\s+[a-h][1-8][a-h][1-8][qrbn]?)+",  // a non-empty list of moves
            ).as_str()
        ).unwrap();
    }
    let mut params = GoParams::default();
    for captures in RE.captures_iter(s) {
        let keyword = captures.name("keyword").unwrap();
        match keyword.as_str() {
            "searchmoves" => {
                if let Some(moves) = captures.name("moves") {
                    params.searchmoves = moves
                        .as_str()
                        .split_whitespace()
                        .map(|x| x.to_string())
                        .collect();
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
                    let field = match keyword.as_str() {
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
                    *field = number.as_str().parse::<u64>().ok();
                }
            }
        }
    }
    Ok(UciCommand::Go(params))
}


#[cfg(test)]
mod tests {
    #[test]
    fn parse_go_params() {
        use super::{parse_go_params, UciCommand};
        let params = [" wtime22000  ",
                      " wtime    22000  ",
                      "wtime 22000",
                      "wtime 99999999999999998888888888999999999999999999",
                      "wtime 22000",
                      "searchmoves   e2e4  c7c8q  ",
                      "searchmoves   e2e4  c7c8q,ponder  ",
                      "searchmoves aabb",
                      "infinite wtime 22000",
                      "wtime 22000 infinite btime 11000",
                      "wtime fdfee / 22000 infinite btime 11000 fdfds",
                      "wtime 22000 infinite btime 11000 ponder",
                      "searchmoves"];
        for (i, s) in params.iter().enumerate() {
            if let Some(UciCommand::Go(p)) = parse_go_params(s).ok() {
                match i {
                    0 => {
                        assert_eq!(p.wtime, None);
                    }
                    1 => {
                        assert_eq!(p.wtime, Some(22000));
                        assert_eq!(p.ponder, false);
                    }
                    2 => {
                        assert_eq!(p.wtime, Some(22000));
                    }
                    3 => {
                        assert_eq!(p.wtime, None);
                    }
                    4 => {
                        assert_eq!(p.infinite, false);
                    }
                    5 => {
                        assert_eq!(p.searchmoves, vec!["e2e4".to_string(), "c7c8q".to_string()]);
                    }
                    6 => {
                        assert_eq!(p.searchmoves, vec!["e2e4".to_string()]);
                    }
                    7 => {
                        assert!(p.searchmoves.is_empty());
                    }
                    8 => {
                        assert_eq!(p.wtime, Some(22000));
                        assert_eq!(p.infinite, true);
                    }
                    9 => {
                        assert_eq!(p.infinite, true);
                        assert_eq!(p.wtime, Some(22000));
                        assert_eq!(p.btime, Some(11000));
                    }
                    10 => {
                        assert_eq!(p.infinite, true);
                        assert_eq!(p.wtime, None);
                        assert_eq!(p.btime, Some(11000));
                    }
                    11 => {
                        assert_eq!(p.infinite, true);
                        assert_eq!(p.wtime, Some(22000));
                        assert_eq!(p.btime, Some(11000));
                        assert_eq!(p.ponder, true);
                        assert!(p.searchmoves.is_empty());
                    }
                    12 => {
                        assert!(p.searchmoves.is_empty());
                    }
                    _ => (),
                }
            } else {
                panic!("unsuccessful parsing: {}", s);
            }
        }
    }

    #[test]
    fn parse_setoption_params() {
        use super::{parse_setoption_params, UciCommand};
        let params = ["name   xxx  value   yyy  ",
                      "name xxx value yyy",
                      "name xxx   value  ",
                      "name xxx    "];
        for (i, s) in params.iter().enumerate() {
            if let Some(UciCommand::SetOption { name, value }) = parse_setoption_params(s).ok() {
                match i {
                    0 => {
                        assert_eq!(name, "xxx");
                        assert_eq!(value, "yyy");
                    }
                    1 => {
                        assert_eq!(name, "xxx");
                        assert_eq!(value, "yyy");
                    }
                    2 => {
                        assert_eq!(name, "xxx");
                        assert_eq!(value, "");
                    }
                    3 => {
                        assert_eq!(name, "xxx");
                        assert_eq!(value, "");
                    }
                    _ => (),
                }
            } else {
                panic!("unsuccessful parsing: {}", s);
            }
        }
        assert!(parse_setoption_params("name     ").is_err());
        assert!(parse_setoption_params("namexxx     ").is_err());
    }

    #[test]
    fn parse_position_params() {
        use super::{parse_position_params, UciCommand};
        let params = ["startpos  ",
                      "startpos ",
                      "startpos   moves  ",
                      "startpos   moves   e2e4   d2d4 ",
                      "fen 8/8/8/8/8/8/8/k6K w KQk e6 0 1 moves e2e4",
                      "fen   8/8/8/8/8/8/8/k6K w - - 0 1  moves e2e4",
                      "fen   8/8/8/8/8/8/8/k6K   w   -  -  0  1    moves e2e4",
                      "fen   8/8/8/8/8/8/8/k6K w - - 0 1    moves",
                      "fen   8/8/8/8/8/8/8/k6K w - - 0 1   "];
        for (i, s) in params.iter().enumerate() {
            if let Some(UciCommand::Position { fen, moves }) = parse_position_params(s).ok() {
                match i {
                    0 => {
                        assert_eq!(fen,
                                   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1");
                        assert_eq!(moves.len(), 0);
                    }
                    1 => {
                        assert_eq!(fen,
                                   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1");
                        assert_eq!(moves.len(), 0);
                    }
                    2 => {
                        assert_eq!(fen,
                                   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1");
                        assert_eq!(moves.len(), 0);
                    }
                    3 => {
                        assert_eq!(fen,
                                   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1");
                        assert_eq!(moves.split_whitespace().count(), 2);
                    }
                    4 => {
                        assert_eq!(moves.split_whitespace().count(), 1);
                    }
                    5 => {
                        assert_eq!(fen, "8/8/8/8/8/8/8/k6K w - - 0 1".to_string());
                        assert_eq!(moves.split_whitespace().count(), 1);
                    }
                    6 => {
                        assert_eq!(fen, "8/8/8/8/8/8/8/k6K   w   -  -  0  1".to_string());
                        assert_eq!(moves.split_whitespace().count(), 1);
                    }
                    7 => {
                        assert_eq!(fen, "8/8/8/8/8/8/8/k6K w - - 0 1".to_string());
                        assert_eq!(moves.len(), 0);
                    }
                    8 => {
                        assert_eq!(fen, "8/8/8/8/8/8/8/k6K w - - 0 1".to_string());
                        assert_eq!(moves.len(), 0);
                    }
                    _ => (),
                }
            } else {
                panic!("unsuccessful parsing: {}", s);
            }
        }
    }

    #[test]
    fn parse_uci_command() {
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
                    UciCommand::Position { .. } => true,
                    _ => false,
                });
        assert!(match parse_uci_command("position fen k7/8/8/8/8/8/8/7K w - - 0 1")
                          .ok()
                          .unwrap() {
                    UciCommand::Position { .. } => true,
                    _ => false,
                });
        assert!(match parse_uci_command("position fen k7/8/8/8/8/8/8/7K w - - 0 1 moves h1h2")
                          .ok()
                          .unwrap() {
                    UciCommand::Position { .. } => true,
                    _ => false,
                });
        assert!(parse_uci_command("position fen k7/8/8/8/8/8/8/7K w - - 0 1 moves h1h2 aabb")
                    .is_err());
        assert!(match parse_uci_command("setoption name x value y")
                          .ok()
                          .unwrap() {
                    UciCommand::SetOption { .. } => true,
                    _ => false,
                });
        assert!(match parse_uci_command("go infinite").ok().unwrap() {
                    UciCommand::Go(_) => true,
                    _ => false,
                });
    }
}
