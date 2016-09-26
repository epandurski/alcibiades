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
//! only requires the programmer to define two `struct`s that
//! implement the `UciEngine` and `UciEngineFactory` traits. Then
//! `Server` will handle the communication with the GUI all by itself.

use std::time;
use std::thread;
use std::io;
use std::io::{Write, BufWriter, BufRead, ErrorKind};
use std::sync::mpsc::{channel, TryRecvError};
use regex::Regex;


/// A command from the GUI to the engine.
enum UciCommand {
    /// This is sent to the engine when the user wants to change the
    /// value of some internal parameter of the engine.
    SetOption {
        name: String,
        value: String,
    },

    /// This is used to synchronize the engine with the GUI.
    IsReady,

    /// This is sent to the engine when the next search (started with
    /// `UciCommand::Position` and `UciCommand::Go`) will be from a
    /// different game.
    UciNewGame,

    /// Set up the position described in `fen` and play the suppied
    /// `moves` on the internal chess board.
    Position {
        fen: String,
        moves: String,
    },

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


/// A reply from the engine to the GUI.
///
/// The engine reply is either a best move found, or a new/updated
/// information item. The move format is long algebraic
/// notation. Examples: `e2e4`, `e7e5`, `e1g1` (white short castling),
/// `e7e8q` (for promotion). If supplied, `ponder_move` is the
/// response on which the engine would like to ponder.
pub enum EngineReply {
    BestMove {
        best_move: String,
        ponder_move: Option<String>,
    },
    Info(Vec<(InfoType, String)>),
}


/// Specific information article that the engine sends to the GUI.
///
/// There are many standard types of information that GUIs visualize
/// and therefore expect the engine to send. Here are some of the most
/// important ones:
///
/// * `"depth"`: search depth in plies;
/// 
/// * `"time"`: the time searched in milliseconds, this should be sent
///   together with the PV;
/// 
/// * `"nodes"`: nodes searched, the engine should send this info
///   regularly;
/// 
/// * `"pv"`: the best line found;
/// 
/// * `"multipv"`: for the multi PV mode;
///
/// * `"score"`: the score from the engine's point of view;
///
/// * `"nps"`: nodes per second searched, the engine should send this
///   info regularly;
/// 
/// * `"string"`: any string that will be displayed;
///
/// * `"currmove"`: currently searching this move;
/// 
/// * `"currmovenumber"`: currently searching this move number;
/// 
/// * `"currline"`: the current line the engine is calculating.
pub type InfoType = String;


/// Name of a configuration option supported by the engine.
///
/// Examples of option names supported by many popular chess engines:
///
/// * `"Hash"`
/// * `"OwnBook"`
/// * `"MultiPV"`
/// * `"UCI_AnalyseMode"`
pub type OptionName = String;


/// Describes a configuration option supported by the engine.
///
/// Configurable options can be of several different types, depending
/// on their intended appearance in the GUI: check box, spin box,
/// combo box, string box, or button.
pub enum OptionDescription {
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


/// UCI protocol server -- connects the engine to the GUI.
///
/// # Example:
/// ```rust
/// use uci;
/// 
/// fn main() {
///     if let Ok(mut server) = uci::Server::wait_for_hanshake(MyEngineFactory) {
///         match server.serve() {
///             Ok(_) => {
///                 exit(0);
///             }
///             Err(_) => {
///                 exit(1);
///             }
///         }
///     }
///     exit(2);
/// }
/// ```
pub struct Server<F, E>
    where F: UciEngineFactory<E>,
          E: UciEngine
{
    engine_factory: F,
    engine: Option<E>,
}


impl<F, E> Server<F, E>
    where F: UciEngineFactory<E>,
          E: UciEngine
{
    /// Waits for UCI handshake from the GUI.
    ///
    /// Will return `Err` if the handshake was unsuccessful, or if an
    /// IO error had occurred. The current thread will be blocked
    /// until the handshake is finalized.
    pub fn wait_for_hanshake(engine_factory: F) -> io::Result<Self> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"\buci(?:\s|$)").unwrap();
        }
        let stdin = io::stdin();
        let mut reader = stdin.lock();
        let mut writer = BufWriter::new(io::stdout());
        let mut line = String::new();
        if try!(reader.read_line(&mut line)) == 0 {
            return Err(io::Error::new(ErrorKind::UnexpectedEof, "EOF"));
        }
        if !RE.is_match(line.as_str()) {
            return Err(io::Error::new(ErrorKind::Other, "unrecognized protocol"));
        }
        try!(write!(writer, "id name {}\n", engine_factory.name()));
        try!(write!(writer, "id author {}\n", engine_factory.author()));
        for (name, description) in engine_factory.options() {
            try!(write!(writer,
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
                                        list.into_iter().fold(String::new(), |mut acc, x| {
                                            acc.push_str(" var ");
                                            acc.push_str(x.as_str());
                                            acc
                                        }))
                            }
                            OptionDescription::String { default } => {
                                format!("string default {}", default)
                            }
                            OptionDescription::Button => "button".to_string(),
                        }));
        }
        try!(write!(writer, "uciok\n"));
        try!(writer.flush());
        Ok(Server {
            engine_factory: engine_factory,
            engine: None,
        })
    }

    /// Blocks the current thread and serves UCI commands until a
    /// "quit" command is received.
    ///
    /// Will return `Err` if an IO error had occurred.
    pub fn serve(&mut self) -> io::Result<()> {
        let mut writer = BufWriter::new(io::stdout());
        let (tx, rx) = channel();

        // Spawn a thread that reads from `stdin` and writes to `tx`.
        let read_thread = thread::spawn(move || -> io::Result<()> {
            let stdin = io::stdin();
            let mut reader = stdin.lock();
            let mut line = String::new();
            loop {
                if let Ok(cmd) = match reader.read_line(&mut line) {
                    Err(x) => return Err(x),
                    Ok(0) => return Err(io::Error::new(ErrorKind::UnexpectedEof, "EOF")),
                    Ok(_) => parse_uci_command(line.as_str()),
                } {
                    // Check if this is a "quit" command. The UCI protocol
                    // requires that we do not initialize the engine
                    // before "isready", "setoption", or other non-"quit"
                    // command had been received.
                    if let UciCommand::Quit = cmd {
                        return Ok(());
                    }
                    if tx.send(cmd).is_err() {
                        // Normally, this should not happen.
                        return Err(io::Error::new(ErrorKind::Other, "broken channel"));
                    }
                }
                line.clear();
            }
        });

        'mainloop: loop {

            // Try to read a command from the GUI, fetch it to the
            // engine.
            while let Some(cmd) = match rx.try_recv() {
                Ok(cmd) => Some(cmd),
                Err(TryRecvError::Empty) => None,
                Err(TryRecvError::Disconnected) => break 'mainloop,
            } {
                // Initialize the engine if necessery.
                let engine = match self.engine {
                    None => {
                        // The UCI specification states that the
                        // "Hash" `setoption` command, should be the
                        // first command passed to the engine.
                        if let UciCommand::SetOption { ref name, ref value } = cmd {
                            if name == "Hash" {
                                self.engine = Some(self.engine_factory
                                                       .create(value.parse::<usize>().ok()));
                                continue;
                            }
                        }
                        self.engine = Some(self.engine_factory.create(None));
                        self.engine.as_mut().unwrap()
                    }
                    Some(ref mut x) => x,
                };

                // Fetch the received command to the engine. (Except
                // for the "isready" command, to which we can reply
                // directly.)
                match cmd {
                    UciCommand::IsReady => {
                        try!(write!(writer, "readyok\n"));
                        try!(writer.flush());
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
                        // send the best move. Here we break the
                        // read-command cycle so that the engine can
                        // reply before the next command.
                        break;
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
                    UciCommand::Quit => panic!("This should never happen!"),
                }
            }

            // If the engine is instantiated -- try to read replies.
            if let Some(ref mut engine) = self.engine {
                let mut count = 0;
                while let Some(reply) = engine.get_reply() {

                    // Fetch the reply to `stdout`.
                    match reply {
                        EngineReply::BestMove { best_move, ponder_move } => {
                            try!(write!(writer,
                                        "bestmove {}{}",
                                        best_move,
                                        match ponder_move {
                                            None => "\n".to_string(),
                                            Some(m) => format!(" ponder {}\n", m),
                                        }))
                        }
                        EngineReply::Info(infos) => {
                            if infos.len() > 0 {
                                try!(write!(writer, "info"));
                                for (name, value) in infos {
                                    try!(write!(writer, " {} {}", name, value));
                                }
                                try!(write!(writer, "\n"));
                            }
                        }
                    }

                    // Make sure a crazy engine can not block the
                    // mainloop with too many replies.
                    if count >= 50 {
                        break;
                    } else {
                        count += 1;
                    }
                }
                try!(writer.flush());
            }

            // Yield to another thread.
            thread::sleep(time::Duration::from_millis(25));
        }

        // End of the UCI session.
        if let Some(ref mut engine) = self.engine {
            engine.exit();
        }
        read_thread.join().unwrap()
    }
}


/// UCI-compatible chess engine factory.
pub trait UciEngineFactory<E: UciEngine> {
    /// Returns the name of the engine.
    fn name(&self) -> String;

    /// Returns the author of the engine.
    fn author(&self) -> String;

    /// Returns all configuration options supported by the engine.
    ///
    /// The GUI will use this information to configure the
    /// engine. Most commonly it will build a dialog box according to
    /// the received option names and descriptions so that GUI users
    /// can configure the engine themselves.
    fn options(&self) -> Vec<(OptionName, OptionDescription)>;

    /// Returns a fully initialized engine.
    ///
    /// `hash_size_mb` is the preferred total size of the hash tables
    /// in Mbytes.
    fn create(&self, hash_size_mb: Option<usize>) -> E;
}


/// UCI-compatible chess engine.
///
/// Methods in this trait **must not block** the current thread. This
/// means that all the engine calculations should be done in a
/// separate thread(s).
pub trait UciEngine {
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
    fn position(&mut self, fen: &str, moves: &mut Iterator<Item = &str>);

    /// Tells the engine to start thinking.
    ///
    /// Engine's thinking can be influenced by many parameters:
    /// 
    /// * *searchmoves:* Restricts the search to a subset of moves
    ///   only. The move format is long algebraic notation. Examples:
    ///   `e2e4`, `e7e5`, `e1g1` (white short castling), `e7e8q` (for
    ///   promotion).
    /// 
    /// * *ponder:* Starts searching in pondering mode. The last move
    ///   sent in in the position string is the ponder move. The
    ///   engine can do what it wants to do, but after a
    ///   `ponder_hit()` command it should execute the suggested move
    ///   to ponder on. This means that the ponder move sent by the
    ///   GUI can be interpreted as a recommendation about which move
    ///   to ponder. However, if the engine decides to ponder on a
    ///   different move, it should not display any mainlines as they
    ///   are likely to be misinterpreted by the GUI because the GUI
    ///   expects the engine to ponder on the suggested move.
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
    ///   the search without being told so in this mode!
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

    /// Checks if there is an engine reply available.
    ///
    /// The engine reply can be either `EngineReply::BestMove`
    /// indicating the best move found, or `EngineReply::Info`
    /// indicating a new/updated information item.
    fn get_reply(&mut self) -> Option<EngineReply>;

    /// Terminates the engine permanently.
    ///
    /// After calling `exit`, no other methods on this instance should
    /// be called.
    fn exit(&mut self);
}


/// Represents a parse error.
struct ParseError;


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
            "setoption" => Ok(try!(parse_setoption_params(params_str))),
            "position" => Ok(try!(parse_position_params(params_str))),
            "go" => Ok(UciCommand::Go(parse_go_params(params_str))),
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
            name: captures.at(1).unwrap().to_string(),
            value: captures.at(2).unwrap_or("").to_string(),
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
                fen.to_string()
            } else {
                STARTPOS.to_string()
            },
            moves: captures.name("moves").unwrap_or("").to_string(),
        })
    } else {
        Err(ParseError)
    }
}


/// Parameters for `UciCommand::Go`.
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
                    }
                    1 => {
                        assert_eq!(value, "yyy");
                    }
                    2 => {
                        assert_eq!(value, "");
                    }
                    3 => {
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
    fn test_parse_position_params() {
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
                    }
                    1 => {
                        assert_eq!(moves.len(), 0);
                    }
                    2 => {
                        assert_eq!(moves.len(), 0);
                    }
                    3 => {
                        assert_eq!(moves.split_whitespace().count(), 2);
                    }
                    4 => {
                        assert_eq!(moves.split_whitespace().count(), 1);
                    }
                    5 => {
                        assert_eq!(fen, "8/8/8/8/8/8/8/k6K w - - 0 1".to_string());
                    }
                    6 => {
                        assert_eq!(fen, "8/8/8/8/8/8/8/k6K   w   -  -  0  1".to_string());
                    }
                    7 => {
                        assert_eq!(fen, "8/8/8/8/8/8/8/k6K w - - 0 1".to_string());
                    }
                    8 => {
                        assert_eq!(fen, "8/8/8/8/8/8/8/k6K w - - 0 1".to_string());
                    }
                    _ => (),
                }
            } else {
                panic!("unsuccessful parsing: {}", s);
            }
        }
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
        assert!(match parse_uci_command("setoption name x value y").ok().unwrap() {
            UciCommand::SetOption { .. } => true,
            _ => false,
        });
        assert!(match parse_uci_command("go infinite").ok().unwrap() {
            UciCommand::Go(_) => true,
            _ => false,
        });
    }
}
