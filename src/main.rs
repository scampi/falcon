#![recursion_limit = "256"]

use crate::{
    errors::{EvaluationError, ParseError},
    interpreter::Runtime,
    parser::{ast::Program, program::parse_program},
};
use atty;
use combine::{parser::Parser, stream::state::State};
use exitcode;
use std::{
    fs::{self, File},
    io::{self, BufRead, BufReader},
    path::PathBuf,
    process,
};
use structopt::{clap::AppSettings, StructOpt};

mod errors;
mod interpreter;
mod parser;

/// CLI options of the falcon binary.
#[derive(StructOpt, Debug)]
#[structopt(raw(setting = "AppSettings::ColoredHelp"))]
#[structopt(raw(setting = "AppSettings::ArgRequiredElseHelp"))]
struct Cli {
    #[structopt(name = "file", short, long, value_name = "FILE", parse(from_os_str))]
    /// The path to the AWK script file to execute.
    program_file: Option<PathBuf>,
    #[structopt(name = "dump-ast", long)]
    /// Dump the AST and exit.
    dump_ast: bool,
    #[structopt(name = "program")]
    /// The AWK script passed as argument.
    program_str: Option<String>,
    #[structopt(parse(from_os_str))]
    /// Input files to run the AWK script over.
    inputs: Vec<PathBuf>,
}

fn main() {
    let mut cli = Cli::from_args();

    let program = if let Some(program_file) = cli.program_file {
        if !program_file.is_file() {
            eprintln!(
                "Program file at {} could not be read or did not exist",
                program_file.display()
            );
            process::exit(exitcode::NOINPUT);
        }
        let content = match fs::read_to_string(&program_file) {
            Ok(p) => p,
            Err(e) => {
                eprintln!(
                    "Program file at {} could not be read.\n{}",
                    program_file.display(),
                    e
                );
                process::exit(exitcode::NOINPUT);
            },
        };
        if let Some(file) = cli.program_str {
            // in case the "file" option is used, the first input is interpreted as a value
            // of the program_str positional
            cli.inputs.insert(0, PathBuf::from(file));
        }
        get_program(&content)
    } else {
        get_program(&cli.program_str.unwrap())
    };
    if cli.dump_ast {
        println!("{:#?}", program);
    } else if let Err(e) = run_program(program, cli.inputs) {
        eprintln!("{}", e);
        process::exit(exitcode::USAGE);
    }
    process::exit(exitcode::OK);
}

/// Parse the the AWK script and returns the Program.
fn get_program(input: &str) -> Program {
    let prog = match parse_program().easy_parse(State::new(input)) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{}", e);
            process::exit(exitcode::USAGE);
        },
    };
    if !prog.1.input.is_empty() {
        eprintln!("{}", ParseError::LeftOver);
        process::exit(exitcode::SOFTWARE);
    } else {
        prog.0
    }
}

/// Executes the parsed AWK script against the set of input files. If there are
/// no input files, it reads data from standard input.
fn run_program(program: Program, inputs: Vec<PathBuf>) -> Result<(), EvaluationError> {
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    let mut rt = Runtime::new(program, &mut handle)?;

    rt.execute_begin_patterns()?;
    if !inputs.is_empty() {
        for input in inputs {
            let file = match File::open(&input) {
                Ok(f) => f,
                Err(e) => return Err(EvaluationError::IoError(input.display().to_string(), e)),
            };
            let filereader = BufReader::new(&file);
            for line in filereader.lines() {
                let line = match line {
                    Ok(l) => l,
                    Err(e) => return Err(EvaluationError::IoError(input.display().to_string(), e)),
                };
                rt.set_next_record(line);
                rt.execute_main_patterns()?;
            }
        }
    } else if atty::isnt(atty::Stream::Stdin) {
        let stdin = io::stdin();
        let handle = stdin.lock();
        for line in handle.lines() {
            rt.set_next_record(line?);
            rt.execute_main_patterns()?;
        }
    }
    rt.execute_end_patterns()?;
    Ok(())
}
