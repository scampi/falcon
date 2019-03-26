#![recursion_limit = "256"]

use crate::{
    errors::{EvaluationError, ParseError},
    interpreter::Runtime,
    parser::{ast::Program, program::parse_program},
};
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

#[derive(StructOpt, Debug)]
#[structopt(raw(setting = "AppSettings::AllowMissingPositional"))]
struct Cli {
    #[structopt(name = "file", short, long, parse(from_os_str))]
    /// The path to the AWK script file to execute
    program_file: Option<PathBuf>,
    /// The AWK script passed as argument
    #[structopt(name = "program")]
    program_str: Option<String>,
    #[structopt(parse(from_os_str))]
    /// Input files to run the AWK script over
    inputs: Vec<PathBuf>,
}

fn main() {
    let cli = Cli::from_args();

    let program = if let Some(program) = cli.program_str {
        get_program(&program)
    } else if let Some(program_file) = cli.program_file {
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
        get_program(&content)
    } else {
        Cli::clap().print_help().unwrap();
        process::exit(exitcode::USAGE);
    };
    if let Err(e) = run_program(program, cli.inputs) {
        eprintln!("{}", e);
        process::exit(exitcode::USAGE);
    }
    process::exit(exitcode::OK);
}

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

fn run_program(program: Program, inputs: Vec<PathBuf>) -> Result<(), EvaluationError> {
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    let mut rt = Runtime::new(program, &mut handle)?;

    rt.execute_begin_patterns()?;
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
    rt.execute_end_patterns()?;
    Ok(())
}
