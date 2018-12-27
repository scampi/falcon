use parser::expr::LValueType;
use std::collections::HashMap;

mod errors;
mod expr;

#[derive(Default)]
struct AwkVariables {
    argc: usize,
    argv: Vec<String>,
    convfmt: String,
    env: HashMap<String, String>,
    filename: String,
    fnr: usize,
    fs: String,
    nf: usize,
    nr: usize,
    ofmt: String,
    ofs: String,
    ors: String,
    rlength: usize,
    rs: String,
    rstart: usize,
    subsep: String,
}

struct Context<'a> {
    vars: AwkVariables,
    line: String,
    split_line: Vec<&'a str>,
    //runtime: Runtime,
}

impl<'a> Context<'a> {
    fn new() -> Context<'a> {
        Context {
            vars: Default::default(),
            line: String::new(),
            split_line: Vec::new(),
        }
    }
}

#[derive(Debug)]
enum Value<'a> {
    None,
    String(&'a str),
    Number(f64),
}

impl<'a> Value<'a> {
    fn as_number(&self) -> f64 {
        match self {
            Value::None => 0.0,
            Value::String(s) => match s.parse::<f64>() {
                Ok(n) => n,
                Err(_) => 0.0,
            },
            Value::Number(n) => *n,
        }
    }
}

//#[derive(Default)]
//struct Runtime {
//vars: HashMap<LValueType, Value>,
//}

trait Eval {
    fn eval(&self, cxt: &Context) -> Result<Value, errors::EvaluationError>;
}
