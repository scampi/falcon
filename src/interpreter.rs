use crate::{
    errors::EvaluationError,
    parser::expr::{CmpOperator, LValueType},
};
use std::collections::HashMap;

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

#[derive(Debug, PartialEq)]
enum Value<'a> {
    String(&'a str),
    Number(f64),
    Bool(bool),
}

impl<'a> From<f64> for Value<'a> {
    fn from(value: f64) -> Value<'a> {
        Value::Number(value)
    }
}

impl<'a> From<bool> for Value<'a> {
    fn from(value: bool) -> Value<'a> {
        Value::Bool(value)
    }
}

impl<'a> From<&'a String> for Value<'a> {
    fn from(value: &'a String) -> Value<'a> {
        Value::String(value)
    }
}

impl<'a> Value<'a> {
    fn as_number(&self) -> f64 {
        match self {
            Value::Bool(v) => {
                if *v {
                    1.0
                } else {
                    0.0
                }
            },
            Value::String(s) => match s.parse::<f64>() {
                Ok(n) => n,
                Err(_) => 0.0,
            },
            Value::Number(n) => *n,
        }
    }

    fn compare(op: &CmpOperator, a: &Value, b: &Value) -> Value<'a> {
        let res = match (a, b) {
            (Value::Number(a), Value::Number(b)) => op.compare(a, b),
            (Value::Number(a), Value::String(b)) => match b.parse::<f64>() {
                Ok(num) => op.compare(a, &num),
                Err(_) => op.compare(&a.to_string().as_str(), b),
            },
            (Value::String(a), Value::Number(b)) => match a.parse::<f64>() {
                Ok(num) => op.compare(&num, b),
                Err(_) => op.compare(a, &b.to_string().as_str()),
            },
            (Value::String(a), Value::String(b)) => op.compare(a, b),
            _ => unreachable!(),
        };
        Value::Bool(res)
    }
}

//#[derive(Default)]
//struct Runtime {
//vars: HashMap<LValueType, Value>,
//}

trait Eval {
    fn eval(&self, cxt: &Context) -> Result<Value, EvaluationError>;
}
