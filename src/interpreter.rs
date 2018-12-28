use crate::{errors::EvaluationError, parser::expr::CmpOperator};
use std::{borrow::Cow, collections::HashMap, fmt};

mod expr;

struct AwkVariables {
    ///// The number of arguments
    //argc: usize,
    ///// The command line arguments
    //argv: Vec<String>,
    //convfmt: String,
    //env: HashMap<String, String>,
    ///// The pathname to the current input file
    //filename: String,
    /// The ordinal number of the current record in the current file
    fnr: usize,
    /// Input field separator regular expression
    fs: String,
    /// The number of fields in the current record
    nf: usize,
    /// The ordinal number of the current record from the start of input
    nr: usize,
    //ofmt: String,
    //ofs: String,
    //ors: String,
    //rlength: usize,
    //rs: String,
    //rstart: usize,
    subsep: String,
}

impl AwkVariables {
    fn new() -> AwkVariables {
        AwkVariables {
            fnr: 0,
            fs: String::from(" "),
            nf: 0,
            nr: 0,
            subsep: String::new(),
        }
    }
}

struct Context<'a> {
    awk_vars: AwkVariables,
    vars: HashMap<&'a str, Value<'a>>,
    arrays: HashMap<&'a str, HashMap<String, Value<'a>>>,
    line: &'a str,
    fields: Vec<&'a str>,
}

impl<'a> Context<'a> {
    fn new() -> Context<'a> {
        Context {
            awk_vars: AwkVariables::new(),
            vars: HashMap::new(),
            arrays: HashMap::new(),
            line: "",
            fields: Vec::new(),
        }
    }

    fn set_line(&mut self, line: &'a str) {
        self.line = line;
        self.fields = line.split(&self.awk_vars.fs).collect();
        // update line numbers
        self.awk_vars.fnr += 1;
        self.awk_vars.nr += 1;
        self.awk_vars.nf = self.fields.len();
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Value<'a> {
    Uninitialised,
    Bool(bool),
    Number(f64),
    String(Cow<'a, str>),
}

impl<'a> From<f64> for Value<'a> {
    fn from(value: f64) -> Value<'a> {
        Value::Number(value)
    }
}

impl<'a> From<usize> for Value<'a> {
    fn from(value: usize) -> Value<'a> {
        Value::Number(value as f64)
    }
}

impl<'a> From<bool> for Value<'a> {
    fn from(value: bool) -> Value<'a> {
        Value::Bool(value)
    }
}

impl<'a> From<String> for Value<'a> {
    fn from(value: String) -> Value<'a> {
        Value::String(Cow::from(value))
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Uninitialised => Ok(()),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => {
                if *b {
                    write!(f, "1")
                } else {
                    write!(f, "0")
                }
            },
        }
    }
}

impl<'a> Value<'a> {
    fn as_string(&self) -> String {
        match self {
            Value::Uninitialised => String::new(),
            Value::Bool(v) => String::from(if *v { "1" } else { "0" }),
            Value::String(s) => s.to_string(),
            Value::Number(n) => n.to_string(),
        }
    }

    fn as_bool(&self) -> bool {
        match self {
            Value::Uninitialised => false,
            Value::Bool(v) => *v,
            Value::String(s) => !s.is_empty(),
            Value::Number(n) => *n != 0.0,
        }
    }

    fn as_number(&self) -> f64 {
        match self {
            Value::Uninitialised => 0.0,
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

    fn compare(op: &CmpOperator, a: &Value<'a>, b: &Value<'a>) -> Value<'a> {
        let res = match (a, b) {
            (Value::Number(a), Value::Number(b)) => op.compare(a, b),
            (Value::Number(a), Value::String(b)) => match b.parse::<f64>() {
                Ok(num) => op.compare(a, &num),
                Err(_) => op.compare(&Cow::from(a.to_string()), b),
            },
            (Value::String(a), Value::Number(b)) => match a.parse::<f64>() {
                Ok(num) => op.compare(&num, b),
                Err(_) => op.compare(a, &Cow::from(b.to_string())),
            },
            (Value::Number(a), Value::Uninitialised) => op.compare(a, &0.0),
            (Value::Uninitialised, Value::Number(b)) => op.compare(&0.0, b),
            (Value::Uninitialised, Value::Uninitialised) => op.compare(&0.0, &0.0),
            (Value::String(a), Value::String(b)) => op.compare(a, b),
            _ => unreachable!(),
        };
        Value::Bool(res)
    }
}

trait Eval {
    fn eval<'a>(&'a self, cxt: &mut Context<'a>) -> Result<Value, EvaluationError>;
}
