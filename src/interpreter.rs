use crate::{
    errors::EvaluationError,
    parser::expr::{AssignType, CmpOperator},
};
use std::{collections::HashMap, fmt};

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

    fn get(&self, field: &str) -> Option<Value> {
        match field {
            "FNR" => Some(Value::from(self.fnr)),
            "FS" => Some(Value::from(self.fs.to_owned())),
            "NF" => Some(Value::from(self.nf)),
            "NR" => Some(Value::from(self.nr)),
            "SUBSEP" => Some(Value::from(self.subsep.to_owned())),
            _ => None,
        }
    }

    fn set(&mut self, field: &str, value: &Value) -> bool {
        match field {
            "FNR" => self.fnr = value.as_number() as usize,
            "FS" => self.fs = value.as_string(),
            "NF" => self.nf = value.as_number() as usize,
            "NR" => self.nr = value.as_number() as usize,
            "SUBSEP" => self.subsep = value.as_string(),
            _ => return false,
        }
        true
    }
}

struct Context {
    awk_vars: AwkVariables,
    vars: HashMap<String, Value>,
    arrays: HashMap<String, HashMap<String, Value>>,
    record: String,
    fields: Vec<String>,
}

impl Context {
    fn new() -> Context {
        Context {
            awk_vars: AwkVariables::new(),
            vars: HashMap::new(),
            arrays: HashMap::new(),
            record: String::new(),
            fields: Vec::new(),
        }
    }

    fn set_next_record(&mut self, record: String) {
        self.update_record(record);
        // update record numbers
        self.awk_vars.fnr += 1;
        self.awk_vars.nr += 1;
    }

    fn update_record(&mut self, record: String) {
        self.record = record;
        self.fields = self
            .record
            .split(&self.awk_vars.fs)
            .map(|s| s.to_owned())
            .collect();
        self.awk_vars.nf = self.fields.len();
    }

    fn update_field<F>(&mut self, index: usize, field: F) -> Result<(), EvaluationError>
    where
        F: Fn(Option<&str>) -> Result<String, EvaluationError>,
    {
        match self.fields.get_mut(index) {
            Some(f) => *f = field(Some(f))?,
            None => {
                for _ in 0..index - self.fields.len() {
                    self.fields.push(String::new());
                }
                self.fields.push(field(None)?);
            },
        }
        self.record = self.fields.join(&self.awk_vars.fs);
        self.awk_vars.nf = self.fields.len();
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Uninitialised,
    Bool(bool),
    Number(f64),
    String(String),
}

impl From<f64> for Value {
    fn from(value: f64) -> Value {
        Value::Number(value)
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Value {
        Value::Number(value as f64)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Value {
        Value::Bool(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Value {
        Value::String(value)
    }
}

impl fmt::Display for Value {
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

impl Value {
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

    fn compare(op: &CmpOperator, a: &Value, b: &Value) -> Value {
        let res = match (a, b) {
            (Value::Number(a), Value::Number(b)) => op.compare(a, b),
            (Value::Number(a), Value::String(b)) => match b.parse::<f64>() {
                Ok(num) => op.compare(a, &num),
                Err(_) => op.compare(&a.to_string(), b),
            },
            (Value::String(a), Value::Number(b)) => match a.parse::<f64>() {
                Ok(num) => op.compare(&num, b),
                Err(_) => op.compare(a, &b.to_string()),
            },
            (Value::Number(a), Value::Uninitialised) => op.compare(a, &0.0),
            (Value::Uninitialised, Value::Number(b)) => op.compare(&0.0, b),
            (Value::Uninitialised, Value::Uninitialised) => op.compare(&0.0, &0.0),
            (Value::String(a), Value::String(b)) => op.compare(a, b),
            _ => unreachable!(),
        };
        Value::Bool(res)
    }

    fn compute(op: &AssignType, a: &Value, b: &Value) -> Result<f64, EvaluationError> {
        let result = match op {
            AssignType::Pow => a.as_number().powf(b.as_number()),
            AssignType::Mod => a.as_number() % b.as_number(),
            AssignType::Mul => a.as_number() * b.as_number(),
            AssignType::Div => {
                let bnum = b.as_number();
                if bnum == 0.0 {
                    return Err(EvaluationError::DivisionByZero);
                }
                a.as_number() / bnum
            },
            AssignType::Add => a.as_number() + b.as_number(),
            AssignType::Sub => a.as_number() - b.as_number(),
            _ => unreachable!(),
        };
        Ok(result)
    }
}

trait Eval {
    fn eval(&self, cxt: &mut Context) -> Result<Value, EvaluationError>;
}
