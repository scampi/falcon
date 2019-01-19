use crate::{
    errors::EvaluationError,
    parser::ast::{AssignType, CmpOperator},
};
use std::{collections::HashMap, fmt};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Uninitialised,
    Bool(bool),
    Number(f64),
    String(String),
    Array(HashMap<String, Value>),
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

impl From<i32> for Value {
    fn from(value: i32) -> Value {
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
            Value::Array(_) => unreachable!(),
        }
    }
}

impl Value {
    pub fn is_scalar(&self) -> bool {
        match self {
            Value::Bool(_) | Value::String(_) | Value::Number(_) => true,
            _ => false,
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Value::Array(..) => unreachable!(),
            Value::Uninitialised => String::new(),
            Value::Bool(v) => String::from(if *v { "1" } else { "0" }),
            Value::String(s) => s.to_string(),
            Value::Number(n) => n.to_string(),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Array(..) => unreachable!(),
            Value::Uninitialised => false,
            Value::Bool(v) => *v,
            Value::String(s) => !s.is_empty(),
            Value::Number(n) => *n != 0.0,
        }
    }

    pub fn as_number(&self) -> f64 {
        match self {
            Value::Array(..) => unreachable!(),
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

    pub fn compare(op: &CmpOperator, a: &Value, b: &Value) -> Value {
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

    pub fn compute(op: &AssignType, a: Value, b: Value) -> Result<Value, EvaluationError> {
        match op {
            AssignType::Pow => Ok(Value::from(a.as_number().powf(b.as_number()))),
            AssignType::Mod => Ok(Value::from(a.as_number() % b.as_number())),
            AssignType::Mul => Ok(Value::from(a.as_number() * b.as_number())),
            AssignType::Div => {
                let bnum = b.as_number();
                if bnum == 0.0 {
                    return Err(EvaluationError::DivisionByZero);
                }
                Ok(Value::from(a.as_number() / bnum))
            },
            AssignType::Add => Ok(Value::from(a.as_number() + b.as_number())),
            AssignType::Sub => Ok(Value::from(a.as_number() - b.as_number())),
            AssignType::Normal => Ok(b),
        }
    }
}
