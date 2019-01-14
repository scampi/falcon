use crate::{
    errors::EvaluationError,
    parser::ast::{AssignType, CmpOperator},
};
use std::{collections::HashMap, fmt};

#[derive(Debug)]
pub enum VariableValue {
    Uninitialised,
    Scalar(ExprValue),
    Array(HashMap<String, ExprValue>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprValue {
    Uninitialised,
    Bool(bool),
    Number(f64),
    String(String),
}

impl From<f64> for ExprValue {
    fn from(value: f64) -> ExprValue {
        ExprValue::Number(value)
    }
}

impl From<usize> for ExprValue {
    fn from(value: usize) -> ExprValue {
        ExprValue::Number(value as f64)
    }
}

impl From<i32> for ExprValue {
    fn from(value: i32) -> ExprValue {
        ExprValue::Number(value as f64)
    }
}

impl From<bool> for ExprValue {
    fn from(value: bool) -> ExprValue {
        ExprValue::Bool(value)
    }
}

impl From<String> for ExprValue {
    fn from(value: String) -> ExprValue {
        ExprValue::String(value)
    }
}

impl fmt::Display for ExprValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprValue::Uninitialised => Ok(()),
            ExprValue::Number(n) => write!(f, "{}", n),
            ExprValue::String(s) => write!(f, "{}", s),
            ExprValue::Bool(b) => {
                if *b {
                    write!(f, "1")
                } else {
                    write!(f, "0")
                }
            },
        }
    }
}

impl ExprValue {
    pub fn as_string(&self) -> String {
        match self {
            ExprValue::Uninitialised => String::new(),
            ExprValue::Bool(v) => String::from(if *v { "1" } else { "0" }),
            ExprValue::String(s) => s.to_string(),
            ExprValue::Number(n) => n.to_string(),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            ExprValue::Uninitialised => false,
            ExprValue::Bool(v) => *v,
            ExprValue::String(s) => !s.is_empty(),
            ExprValue::Number(n) => *n != 0.0,
        }
    }

    pub fn as_number(&self) -> f64 {
        match self {
            ExprValue::Uninitialised => 0.0,
            ExprValue::Bool(v) => {
                if *v {
                    1.0
                } else {
                    0.0
                }
            },
            ExprValue::String(s) => match s.parse::<f64>() {
                Ok(n) => n,
                Err(_) => 0.0,
            },
            ExprValue::Number(n) => *n,
        }
    }

    pub fn compare(op: &CmpOperator, a: &ExprValue, b: &ExprValue) -> ExprValue {
        let res = match (a, b) {
            (ExprValue::Number(a), ExprValue::Number(b)) => op.compare(a, b),
            (ExprValue::Number(a), ExprValue::String(b)) => match b.parse::<f64>() {
                Ok(num) => op.compare(a, &num),
                Err(_) => op.compare(&a.to_string(), b),
            },
            (ExprValue::String(a), ExprValue::Number(b)) => match a.parse::<f64>() {
                Ok(num) => op.compare(&num, b),
                Err(_) => op.compare(a, &b.to_string()),
            },
            (ExprValue::Number(a), ExprValue::Uninitialised) => op.compare(a, &0.0),
            (ExprValue::Uninitialised, ExprValue::Number(b)) => op.compare(&0.0, b),
            (ExprValue::Uninitialised, ExprValue::Uninitialised) => op.compare(&0.0, &0.0),
            (ExprValue::String(a), ExprValue::String(b)) => op.compare(a, b),
            _ => unreachable!(),
        };
        ExprValue::Bool(res)
    }

    pub fn compute(
        op: &AssignType,
        a: ExprValue,
        b: ExprValue,
    ) -> Result<ExprValue, EvaluationError> {
        match op {
            AssignType::Pow => Ok(ExprValue::from(a.as_number().powf(b.as_number()))),
            AssignType::Mod => Ok(ExprValue::from(a.as_number() % b.as_number())),
            AssignType::Mul => Ok(ExprValue::from(a.as_number() * b.as_number())),
            AssignType::Div => {
                let bnum = b.as_number();
                if bnum == 0.0 {
                    return Err(EvaluationError::DivisionByZero);
                }
                Ok(ExprValue::from(a.as_number() / bnum))
            },
            AssignType::Add => Ok(ExprValue::from(a.as_number() + b.as_number())),
            AssignType::Sub => Ok(ExprValue::from(a.as_number() - b.as_number())),
            AssignType::Normal => Ok(b),
        }
    }
}
