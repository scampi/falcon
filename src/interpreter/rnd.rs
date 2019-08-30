use crate::{
    errors::EvaluationError,
    interpreter::{value::Value, Eval, RuntimeMut},
    parser::ast::ExprList,
};
use rand::prelude::*;
use std::{
    io::Write,
    time::{SystemTime, UNIX_EPOCH},
};

#[derive(Debug)]
pub struct Rnd {
    rng: StdRng,
    seed: Option<u64>,
}

impl Rnd {
    pub fn new() -> Rnd {
        Rnd {
            rng: StdRng::from_entropy(),
            seed: None,
        }
    }

    pub fn srand_arg<Output: Write>(
        args: &ExprList,
        rt: &mut RuntimeMut<'_, Output>,
    ) -> Result<u64, EvaluationError> {
        match args.0.as_slice() {
            [] => {
                let start = SystemTime::now();
                let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap().as_secs();
                Ok(since_the_epoch)
            },
            [seed] => Ok(seed.eval(rt)?.as_number().trunc() as u64),
            _ => Err(EvaluationError::InvalidNumberOfArgumentsRange(
                String::from("srand"),
                0,
                1,
                args.len(),
            )),
        }
    }

    pub fn srand(&mut self, seed: u64) -> Result<Value, EvaluationError> {
        self.rng = StdRng::seed_from_u64(seed);
        match self.seed.replace(seed) {
            Some(prev) => Ok(Value::from(prev as f64)),
            None => Ok(Value::from(seed as f64)),
        }
    }

    pub fn rand(&mut self, args: &ExprList) -> Result<Value, EvaluationError> {
        match args.0.as_slice() {
            [] => Ok(Value::from(self.rng.gen::<f64>())),
            _ => Err(EvaluationError::InvalidNumberOfArguments(
                String::from("rand"),
                0,
                args.len(),
            )),
        }
    }
}
