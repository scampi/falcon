use crate::{
    errors::EvaluationError,
    interpreter::{value::Value, Eval, RuntimeMut},
    parser::ast::ExprList,
};
use std::io::Write;

pub fn execute<Output: Write>(
    args: &ExprList,
    rt: &mut RuntimeMut<'_, Output>,
) -> Result<Value, EvaluationError> {
    match args.0.as_slice() {
        [s] => {
            let s = s.eval(rt)?.as_string();
            Ok(Value::from(s.to_uppercase()))
        },
        _ => Err(EvaluationError::InvalidNumberOfArguments(
            String::from("toupper"),
            1,
            args.len(),
        )),
    }
}
