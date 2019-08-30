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
        [] => {
            let s = rt.record.get(0)?.as_string();
            Ok(Value::from(s.len()))
        },
        [s] => {
            let s = s.eval(rt)?.as_string();
            Ok(Value::from(s.len()))
        },
        _ => Err(EvaluationError::InvalidNumberOfArgumentsRange(
            String::from("length"),
            0,
            1,
            args.len(),
        )),
    }
}
