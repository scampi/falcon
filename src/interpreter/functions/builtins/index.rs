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
        [string, occ] => {
            let string = string.eval(rt)?.as_string();
            let occ = occ.eval(rt)?.as_string();
            Ok(Value::from(if let Some(index) = string.find(&occ) {
                index + 1
            } else {
                0
            }))
        },
        _ => Err(EvaluationError::InvalidNumberOfArguments(
            String::from("index"),
            2,
            args.len(),
        )),
    }
}
