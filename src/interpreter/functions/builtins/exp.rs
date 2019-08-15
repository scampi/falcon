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
        [x] => {
            let x = x.eval(rt)?.as_number();
            Ok(Value::from(x.exp()))
        },
        _ => Err(EvaluationError::InvalidNumberOfArguments(
            String::from("exp"),
            1,
            args.len(),
        )),
    }
}
