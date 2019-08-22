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
        [string, offset] => {
            let string = string.eval(rt)?.as_string();
            let offset = offset.eval(rt)?.as_number();
            Ok(Value::from(substr(&string, offset as usize, string.len())))
        },
        [string, offset, len] => {
            let string = string.eval(rt)?.as_string();
            let offset = offset.eval(rt)?.as_number();
            let len = len.eval(rt)?.as_number();
            Ok(Value::from(substr(&string, offset as usize, len as usize)))
        },
        _ => Err(EvaluationError::InvalidNumberOfArguments(
            String::from("substr"),
            2,
            args.len(),
        )),
    }
}

/// Returns the substring of length len starting at offset.
///
/// The offset is 1-based.
fn substr(string: &str, offset: usize, len: usize) -> String {
    let offset = offset - 1;
    if offset > string.len() {
        String::new()
    } else if len > string.len() - offset {
        string[offset..].to_owned()
    } else {
        string[offset..offset + len].to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::substr;

    #[test]
    fn simple() {
        assert_eq!(substr("123", 42, 2), String::new());
        assert_eq!(substr("123", 2, 2), String::from("23"));
        assert_eq!(substr("123", 2, 42), String::from("23"));
        assert_eq!(substr("123", 1, 2), String::from("12"));
    }
}
