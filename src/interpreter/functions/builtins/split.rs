use crate::{
    errors::EvaluationError,
    interpreter::{value::Value, Eval, RuntimeMut},
    parser::ast::{AssignType, Expr, ExprList, LValueType},
};
use lazy_static::lazy_static;
use regex::Regex;
use std::io::Write;

pub fn execute<Output: Write>(
    args: &ExprList,
    rt: &mut RuntimeMut<'_, Output>,
) -> Result<Value, EvaluationError> {
    let (values, arr) = match args.0.as_slice() {
        [s, arr] => {
            let values = split_on_fs(&s.eval(rt)?.as_string(), &rt.vars.fs)?;
            (values, arr)
        },
        [s, arr, fs] => {
            let input = &s.eval(rt)?.as_string();
            let values = match fs {
                Expr::Regexp(reg) => split_on_regex(input, &reg.0),
                _ => split_on_fs(input, &fs.eval(rt)?.as_string())?,
            };
            (values, arr)
        },
        _ => {
            return Err(EvaluationError::InvalidNumberOfArgumentsRange(
                String::from("split"),
                2,
                3,
                args.len(),
            ));
        },
    };
    let array_name = if let Expr::LValue(LValueType::Name(name)) = arr {
        name
    } else {
        return Err(EvaluationError::NotAnArray(String::from("split")));
    };
    let size = values.len();
    rt.vars.clear(&array_name)?;
    for (i, value) in values.into_iter().enumerate() {
        rt.vars.set(
            AssignType::Normal,
            &array_name,
            Some((i + 1).to_string().as_str()),
            Value::from(value),
        )?;
    }
    Ok(Value::from(size))
}

/// Splits the input string on the given FS separator and returns a list of the
/// parts.
///
/// # FS Separator Splitting Logic
///
/// If the separator is a space, then the input is split on one or more
/// repetitions of the space and tabulation characters.
/// If the separator contains only one character, then the input is split on it.
/// Finally, the separator is interpreted as a regular expression.
pub fn split_on_fs(input: &str, sep: &str) -> Result<Vec<String>, EvaluationError> {
    let arr: Vec<String> = if sep == " " {
        lazy_static! {
            static ref FS_REGEX: Regex = Regex::new(r"[ \t]+").unwrap();
        }
        FS_REGEX
            .split(input)
            .skip_while(|s| s.is_empty())
            .map(|s| s.to_owned())
            .collect()
    } else if sep.len() == 1 {
        input.split(sep).map(|s| s.to_owned()).collect()
    } else {
        match Regex::new(sep) {
            Ok(fs_pattern) => split_on_regex(input, &fs_pattern),
            Err(e) => return Err(EvaluationError::InvalidRegex(e)),
        }
    };
    Ok(arr)
}

/// Splits the input string on the given regular expression and returns a list
/// of the parts.
fn split_on_regex(input: &str, regex: &Regex) -> Vec<String> {
    let mut values = Vec::new();
    let mut offset = 0;
    for m in regex.find_iter(input) {
        values.push(input[offset..m.start()].to_owned());
        offset = m.end();
    }
    if offset >= input.len() {
        values.push(String::new());
    } else {
        values.push(input[offset..].to_owned());
    }
    values
}
