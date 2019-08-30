use crate::{
    errors::EvaluationError,
    interpreter::{value::Value, Eval, RuntimeMut},
    parser::ast::{AssignType, Expr, ExprList, LValueType},
};
use std::io::Write;

pub fn replace<Output: Write>(
    rt: &mut RuntimeMut<'_, Output>,
    ere: &Expr,
    input: &mut String,
    mut repl: String,
) -> Result<bool, EvaluationError> {
    let (start, end) = match ere {
        Expr::Regexp(reg) => match reg.0.find(input) {
            Some(m) => (m.start(), m.end()),
            None => return Ok(false),
        },
        _ => {
            let ere = ere.eval(rt)?.as_string();
            match input.find(&ere) {
                Some(offset) => (offset, offset + ere.len()),
                None => return Ok(false),
            }
        },
    };
    let ampersands = repl
        .rmatch_indices('&')
        .map(|(i, _)| i)
        .collect::<Vec<usize>>();
    for i in ampersands.into_iter() {
        if i == 0 {
            repl.replace_range(i..i + 1, &input[start..end]);
        } else if i == 1 {
            match repl.get(i - 1..i) {
                // FIXME: print a warning that an escaped & was treated as a plain &
                Some("\\") => repl.replace_range(i - 1..i + 1, &input[start..end]),
                _ => repl.replace_range(i..i + 1, &input[start..end]),
            }
        } else {
            match (repl.get(i - 1..i), repl.get(i - 1..i)) {
                (Some("\\"), Some("\\")) => repl.replace_range(i - 2..i + 1, "&"),
                // FIXME: print a warning that an escaped & was treated as a plain &
                (Some(_), Some("\\")) => repl.replace_range(i - 1..i + 1, &input[start..end]),
                _ => repl.replace_range(i..i + 1, &input[start..end]),
            }
        }
    }
    input.replace_range(start..end, &repl);
    Ok(true)
}

pub fn sub<Output: Write>(
    args: &ExprList,
    rt: &mut RuntimeMut<'_, Output>,
) -> Result<Value, EvaluationError> {
    match args.0.as_slice() {
        [ere, repl] => {
            let repl = repl.eval(rt)?.as_string();
            let mut line = rt.record.get(0)?.as_string();

            if !replace(rt, ere, &mut line, repl)? {
                return Ok(Value::from(0));
            }
            rt.record
                .set(rt.vars, AssignType::Normal, 0, Value::from(line))?;
            Ok(Value::from(1))
        },
        [ere, repl, input] => {
            let mut input_str = input.eval(rt)?.as_string();
            let repl = repl.eval(rt)?.as_string();

            if !replace(rt, ere, &mut input_str, repl)? {
                return Ok(Value::from(0));
            }
            match input {
                Expr::LValue(LValueType::Name(name)) => {
                    rt.vars
                        .set(AssignType::Normal, name, None, Value::from(input_str))?;
                },
                Expr::LValue(LValueType::Dollar(expr)) => {
                    let offset = expr.eval(rt)?.as_number() as isize;
                    rt.record
                        .set(rt.vars, AssignType::Normal, offset, Value::from(input_str))?;
                },
                Expr::LValue(LValueType::Brackets(name, exprs)) => {
                    let subscript = exprs.eval(rt)?;
                    let subscript = subscript
                        .into_iter()
                        .map(|v| v.as_string())
                        .collect::<Vec<String>>()
                        .as_slice()
                        .join(&rt.vars.subsep);
                    rt.vars.set(
                        AssignType::Normal,
                        name,
                        Some(&subscript),
                        Value::from(input_str),
                    )?;
                },
                _ => unimplemented!(),
            }
            Ok(Value::from(1))
        },
        _ => Err(EvaluationError::InvalidNumberOfArgumentsRange(
            String::from("sub"),
            2,
            3,
            args.len(),
        )),
    }
}
