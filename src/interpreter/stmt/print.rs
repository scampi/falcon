use crate::{
    errors::EvaluationError,
    interpreter::{
        stmt::{printf::execute_value, redirections::file_name, StmtResult},
        value::Value,
        Eval, RuntimeMut,
    },
    parser::ast::{ExprList, OutputRedirection},
};
use std::io::Write;

pub fn execute<Output>(
    rt: &mut RuntimeMut<'_, Output>,
    exprs: &ExprList,
    redir: &Option<OutputRedirection>,
) -> Result<Option<StmtResult>, EvaluationError>
where
    Output: Write,
{
    let path = if let Some(redir) = redir {
        let path = file_name(rt, &redir)?;
        rt.redirs.add_file(&path, redir)?;
        Some(path)
    } else {
        None
    };
    if exprs.0.is_empty() {
        match &path {
            Some(path) => {
                let file = rt.redirs.get_file(path);
                write!(file, "{}", rt.record.get(0)?.as_string())?;
            },
            None => write!(rt.output, "{}", rt.record.get(0)?.as_string())?,
        }
    } else {
        let ofmt = rt.vars.get("OFMT", None)?.as_string();

        for (i, expr) in exprs.0.iter().enumerate() {
            let val = expr.eval(rt)?;
            let sep = if i + 1 == exprs.len() {
                ""
            } else {
                rt.vars.ofs.as_str()
            };
            match &path {
                Some(path) => {
                    let mut file = rt.redirs.get_file(path);
                    if let Value::Number(_) = val {
                        execute_value(&ofmt, Some(val), &mut file)?;
                    } else {
                        write!(file, "{}{}", val.as_string(), sep)?;
                    }
                },
                None => {
                    if let Value::Number(_) = val {
                        execute_value(&ofmt, Some(val), &mut rt.output)?;
                    } else {
                        write!(rt.output, "{}{}", val.as_string(), sep)?;
                    }
                },
            }
        }
    }
    match &path {
        Some(path) => {
            let file = rt.redirs.get_file(path);
            write!(file, "{}", rt.vars.ors)?;
        },
        None => write!(rt.output, "{}", rt.vars.ors)?,
    }
    Ok(None)
}
