//! Evaluates AWK's reserved functions.
use crate::{
    errors::EvaluationError,
    interpreter::{rnd::Rnd, stmt::formatting::sprintf, value::Value, RuntimeMut},
    parser::ast::ExprList,
};
use std::io::Write;

/// Returns true if the given name is a builtin.
pub fn is_builtin(name: &str) -> bool {
    match name {
        "atan2" | "close" | "cos" | "exp" | "gsub" | "index" | "int" | "length" | "log"
        | "match" | "rand" | "sin" | "split" | "sprintf" | "sqrt" | "srand" | "sub" | "substr"
        | "system" | "tolower" | "toupper" => true,
        _ => false,
    }
}

/// Evalues the named builtin with the given arguments.
pub fn call_builtin<Output>(
    name: &str,
    args: &ExprList,
    rt: &mut RuntimeMut<'_, Output>,
) -> Result<Value, EvaluationError>
where
    Output: Write,
{
    match name {
        "index" => index::execute(args, rt),
        "substr" => substr::execute(args, rt),
        "atan2" => atan2::execute(args, rt),
        "sin" => sin::execute(args, rt),
        "cos" => cos::execute(args, rt),
        "exp" => exp::execute(args, rt),
        "log" => log::execute(args, rt),
        "toupper" => toupper::execute(args, rt),
        "tolower" => tolower::execute(args, rt),
        "rand" => rt.rnd.rand(args),
        "srand" => {
            let seed = Rnd::srand_arg(args, rt)?;
            rt.rnd.srand(seed)
        },
        "sprintf" => sprintf(args, rt),
        "sub" => substitute::sub(args, rt),
        "length" => length::execute(args, rt),
        _ => unreachable!(),
    }
}

// Arithmetic functions
mod atan2;
mod cos;
mod exp;
mod log;
mod sin;
// String functions
mod index;
mod length;
mod substitute;
mod substr;
mod tolower;
mod toupper;
