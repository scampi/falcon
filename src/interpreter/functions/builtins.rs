use crate::{
    errors::EvaluationError,
    interpreter::{rnd::Rnd, value::Value, RuntimeMut},
    parser::ast::ExprList,
};
use std::io::Write;

pub fn is_builtin(name: &str) -> bool {
    match name {
        "index" => true,
        "substr" => true,
        "atan2" => true,
        "sin" => true,
        "cos" => true,
        "exp" => true,
        "log" => true,
        "toupper" => true,
        "tolower" => true,
        "rand" => true,
        "srand" => true,
        _ => false,
    }
}

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
mod substr;
mod tolower;
mod toupper;
