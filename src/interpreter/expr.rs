use crate::{
    errors::EvaluationError,
    interpreter::{
        functions::Functions, record::Record, value::ExprValue, variables::Variables, Eval,
    },
    parser::ast::{AssignType, Expr, ExprList, LValueType},
};
use regex::Regex;

impl Eval for ExprList {
    type EvalResult = Vec<ExprValue>;
    fn eval(
        &self,
        vars: &mut Variables,
        record: &mut Record,
        funcs: &mut Functions,
    ) -> Result<Vec<ExprValue>, EvaluationError> {
        let mut values = Vec::with_capacity(self.len());
        for expr in &self.0 {
            values.push(expr.eval(vars, record, funcs)?);
        }
        Ok(values)
    }
}

impl Eval for Expr {
    type EvalResult = ExprValue;
    fn eval(
        &self,
        vars: &mut Variables,
        record: &mut Record,
        funcs: &mut Functions,
    ) -> Result<ExprValue, EvaluationError> {
        match self {
            Expr::Mod(l, r) => Ok(ExprValue::from(
                l.eval(vars, record, funcs)?.as_number() % r.eval(vars, record, funcs)?.as_number(),
            )),
            Expr::Pow(l, r) => Ok(ExprValue::from(
                l.eval(vars, record, funcs)?
                    .as_number()
                    .powf(r.eval(vars, record, funcs)?.as_number()),
            )),
            Expr::Add(l, r) => Ok(ExprValue::from(
                l.eval(vars, record, funcs)?.as_number() + r.eval(vars, record, funcs)?.as_number(),
            )),
            Expr::Minus(l, r) => Ok(ExprValue::from(
                l.eval(vars, record, funcs)?.as_number() - r.eval(vars, record, funcs)?.as_number(),
            )),
            Expr::Div(l, r) => {
                let rvalue = r.eval(vars, record, funcs)?.as_number();
                if rvalue == 0.0 {
                    return Err(EvaluationError::DivisionByZero);
                }
                Ok(ExprValue::from(
                    l.eval(vars, record, funcs)?.as_number() / rvalue,
                ))
            },
            Expr::Mul(l, r) => Ok(ExprValue::from(
                l.eval(vars, record, funcs)?.as_number() * r.eval(vars, record, funcs)?.as_number(),
            )),
            Expr::Comparison(op, l, r) => {
                let lvalue = l.eval(vars, record, funcs)?;
                let rvalue = r.eval(vars, record, funcs)?;
                Ok(ExprValue::compare(op, &lvalue, &rvalue))
            },
            Expr::Concat(l, r) => Ok(ExprValue::String(format!(
                "{}{}",
                l.eval(vars, record, funcs)?,
                r.eval(vars, record, funcs)?
            ))),
            Expr::LogicalAnd(l, r) => {
                if l.eval(vars, record, funcs)?.as_bool() {
                    Ok(ExprValue::from(r.eval(vars, record, funcs)?.as_bool()))
                } else {
                    Ok(ExprValue::from(false))
                }
            },
            Expr::LogicalOr(l, r) => {
                if l.eval(vars, record, funcs)?.as_bool() {
                    Ok(ExprValue::from(true))
                } else {
                    Ok(ExprValue::from(r.eval(vars, record, funcs)?.as_bool()))
                }
            },
            Expr::LogicalNot(e) => Ok(ExprValue::from(!e.eval(vars, record, funcs)?.as_bool())),
            Expr::Conditional(cond, ok, ko) => {
                if cond.eval(vars, record, funcs)?.as_bool() {
                    Ok(ExprValue::from(ok.eval(vars, record, funcs)?))
                } else {
                    Ok(ExprValue::from(ko.eval(vars, record, funcs)?))
                }
            },
            Expr::LValue(lvalue) => match lvalue {
                LValueType::Name(name) => vars.get(funcs, name, None),
                LValueType::Dollar(e) => {
                    let index = e.eval(vars, record, funcs)?.as_number() as isize;
                    record.get(index)
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(vars, record, funcs)?)?;
                    vars.get(funcs, name, Some(&key_str))
                },
            },
            Expr::Assign(ty, lvalue, rvalue) => {
                let new_value = rvalue.eval(vars, record, funcs)?;
                match lvalue {
                    LValueType::Name(name) => vars.set(funcs, ty, name, None, new_value),
                    LValueType::Dollar(e) => {
                        let index = e.eval(vars, record, funcs)?.as_number() as isize;
                        record.set(vars, ty, index, new_value)
                    },
                    LValueType::Brackets(name, key) => {
                        let key_str = Variables::array_key(key.eval(vars, record, funcs)?)?;
                        vars.set(funcs, ty, name, Some(&key_str), new_value)
                    },
                }
            },
            Expr::PreIncrement(lvalue) => match lvalue {
                LValueType::Name(name) => {
                    vars.set(funcs, &AssignType::Add, name, None, ExprValue::from(1))
                },
                LValueType::Dollar(e) => {
                    let index = e.eval(vars, record, funcs)?.as_number() as isize;
                    record.set(vars, &AssignType::Add, index, ExprValue::from(1))
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(vars, record, funcs)?)?;
                    vars.set(
                        funcs,
                        &AssignType::Add,
                        name,
                        Some(&key_str),
                        ExprValue::from(1),
                    )
                },
            },
            Expr::PreDecrement(lvalue) => match lvalue {
                LValueType::Name(name) => {
                    vars.set(funcs, &AssignType::Sub, name, None, ExprValue::from(1))
                },
                LValueType::Dollar(e) => {
                    let index = e.eval(vars, record, funcs)?.as_number() as isize;
                    record.set(vars, &AssignType::Sub, index, ExprValue::from(1))
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(vars, record, funcs)?)?;
                    vars.set(
                        funcs,
                        &AssignType::Sub,
                        name,
                        Some(&key_str),
                        ExprValue::from(1),
                    )
                },
            },
            Expr::PostIncrement(lvalue) => match lvalue {
                LValueType::Name(name) => {
                    let value = vars.get(funcs, name, None);
                    vars.set(funcs, &AssignType::Add, name, None, ExprValue::from(1))?;
                    value
                },
                LValueType::Dollar(e) => {
                    let index = e.eval(vars, record, funcs)?.as_number() as isize;
                    let value = record.get(index);
                    record.set(vars, &AssignType::Add, index, ExprValue::from(1))?;
                    value
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(vars, record, funcs)?)?;
                    let value = vars.get(funcs, name, Some(&key_str));
                    vars.set(
                        funcs,
                        &AssignType::Add,
                        name,
                        Some(&key_str),
                        ExprValue::from(1),
                    )?;
                    value
                },
            },
            Expr::PostDecrement(lvalue) => match lvalue {
                LValueType::Name(name) => {
                    let value = vars.get(funcs, name, None);
                    vars.set(funcs, &AssignType::Sub, name, None, ExprValue::from(1))?;
                    value
                },
                LValueType::Dollar(e) => {
                    let index = e.eval(vars, record, funcs)?.as_number() as isize;
                    let value = record.get(index);
                    record.set(vars, &AssignType::Sub, index, ExprValue::from(1))?;
                    value
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(vars, record, funcs)?)?;
                    let value = vars.get(funcs, name, Some(&key_str));
                    vars.set(
                        funcs,
                        &AssignType::Sub,
                        name,
                        Some(&key_str),
                        ExprValue::from(1),
                    )?;
                    value
                },
            },
            Expr::Regexp(reg) => Ok(ExprValue::from(reg.is_match(&record.get(0)?.as_string()))),
            Expr::Match(neg, s, reg) => {
                let reg_eval = match Regex::new(&reg.eval(vars, record, funcs)?.as_string()) {
                    Ok(reg) => reg,
                    Err(e) => return Err(EvaluationError::InvalidRegex(e)),
                };
                let s_eval = s.eval(vars, record, funcs)?.as_string();
                let res = reg_eval.is_match(&s_eval);
                Ok(ExprValue::from(res == !*neg))
            },
            Expr::FunctionCall(name, args) => funcs.call(name, args, vars, record),
            Expr::UnaryMinus(um) => Ok(ExprValue::from(-um.eval(vars, record, funcs)?.as_number())),
            Expr::UnaryPlus(up) => up.eval(vars, record, funcs),
            Expr::Grouping(g) => g.eval(vars, record, funcs),
            Expr::Number(n) => Ok(ExprValue::from(*n)),
            Expr::String(s) => Ok(ExprValue::String(s.to_owned())),
            _ => unimplemented!("{:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{interpreter::Context, parser::expr::get_expr};

    fn eval_expr(expr: &Expr, cxt: &mut Context) -> Result<ExprValue, EvaluationError> {
        expr.eval(&mut cxt.vars, &mut cxt.record, &mut cxt.funcs)
    }

    #[test]
    fn arithmetic() {
        let mut cxt = Context::new();

        let expr = get_expr("1 + 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(3f64));

        let expr = get_expr("1 - 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(-1f64));

        let expr = get_expr("1 / 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(0.5));

        let expr = get_expr("2 * 3");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(6f64));

        let expr = get_expr("2 / 0");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);

        let expr = get_expr(r#"2 / "a""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);

        let expr = get_expr(r#"2 * "a""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(0.0));

        let expr = get_expr("2 ^ 3");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(8.0));

        let expr = get_expr("(2 + 1) ^ 3");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(27.0));

        let expr = get_expr("7 % 3");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(1.0));

        let expr = get_expr("- 2 ^ 3");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(-8.0));

        let expr = get_expr(r#"2 + "3""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(5.0));
    }

    #[test]
    fn comparison() {
        let mut cxt = Context::new();

        let expr = get_expr("2 < 3");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr("2 > 3");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(false));

        let expr = get_expr(r#"2 == "2""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr(r#""2" == 2"#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr(r#""a" == "b""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(false));

        let expr = get_expr(r#""1" < "a""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr(r#""a" < "b""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr(r#""a" > "b""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(false));
    }

    #[test]
    fn concat() {
        let mut cxt = Context::new();

        let expr = get_expr(r#"1 " aaa " 2"#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(
            res.unwrap(),
            ExprValue::from("1 aaa 2".to_owned()),
            "{:?}",
            expr
        );

        let expr = get_expr(r#""aaa" (1 < 2)"#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("aaa1".to_owned()));

        let expr = get_expr(r#""aaa" (1 == 2)"#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("aaa0".to_owned()));

        let expr = get_expr("1 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("12".to_owned()));
    }

    #[test]
    fn logical_operation() {
        let mut cxt = Context::new();

        let expr = get_expr("1 && 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr(r#""" && 2"#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(false));

        let expr = get_expr(r#"1 && "2""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr("1 && 0");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(false));

        let expr = get_expr("1 < 2 && 3");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr("0 || 1");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr("1 || 0");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr("!(1 || 0)");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(false));
    }

    #[test]
    fn conditional() {
        let mut cxt = Context::new();

        let expr = get_expr(r#"1 == 1 ? "OK" : "KO""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("OK".to_owned()));

        let expr = get_expr(r#"1 != 1 ? "OK" : "KO""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("KO".to_owned()));

        let expr = get_expr(r#"(1 == 1 ? "OK" : 2) + 2"#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(2.0));

        let expr = get_expr(r#"(1 < 1 ? "OK" : 2) + 2"#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(4.0));
    }

    #[test]
    fn field_lvalue() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());

        let expr = get_expr("$0");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("john connor".to_owned()));

        let expr = get_expr("$1");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("john".to_owned()));

        let expr = get_expr("$2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("connor".to_owned()));

        let expr = get_expr("$3");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::Uninitialised);

        let expr = get_expr("$(2 - 1)");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("john".to_owned()));

        let expr = get_expr("$(1 != 1)");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("john connor".to_owned()));

        let expr = get_expr("$(-42)");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap_err(), EvaluationError::NegativeFieldIndex(-42));
    }

    #[test]
    fn var_lvalue() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());

        let expr = get_expr("NF");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(2.0));
        assert!(!cxt.vars.has_user_vars());

        let expr = get_expr("nf");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::Uninitialised);
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "nf", None),
            Ok(ExprValue::Uninitialised)
        );
    }

    #[test]
    fn array_lvalue() {
        let mut cxt = Context::new();

        let expr = get_expr("a[0]");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::Uninitialised);
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", Some("0")),
            Ok(ExprValue::Uninitialised)
        );

        let expr = get_expr("b[0,1,2]");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::Uninitialised);
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "b", Some("012")),
            Ok(ExprValue::Uninitialised)
        );

        cxt.vars.subsep = String::from("#");
        let expr = get_expr("b[0,1,2]");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::Uninitialised);
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "b", Some("0#1#2")),
            Ok(ExprValue::Uninitialised)
        );
    }

    #[test]
    fn assignment_name_lvalue() {
        let mut cxt = Context::new();

        let expr = get_expr("a = 42");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(42));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(42))
        );

        let expr = get_expr("a = b = 5");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(5));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(5))
        );
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "b", None),
            Ok(ExprValue::from(5))
        );

        let expr = get_expr("a ^= 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(25));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(25))
        );

        let expr = get_expr("a = 2 + 3");
        eval_expr(&expr, &mut cxt).unwrap();
        let expr = get_expr("a *= 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(10));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(10))
        );

        let expr = get_expr("a = 2 + 3");
        eval_expr(&expr, &mut cxt).unwrap();
        let expr = get_expr("a /= 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(2.5));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(2.5))
        );

        let expr = get_expr("a = 2 + 3");
        eval_expr(&expr, &mut cxt).unwrap();
        let expr = get_expr("a -= 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(3));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(3))
        );

        let expr = get_expr("a = 2 + 3");
        eval_expr(&expr, &mut cxt).unwrap();
        let expr = get_expr("a %= 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(1));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(1))
        );

        let expr = get_expr("c /= 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(0));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "c", None),
            Ok(ExprValue::from(0))
        );

        let expr = get_expr(r#"FS = "@""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("@".to_owned()));
        assert_eq!(cxt.vars.fs, "@".to_owned());

        let expr = get_expr(r#"FS *= 2"#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("0".to_owned()));
        assert_eq!(cxt.vars.fs, "0".to_owned());
    }

    #[test]
    fn assignment_dollar_lvalue() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());

        let expr = get_expr("$0 = 42");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("42".to_owned()));
        assert_eq!(cxt.record.get(0), Ok(ExprValue::from("42".to_owned())));
        assert_eq!(cxt.vars.nf, 1);

        cxt.set_next_record("john connor".to_owned());

        let expr = get_expr(r#"$2 = "moo""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("moo".to_owned()));
        assert_eq!(
            cxt.record.get(0),
            Ok(ExprValue::from("john moo".to_owned()))
        );
        assert_eq!(cxt.record.get(1), Ok(ExprValue::from("john".to_owned())));
        assert_eq!(cxt.record.get(2), Ok(ExprValue::from("moo".to_owned())));
        assert_eq!(cxt.vars.nf, 2);

        cxt.set_next_record("john connor".to_owned());

        let expr = get_expr(r#"$10 = "moo""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("moo".to_owned()));
        assert_eq!(
            cxt.record.get(0),
            Ok(ExprValue::from("john connor        moo".to_owned()))
        );
        assert_eq!(cxt.record.get(1), Ok(ExprValue::from("john".to_owned())));
        assert_eq!(cxt.record.get(2), Ok(ExprValue::from("connor".to_owned())));
        assert_eq!(cxt.record.get(3), Ok(ExprValue::from(String::new())));
        assert_eq!(cxt.record.get(4), Ok(ExprValue::from(String::new())));
        assert_eq!(cxt.record.get(5), Ok(ExprValue::from(String::new())));
        assert_eq!(cxt.record.get(6), Ok(ExprValue::from(String::new())));
        assert_eq!(cxt.record.get(7), Ok(ExprValue::from(String::new())));
        assert_eq!(cxt.record.get(8), Ok(ExprValue::from(String::new())));
        assert_eq!(cxt.record.get(9), Ok(ExprValue::from(String::new())));
        assert_eq!(cxt.record.get(10), Ok(ExprValue::from("moo".to_owned())));
        assert_eq!(cxt.vars.nf, 10);

        cxt.set_next_record("there are 5 apples".to_owned());

        let expr = get_expr("$3 *= 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("10".to_owned()));
        assert_eq!(
            cxt.record.get(0),
            Ok(ExprValue::from("there are 10 apples".to_owned()))
        );
        assert_eq!(cxt.vars.nf, 4);

        cxt.set_next_record("there are 5 apples".to_owned());

        let expr = get_expr("$3 /= 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("2.5".to_owned()));
        assert_eq!(
            cxt.record.get(0),
            Ok(ExprValue::from("there are 2.5 apples".to_owned()))
        );
        assert_eq!(cxt.vars.nf, 4);

        cxt.set_next_record("aaa bbb ccc".to_owned());

        let expr = get_expr("$2 = $3 = 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("2".to_owned()));
        assert_eq!(cxt.record.get(0), Ok(ExprValue::from("aaa 2 2".to_owned())));
        assert_eq!(cxt.vars.nf, 3);

        cxt.set_next_record("there are 5 apples".to_owned());

        let expr = get_expr("$3 /= 0");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);
    }

    #[test]
    fn assignment_brackets_lvalue() {
        let mut cxt = Context::new();

        let expr = get_expr("a[0] = 42");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(42));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", Some("0")),
            Ok(ExprValue::from(42))
        );

        let expr = get_expr("a[0] /= 2");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(21));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", Some("0")),
            Ok(ExprValue::from(21))
        );

        let expr = get_expr("a[1] = 5");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(5));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", Some("1")),
            Ok(ExprValue::from(5))
        );
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", Some("0")),
            Ok(ExprValue::from(21))
        );
    }

    #[test]
    fn record_count() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());
        assert_eq!(cxt.vars.nr, 1);
        assert_eq!(cxt.vars.fnr, 1);
        cxt.set_next_record("john connor".to_owned());
        assert_eq!(cxt.vars.nr, 2);
        assert_eq!(cxt.vars.fnr, 2);
    }

    #[test]
    fn preincrement() {
        let mut cxt = Context::new();

        // preincrement a variable
        let expr = get_expr("a = 15");
        eval_expr(&expr, &mut cxt).unwrap();
        let expr = get_expr("++a");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(16));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(16))
        );
        // preincrement an array element
        let expr = get_expr("++b[0]");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(1));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "b", Some("0")),
            Ok(ExprValue::from(1))
        );
        // preincrement a field value
        cxt.set_next_record("10".to_owned());
        let expr = get_expr("++$1");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("11".to_owned()));
        assert_eq!(cxt.record.get(1), Ok(ExprValue::from("11".to_owned())));
    }

    #[test]
    fn postincrement() {
        let mut cxt = Context::new();

        // postincrement a variable
        let expr = get_expr("a = 15");
        eval_expr(&expr, &mut cxt).unwrap();
        let expr = get_expr("a++");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(15));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(16))
        );
        // postincrement an array element
        let expr = get_expr("b[0]++");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::Uninitialised);
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "b", Some("0")),
            Ok(ExprValue::from(1))
        );
        // postincrement a field value
        cxt.set_next_record("10".to_owned());
        let expr = get_expr("$1++");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("10".to_owned()));
        assert_eq!(cxt.record.get(1), Ok(ExprValue::from("11".to_owned())));
    }

    #[test]
    fn predecrement() {
        let mut cxt = Context::new();

        // preincrement a variable
        let expr = get_expr("a = 15");
        eval_expr(&expr, &mut cxt).unwrap();
        let expr = get_expr("--a");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(14));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(14))
        );
        // preincrement an array element
        let expr = get_expr("--b[0]");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(-1));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "b", Some("0")),
            Ok(ExprValue::from(-1))
        );
        // preincrement a field value
        cxt.set_next_record("10".to_owned());
        let expr = get_expr("--$1");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("9".to_owned()));
        assert_eq!(cxt.record.get(1), Ok(ExprValue::from("9".to_owned())));
    }

    #[test]
    fn postdecrement() {
        let mut cxt = Context::new();

        // postincrement a variable
        let expr = get_expr("a = 15");
        eval_expr(&expr, &mut cxt).unwrap();
        let expr = get_expr("a--");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(15));
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "a", None),
            Ok(ExprValue::from(14))
        );
        // postincrement an array element
        let expr = get_expr("b[0]--");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::Uninitialised);
        assert_eq!(
            cxt.vars.get(&mut cxt.funcs, "b", Some("0")),
            Ok(ExprValue::from(-1))
        );
        // postincrement a field value
        cxt.set_next_record("10".to_owned());
        let expr = get_expr("$1--");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from("10".to_owned()));
        assert_eq!(cxt.record.get(1), Ok(ExprValue::from("9".to_owned())));
    }

    #[test]
    fn regexp() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());

        let expr = get_expr("/^j.*r$/");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        let expr = get_expr("/jane/");
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(false));
    }

    #[test]
    fn r#match() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());

        let expr = get_expr(r#"$2 ~ "con.or""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));

        cxt.set_next_record("john cannor".to_owned());
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(false));

        cxt.set_next_record("john connor".to_owned());

        let expr = get_expr(r#"$2 !~ "con.or""#);
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(false));

        cxt.set_next_record("john cannor".to_owned());
        let res = eval_expr(&expr, &mut cxt);
        assert_eq!(res.unwrap(), ExprValue::from(true));
    }

    #[test]
    fn scalar_array_misuse() {
        let mut cxt = Context::new();

        let expr = get_expr("FS[0]=42");
        let err = eval_expr(&expr, &mut cxt).unwrap_err();
        assert_eq!(err, EvaluationError::UseScalarAsArray);
    }
}
