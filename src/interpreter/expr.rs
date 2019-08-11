use crate::{
    errors::EvaluationError,
    interpreter::{value::Value, variables::Variables, Eval, RuntimeMut},
    parser::ast::{AssignType, Expr, ExprList, LValueType},
};
use regex::Regex;
use std::io::Write;

impl Eval for ExprList {
    type EvalResult = Vec<Value>;
    fn eval<Output>(&self, rt: &mut RuntimeMut<'_, Output>) -> Result<Self::EvalResult, EvaluationError>
    where
        Output: Write,
    {
        let mut values = Vec::with_capacity(self.len());
        for expr in &self.0 {
            values.push(expr.eval(rt)?);
        }
        Ok(values)
    }
}

impl Eval for Expr {
    type EvalResult = Value;
    fn eval<Output>(&self, rt: &mut RuntimeMut<'_, Output>) -> Result<Value, EvaluationError>
    where
        Output: Write,
    {
        match self {
            Expr::Mod(l, r) => Ok(Value::from(
                l.eval(rt)?.as_number() % r.eval(rt)?.as_number(),
            )),
            Expr::Pow(l, r) => Ok(Value::from(
                l.eval(rt)?.as_number().powf(r.eval(rt)?.as_number()),
            )),
            Expr::Add(l, r) => Ok(Value::from(
                l.eval(rt)?.as_number() + r.eval(rt)?.as_number(),
            )),
            Expr::Minus(l, r) => Ok(Value::from(
                l.eval(rt)?.as_number() - r.eval(rt)?.as_number(),
            )),
            Expr::Div(l, r) => {
                let rvalue = r.eval(rt)?.as_number();
                if rvalue == 0.0 {
                    return Err(EvaluationError::DivisionByZero);
                }
                Ok(Value::from(l.eval(rt)?.as_number() / rvalue))
            },
            Expr::Mul(l, r) => Ok(Value::from(
                l.eval(rt)?.as_number() * r.eval(rt)?.as_number(),
            )),
            Expr::Comparison(op, l, r) => {
                let lvalue = l.eval(rt)?;
                let rvalue = r.eval(rt)?;
                Ok(Value::compare(op, &lvalue, &rvalue))
            },
            Expr::Concat(l, r) => Ok(Value::String(format!("{}{}", l.eval(rt)?, r.eval(rt)?))),
            Expr::LogicalAnd(l, r) => {
                if l.eval(rt)?.as_bool() {
                    Ok(Value::from(r.eval(rt)?.as_bool()))
                } else {
                    Ok(Value::from(false))
                }
            },
            Expr::LogicalOr(l, r) => {
                if l.eval(rt)?.as_bool() {
                    Ok(Value::from(true))
                } else {
                    Ok(Value::from(r.eval(rt)?.as_bool()))
                }
            },
            Expr::LogicalNot(e) => Ok(Value::from(!e.eval(rt)?.as_bool())),
            Expr::Conditional(cond, ok, ko) => {
                if cond.eval(rt)?.as_bool() {
                    Ok(Value::from(ok.eval(rt)?))
                } else {
                    Ok(Value::from(ko.eval(rt)?))
                }
            },
            Expr::LValue(lvalue) => match lvalue {
                LValueType::Name(name) => rt.vars.get(name, None),
                LValueType::Dollar(e) => {
                    let index = e.eval(rt)?.as_number() as isize;
                    rt.record.get(index)
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(rt)?)?;
                    rt.vars.get(name, Some(&key_str))
                },
            },
            Expr::Assign(ty, lvalue, rvalue) => {
                let new_value = rvalue.eval(rt)?;
                match lvalue {
                    LValueType::Name(name) => rt.vars.set(ty, name, None, new_value),
                    LValueType::Dollar(e) => {
                        let index = e.eval(rt)?.as_number() as isize;
                        rt.record.set(&mut rt.vars, ty, index, new_value)
                    },
                    LValueType::Brackets(name, key) => {
                        let key_str = Variables::array_key(key.eval(rt)?)?;
                        rt.vars.set(ty, name, Some(&key_str), new_value)
                    },
                }
            },
            Expr::PreIncrement(lvalue) => match lvalue {
                LValueType::Name(name) => rt.vars.set(&AssignType::Add, name, None, Value::from(1)),
                LValueType::Dollar(e) => {
                    let index = e.eval(rt)?.as_number() as isize;
                    rt.record
                        .set(&mut rt.vars, &AssignType::Add, index, Value::from(1))
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(rt)?)?;
                    rt.vars
                        .set(&AssignType::Add, name, Some(&key_str), Value::from(1))
                },
            },
            Expr::PreDecrement(lvalue) => match lvalue {
                LValueType::Name(name) => rt.vars.set(&AssignType::Sub, name, None, Value::from(1)),
                LValueType::Dollar(e) => {
                    let index = e.eval(rt)?.as_number() as isize;
                    rt.record
                        .set(&mut rt.vars, &AssignType::Sub, index, Value::from(1))
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(rt)?)?;
                    rt.vars
                        .set(&AssignType::Sub, name, Some(&key_str), Value::from(1))
                },
            },
            Expr::PostIncrement(lvalue) => match lvalue {
                LValueType::Name(name) => {
                    let value = rt.vars.get(name, None);
                    rt.vars.set(&AssignType::Add, name, None, Value::from(1))?;
                    value
                },
                LValueType::Dollar(e) => {
                    let index = e.eval(rt)?.as_number() as isize;
                    let value = rt.record.get(index);
                    rt.record
                        .set(&mut rt.vars, &AssignType::Add, index, Value::from(1))?;
                    value
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(rt)?)?;
                    let value = rt.vars.get(name, Some(&key_str));
                    rt.vars
                        .set(&AssignType::Add, name, Some(&key_str), Value::from(1))?;
                    value
                },
            },
            Expr::PostDecrement(lvalue) => match lvalue {
                LValueType::Name(name) => {
                    let value = rt.vars.get(name, None);
                    rt.vars.set(&AssignType::Sub, name, None, Value::from(1))?;
                    value
                },
                LValueType::Dollar(e) => {
                    let index = e.eval(rt)?.as_number() as isize;
                    let value = rt.record.get(index);
                    rt.record
                        .set(&mut rt.vars, &AssignType::Sub, index, Value::from(1))?;
                    value
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Variables::array_key(key.eval(rt)?)?;
                    let value = rt.vars.get(name, Some(&key_str));
                    rt.vars
                        .set(&AssignType::Sub, name, Some(&key_str), Value::from(1))?;
                    value
                },
            },
            Expr::Regexp(reg) => Ok(Value::from(reg.is_match(&rt.record.get(0)?.as_string()))),
            Expr::Match(neg, s, reg) => {
                let reg_eval = match Regex::new(&reg.eval(rt)?.as_string()) {
                    Ok(reg) => reg,
                    Err(e) => return Err(EvaluationError::InvalidRegex(e)),
                };
                let s_eval = s.eval(rt)?.as_string();
                let res = reg_eval.is_match(&s_eval);
                Ok(Value::from(res == !*neg))
            },
            Expr::FunctionCall(name, args) => rt.funcs.call(name, args, rt),
            Expr::UnaryMinus(um) => Ok(Value::from(-um.eval(rt)?.as_number())),
            Expr::UnaryPlus(up) => up.eval(rt),
            Expr::Grouping(g) => g.eval(rt),
            Expr::Number(n) => Ok(Value::from(*n)),
            Expr::String(s) => Ok(Value::String(s.to_owned())),
            _ => unimplemented!("{:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        interpreter::Runtime,
        parser::{ast::Program, expr::get_expr},
    };
    use std::io::Cursor;

    fn eval_expr(expr: &Expr, rt: &mut Runtime<'_, Cursor<Vec<u8>>>) -> Result<Value, EvaluationError> {
        let mut rt_mut = RuntimeMut::new(rt.output, &mut rt.vars, &mut rt.record, &rt.funcs);
        expr.eval(&mut rt_mut)
    }

    #[test]
    fn arithmetic() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        let expr = get_expr("1 + 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(3f64));

        let expr = get_expr("1 - 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(-1f64));

        let expr = get_expr("1 / 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(0.5));

        let expr = get_expr("2 * 3");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(6f64));

        let expr = get_expr("2 / 0");
        let res = eval_expr(&expr, &mut rt);
        match res.unwrap_err() {
            EvaluationError::DivisionByZero => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };

        let expr = get_expr(r#"2 / "a""#);
        let res = eval_expr(&expr, &mut rt);
        match res.unwrap_err() {
            EvaluationError::DivisionByZero => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };

        let expr = get_expr(r#"2 * "a""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(0.0));

        let expr = get_expr("2 ^ 3");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(8.0));

        let expr = get_expr("(2 + 1) ^ 3");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(27.0));

        let expr = get_expr("7 % 3");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(1.0));

        let expr = get_expr("- 2 ^ 3");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(-8.0));

        let expr = get_expr(r#"2 + "3""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(5.0));
    }

    #[test]
    fn comparison() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        let expr = get_expr("2 < 3");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr("2 > 3");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = get_expr(r#"2 == "2""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr(r#""2" == 2"#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr(r#""a" == "b""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = get_expr(r#""1" < "a""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr(r#""a" < "b""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr(r#""a" > "b""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(false));
    }

    #[test]
    fn concat() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        let expr = get_expr(r#"1 " aaa " 2"#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(
            res.unwrap(),
            Value::from("1 aaa 2".to_owned()),
            "{:?}",
            expr
        );

        let expr = get_expr(r#""aaa" (1 < 2)"#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("aaa1".to_owned()));

        let expr = get_expr(r#""aaa" (1 == 2)"#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("aaa0".to_owned()));

        let expr = get_expr("1 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("12".to_owned()));
    }

    #[test]
    fn logical_operation() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        let expr = get_expr("1 && 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr(r#""" && 2"#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = get_expr(r#"1 && "2""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr("1 && 0");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = get_expr("1 < 2 && 3");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr("0 || 1");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr("1 || 0");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr("!(1 || 0)");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(false));
    }

    #[test]
    fn conditional() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        let expr = get_expr(r#"1 == 1 ? "OK" : "KO""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("OK".to_owned()));

        let expr = get_expr(r#"1 != 1 ? "OK" : "KO""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("KO".to_owned()));

        let expr = get_expr(r#"(1 == 1 ? "OK" : 2) + 2"#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(2.0));

        let expr = get_expr(r#"(1 < 1 ? "OK" : 2) + 2"#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(4.0));
    }

    #[test]
    fn field_lvalue() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        rt.set_next_record("john connor".to_owned());

        let expr = get_expr("$0");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("john connor".to_owned()));

        let expr = get_expr("$1");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("john".to_owned()));

        let expr = get_expr("$2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("connor".to_owned()));

        let expr = get_expr("$3");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::Uninitialised);

        let expr = get_expr("$(2 - 1)");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("john".to_owned()));

        let expr = get_expr("$(1 != 1)");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("john connor".to_owned()));

        let expr = get_expr("$(-42)");
        let res = eval_expr(&expr, &mut rt);
        match res.unwrap_err() {
            EvaluationError::NegativeFieldIndex(-42) => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };
    }

    #[test]
    fn var_lvalue() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        rt.set_next_record("john connor".to_owned());

        let expr = get_expr("NF");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(2.0));
        assert!(!rt.vars.has_user_vars());

        let expr = get_expr("nf");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(rt.vars.get("nf", None).unwrap(), Value::Uninitialised);
    }

    #[test]
    fn array_lvalue() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        let expr = get_expr("a[0]");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(rt.vars.get("a", Some("0")).unwrap(), Value::Uninitialised);

        let expr = get_expr("b[0,1,2]");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(rt.vars.get("b", Some("012")).unwrap(), Value::Uninitialised);

        rt.vars.subsep = String::from("#");
        let expr = get_expr("b[0,1,2]");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(
            rt.vars.get("b", Some("0#1#2")).unwrap(),
            Value::Uninitialised
        );
    }

    #[test]
    fn assignment_name_lvalue() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        let expr = get_expr("a = 42");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(42));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(42));

        let expr = get_expr("a = b = 5");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(5));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(5));
        assert_eq!(rt.vars.get("b", None).unwrap(), Value::from(5));

        let expr = get_expr("a ^= 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(25));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(25));

        let expr = get_expr("a = 2 + 3");
        eval_expr(&expr, &mut rt).unwrap();
        let expr = get_expr("a *= 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(10));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(10));

        let expr = get_expr("a = 2 + 3");
        eval_expr(&expr, &mut rt).unwrap();
        let expr = get_expr("a /= 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(2.5));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(2.5));

        let expr = get_expr("a = 2 + 3");
        eval_expr(&expr, &mut rt).unwrap();
        let expr = get_expr("a -= 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(3));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(3));

        let expr = get_expr("a = 2 + 3");
        eval_expr(&expr, &mut rt).unwrap();
        let expr = get_expr("a %= 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(1));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(1));

        let expr = get_expr("c /= 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(0));
        assert_eq!(rt.vars.get("c", None).unwrap(), Value::from(0));

        let expr = get_expr(r#"FS = "@""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("@".to_owned()));
        assert_eq!(rt.vars.fs, "@".to_owned());

        let expr = get_expr(r#"FS *= 2"#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("0".to_owned()));
        assert_eq!(rt.vars.fs, "0".to_owned());
    }

    #[test]
    fn assignment_dollar_lvalue() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        rt.set_next_record("john connor".to_owned());

        let expr = get_expr("$0 = 42");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("42".to_owned()));
        assert_eq!(rt.record.get(0).unwrap(), Value::from("42".to_owned()));
        assert_eq!(rt.vars.nf, 1);

        rt.set_next_record("john connor".to_owned());

        let expr = get_expr(r#"$2 = "moo""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("moo".to_owned()));
        assert_eq!(
            rt.record.get(0).unwrap(),
            Value::from("john moo".to_owned())
        );
        assert_eq!(rt.record.get(1).unwrap(), Value::from("john".to_owned()));
        assert_eq!(rt.record.get(2).unwrap(), Value::from("moo".to_owned()));
        assert_eq!(rt.vars.nf, 2);

        rt.set_next_record("john connor".to_owned());

        let expr = get_expr(r#"$10 = "moo""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("moo".to_owned()));
        assert_eq!(
            rt.record.get(0).unwrap(),
            Value::from("john connor        moo".to_owned())
        );
        assert_eq!(rt.record.get(1).unwrap(), Value::from("john".to_owned()));
        assert_eq!(rt.record.get(2).unwrap(), Value::from("connor".to_owned()));
        assert_eq!(rt.record.get(3).unwrap(), Value::from(String::new()));
        assert_eq!(rt.record.get(4).unwrap(), Value::from(String::new()));
        assert_eq!(rt.record.get(5).unwrap(), Value::from(String::new()));
        assert_eq!(rt.record.get(6).unwrap(), Value::from(String::new()));
        assert_eq!(rt.record.get(7).unwrap(), Value::from(String::new()));
        assert_eq!(rt.record.get(8).unwrap(), Value::from(String::new()));
        assert_eq!(rt.record.get(9).unwrap(), Value::from(String::new()));
        assert_eq!(rt.record.get(10).unwrap(), Value::from("moo".to_owned()));
        assert_eq!(rt.vars.nf, 10);

        rt.set_next_record("there are 5 apples".to_owned());

        let expr = get_expr("$3 *= 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("10".to_owned()));
        assert_eq!(
            rt.record.get(0).unwrap(),
            Value::from("there are 10 apples".to_owned())
        );
        assert_eq!(rt.vars.nf, 4);

        rt.set_next_record("there are 5 apples".to_owned());

        let expr = get_expr("$3 /= 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("2.5".to_owned()));
        assert_eq!(
            rt.record.get(0).unwrap(),
            Value::from("there are 2.5 apples".to_owned())
        );
        assert_eq!(rt.vars.nf, 4);

        rt.set_next_record("aaa bbb ccc".to_owned());

        let expr = get_expr("$2 = $3 = 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("2".to_owned()));
        assert_eq!(rt.record.get(0).unwrap(), Value::from("aaa 2 2".to_owned()));
        assert_eq!(rt.vars.nf, 3);

        rt.set_next_record("there are 5 apples".to_owned());

        let expr = get_expr("$3 /= 0");
        let res = eval_expr(&expr, &mut rt);
        match res.unwrap_err() {
            EvaluationError::DivisionByZero => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };
    }

    #[test]
    fn assignment_brackets_lvalue() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        let expr = get_expr("a[0] = 42");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(42));
        assert_eq!(rt.vars.get("a", Some("0")).unwrap(), Value::from(42));

        let expr = get_expr("a[0] /= 2");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(21));
        assert_eq!(rt.vars.get("a", Some("0")).unwrap(), Value::from(21));

        let expr = get_expr("a[1] = 5");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(5));
        assert_eq!(rt.vars.get("a", Some("1")).unwrap(), Value::from(5));
        assert_eq!(rt.vars.get("a", Some("0")).unwrap(), Value::from(21));
    }

    #[test]
    fn record_count() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        rt.set_next_record("john connor".to_owned());
        assert_eq!(rt.vars.nr, 1);
        assert_eq!(rt.vars.fnr, 1);
        rt.set_next_record("john connor".to_owned());
        assert_eq!(rt.vars.nr, 2);
        assert_eq!(rt.vars.fnr, 2);
    }

    #[test]
    fn preincrement() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        // preincrement a variable
        let expr = get_expr("a = 15");
        eval_expr(&expr, &mut rt).unwrap();
        let expr = get_expr("++a");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(16));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(16));
        // preincrement an array element
        let expr = get_expr("++b[0]");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(1));
        assert_eq!(rt.vars.get("b", Some("0")).unwrap(), Value::from(1));
        // preincrement a field value
        rt.set_next_record("10".to_owned());
        let expr = get_expr("++$1");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("11".to_owned()));
        assert_eq!(rt.record.get(1).unwrap(), Value::from("11".to_owned()));
    }

    #[test]
    fn postincrement() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        // postincrement a variable
        let expr = get_expr("a = 15");
        eval_expr(&expr, &mut rt).unwrap();
        let expr = get_expr("a++");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(15));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(16));
        // postincrement an array element
        let expr = get_expr("b[0]++");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(rt.vars.get("b", Some("0")).unwrap(), Value::from(1));
        // postincrement a field value
        rt.set_next_record("10".to_owned());
        let expr = get_expr("$1++");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("10".to_owned()));
        assert_eq!(rt.record.get(1).unwrap(), Value::from("11".to_owned()));
    }

    #[test]
    fn predecrement() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        // preincrement a variable
        let expr = get_expr("a = 15");
        eval_expr(&expr, &mut rt).unwrap();
        let expr = get_expr("--a");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(14));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(14));
        // preincrement an array element
        let expr = get_expr("--b[0]");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(-1));
        assert_eq!(rt.vars.get("b", Some("0")).unwrap(), Value::from(-1));
        // preincrement a field value
        rt.set_next_record("10".to_owned());
        let expr = get_expr("--$1");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("9".to_owned()));
        assert_eq!(rt.record.get(1).unwrap(), Value::from("9".to_owned()));
    }

    #[test]
    fn postdecrement() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        // postincrement a variable
        let expr = get_expr("a = 15");
        eval_expr(&expr, &mut rt).unwrap();
        let expr = get_expr("a--");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(15));
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(14));
        // postincrement an array element
        let expr = get_expr("b[0]--");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(rt.vars.get("b", Some("0")).unwrap(), Value::from(-1));
        // postincrement a field value
        rt.set_next_record("10".to_owned());
        let expr = get_expr("$1--");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from("10".to_owned()));
        assert_eq!(rt.record.get(1).unwrap(), Value::from("9".to_owned()));
    }

    #[test]
    fn regexp() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        rt.set_next_record("john connor".to_owned());

        let expr = get_expr("/^j.*r$/");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = get_expr("/jane/");
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(false));
    }

    #[test]
    fn r#match() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        rt.set_next_record("john connor".to_owned());

        let expr = get_expr(r#"$2 ~ "con.or""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));

        rt.set_next_record("john cannor".to_owned());
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(false));

        rt.set_next_record("john connor".to_owned());

        let expr = get_expr(r#"$2 !~ "con.or""#);
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(false));

        rt.set_next_record("john cannor".to_owned());
        let res = eval_expr(&expr, &mut rt);
        assert_eq!(res.unwrap(), Value::from(true));
    }

    #[test]
    fn scalar_array_misuse() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();

        let expr = get_expr("FS[0]=42");
        let err = eval_expr(&expr, &mut rt).unwrap_err();
        match err {
            EvaluationError::UseScalarAsArray => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };
    }
}
