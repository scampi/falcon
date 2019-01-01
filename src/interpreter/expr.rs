use crate::{
    errors::EvaluationError,
    interpreter::{Context, Eval, value::Value},
    interpreter::arrays::Arrays,
    parser::expr::{Expr, LValueType},
};

impl Eval for Expr {
    fn eval(&self, cxt: &mut Context) -> Result<Value, EvaluationError> {
        match self {
            Expr::Mod(l, r) => Ok(Value::from(
                l.eval(cxt)?.as_number() % r.eval(cxt)?.as_number(),
            )),
            Expr::Pow(l, r) => Ok(Value::from(
                l.eval(cxt)?.as_number().powf(r.eval(cxt)?.as_number()),
            )),
            Expr::Add(l, r) => Ok(Value::from(
                l.eval(cxt)?.as_number() + r.eval(cxt)?.as_number(),
            )),
            Expr::Minus(l, r) => Ok(Value::from(
                l.eval(cxt)?.as_number() - r.eval(cxt)?.as_number(),
            )),
            Expr::Div(l, r) => {
                let rvalue = r.eval(cxt)?.as_number();
                if rvalue == 0.0 {
                    return Err(EvaluationError::DivisionByZero);
                }
                Ok(Value::from(l.eval(cxt)?.as_number() / rvalue))
            },
            Expr::Mul(l, r) => Ok(Value::from(
                l.eval(cxt)?.as_number() * r.eval(cxt)?.as_number(),
            )),
            Expr::Comparison(op, l, r) => {
                let lvalue = l.eval(cxt)?;
                let rvalue = r.eval(cxt)?;
                Ok(Value::compare(op, &lvalue, &rvalue))
            },
            Expr::Concat(l, r) => Ok(Value::String(format!("{}{}", l.eval(cxt)?, r.eval(cxt)?))),
            Expr::LogicalAnd(l, r) => {
                if l.eval(cxt)?.as_bool() {
                    Ok(Value::from(r.eval(cxt)?.as_bool()))
                } else {
                    Ok(Value::from(false))
                }
            },
            Expr::LogicalOr(l, r) => {
                if l.eval(cxt)?.as_bool() {
                    Ok(Value::from(true))
                } else {
                    Ok(Value::from(r.eval(cxt)?.as_bool()))
                }
            },
            Expr::LogicalNot(e) => Ok(Value::from(!e.eval(cxt)?.as_bool())),
            Expr::Conditional(cond, ok, ko) => {
                if cond.eval(cxt)?.as_bool() {
                    Ok(Value::from(ok.eval(cxt)?))
                } else {
                    Ok(Value::from(ko.eval(cxt)?))
                }
            },
            Expr::LValue(lvalue) => match lvalue {
                LValueType::Name(name) => Ok(cxt.vars.get(name)),
                LValueType::Dollar(e) => {
                    let index = e.eval(cxt)?.as_number() as isize;
                    cxt.record.get(index)
                },
                LValueType::Brackets(name, key) => {
                    let key_str = Arrays::array_key(cxt, key)?;
                    cxt.arrays.get(name, key_str)
                }
            },
            Expr::Assign(ty, lvalue, rvalue) => {
                let new_value = rvalue.eval(cxt)?;
                match lvalue {
                    LValueType::Name(name) => cxt.vars.set(ty, name, new_value),
                    LValueType::Dollar(e) => {
                        let index = e.eval(cxt)?.as_number() as isize;
                        cxt.record.set(&mut cxt.vars, ty, index, new_value)
                    },
                    LValueType::Brackets(name, key) => {
                        let key_str = Arrays::array_key(cxt, key)?;
                        cxt.arrays.set(ty, name, key_str, new_value)
                    },
                }
            },
            Expr::UnaryMinus(um) => Ok(Value::from(-um.eval(cxt)?.as_number())),
            Expr::UnaryPlus(up) => up.eval(cxt),
            Expr::Grouping(g) => g.eval(cxt),
            Expr::Number(n) => Ok(Value::from(*n)),
            Expr::String(s) => Ok(Value::String(s.to_owned())),
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::expr::parse_expr_str;

    #[test]
    fn arithmetic() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("1 + 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(3f64));

        let expr = parse_expr_str("1 - 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(-1f64));

        let expr = parse_expr_str("1 / 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(0.5));

        let expr = parse_expr_str("2 * 3");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(6f64));

        let expr = parse_expr_str("2 / 0");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);

        let expr = parse_expr_str(r#"2 / "a""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);

        let expr = parse_expr_str(r#"2 * "a""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(0.0));

        let expr = parse_expr_str("2 ^ 3");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(8.0));

        let expr = parse_expr_str("(2 + 1) ^ 3");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(27.0));

        let expr = parse_expr_str("7 % 3");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(1.0));

        let expr = parse_expr_str("- 2 ^ 3");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(-8.0));

        let expr = parse_expr_str(r#"2 + "3""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(5.0));
    }

    #[test]
    fn comparison() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("2 < 3");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("2 > 3");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = parse_expr_str(r#"2 == "2""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""2" == 2"#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""a" == "b""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = parse_expr_str(r#""1" < "a""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""a" < "b""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""a" > "b""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(false));
    }

    #[test]
    fn concat() {
        let mut cxt = Context::new();

        let expr = parse_expr_str(r#"1 " aaa " 2"#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("1 aaa 2".to_owned()));

        let expr = parse_expr_str(r#""aaa" (1 < 2)"#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("aaa1".to_owned()));

        let expr = parse_expr_str(r#""aaa" (1 == 2)"#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("aaa0".to_owned()));

        let expr = parse_expr_str("1 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("12".to_owned()));
    }

    #[test]
    fn logical_operation() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("1 && 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""" && 2"#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = parse_expr_str(r#"1 && "2""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("1 && 0");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = parse_expr_str("1 < 2 && 3");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("0 || 1");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("1 || 0");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("!(1 || 0)");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(false));
    }

    #[test]
    fn conditional() {
        let mut cxt = Context::new();

        let expr = parse_expr_str(r#"1 == 1 ? "OK" : "KO""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("OK".to_owned()));

        let expr = parse_expr_str(r#"1 != 1 ? "OK" : "KO""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("KO".to_owned()));

        let expr = parse_expr_str(r#"(1 == 1 ? "OK" : 2) + 2"#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(2.0));

        let expr = parse_expr_str(r#"(1 < 1 ? "OK" : 2) + 2"#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(4.0));
    }

    #[test]
    fn field_lvalue() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());

        let expr = parse_expr_str("$0");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("john connor".to_owned()));

        let expr = parse_expr_str("$1");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("john".to_owned()));

        let expr = parse_expr_str("$2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("connor".to_owned()));

        let expr = parse_expr_str("$3");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::Uninitialised);

        let expr = parse_expr_str("$(2 - 1)");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("john".to_owned()));

        let expr = parse_expr_str("$(1 != 1)");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("john connor".to_owned()));

        let expr = parse_expr_str("$(-42)");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap_err(), EvaluationError::NegativeFieldIndex(-42));
    }

    #[test]
    fn var_lvalue() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());

        let expr = parse_expr_str("NF");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(2.0));
        assert!(!cxt.vars.has_user_vars());

        let expr = parse_expr_str("nf");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(cxt.vars.get("nf"), Value::Uninitialised);
    }

    #[test]
    fn array_lvalue() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("a[0]");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(
            cxt.arrays.get("a", "0".to_owned()),
            Ok(Value::Uninitialised)
        );

        let expr = parse_expr_str("b[0,1,2]");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(
            cxt.arrays.get("b", "012".to_owned()),
            Ok(Value::Uninitialised)
        );

        cxt.vars.subsep = String::from("#");
        let expr = parse_expr_str("b[0,1,2]");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(
            cxt.arrays.get("b", "0#1#2".to_owned()),
            Ok(Value::Uninitialised)
        );
    }

    #[test]
    fn assignment_name_lvalue() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("a = 42");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(42));
        assert_eq!(cxt.vars.get("a"), Value::from(42));

        let expr = parse_expr_str("a = b = 5");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(5));
        assert_eq!(cxt.vars.get("a"), Value::from(5));
        assert_eq!(cxt.vars.get("b"), Value::from(5));

        let expr = parse_expr_str("a ^= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(25));
        assert_eq!(cxt.vars.get("a"), Value::from(25));

        let expr = parse_expr_str("a = 2 + 3");
        expr.eval(&mut cxt).unwrap();
        let expr = parse_expr_str("a *= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(10));
        assert_eq!(cxt.vars.get("a"), Value::from(10));

        let expr = parse_expr_str("a = 2 + 3");
        expr.eval(&mut cxt).unwrap();
        let expr = parse_expr_str("a /= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(2.5));
        assert_eq!(cxt.vars.get("a"), Value::from(2.5));

        let expr = parse_expr_str("a = 2 + 3");
        expr.eval(&mut cxt).unwrap();
        let expr = parse_expr_str("a -= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(3));
        assert_eq!(cxt.vars.get("a"), Value::from(3));

        let expr = parse_expr_str("a = 2 + 3");
        expr.eval(&mut cxt).unwrap();
        let expr = parse_expr_str("a %= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(1));
        assert_eq!(cxt.vars.get("a"), Value::from(1));

        let expr = parse_expr_str("c /= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(0));
        assert_eq!(cxt.vars.get("c"), Value::from(0));

        let expr = parse_expr_str(r#"FS = "@""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("@".to_owned()));
        assert_eq!(cxt.vars.fs, "@".to_owned());

        let expr = parse_expr_str(r#"FS *= 2"#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("0".to_owned()));
        assert_eq!(cxt.vars.fs, "0".to_owned());
    }

    #[test]
    fn assignment_dollar_lvalue() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());

        let expr = parse_expr_str("$0 = 42");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("42".to_owned()));
        assert_eq!(cxt.record.get(0), Ok(Value::from("42".to_owned())));
        assert_eq!(cxt.vars.nf, 1);

        cxt.set_next_record("john connor".to_owned());

        let expr = parse_expr_str(r#"$2 = "moo""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("moo".to_owned()));
        assert_eq!(cxt.record.get(0), Ok(Value::from("john moo".to_owned())));
        assert_eq!(cxt.record.get(1), Ok(Value::from("john".to_owned())));
        assert_eq!(cxt.record.get(2), Ok(Value::from("moo".to_owned())));
        assert_eq!(cxt.vars.nf, 2);

        cxt.set_next_record("john connor".to_owned());

        let expr = parse_expr_str(r#"$10 = "moo""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("moo".to_owned()));
        assert_eq!(cxt.record.get(0), Ok(Value::from("john connor        moo".to_owned())));
        assert_eq!(cxt.record.get(1), Ok(Value::from("john".to_owned())));
        assert_eq!(cxt.record.get(2), Ok(Value::from("connor".to_owned())));
        assert_eq!(cxt.record.get(3), Ok(Value::from(String::new())));
        assert_eq!(cxt.record.get(4), Ok(Value::from(String::new())));
        assert_eq!(cxt.record.get(5), Ok(Value::from(String::new())));
        assert_eq!(cxt.record.get(6), Ok(Value::from(String::new())));
        assert_eq!(cxt.record.get(7), Ok(Value::from(String::new())));
        assert_eq!(cxt.record.get(8), Ok(Value::from(String::new())));
        assert_eq!(cxt.record.get(9), Ok(Value::from(String::new())));
        assert_eq!(cxt.record.get(10), Ok(Value::from("moo".to_owned())));
        assert_eq!(cxt.vars.nf, 10);

        cxt.set_next_record("there are 5 apples".to_owned());

        let expr = parse_expr_str("$3 *= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("10".to_owned()));
        assert_eq!(cxt.record.get(0), Ok(Value::from("there are 10 apples".to_owned())));
        assert_eq!(cxt.vars.nf, 4);

        cxt.set_next_record("there are 5 apples".to_owned());

        let expr = parse_expr_str("$3 /= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("2.5".to_owned()));
        assert_eq!(cxt.record.get(0), Ok(Value::from("there are 2.5 apples".to_owned())));
        assert_eq!(cxt.vars.nf, 4);

        cxt.set_next_record("aaa bbb ccc".to_owned());

        let expr = parse_expr_str("$2 = $3 = 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("2".to_owned()));
        assert_eq!(cxt.record.get(0), Ok(Value::from("aaa 2 2".to_owned())));
        assert_eq!(cxt.vars.nf, 3);

        cxt.set_next_record("there are 5 apples".to_owned());

        let expr = parse_expr_str("$3 /= 0");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);
    }

    #[test]
    fn assignment_brackets_lvalue() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("a[0] = 42");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(42));
        assert_eq!(
            cxt.arrays.get("a", "0".to_owned()),
            Ok(Value::from(42))
        );

        let expr = parse_expr_str("a[0] /= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(21));
        assert_eq!(
            cxt.arrays.get("a", "0".to_owned()),
            Ok(Value::from(21))
        );

        let expr = parse_expr_str("a[1] = 5");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(5));
        assert_eq!(
            cxt.arrays.get("a", "1".to_owned()),
            Ok(Value::from(5))
        );
        assert_eq!(
            cxt.arrays.get("a", "0".to_owned()),
            Ok(Value::from(21))
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
}
