use crate::{
    errors::EvaluationError,
    interpreter::{Context, Eval, Value},
    parser::expr::{AssignType, Expr, LValueType},
};
use std::collections::{hash_map::Entry, HashMap};

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
                LValueType::Dollar(e) => {
                    let index = e.eval(cxt)?.as_number() as isize;
                    if index < 0 {
                        return Err(EvaluationError::NegativeFieldIndex(index));
                    }
                    if index == 0 {
                        return Ok(Value::String(cxt.record.to_owned()));
                    }
                    match cxt.fields.get(index as usize - 1) {
                        Some(field) => Ok(Value::String(field.to_owned())),
                        None => Ok(Value::Uninitialised),
                    }
                },
                LValueType::Name(name) => match cxt.awk_vars.get(&name) {
                    Some(value) => Ok(value),
                    None => match cxt.vars.entry(name.to_owned()) {
                        Entry::Occupied(entry) => Ok(entry.get().clone()),
                        Entry::Vacant(entry) => Ok(entry.insert(Value::Uninitialised).clone()),
                    },
                },
                LValueType::Brackets(name, key) => {
                    let mut key_str = String::new();
                    for expr in &key.0 {
                        if !key_str.is_empty() {
                            key_str.push_str(&cxt.awk_vars.subsep);
                        }
                        key_str.push_str(&expr.eval(cxt)?.as_string());
                    }
                    match cxt.arrays.entry(name.to_owned()) {
                        Entry::Occupied(mut entry) => match entry.get_mut().entry(key_str) {
                            Entry::Occupied(entry) => Ok(entry.get().clone()),
                            Entry::Vacant(entry) => Ok(entry.insert(Value::Uninitialised).clone()),
                        },
                        Entry::Vacant(entry) => {
                            let mut value = HashMap::new();
                            value.insert(key_str, Value::Uninitialised);
                            entry.insert(value);
                            Ok(Value::Uninitialised)
                        },
                    }
                },
            },
            Expr::Assign(ty, lvalue, rvalue) => {
                let new_value = rvalue.eval(cxt)?;
                match lvalue {
                    LValueType::Name(name) => match ty {
                        AssignType::Normal => {
                            let ret = Ok(new_value.clone());
                            if !cxt.awk_vars.set(name, &new_value) {
                                cxt.vars.insert(name.to_owned(), new_value);
                            }
                            ret
                        },
                        _ => {
                            if let Some(value) = cxt.awk_vars.get(name) {
                                let result = Value::from(Value::compute(
                                    ty,
                                    &Value::from(value),
                                    &new_value,
                                )?);
                                cxt.awk_vars.set(name, &result);
                                return Ok(result);
                            }
                            match cxt.vars.entry(name.to_owned()) {
                                Entry::Occupied(mut entry) => {
                                    let result =
                                        Value::from(Value::compute(ty, entry.get(), &new_value)?);
                                    entry.insert(result.clone());
                                    Ok(result)
                                },
                                Entry::Vacant(entry) => {
                                    let result = Value::from(Value::compute(
                                        ty,
                                        &Value::Uninitialised,
                                        &new_value,
                                    )?);
                                    entry.insert(result.clone());
                                    Ok(result)
                                },
                            }
                        },
                    },
                    LValueType::Dollar(e) => {
                        let index = e.eval(cxt)?.as_number() as isize;
                        if index < 0 {
                            return Err(EvaluationError::NegativeFieldIndex(index));
                        }
                        let index = index as usize;
                        match ty {
                            AssignType::Normal => {
                                let new_value_str = new_value.as_string();
                                let ret = Ok(Value::from(new_value_str.to_owned()));
                                if index == 0 {
                                    cxt.update_record(new_value_str);
                                } else {
                                    cxt.update_field(index - 1, |_| Ok(new_value_str.to_owned()))?;
                                }
                                ret
                            },
                            _ => {
                                cxt.update_field(index - 1, |old_field| {
                                    let result = if let Some(old_field) = old_field {
                                        Value::compute(
                                            ty,
                                            &Value::from(old_field.to_owned()),
                                            &new_value,
                                        )?
                                    } else {
                                        Value::compute(ty, &Value::Uninitialised, &new_value)?
                                    };
                                    Ok(result.to_string())
                                })?;
                                Ok(Value::from(cxt.fields.get(index - 1).unwrap().to_owned()))
                            },
                        }
                    },
                    _ => unimplemented!(),
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
        assert!(cxt.vars.is_empty());

        let expr = parse_expr_str("nf");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(cxt.vars.get("nf"), Some(&Value::Uninitialised));
    }

    #[test]
    fn array_lvalue() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("a[0]");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(
            cxt.arrays.get("a").unwrap().get("0"),
            Some(&Value::Uninitialised)
        );

        let expr = parse_expr_str("b[0,1,2]");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(
            cxt.arrays.get("b").unwrap().get("012"),
            Some(&Value::Uninitialised)
        );

        cxt.awk_vars.subsep = String::from("#");
        let expr = parse_expr_str("b[0,1,2]");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::Uninitialised);
        assert_eq!(
            cxt.arrays.get("b").unwrap().get("0#1#2"),
            Some(&Value::Uninitialised)
        );
    }

    #[test]
    fn assignment_name_lvalue() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("a = 42");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(42));
        assert_eq!(cxt.vars.get("a"), Some(&Value::from(42)));

        let expr = parse_expr_str("a = b = 5");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(5));
        assert_eq!(cxt.vars.get("a"), Some(&Value::from(5)));
        assert_eq!(cxt.vars.get("b"), Some(&Value::from(5)));

        let expr = parse_expr_str("a ^= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(25));
        assert_eq!(cxt.vars.get("a"), Some(&Value::from(25)));

        let expr = parse_expr_str("a = 2 + 3");
        expr.eval(&mut cxt).unwrap();
        let expr = parse_expr_str("a *= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(10));
        assert_eq!(cxt.vars.get("a"), Some(&Value::from(10)));

        let expr = parse_expr_str("a = 2 + 3");
        expr.eval(&mut cxt).unwrap();
        let expr = parse_expr_str("a /= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(2.5));
        assert_eq!(cxt.vars.get("a"), Some(&Value::from(2.5)));

        let expr = parse_expr_str("a = 2 + 3");
        expr.eval(&mut cxt).unwrap();
        let expr = parse_expr_str("a -= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(3));
        assert_eq!(cxt.vars.get("a"), Some(&Value::from(3)));

        let expr = parse_expr_str("a = 2 + 3");
        expr.eval(&mut cxt).unwrap();
        let expr = parse_expr_str("a %= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(1));
        assert_eq!(cxt.vars.get("a"), Some(&Value::from(1)));

        let expr = parse_expr_str("c /= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(0));
        assert_eq!(cxt.vars.get("c"), Some(&Value::from(0)));

        let expr = parse_expr_str(r#"FS = "@""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("@".to_owned()));
        assert_eq!(cxt.awk_vars.fs, "@".to_owned());

        let expr = parse_expr_str(r#"FS *= 2"#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from(0));
        assert_eq!(cxt.awk_vars.fs, "0".to_owned());
    }

    #[test]
    fn assignment_dollar_lvalue() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());

        let expr = parse_expr_str("$0 = 42");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("42".to_owned()));
        assert_eq!(cxt.record, "42".to_owned());
        assert_eq!(cxt.fields, vec!["42".to_owned()]);
        assert_eq!(cxt.awk_vars.nf, 1);

        cxt.set_next_record("john connor".to_owned());

        let expr = parse_expr_str(r#"$2 = "moo""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("moo".to_owned()));
        assert_eq!(cxt.record, "john moo".to_owned());
        assert_eq!(cxt.fields, vec!["john".to_owned(), "moo".to_owned()]);
        assert_eq!(cxt.awk_vars.nf, 2);

        cxt.set_next_record("john connor".to_owned());

        let expr = parse_expr_str(r#"$10 = "moo""#);
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("moo".to_owned()));
        assert_eq!(cxt.record, "john connor        moo".to_owned());
        assert_eq!(
            cxt.fields,
            vec![
                "john".to_owned(),
                "connor".to_owned(),
                String::new(),
                String::new(),
                String::new(),
                String::new(),
                String::new(),
                String::new(),
                String::new(),
                "moo".to_owned(),
            ]
        );
        assert_eq!(cxt.awk_vars.nf, 10);

        cxt.set_next_record("there are 5 apples".to_owned());

        let expr = parse_expr_str("$3 *= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("10".to_owned()));
        assert_eq!(cxt.record, "there are 10 apples".to_owned());
        assert_eq!(cxt.awk_vars.nf, 4);

        cxt.set_next_record("there are 5 apples".to_owned());

        let expr = parse_expr_str("$3 /= 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("2.5".to_owned()));
        assert_eq!(cxt.record, "there are 2.5 apples".to_owned());
        assert_eq!(cxt.awk_vars.nf, 4);

        cxt.set_next_record("aaa bbb ccc".to_owned());

        let expr = parse_expr_str("$2 = $3 = 2");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap(), Value::from("2".to_owned()));
        assert_eq!(cxt.record, "aaa 2 2".to_owned());
        assert_eq!(cxt.awk_vars.nf, 3);

        cxt.set_next_record("there are 5 apples".to_owned());

        let expr = parse_expr_str("$3 /= 0");
        let res = expr.eval(&mut cxt);
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);
    }

    #[test]
    fn record_count() {
        let mut cxt = Context::new();

        cxt.set_next_record("john connor".to_owned());
        assert_eq!(cxt.awk_vars.nr, 1);
        assert_eq!(cxt.awk_vars.fnr, 1);
        cxt.set_next_record("john connor".to_owned());
        assert_eq!(cxt.awk_vars.nr, 2);
        assert_eq!(cxt.awk_vars.fnr, 2);
    }
}
