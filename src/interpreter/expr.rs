use crate::{
    errors::EvaluationError,
    interpreter::{Context, Eval, Value},
    parser::expr::{Expr, LValueType},
};
use std::collections::hash_map::Entry;

impl Eval for Expr {
    fn eval<'a>(&'a self, cxt: &mut Context<'a>) -> Result<Value, EvaluationError> {
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
                Ok(Value::Number(l.eval(cxt)?.as_number() / rvalue))
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
                    let index = e.eval(cxt)?.as_number() as usize;
                    if index == 0 {
                        return Ok(Value::from(cxt.line.to_owned()));
                    }
                    match cxt.fields.get(index - 1) {
                        Some(field) => Ok(Value::from(field.to_string())),
                        None => Ok(Value::from(String::new())),
                    }
                },
                LValueType::Name(name) => match cxt.vars.entry(name.as_str()) {
                    Entry::Occupied(entry) => Ok(entry.get().clone()),
                    Entry::Vacant(entry) => Ok(entry.insert(Value::from(String::new())).clone()),
                },
                _ => unimplemented!(),
            },
            Expr::UnaryMinus(um) => Ok(Value::from(-um.eval(cxt)?.as_number())),
            Expr::UnaryPlus(up) => up.eval(cxt),
            Expr::Grouping(g) => g.eval(cxt),
            Expr::Number(n) => Ok(Value::from(*n)),
            Expr::String(s) => Ok(Value::from(s.to_owned())),
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
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 3f64);

        let expr = parse_expr_str("1 - 2");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), -1f64);

        let expr = parse_expr_str("1 / 2");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 0.5);

        let expr = parse_expr_str("2 * 3");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 6f64);

        let expr = parse_expr_str("2 / 0");
        let res = expr.eval(&mut cxt);
        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);

        let expr = parse_expr_str(r#"2 / "a""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);

        let expr = parse_expr_str(r#"2 * "a""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 0.0);

        let expr = parse_expr_str("2 ^ 3");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 8.0);

        let expr = parse_expr_str("(2 + 1) ^ 3");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 27.0);

        let expr = parse_expr_str("7 % 3");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 1.0);

        let expr = parse_expr_str("- 2 ^ 3");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), -8.0);

        let expr = parse_expr_str(r#"2 + "3""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 5.0);
    }

    #[test]
    fn comparison() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("2 < 3");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("2 > 3");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = parse_expr_str(r#"2 == "2""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""2" == 2"#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""a" == "b""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = parse_expr_str(r#""1" < "a""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""a" < "b""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""a" > "b""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(false));
    }

    #[test]
    fn concat() {
        let mut cxt = Context::new();

        let expr = parse_expr_str(r#"1 " aaa " 2"#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("1 aaa 2".to_owned()));

        let expr = parse_expr_str(r#""aaa" (1 < 2)"#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("aaa1".to_owned()));

        let expr = parse_expr_str(r#""aaa" (1 == 2)"#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("aaa0".to_owned()));

        let expr = parse_expr_str("1 2");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("12".to_owned()));
    }

    #[test]
    fn logical_operation() {
        let mut cxt = Context::new();

        let expr = parse_expr_str("1 && 2");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str(r#""" && 2"#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = parse_expr_str(r#"1 && "2""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("1 && 0");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(false));

        let expr = parse_expr_str("1 < 2 && 3");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("0 || 1");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("1 || 0");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(true));

        let expr = parse_expr_str("!(1 || 0)");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(false));
    }

    #[test]
    fn conditional() {
        let mut cxt = Context::new();

        let expr = parse_expr_str(r#"1 == 1 ? "OK" : "KO""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("OK".to_owned()));

        let expr = parse_expr_str(r#"1 != 1 ? "OK" : "KO""#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("KO".to_owned()));

        let expr = parse_expr_str(r#"(1 == 1 ? "OK" : 2) + 2"#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(2.0));

        let expr = parse_expr_str(r#"(1 < 1 ? "OK" : 2) + 2"#);
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(4.0));
    }

    #[test]
    fn lvalue() {
        let mut cxt = Context::new();

        cxt.set_line("john connor");

        let expr = parse_expr_str("$0");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("john connor".to_owned()));

        let expr = parse_expr_str("$1");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("john".to_owned()));

        let expr = parse_expr_str("$2");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("connor".to_owned()));

        let expr = parse_expr_str("$3");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from(String::new()));

        let expr = parse_expr_str("$(2 - 1)");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("john".to_owned()));

        let expr = parse_expr_str("$(1 != 1)");
        let res = expr.eval(&mut cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::from("john connor".to_owned()));
    }
}
