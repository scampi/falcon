use crate::{
    errors::EvaluationError,
    interpreter::{Context, Eval, Value},
    parser::expr::Expr,
};

impl Eval for Expr {
    fn eval(&self, cxt: &Context) -> Result<Value, EvaluationError> {
        match self {
            Expr::Mod(l, r) => Ok(Value::Number(
                l.eval(cxt)?.as_number() % r.eval(cxt)?.as_number(),
            )),
            Expr::Pow(l, r) => Ok(Value::Number(
                l.eval(cxt)?.as_number().powf(r.eval(cxt)?.as_number()),
            )),
            Expr::Add(l, r) => Ok(Value::Number(
                l.eval(cxt)?.as_number() + r.eval(cxt)?.as_number(),
            )),
            Expr::Minus(l, r) => Ok(Value::Number(
                l.eval(cxt)?.as_number() - r.eval(cxt)?.as_number(),
            )),
            Expr::Div(l, r) => {
                let rvalue = r.eval(cxt)?.as_number();
                if rvalue == 0.0 {
                    return Err(EvaluationError::DivisionByZero);
                }
                Ok(Value::Number(l.eval(cxt)?.as_number() / rvalue))
            },
            Expr::Mul(l, r) => Ok(Value::Number(
                l.eval(cxt)?.as_number() * r.eval(cxt)?.as_number(),
            )),
            Expr::UnaryMinus(um) => Ok(Value::Number(-um.eval(cxt)?.as_number())),
            Expr::UnaryPlus(up) => up.eval(cxt),
            Expr::Grouping(g) => g.eval(cxt),
            Expr::Number(n) => Ok(Value::Number(*n)),
            Expr::String(s) => Ok(Value::String(s)),
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
        let cxt = Context::new();

        let expr = parse_expr_str("1 + 2");
        let res = expr.eval(&cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 3f64);

        let expr = parse_expr_str("1 - 2");
        let res = expr.eval(&cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), -1f64);

        let expr = parse_expr_str("1 / 2");
        let res = expr.eval(&cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 0.5);

        let expr = parse_expr_str("2 * 3");
        let res = expr.eval(&cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 6f64);

        let expr = parse_expr_str("2 / 0");
        let res = expr.eval(&cxt);
        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);

        let expr = parse_expr_str(r#"2 / "a""#);
        let res = expr.eval(&cxt);
        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), EvaluationError::DivisionByZero);

        let expr = parse_expr_str(r#"2 * "a""#);
        let res = expr.eval(&cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 0.0);

        let expr = parse_expr_str("2 ^ 3");
        let res = expr.eval(&cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 8.0);

        let expr = parse_expr_str("(2 + 1) ^ 3");
        let res = expr.eval(&cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 27.0);

        let expr = parse_expr_str("7 % 3");
        let res = expr.eval(&cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), 1.0);

        let expr = parse_expr_str("- 2 ^ 3");
        let res = expr.eval(&cxt);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().as_number(), -8.0);
    }
}
