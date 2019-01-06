use crate::{
    errors::EvaluationError,
    interpreter::{Context, Eval},
    parser::ast::Stmt,
};

impl Eval for Stmt {
    type EvalResult = ();
    fn eval(&self, cxt: &mut Context) -> Result<(), EvaluationError> {
        match self {
            Stmt::For(init, cond, step, body) => match (init, cond, step) {
                (Some(init), Some(cond), Some(step)) => {
                    init.eval(cxt)?;
                    while cond.eval(cxt)?.as_bool() {
                        body.eval(cxt)?;
                        step.eval(cxt)?;
                    }
                    Ok(())
                },
                (..) => unimplemented!(),
            },
            Stmt::DoWhile(cond, stmt) => {
                loop {
                    stmt.eval(cxt)?;
                    if !cond.eval(cxt)?.as_bool() {
                        break;
                    }
                }
                Ok(())
            },
            Stmt::While(cond, stmt) => {
                while cond.eval(cxt)?.as_bool() {
                    stmt.eval(cxt)?
                }
                Ok(())
            },
            Stmt::IfElse(cond, ok, ko) => {
                if cond.eval(cxt)?.as_bool() {
                    ok.eval(cxt)
                } else if let Some(ko) = ko {
                    ko.eval(cxt)
                } else {
                    Ok(())
                }
            },
            Stmt::Block(stmts) => {
                for stmt in &stmts.0 {
                    stmt.eval(cxt)?;
                }
                Ok(())
            },
            Stmt::Expr(e) => e.eval(cxt).map(|_| ()),
            _ => unimplemented!("{:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{interpreter::value::Value, parser::stmt::get_stmt};

    #[test]
    fn if_else() {
        let mut cxt = Context::new();
        let stmt = get_stmt(r#"if ($0 == 1) { a = "OK" } else { a = "KO" }"#);
        cxt.set_next_record("1".to_owned());
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(
            cxt.vars.get("a"),
            Value::from("OK".to_owned()),
            "{:?}",
            stmt
        );

        cxt.set_next_record("2".to_owned());
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(
            cxt.vars.get("a"),
            Value::from("KO".to_owned()),
            "{:?}",
            stmt
        );

        let stmt = get_stmt(r#"if ($1 == 2) a = "OK"; else a = "KO""#);
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(
            cxt.vars.get("a"),
            Value::from("OK".to_owned()),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn block() {
        let mut cxt = Context::new();
        let stmt = get_stmt("{ a = 1; b = 2; c = a + b }");
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(cxt.vars.get("c"), Value::from(3.0), "{:?}", stmt);
    }

    #[test]
    fn r#while() {
        let mut cxt = Context::new();
        let stmt = get_stmt("while (a < 5) a += 2");
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a"), Value::from(6.0), "{:?}", stmt);

        let stmt = get_stmt("do a += 2; while (a < 5)");
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a"), Value::from(8.0), "{:?}", stmt);
    }

    #[test]
    fn r#for() {
        let mut cxt = Context::new();
        let stmt = get_stmt("for (i = 0; i < 5; i++) a = a i");
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(
            cxt.vars.get("a"),
            Value::from("01234".to_owned()),
            "{:?}",
            stmt
        );
    }
}
