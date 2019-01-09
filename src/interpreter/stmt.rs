use crate::{
    errors::EvaluationError,
    interpreter::{value::Value, Context, Eval},
    parser::ast::{AssignType, Stmt},
};

#[derive(Debug)]
pub enum StmtResult {
    Break,
    Continue,
    Return(Value),
    Exit(isize),
}

impl Eval for Stmt {
    type EvalResult = Option<StmtResult>;
    fn eval(&self, cxt: &mut Context) -> Result<Option<StmtResult>, EvaluationError> {
        match self {
            Stmt::ForIn(var, array, body) => {
                if let Some(keys) = cxt.arrays.keys(array) {
                    for key in keys {
                        cxt.vars
                            .set(&AssignType::Normal, &var, Value::from(key.to_owned()))?;
                        if let Some(res) = body.eval(cxt)? {
                            match res {
                                StmtResult::Break => break,
                                StmtResult::Continue => (),
                                _ => unimplemented!("{:?}", res),
                            }
                        }
                    }
                }
                Ok(None)
            },
            Stmt::For(init, cond, step, body) => match (init, cond, step) {
                (Some(init), Some(cond), Some(step)) => {
                    init.eval(cxt)?;
                    while cond.eval(cxt)?.as_bool() {
                        if let Some(res) = body.eval(cxt)? {
                            match res {
                                StmtResult::Break => break,
                                StmtResult::Continue => (),
                                _ => unimplemented!("{:?}", res),
                            }
                        }
                        step.eval(cxt)?;
                    }
                    Ok(None)
                },
                (..) => unimplemented!(),
            },
            Stmt::DoWhile(cond, stmt) => {
                loop {
                    if let Some(res) = stmt.eval(cxt)? {
                        match res {
                            StmtResult::Break => break,
                            StmtResult::Continue => (),
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                    if !cond.eval(cxt)?.as_bool() {
                        break;
                    }
                }
                Ok(None)
            },
            Stmt::While(cond, stmt) => {
                while cond.eval(cxt)?.as_bool() {
                    if let Some(res) = stmt.eval(cxt)? {
                        match res {
                            StmtResult::Break => break,
                            StmtResult::Continue => continue,
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                }
                Ok(None)
            },
            Stmt::IfElse(cond, ok, ko) => {
                if cond.eval(cxt)?.as_bool() {
                    ok.eval(cxt)
                } else if let Some(ko) = ko {
                    ko.eval(cxt)
                } else {
                    Ok(None)
                }
            },
            Stmt::Block(stmts) => {
                for stmt in &stmts.0 {
                    if let Some(res) = stmt.eval(cxt)? {
                        match res {
                            StmtResult::Break => return Ok(Some(StmtResult::Break)),
                            StmtResult::Continue => return Ok(Some(StmtResult::Continue)),
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                }
                Ok(None)
            },
            Stmt::Expr(e) => e.eval(cxt).map(|_| None),
            Stmt::Break => Ok(Some(StmtResult::Break)),
            Stmt::Continue => Ok(Some(StmtResult::Continue)),
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

    #[test]
    fn r#break() {
        let mut cxt = Context::new();
        let stmt = get_stmt("while (a < 10) if (a < 5) a += 2; else break;");
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a"), Value::from(6.0), "{:?}", stmt);

        let stmt = get_stmt("do if (b < 5) b += 2; else break; while (b < 10)");
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(cxt.vars.get("b"), Value::from(6.0), "{:?}", stmt);

        let stmt =
            get_stmt(r#"for (i = 0; i < 10; i++) { c = c i; if (c == "2100123") break; c = i c }"#);
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(
            cxt.vars.get("c"),
            Value::from("2100123".to_owned()),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn r#continue() {
        let mut cxt = Context::new();
        let stmt = get_stmt("while (a1 < 10) { a1++; if (a1 < 5) continue; a2++; }");
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a1"), Value::from(10.0), "{:?}", stmt);
        assert_eq!(cxt.vars.get("a2"), Value::from(6.0), "{:?}", stmt);

        let stmt = get_stmt("do { b1++; if (b1 < 5) continue; b2++; } while (b1 < 10)");
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(cxt.vars.get("b1"), Value::from(10.0), "{:?}", stmt);
        assert_eq!(cxt.vars.get("b2"), Value::from(6.0), "{:?}", stmt);

        let stmt =
            get_stmt(r#"for (i = 0; i < 5; i++) { c = c i; if (c % 2 == 0) continue; c = i c }"#);
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(
            cxt.vars.get("c"),
            Value::from("3101234".to_owned()),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn for_in() {
        let mut cxt = Context::new();
        let stmt = get_stmt(
            r#"{
            a[0] = 5;
            a[1] = 10;
            a[2] = 15;
            a[3] = 20;
            for (i in a) {
                a[i] *= 2;
            }
        }"#,
        );
        stmt.eval(&mut cxt).unwrap();
        assert_eq!(
            cxt.arrays.get("a", "0"),
            Ok(Value::from(10.0)),
            "{:?}",
            stmt
        );
        assert_eq!(
            cxt.arrays.get("a", "1"),
            Ok(Value::from(20.0)),
            "{:?}",
            stmt
        );
        assert_eq!(
            cxt.arrays.get("a", "2"),
            Ok(Value::from(30.0)),
            "{:?}",
            stmt
        );
        assert_eq!(
            cxt.arrays.get("a", "3"),
            Ok(Value::from(40.0)),
            "{:?}",
            stmt
        );
    }
}
