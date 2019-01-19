use crate::{
    errors::EvaluationError,
    interpreter::{
        functions::Functions, record::Record, value::Value, variables::Variables, Context, Eval,
    },
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
    fn eval(
        &self,
        cxt: Context,
        vars: &mut Variables,
        record: &mut Record,
        funcs: &Functions,
    ) -> Result<Option<StmtResult>, EvaluationError> {
        match self {
            Stmt::ForIn(var, array, body) => {
                for key in vars.array_keys(array)? {
                    vars.set(&AssignType::Normal, &var, None, Value::from(key.to_owned()))?;
                    if let Some(res) = body.eval(cxt, vars, record, funcs)? {
                        match res {
                            StmtResult::Break => break,
                            StmtResult::Continue => (),
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                }
                Ok(None)
            },
            Stmt::For(init, cond, step, body) => match (init, cond, step) {
                (Some(init), Some(cond), Some(step)) => {
                    init.eval(cxt, vars, record, funcs)?;
                    while cond.eval(cxt, vars, record, funcs)?.as_bool() {
                        if let Some(res) = body.eval(cxt, vars, record, funcs)? {
                            match res {
                                StmtResult::Break => break,
                                StmtResult::Continue => (),
                                _ => unimplemented!("{:?}", res),
                            }
                        }
                        step.eval(cxt, vars, record, funcs)?;
                    }
                    Ok(None)
                },
                (..) => unimplemented!(),
            },
            Stmt::DoWhile(cond, stmt) => {
                loop {
                    if let Some(res) = stmt.eval(cxt, vars, record, funcs)? {
                        match res {
                            StmtResult::Break => break,
                            StmtResult::Continue => (),
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                    if !cond.eval(cxt, vars, record, funcs)?.as_bool() {
                        break;
                    }
                }
                Ok(None)
            },
            Stmt::While(cond, stmt) => {
                while cond.eval(cxt, vars, record, funcs)?.as_bool() {
                    if let Some(res) = stmt.eval(cxt, vars, record, funcs)? {
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
                if cond.eval(cxt, vars, record, funcs)?.as_bool() {
                    ok.eval(cxt, vars, record, funcs)
                } else if let Some(ko) = ko {
                    ko.eval(cxt, vars, record, funcs)
                } else {
                    Ok(None)
                }
            },
            Stmt::Block(stmts) => {
                for stmt in &stmts.0 {
                    if let Some(res) = stmt.eval(cxt, vars, record, funcs)? {
                        match res {
                            StmtResult::Break => return Ok(Some(StmtResult::Break)),
                            StmtResult::Continue => return Ok(Some(StmtResult::Continue)),
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                }
                Ok(None)
            },
            Stmt::Expr(e) => e.eval(cxt, vars, record, funcs).map(|_| None),
            Stmt::Break => Ok(Some(StmtResult::Break)),
            Stmt::Continue => Ok(Some(StmtResult::Continue)),
            Stmt::Return(expr) => match expr {
                Some(expr) => {
                    let ret = expr.eval(cxt, vars, record, funcs)?;
                    Ok(Some(StmtResult::Return(ret)))
                },
                None => Ok(Some(StmtResult::Return(Value::Uninitialised))),
            },
            Stmt::Delete(array, index) => {
                let key_str = Variables::array_key(index.eval(cxt, vars, record, funcs)?)?;
                vars.delete(array, &key_str)?;
                Ok(None)
            },
            _ => unimplemented!("{:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        interpreter::{value::Value, Runtime},
        parser::stmt::get_stmt,
    };

    fn eval_stmt(stmt: &Stmt, rt: &mut Runtime) -> Result<Option<StmtResult>, EvaluationError> {
        stmt.eval(Context::Scalar, &mut rt.vars, &mut rt.record, &rt.funcs)
    }

    #[test]
    fn if_else() {
        let mut rt = Runtime::new();
        let stmt = get_stmt(r#"if ($0 == 1) { a = "OK" } else { a = "KO" }"#);
        rt.set_next_record("1".to_owned());
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", None),
            Ok(Value::from("OK".to_owned())),
            "{:?}",
            stmt
        );

        rt.set_next_record("2".to_owned());
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", None),
            Ok(Value::from("KO".to_owned())),
            "{:?}",
            stmt
        );

        let stmt = get_stmt(r#"if ($1 == 2) a = "OK"; else a = "KO""#);
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", None),
            Ok(Value::from("OK".to_owned())),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn block() {
        let mut rt = Runtime::new();
        let stmt = get_stmt("{ a = 1; b = 2; c = a + b }");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "c", None),
            Ok(Value::from(3.0)),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn r#while() {
        let mut rt = Runtime::new();
        let stmt = get_stmt("while (a < 5) a += 2");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", None),
            Ok(Value::from(6.0)),
            "{:?}",
            stmt
        );

        let stmt = get_stmt("do a += 2; while (a < 5)");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", None),
            Ok(Value::from(8.0)),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn r#for() {
        let mut rt = Runtime::new();
        let stmt = get_stmt("for (i = 0; i < 5; i++) a = a i");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", None),
            Ok(Value::from("01234".to_owned())),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn r#break() {
        let mut rt = Runtime::new();
        let stmt = get_stmt("while (a < 10) if (a < 5) a += 2; else break;");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", None),
            Ok(Value::from(6.0)),
            "{:?}",
            stmt
        );

        let stmt = get_stmt("do if (b < 5) b += 2; else break; while (b < 10)");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "b", None),
            Ok(Value::from(6.0)),
            "{:?}",
            stmt
        );

        let stmt =
            get_stmt(r#"for (i = 0; i < 10; i++) { c = c i; if (c == "2100123") break; c = i c }"#);
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "c", None),
            Ok(Value::from("2100123".to_owned())),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn r#continue() {
        let mut rt = Runtime::new();
        let stmt = get_stmt("while (a1 < 10) { a1++; if (a1 < 5) continue; a2++; }");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a1", None),
            Ok(Value::from(10.0)),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get(Context::Scalar, "a2", None),
            Ok(Value::from(6.0)),
            "{:?}",
            stmt
        );

        let stmt = get_stmt("do { b1++; if (b1 < 5) continue; b2++; } while (b1 < 10)");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "b1", None),
            Ok(Value::from(10.0)),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get(Context::Scalar, "b2", None),
            Ok(Value::from(6.0)),
            "{:?}",
            stmt
        );

        let stmt =
            get_stmt(r#"for (i = 0; i < 5; i++) { c = c i; if (c % 2 == 0) continue; c = i c }"#);
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "c", None),
            Ok(Value::from("3101234".to_owned())),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn for_in() {
        let mut rt = Runtime::new();
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
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", Some("0")),
            Ok(Value::from(10.0)),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", Some("1")),
            Ok(Value::from(20.0)),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", Some("2")),
            Ok(Value::from(30.0)),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", Some("3")),
            Ok(Value::from(40.0)),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn delete() {
        let mut rt = Runtime::new();
        let stmt = get_stmt(
            r#"{
            a[0] = 5;
            a[1] = 10;
            a[2] = 15;
            a[3] = 20;
            for (i in a) {
                if (a[i] < 12) {
                    delete a[i];
                }
            }
        }"#,
        );
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", Some("0")),
            Ok(Value::Uninitialised),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", Some("1")),
            Ok(Value::Uninitialised),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", Some("2")),
            Ok(Value::from(15.0)),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get(Context::Scalar, "a", Some("3")),
            Ok(Value::from(20.0)),
            "{:?}",
            stmt
        );
    }
}
