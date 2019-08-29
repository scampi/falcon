use crate::{
    errors::EvaluationError,
    interpreter::{value::Value, variables::Variables, Eval, RuntimeMut},
    parser::ast::{AssignType, Stmt},
};
use std::io::Write;

mod print;
pub mod printf;
pub mod redirections;

#[derive(Debug)]
pub enum StmtResult {
    Break,
    Continue,
    Return(Value),
    Exit(isize),
}

impl Eval for Stmt {
    type EvalResult = Option<StmtResult>;
    fn eval<Output>(
        &self,
        rt: &mut RuntimeMut<'_, Output>,
    ) -> Result<Option<StmtResult>, EvaluationError>
    where
        Output: Write,
    {
        match self {
            Stmt::Print(exprs, redir) => print::execute(rt, exprs, redir),
            Stmt::Printf(exprs, redir) => printf::execute(rt, exprs, redir),
            Stmt::ForIn(var, array, body) => {
                for key in rt.vars.array_keys(array)? {
                    rt.vars
                        .set(AssignType::Normal, &var, None, Value::from(key.to_owned()))?;
                    if let Some(res) = body.eval(rt)? {
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
                    init.eval(rt)?;
                    while cond.eval(rt)?.as_bool() {
                        if let Some(res) = body.eval(rt)? {
                            match res {
                                StmtResult::Break => break,
                                StmtResult::Continue => (),
                                _ => unimplemented!("{:?}", res),
                            }
                        }
                        step.eval(rt)?;
                    }
                    Ok(None)
                },
                (..) => unimplemented!(),
            },
            Stmt::DoWhile(cond, stmt) => {
                loop {
                    if let Some(res) = stmt.eval(rt)? {
                        match res {
                            StmtResult::Break => break,
                            StmtResult::Continue => (),
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                    if !cond.eval(rt)?.as_bool() {
                        break;
                    }
                }
                Ok(None)
            },
            Stmt::While(cond, stmt) => {
                while cond.eval(rt)?.as_bool() {
                    if let Some(res) = stmt.eval(rt)? {
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
                if cond.eval(rt)?.as_bool() {
                    ok.eval(rt)
                } else if let Some(ko) = ko {
                    ko.eval(rt)
                } else {
                    Ok(None)
                }
            },
            Stmt::Block(stmts) => {
                for stmt in &stmts.0 {
                    if let Some(res) = stmt.eval(rt)? {
                        match res {
                            StmtResult::Break => return Ok(Some(StmtResult::Break)),
                            StmtResult::Continue => return Ok(Some(StmtResult::Continue)),
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                }
                Ok(None)
            },
            Stmt::Expr(e) => e.eval(rt).map(|_| None),
            Stmt::Break => Ok(Some(StmtResult::Break)),
            Stmt::Continue => Ok(Some(StmtResult::Continue)),
            Stmt::Return(expr) => match expr {
                Some(expr) => {
                    let ret = expr.eval(rt)?;
                    Ok(Some(StmtResult::Return(ret)))
                },
                None => Ok(Some(StmtResult::Return(Value::Uninitialised))),
            },
            Stmt::Delete(array, index) => {
                let key_str = Variables::array_key(index.eval(rt)?)?;
                rt.vars.delete(array, &key_str)?;
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
        parser::{ast::Program, stmt::get_stmt},
    };
    use std::io::Cursor;

    fn eval_stmt(
        stmt: &Stmt,
        rt: &mut Runtime<'_, Cursor<Vec<u8>>>,
    ) -> Result<Option<StmtResult>, EvaluationError> {
        let mut rt_mut = RuntimeMut::new(
            rt.output,
            &mut rt.vars,
            &mut rt.record,
            &rt.funcs,
            &mut rt.redirs,
            &mut rt.rnd,
        );
        stmt.eval(&mut rt_mut)
    }

    #[test]
    fn if_else() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();
        let stmt = get_stmt(r#"if ($0 == 1) { a = "OK" } else { a = "KO" }"#);
        rt.set_next_record("1".to_owned());
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("a", None).unwrap(),
            Value::from("OK".to_owned()),
            "{:?}",
            stmt
        );

        rt.set_next_record("2".to_owned());
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("a", None).unwrap(),
            Value::from("KO".to_owned()),
            "{:?}",
            stmt
        );

        let stmt = get_stmt(r#"if ($1 == 2) a = "OK"; else a = "KO""#);
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("a", None).unwrap(),
            Value::from("OK".to_owned()),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn block() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();
        let stmt = get_stmt("{ a = 1; b = 2; c = a + b }");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("c", None).unwrap(),
            Value::from(3.0),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn r#while() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();
        let stmt = get_stmt("while (a < 5) a += 2");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("a", None).unwrap(),
            Value::from(6.0),
            "{:?}",
            stmt
        );

        let stmt = get_stmt("do a += 2; while (a < 5)");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("a", None).unwrap(),
            Value::from(8.0),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn r#for() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();
        let stmt = get_stmt("for (i = 0; i < 5; i++) a = a i");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("a", None).unwrap(),
            Value::from("01234".to_owned()),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn r#break() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();
        let stmt = get_stmt("while (a < 10) if (a < 5) a += 2; else break;");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("a", None).unwrap(),
            Value::from(6.0),
            "{:?}",
            stmt
        );

        let stmt = get_stmt("do if (b < 5) b += 2; else break; while (b < 10)");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("b", None).unwrap(),
            Value::from(6.0),
            "{:?}",
            stmt
        );

        let stmt =
            get_stmt(r#"for (i = 0; i < 10; i++) { c = c i; if (c == "2100123") break; c = i c }"#);
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("c", None).unwrap(),
            Value::from("2100123".to_owned()),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn r#continue() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();
        let stmt = get_stmt("while (a1 < 10) { a1++; if (a1 < 5) continue; a2++; }");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("a1", None).unwrap(),
            Value::from(10.0),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get("a2", None).unwrap(),
            Value::from(6.0),
            "{:?}",
            stmt
        );

        let stmt = get_stmt("do { b1++; if (b1 < 5) continue; b2++; } while (b1 < 10)");
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("b1", None).unwrap(),
            Value::from(10.0),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get("b2", None).unwrap(),
            Value::from(6.0),
            "{:?}",
            stmt
        );

        let stmt =
            get_stmt(r#"for (i = 0; i < 5; i++) { c = c i; if (c % 2 == 0) continue; c = i c }"#);
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("c", None).unwrap(),
            Value::from("3101234".to_owned()),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn for_in() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();
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
            rt.vars.get("a", Some("0")).unwrap(),
            Value::from(10.0),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get("a", Some("1")).unwrap(),
            Value::from(20.0),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get("a", Some("2")).unwrap(),
            Value::from(30.0),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get("a", Some("3")).unwrap(),
            Value::from(40.0),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn delete() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();
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
            rt.vars.get("a", Some("0")).unwrap(),
            Value::Uninitialised,
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get("a", Some("1")).unwrap(),
            Value::Uninitialised,
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get("a", Some("2")).unwrap(),
            Value::from(15.0),
            "{:?}",
            stmt
        );
        assert_eq!(
            rt.vars.get("a", Some("3")).unwrap(),
            Value::from(20.0),
            "{:?}",
            stmt
        );
    }

    #[test]
    fn print() {
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(Program::empty(), &mut out).unwrap();
        let stmt = get_stmt(
            r#"{
                a = 5;
                b = 10;
                print "a =", a, ", b =", b;
            }"#,
        );
        eval_stmt(&stmt, &mut rt).unwrap();
        assert_eq!(
            String::from_utf8(out.into_inner()).unwrap(),
            "a = 5 , b = 10\n",
            "{:?}",
            stmt
        );
    }
}
