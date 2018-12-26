use nom::{types::CompleteStr, *};
use std::fmt;

use crate::expr::*;
use crate::util::parse_name;

#[derive(Debug, PartialEq)]
pub struct StmtList(pub Vec<Stmt>);

impl fmt::Display for StmtList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.0 {
            write!(f, "{}, ", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Block(StmtList),
    IfElse(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    DoWhile(Expr, Box<Stmt>),
    For(
        Option<Box<Stmt>>,
        Option<Expr>,
        Option<Box<Stmt>>,
        Box<Stmt>,
    ),
    ForIn(String, String, Box<Stmt>),
    Expr(Expr),
    Break,
    Continue,
    Next,
    Exit(Option<Expr>),
    Return(Option<Expr>),
    Delete(String, ExprList),
    Print(ExprList, Option<OutputRedirection>),
    Printf(ExprList, Option<OutputRedirection>),
}

#[derive(Debug, PartialEq)]
pub enum OutputRedirection {
    Truncate(Expr),
    Append(Expr),
    Pipe(Expr),
}

impl fmt::Display for OutputRedirection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OutputRedirection::Truncate(e) => write!(f, "> {}", e),
            OutputRedirection::Append(e) => write!(f, ">> {}", e),
            OutputRedirection::Pipe(e) => write!(f, "| {}", e),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Print(exprs, None) => write!(f, "print({})", exprs),
            Stmt::Printf(exprs, None) => write!(f, "printf({})", exprs),
            Stmt::Print(exprs, Some(redir)) => write!(f, "print({}) {}", exprs, redir),
            Stmt::Printf(exprs, Some(redir)) => write!(f, "printf({}) {}", exprs, redir),
            Stmt::Block(stmts) => write!(f, "{{ {} }}", stmts),
            Stmt::IfElse(cond, ok, ko) => {
                write!(f, "if ({}) {}", cond, ok)?;
                if let Some(ko) = ko {
                    write!(f, " else {}", ko)?;
                }
                Ok(())
            }
            Stmt::For(start, until, next, body) => {
                write!(f, "for (")?;
                if let Some(start) = start {
                    write!(f, "{}", start)?;
                }
                write!(f, ";")?;
                if let Some(until) = until {
                    write!(f, " {}", until)?;
                }
                write!(f, ";")?;
                if let Some(next) = next {
                    write!(f, " {}", next)?;
                }
                write!(f, ") {}", body)
            }
            Stmt::ForIn(a, b, body) => write!(f, "for ({} in {}) {}", a, b, body),
            Stmt::While(cond, body) => write!(f, "while ({}) {}", cond, body),
            Stmt::DoWhile(cond, body) => write!(f, "do {} while ({})", body, cond),
            Stmt::Expr(e) => write!(f, "{}", e),
            Stmt::Break => write!(f, "break"),
            Stmt::Continue => write!(f, "continue"),
            Stmt::Next => write!(f, "next"),
            Stmt::Exit(e) => {
                write!(f, "exit")?;
                if let Some(e) = e {
                    write!(f, "{}", e)?;
                }
                Ok(())
            }
            Stmt::Return(e) => {
                write!(f, "return")?;
                if let Some(e) = e {
                    write!(f, "{}", e)?;
                }
                Ok(())
            }
            Stmt::Delete(name, exprs) => write!(f, "delete {}[{}]", name, exprs),
        }
    }
}

crate fn parse_stmt(input: CompleteStr) -> IResult<CompleteStr, Stmt> {
    alt!(
        input,
        parse_terminatable
        | parse_if_else
        | parse_while
        | parse_do_while
        | parse_for
        | parse_for_in
        | parse_stmt_list => { |stmts| Stmt::Block(stmts) }
    )
}

#[rustfmt::skip]
crate fn parse_stmt_list(input: CompleteStr) -> IResult<CompleteStr, StmtList> {
    do_parse!(
        input,
        ws!(char!('{')) >>
        stmts: separated_list!(ws!(char!(';')), parse_stmt) >>
        // allow trailing semicolon
        opt!(ws!(char!(';'))) >>
        ws!(char!('}')) >>
        (StmtList(stmts))
    )
}

named!(parse_if_else<CompleteStr, Stmt>,
    do_parse!(
        ws!(tag!("if")) >>
        cond: delimited!(ws!(char!('(')), parse_expr, ws!(char!(')'))) >>
        ok: parse_stmt >>
        ko: opt!(do_parse!(
                ws!(tag!("else")) >>
                ko_content: parse_stmt >>
                (Box::new(ko_content))
        )) >>
        (Stmt::IfElse(cond, Box::new(ok), ko))
    )
);

named!(parse_while<CompleteStr, Stmt>,
    do_parse!(
        ws!(tag!("while")) >>
        cond: delimited!(ws!(char!('(')), parse_expr, ws!(char!(')'))) >>
        body: parse_stmt >>
        (Stmt::While(cond, Box::new(body)))
    )
);

named!(parse_do_while<CompleteStr, Stmt>,
    do_parse!(
        ws!(tag!("do")) >>
        body: parse_stmt >>
        ws!(tag!("while")) >>
        cond: delimited!(ws!(char!('(')), parse_expr, ws!(char!(')'))) >>
        (Stmt::DoWhile(cond, Box::new(body)))
    )
);

named!(parse_for<CompleteStr, Stmt>,
    do_parse!(
        ws!(tag!("for")) >>
        ws!(char!('(')) >>
        start: opt!(parse_simple_stmt) >>
        ws!(char!(';')) >>
        until: opt!(parse_expr) >>
        ws!(char!(';')) >>
        next: opt!(parse_simple_stmt) >>
        ws!(char!(')')) >>
        body: parse_stmt >>
        (Stmt::For(
            start.map(|s| Box::new(s)),
            until,
            next.map(|s| Box::new(s)),
            Box::new(body)
        ))
    )
);

named!(parse_for_in<CompleteStr, Stmt>,
    do_parse!(
        ws!(tag!("for")) >>
        ws!(char!('(')) >>
        a: parse_name >>
        ws!(tag!("in")) >>
        b: parse_name >>
        ws!(char!(')')) >>
        body: parse_stmt >>
        (Stmt::ForIn(a, b, Box::new(body)))
    )
);

named!(parse_terminatable<CompleteStr, Stmt>,
    alt!(
        parse_simple_stmt
        | ws!(tag!("break")) => { |_| Stmt::Break }
        | ws!(tag!("continue")) => { |_| Stmt::Continue }
        | ws!(tag!("next")) => { |_| Stmt::Next }
        | do_parse!(
            ws!(tag!("exit")) >>
            code: opt!(parse_expr) >>
            (Stmt::Exit(code))
        )
        | do_parse!(
            ws!(tag!("return")) >>
            code: opt!(parse_expr) >>
            (Stmt::Return(code))
        )
    )
);

named!(parse_simple_stmt<CompleteStr, Stmt>,
    alt!(
        parse_delete
        | parse_expr => { |e| Stmt::Expr(e) }
        | parse_print_stmt
    )
);

named!(parse_delete<CompleteStr, Stmt>,
    do_parse!(
        ws!(tag!("delete")) >>
        name: parse_name >>
        exprs: delimited!(
            ws!(char!('[')),
            parse_expr_list1,
            ws!(char!(']'))
        ) >>
        (Stmt::Delete(name, exprs))
    )
);

named!(parse_print_stmt<CompleteStr, Stmt>,
    alt!(
        do_parse!(
            ws!(tag!("printf")) >>
            exprs: delimited!(
                ws!(char!('(')),
                parse_expr_list1,
                ws!(char!(')'))
            ) >>
            redir: opt!(parse_output_redirection) >>
            (Stmt::Printf(exprs, redir))
        )
        | do_parse!(
            ws!(tag!("print")) >>
            exprs: delimited!(
                ws!(char!('(')),
                parse_expr_list1,
                ws!(char!(')'))
            ) >>
            redir: opt!(parse_output_redirection) >>
            (Stmt::Print(exprs, redir))
        )
        | do_parse!(
            ws!(tag!("printf")) >>
            exprs: parse_print_expr_list1 >>
            redir: opt!(parse_output_redirection) >>
            (Stmt::Printf(exprs, redir))
        )
        | do_parse!(
            ws!(tag!("print")) >>
            exprs: parse_print_expr_list1 >>
            redir: opt!(parse_output_redirection) >>
            (Stmt::Print(exprs, redir))
        )
    )
);

named!(parse_output_redirection<CompleteStr, OutputRedirection>,
    alt!(
        preceded!(ws!(tag!(">>")), parse_print_expr) => {
            |expr| OutputRedirection::Append(expr)
        }
        | preceded!(ws!(char!('>')), parse_print_expr) => {
            |expr| OutputRedirection::Truncate(expr)
        }
        | preceded!(ws!(char!('|')), parse_print_expr) => {
            |expr| OutputRedirection::Pipe(expr)
        }
    )
);

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_stmt(input: &str, expected: Stmt) {
        let stmt = parse_stmt(CompleteStr::from(input));
        assert!(stmt.is_ok(), "input: {}\n{:?}", input, stmt);
        let stmt = stmt.unwrap();
        assert!(stmt.0.is_empty(), "input: {}\n{:?}", input, stmt);
        assert_eq!(
            stmt.1, expected,
            "\nexpected:\n{}\n\nactual:\n{}",
            expected, stmt.1
        );
    }

    fn assert_stmt_with_leftovers(input: &str, expected: Stmt, leftover: &str) {
        let stmt = parse_stmt(CompleteStr::from(input));
        assert!(stmt.is_ok(), "{:?}", stmt);
        let stmt = stmt.unwrap();
        assert!(!stmt.0.is_empty());
        assert_eq!(stmt.0.as_ref(), leftover);
        assert_eq!(stmt.1, expected, "{}", stmt.1);
    }

    #[test]
    fn if_else() {
        assert_stmt(
            "if (a == 1) break",
            Stmt::IfElse(
                Expr::Equal(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(1f64)),
                ),
                Box::new(Stmt::Break),
                None,
            ),
        );
        assert_stmt(
            "if (a == 1) continue else break",
            Stmt::IfElse(
                Expr::Equal(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(1f64)),
                ),
                Box::new(Stmt::Continue),
                Some(Box::new(Stmt::Break)),
            ),
        );
        assert_stmt(
            r#"if ( a == 1 ) 42 else "gargoyles""#,
            Stmt::IfElse(
                Expr::Equal(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(1f64)),
                ),
                Box::new(Stmt::Expr(Expr::Number(42f64))),
                Some(Box::new(Stmt::Expr(Expr::String("gargoyles".to_owned())))),
            ),
        );
        assert_stmt(
            "if (a == 1) continue else if (a == 2) break",
            Stmt::IfElse(
                Expr::Equal(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(1f64)),
                ),
                Box::new(Stmt::Continue),
                Some(Box::new(Stmt::IfElse(
                    Expr::Equal(
                        Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                        Box::new(Expr::Number(2f64)),
                    ),
                    Box::new(Stmt::Break),
                    None,
                ))),
            ),
        );
    }

    #[test]
    fn r#while() {
        assert_stmt(
            "while (a < 10) a++",
            Stmt::While(
                Expr::LessThan(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(10f64)),
                ),
                Box::new(Stmt::Expr(Expr::PostIncrement(LValueType::Name(
                    "a".to_owned(),
                )))),
            ),
        );
        assert_stmt(
            "while (a < 10) if (a == 5) break else a++",
            Stmt::While(
                Expr::LessThan(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(10f64)),
                ),
                Box::new(Stmt::IfElse(
                    Expr::Equal(
                        Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                        Box::new(Expr::Number(5f64)),
                    ),
                    Box::new(Stmt::Break),
                    Some(Box::new(Stmt::Expr(Expr::PostIncrement(LValueType::Name(
                        "a".to_owned(),
                    ))))),
                )),
            ),
        );
    }

    #[test]
    fn r#for() {
        assert_stmt(
            "for (;;) 42",
            Stmt::For(None, None, None, Box::new(Stmt::Expr(Expr::Number(42f64)))),
        );
        assert_stmt(
            "for (41;;) 42",
            Stmt::For(
                Some(Box::new(Stmt::Expr(Expr::Number(41f64)))),
                None,
                None,
                Box::new(Stmt::Expr(Expr::Number(42f64))),
            ),
        );
        assert_stmt(
            "for (;a==2;) 42",
            Stmt::For(
                None,
                Some(Expr::Equal(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(2f64)),
                )),
                None,
                Box::new(Stmt::Expr(Expr::Number(42f64))),
            ),
        );
        assert_stmt(
            "for (;;41) 42",
            Stmt::For(
                None,
                None,
                Some(Box::new(Stmt::Expr(Expr::Number(41f64)))),
                Box::new(Stmt::Expr(Expr::Number(42f64))),
            ),
        );
        assert_stmt(
            r#"for (;;) if (42) "gargoyles""#,
            Stmt::For(
                None,
                None,
                None,
                Box::new(Stmt::IfElse(
                    Expr::Number(42f64),
                    Box::new(Stmt::Expr(Expr::String("gargoyles".to_owned()))),
                    None,
                )),
            ),
        );
    }

    #[test]
    fn for_in() {
        assert_stmt(
            "for (a in array) break",
            Stmt::ForIn("a".to_owned(), "array".to_owned(), Box::new(Stmt::Break)),
        );
    }

    #[test]
    fn exit_return() {
        assert_stmt("exit", Stmt::Exit(None));
        assert_stmt("return", Stmt::Return(None));
        assert_stmt("exit 42", Stmt::Exit(Some(Expr::Number(42f64))));
        assert_stmt("return 42", Stmt::Return(Some(Expr::Number(42f64))));
    }

    #[test]
    fn do_while() {
        assert_stmt(
            "do a++ while (a < 10)",
            Stmt::DoWhile(
                Expr::LessThan(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(10f64)),
                ),
                Box::new(Stmt::Expr(Expr::PostIncrement(LValueType::Name(
                    "a".to_owned(),
                )))),
            ),
        );
        assert_stmt(
            "do if (a == 5) break else a++ while (a < 10)",
            Stmt::DoWhile(
                Expr::LessThan(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(10f64)),
                ),
                Box::new(Stmt::IfElse(
                    Expr::Equal(
                        Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                        Box::new(Expr::Number(5f64)),
                    ),
                    Box::new(Stmt::Break),
                    Some(Box::new(Stmt::Expr(Expr::PostIncrement(LValueType::Name(
                        "a".to_owned(),
                    ))))),
                )),
            ),
        );
        assert_stmt(
            "do do a++ while (1) while (a < 10)",
            Stmt::DoWhile(
                Expr::LessThan(
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(10f64)),
                ),
                Box::new(Stmt::DoWhile(
                    Expr::Number(1f64),
                    Box::new(Stmt::Expr(Expr::PostIncrement(LValueType::Name(
                        "a".to_owned(),
                    )))),
                )),
            ),
        );
    }

    #[test]
    fn delete() {
        assert_stmt(
            "delete a[1,2]",
            Stmt::Delete(
                "a".to_owned(),
                ExprList(vec![Expr::Number(1f64), Expr::Number(2f64)]),
            ),
        );
    }

    #[test]
    fn block() {
        assert_stmt("{ }", Stmt::Block(StmtList(vec![])));
        assert_stmt(
            "{ 42 }",
            Stmt::Block(StmtList(vec![Stmt::Expr(Expr::Number(42f64))])),
        );
        assert_stmt(
            "{ 42; }",
            Stmt::Block(StmtList(vec![Stmt::Expr(Expr::Number(42f64))])),
        );
        assert_stmt(
            "{ 42; 24 }",
            Stmt::Block(StmtList(vec![
                Stmt::Expr(Expr::Number(42f64)),
                Stmt::Expr(Expr::Number(24f64)),
            ])),
        );
    }

    #[test]
    fn print_stmt() {
        assert_stmt(
            "print 42",
            Stmt::Print(ExprList(vec![Expr::Number(42f64)]), None),
        );
        assert_stmt(
            "print ( 42 )",
            Stmt::Print(ExprList(vec![Expr::Number(42f64)]), None),
        );
        assert_stmt(
            "print 42, 24",
            Stmt::Print(
                ExprList(vec![Expr::Number(42f64), Expr::Number(24f64)]),
                None,
            ),
        );
        assert_stmt(
            "printf 42",
            Stmt::Printf(ExprList(vec![Expr::Number(42f64)]), None),
        );
        assert_stmt(
            "printf ( 42 )",
            Stmt::Printf(ExprList(vec![Expr::Number(42f64)]), None),
        );
        assert_stmt(
            "print 42 > aaa",
            Stmt::Print(
                ExprList(vec![Expr::Number(42f64)]),
                Some(OutputRedirection::Truncate(Expr::LValue(LValueType::Name(
                    "aaa".to_owned(),
                )))),
            ),
        );
        assert_stmt(
            "print (42 > 2) > aaa",
            Stmt::Print(
                ExprList(vec![Expr::GreaterThan(
                    Box::new(Expr::Number(42f64)),
                    Box::new(Expr::Number(2f64)),
                )]),
                Some(OutputRedirection::Truncate(Expr::LValue(LValueType::Name(
                    "aaa".to_owned(),
                )))),
            ),
        );
        assert_stmt_with_leftovers(
            "print 42 > 2 > aaa",
            Stmt::Print(
                ExprList(vec![Expr::Number(42f64)]),
                Some(OutputRedirection::Truncate(Expr::Number(2f64))),
            ),
            " > aaa",
        );
    }
}
