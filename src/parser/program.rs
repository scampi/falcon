use nom::{types::CompleteStr, *};
use std::fmt;

use crate::parser::{expr::*, stmt::*, util::parse_name};

#[derive(Debug, PartialEq)]
pub struct Program {
    items: Vec<Item>,
}

impl Program {
    fn new(items: Vec<Item>) -> Program {
        Program { items }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for item in &self.items {
            write!(f, "{};", item)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Item {
    Action(StmtList),
    PatternAction(Pattern, StmtList),
    FunctionDef(String, Vec<String>, StmtList),
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Item::Action(stmts) => write!(f, "{}", stmts),
            Item::PatternAction(pattern, action) => write!(f, "{} {}", pattern, action),
            Item::FunctionDef(name, args, stmts) => {
                write!(f, "function {}(", name)?;
                for arg in args {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ") {}", stmts)
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Begin,
    End,
    Exprs(ExprList),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Begin => write!(f, "BEGIN"),
            Pattern::End => write!(f, "END"),
            Pattern::Exprs(exprs) => write!(f, "{}", exprs),
        }
    }
}

named!(eat_terminator<CompleteStr, CompleteStr>, eat_separator!(" \r\n\t;"));

pub fn parse_program(input: &str) -> Program {
    let complete_input = CompleteStr::from(input);
    exact!(
        &complete_input,
        map!(many0!(wrap_sep!(eat_terminator, parse_item)), |items| {
            Program::new(items)
        })
    )
    .unwrap()
    .1
}

#[rustfmt::skip]
#[allow(dead_code)]
fn parse_item(input: CompleteStr) -> IResult<CompleteStr, Item> {
    alt!(
        input,
        do_parse!(
            pattern: parse_pattern >>
            action: parse_stmt_list >>
            (Item::PatternAction(pattern, action))
        )
        | parse_stmt_list => { |stmts| Item::Action(stmts) }
        | parse_function_def
    )
}

#[allow(dead_code)]
fn parse_pattern(input: CompleteStr) -> IResult<CompleteStr, Pattern> {
    alt!(
        input,
        ws!(tag!("BEGIN")) => { |_| Pattern::Begin }
        | ws!(tag!("END")) => { |_| Pattern::End }
        | separated_pair!(parse_expr, ws!(char!(',')), parse_expr) => {
            |(e1, e2)| Pattern::Exprs(ExprList(vec![e1, e2]))
        }
        | parse_expr => { |expr| Pattern::Exprs(ExprList(vec![expr])) }
    )
}

#[rustfmt::skip]
#[allow(dead_code)]
fn parse_function_def(input: CompleteStr) -> IResult<CompleteStr, Item> {
    do_parse!(
        input,
        ws!(tag!("function")) >>
        fname: parse_name >>
        char!('(') >>
        args: separated_list!(ws!(char!(',')), parse_name) >>
        char!(')') >>
        body: parse_stmt_list >>
        (Item::FunctionDef(fname, args, body))
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_program(input: &str, expected: Program) {
        let prog = parse_program(input);
        assert_eq!(
            prog, expected,
            "\nexpected:\n{}\n\nactual:\n{}",
            expected, prog
        );
    }

    fn assert_item(input: &str, expected: Item) {
        let item = parse_item(CompleteStr::from(input));
        assert!(item.is_ok(), "input: {}\n{:?}", input, item);
        let item = item.unwrap();
        assert!(item.0.is_empty(), "input: {}\n{:?}", input, item);
        assert_eq!(
            item.1, expected,
            "\nexpected:\n{}\n\nactual:\n{}",
            expected, item.1
        );
    }

    #[test]
    fn pattern_action() {
        assert_item(
            "{ print 42 }",
            Item::Action(StmtList(vec![Stmt::Print(
                ExprList(vec![Expr::Number(42f64)]),
                None,
            )])),
        );
        assert_item(
            "BEGIN { print 42 }",
            Item::PatternAction(
                Pattern::Begin,
                StmtList(vec![Stmt::Print(ExprList(vec![Expr::Number(42f64)]), None)]),
            ),
        );
        assert_item(
            "/42/ { print 42 }",
            Item::PatternAction(
                Pattern::Exprs(ExprList(vec![Expr::Regexp("42".to_owned())])),
                StmtList(vec![Stmt::Print(ExprList(vec![Expr::Number(42f64)]), None)]),
            ),
        );
    }

    #[test]
    fn function_def() {
        assert_item(
            "function my_func1() {}",
            Item::FunctionDef("my_func1".to_owned(), vec![], StmtList(vec![])),
        );
        assert_item(
            "function my_func2(a, b) {}",
            Item::FunctionDef(
                "my_func2".to_owned(),
                vec!["a".to_owned(), "b".to_owned()],
                StmtList(vec![]),
            ),
        );
    }

    #[test]
    fn program() {
        assert_program(
            r#"BEGIN { print "start" } END { print "end" }"#,
            Program::new(vec![
                Item::PatternAction(
                    Pattern::Begin,
                    StmtList(vec![Stmt::Print(
                        ExprList(vec![Expr::String("start".to_owned())]),
                        None,
                    )]),
                ),
                Item::PatternAction(
                    Pattern::End,
                    StmtList(vec![Stmt::Print(
                        ExprList(vec![Expr::String("end".to_owned())]),
                        None,
                    )]),
                ),
            ]),
        );
        assert_program(
            r#"BEGIN { print "start" }; ; END { print "end" }"#,
            Program::new(vec![
                Item::PatternAction(
                    Pattern::Begin,
                    StmtList(vec![Stmt::Print(
                        ExprList(vec![Expr::String("start".to_owned())]),
                        None,
                    )]),
                ),
                Item::PatternAction(
                    Pattern::End,
                    StmtList(vec![Stmt::Print(
                        ExprList(vec![Expr::String("end".to_owned())]),
                        None,
                    )]),
                ),
            ]),
        );
        assert_program(
            r#"
            BEGIN { print "start" }
            END { print "end" }"#,
            Program::new(vec![
                Item::PatternAction(
                    Pattern::Begin,
                    StmtList(vec![Stmt::Print(
                        ExprList(vec![Expr::String("start".to_owned())]),
                        None,
                    )]),
                ),
                Item::PatternAction(
                    Pattern::End,
                    StmtList(vec![Stmt::Print(
                        ExprList(vec![Expr::String("end".to_owned())]),
                        None,
                    )]),
                ),
            ]),
        );
        assert_program("", Program::new(vec![]));
    }
}
