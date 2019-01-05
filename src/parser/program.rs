use crate::parser::{
    ast::*,
    expr::*,
    stmt::*,
    util::{parse_func_name, parse_name, skip_wrapping_spaces},
};
use combine::{
    error::ParseError,
    parser::{
        char::{char, string},
        choice::{choice, optional},
        item::one_of,
        repeat::{many, sep_by, skip_many},
        sequence::between,
        Parser,
    },
    stream::RangeStream,
};

pub fn parse_program<'a, I>() -> impl Parser<Input = I, Output = Program> + 'a
where
    I: RangeStream<Item = char, Range = &'a str> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many(parse_item().skip(skip_many(one_of(" \t\r\n;".chars())))).map(|items| Program::new(items))
}

fn parse_item<'a, I>() -> impl Parser<Input = I, Output = Item> + 'a
where
    I: RangeStream<Item = char, Range = &'a str> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        parse_pattern()
            .and(parse_stmt_list())
            .map(|(pattern, action)| Item::PatternAction(pattern, action)),
        parse_stmt_list().map(|stmts| Item::Action(stmts)),
        parse_function_def(),
    ))
}

fn parse_pattern<'a, I>() -> impl Parser<Input = I, Output = Pattern> + 'a
where
    I: RangeStream<Item = char, Range = &'a str> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        skip_wrapping_spaces(string("BEGIN")).map(|_| Pattern::Begin),
        skip_wrapping_spaces(string("END")).map(|_| Pattern::End),
        parse_expr()
            .and(optional(skip_wrapping_spaces(char(',')).with(parse_expr())))
            .map(|(e1, e2)| {
                if let Some(e2) = e2 {
                    Pattern::Exprs(ExprList(vec![e1, e2]))
                } else {
                    Pattern::Exprs(ExprList(vec![e1]))
                }
            }),
    ))
}

fn parse_function_def<'a, I>() -> impl Parser<Input = I, Output = Item> + 'a
where
    I: RangeStream<Item = char, Range = &'a str> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        skip_wrapping_spaces(string("function")),
        parse_func_name(),
        between(
            skip_wrapping_spaces(char('(')),
            skip_wrapping_spaces(char(')')),
            sep_by(parse_name(), skip_wrapping_spaces(char(','))),
        ),
        parse_stmt_list(),
    )
        .map(|(_, fname, args, body)| Item::FunctionDef(fname, args, body))
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::stream::state::State;

    fn assert_program(input: &str, expected: Program) {
        let prog = parse_program().easy_parse(State::new(input));
        assert!(prog.is_ok(), "input: {}\n{}", input, prog.unwrap_err());
        let prog = prog.unwrap();
        assert_eq!(
            prog.0, expected,
            "\ninput: {}\nexpected:\n{}\n\nactual:\n{}\n{:?}",
            input, expected, prog.0, prog
        );
    }

    fn assert_item(input: &str, expected: Item) {
        let item = parse_item().easy_parse(input);
        assert!(item.is_ok(), "input: {}\n{}", input, item.unwrap_err());
        let item = item.unwrap();
        assert_eq!(
            item.0, expected,
            "\ninput: {}\nexpected:\n{}\n\nactual:\n{}\n{:?}",
            input, expected, item.0, item
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
                Pattern::Exprs(ExprList(vec![Expr::Regexp(RegexEq::new("42"))])),
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
