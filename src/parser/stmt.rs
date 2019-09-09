use crate::parser::{
    ast::*,
    expr::*,
    util::{
        parse_var_name, skip_comments, skip_newlines, skip_wrapping_newlines, skip_wrapping_spaces,
    },
};
use combine::{
    error::ParseError,
    parser::{
        char::{char, string},
        choice::{choice, optional},
        combinator::attempt,
        item::one_of,
        repeat::{many, skip_many},
        sequence::between,
        Parser,
    },
    stream::RangeStream,
};

// import macros
use combine::{combine_parse_partial, combine_parser_impl, parse_mode, parser};

#[cfg(test)]
pub fn get_stmt(input: &str) -> Stmt {
    use combine::stream::state::State;

    let stmt = parse_stmt().easy_parse(State::new(input));
    assert!(stmt.is_ok(), "input: {}\n{}", input, stmt.unwrap_err());
    let stmt = stmt.unwrap();
    // there is no input leftover
    assert!(stmt.1.input.is_empty(), "{:?}", stmt);
    stmt.0
}

parser! {
    fn parse_stmt['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        skip_comments(
            choice((
                attempt(parse_terminatable()),
                attempt(parse_if_else()),
                attempt(parse_while()),
                attempt(parse_do_while()),
                attempt(parse_for()),
                attempt(parse_for_in()),
                attempt(parse_stmt_list().map(|stmts| Stmt::Block(stmts))),
            ))
            .skip(skip_many(one_of(" \r\n\t;".chars())))
        )
    }
}

parser! {
    pub fn parse_stmt_list['a, I]()(I) -> StmtList
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        between(
            skip_wrapping_newlines(skip_wrapping_spaces(char('{'))),
            skip_wrapping_newlines(skip_wrapping_spaces(char('}'))),
            many::<Vec<Stmt>, _>(parse_stmt()).map(|stmts| StmtList(stmts)),
        )
    }
}

parser! {
    fn parse_if_else['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        (
            skip_wrapping_spaces(string("if")),
            between(
                skip_wrapping_spaces(char('(')),
                skip_wrapping_spaces(char(')')).skip(skip_newlines()),
                parse_expr(),
            ),
            parse_stmt(),
            attempt(optional(
                skip_wrapping_spaces(string("else")).skip(skip_newlines())
                .with(parse_stmt())
            )),
        )
        .map(|(_, cond, ok, ko)| Stmt::IfElse(cond, Box::new(ok), ko.map(|v| Box::new(v))))
    }
}

parser! {
    fn parse_while['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        skip_wrapping_spaces(string("while"))
            .with(between(
                skip_wrapping_spaces(char('(')),
                skip_wrapping_spaces(char(')')).skip(skip_newlines()),
                parse_expr(),
            ))
            .and(parse_stmt())
            .map(|(cond, body)| Stmt::While(cond, Box::new(body)))
    }
}

parser! {
    fn parse_do_while['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        skip_wrapping_spaces(string("do")).skip(skip_newlines())
            .with(parse_stmt())
            .skip(skip_wrapping_spaces(string("while")))
            .and(between(
                skip_wrapping_spaces(char('(')),
                skip_wrapping_spaces(char(')')),
                parse_expr(),
            ))
            .map(|(body, cond)| Stmt::DoWhile(cond, Box::new(body)))
    }
}

parser! {
    fn parse_for['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        (
            skip_wrapping_spaces(string("for")).with(skip_wrapping_spaces(char('('))),
            optional(parse_simple_stmt()),
            skip_wrapping_spaces(char(';')),
            optional(parse_expr()),
            skip_wrapping_spaces(char(';')),
            optional(parse_simple_stmt()),
            skip_wrapping_spaces(char(')')).skip(skip_newlines()),
            parse_stmt(),
        )
        .map(|(_, start, _, until, _, step, _, body)|
            Stmt::For(
                start.map(|s| Box::new(s)),
                until,
                step.map(|s| Box::new(s)),
                Box::new(body)
            )
        )
    }
}

parser! {
    fn parse_for_in['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        (
            skip_wrapping_spaces(string("for")).with(skip_wrapping_spaces(char('('))),
            parse_var_name(),
            skip_wrapping_spaces(string("in")),
            parse_var_name(),
            skip_wrapping_spaces(char(')')).skip(skip_newlines()),
            parse_stmt(),
        )
        .map(|(_, a, _, b, _, body)| Stmt::ForIn(a, b, Box::new(body)))
    }
}

parser! {
    fn parse_terminatable['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice((
            parse_simple_stmt(),
            skip_wrapping_spaces(string("break")).map(|_| Stmt::Break),
            skip_wrapping_spaces(string("continue")).map(|_| Stmt::Continue),
            skip_wrapping_spaces(string("next")).map(|_| Stmt::Next),
            skip_wrapping_spaces(string("exit"))
                .with(optional(parse_expr()))
                .map(|code| Stmt::Exit(code)),
            skip_wrapping_spaces(string("return"))
                .with(optional(parse_expr()))
                .map(|code| Stmt::Return(code)),
        ))
    }
}

parser! {
    fn parse_simple_stmt['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice((
            attempt(parse_expr().map(|e| Stmt::Expr(e))),
            attempt(parse_delete()),
            attempt(parse_print_stmt()),
        ))
    }
}

parser! {
    fn parse_delete['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        skip_wrapping_spaces(string("delete"))
            .with(parse_var_name())
            .and(between(
                skip_wrapping_spaces(char('[')),
                skip_wrapping_spaces(char(']')),
                parse_expr_list1(),
            ))
            .map(|(name, exprs)| Stmt::Delete(name, exprs))
    }
}

parser! {
    fn parse_print_stmt['a, I]()(I) -> Stmt
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice((
            attempt(
                skip_wrapping_spaces(string("printf"))
                    .with(between(
                        skip_wrapping_spaces(char('(')),
                        skip_wrapping_spaces(char(')')),
                        parse_expr_list1(),
                    ))
                    .and(optional(parse_output_redirection())),
            )
            .map(|(exprs, redir)| Stmt::Printf(exprs, redir)),
            attempt(
                skip_wrapping_spaces(string("print"))
                    .with(between(
                        skip_wrapping_spaces(char('(')),
                        skip_wrapping_spaces(char(')')),
                        parse_expr_list1(),
                    ))
                    .and(optional(parse_output_redirection())),
            )
            .map(|(exprs, redir)| Stmt::Print(exprs, redir)),
            attempt(
                skip_wrapping_spaces(string("printf"))
                    .with(parse_print_expr_list())
                    .and(optional(parse_output_redirection())),
            )
            .map(|(exprs, redir)| Stmt::Printf(exprs, redir)),
            attempt(
                skip_wrapping_spaces(string("print"))
                    .with(parse_print_expr_list())
                    .and(optional(parse_output_redirection())),
            )
            .map(|(exprs, redir)| Stmt::Print(exprs, redir)),
        ))
    }
}

parser! {
    fn parse_output_redirection['a, I]()(I) -> OutputRedirection
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice((
            attempt(skip_wrapping_spaces(string(">>")).with(parse_print_expr()))
                .map(|expr| OutputRedirection::Append(expr)),
            attempt(skip_wrapping_spaces(char('>')).with(parse_print_expr()))
                .map(|expr| OutputRedirection::Truncate(expr)),
            attempt(skip_wrapping_spaces(char('|')).with(parse_print_expr()))
                .map(|expr| OutputRedirection::Pipe(expr)),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::stream::state::State;

    fn assert_stmt(input: &str, expected: Stmt) {
        let stmt = parse_stmt().easy_parse(State::new(input));
        assert!(stmt.is_ok(), "input: {}\n{}", input, stmt.unwrap_err());
        let stmt = stmt.unwrap();
        assert_eq!(
            stmt.0, expected,
            "input: {}\nexpected:\n{}\n\nactual:\n{}\n{:?}",
            input, expected, stmt.0, stmt
        );
    }

    fn assert_stmt_with_leftovers(input: &str, expected: Stmt, leftover: &str) {
        let stmt = parse_stmt().easy_parse(input);
        assert!(stmt.is_ok(), "{:?}", stmt);
        let stmt = stmt.unwrap();
        assert_eq!(stmt.1, leftover);
        assert_eq!(stmt.0, expected, "{}", stmt.0);
    }

    #[test]
    fn if_else() {
        assert_stmt(
            "if (a == 1) break",
            Stmt::IfElse(
                Expr::Comparison(
                    CmpOperator::Equal,
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(1f64)),
                ),
                Box::new(Stmt::Break),
                None,
            ),
        );
        assert_stmt(
            "if (a == 1) continue; else break",
            Stmt::IfElse(
                Expr::Comparison(
                    CmpOperator::Equal,
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(1f64)),
                ),
                Box::new(Stmt::Continue),
                Some(Box::new(Stmt::Break)),
            ),
        );
        assert_stmt(
            "if (a == 1) continue; else break;",
            Stmt::IfElse(
                Expr::Comparison(
                    CmpOperator::Equal,
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(1f64)),
                ),
                Box::new(Stmt::Continue),
                Some(Box::new(Stmt::Break)),
            ),
        );
        assert_stmt(
            r#"if ( a == 1 ) 42; else "gargoyles""#,
            Stmt::IfElse(
                Expr::Comparison(
                    CmpOperator::Equal,
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(1f64)),
                ),
                Box::new(Stmt::Expr(Expr::Number(42f64))),
                Some(Box::new(Stmt::Expr(Expr::String("gargoyles".to_owned())))),
            ),
        );
        assert_stmt(
            "if (a == 1) continue; else if (a == 2) break",
            Stmt::IfElse(
                Expr::Comparison(
                    CmpOperator::Equal,
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(1f64)),
                ),
                Box::new(Stmt::Continue),
                Some(Box::new(Stmt::IfElse(
                    Expr::Comparison(
                        CmpOperator::Equal,
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
                Expr::Comparison(
                    CmpOperator::LessThan,
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
                Expr::Comparison(
                    CmpOperator::LessThan,
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(10f64)),
                ),
                Box::new(Stmt::IfElse(
                    Expr::Comparison(
                        CmpOperator::Equal,
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
                Some(Expr::Comparison(
                    CmpOperator::Equal,
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
        assert_stmt(
            "for (i = 0; i < 5; i++) a = a i",
            Stmt::For(
                Some(Box::new(Stmt::Expr(Expr::Assign(
                    AssignType::Normal,
                    LValueType::Name("i".to_owned()),
                    Box::new(Expr::Number(0.0)),
                )))),
                Some(Expr::Comparison(
                    CmpOperator::LessThan,
                    Box::new(Expr::LValue(LValueType::Name("i".to_owned()))),
                    Box::new(Expr::Number(5.0)),
                )),
                Some(Box::new(Stmt::Expr(Expr::PostIncrement(LValueType::Name(
                    "i".to_owned(),
                ))))),
                Box::new(Stmt::Expr(Expr::Assign(
                    AssignType::Normal,
                    LValueType::Name("a".to_owned()),
                    Box::new(Expr::Concat(
                        Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                        Box::new(Expr::LValue(LValueType::Name("i".to_owned()))),
                    )),
                ))),
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
            "do a++; while (a < 10)",
            Stmt::DoWhile(
                Expr::Comparison(
                    CmpOperator::LessThan,
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(10f64)),
                ),
                Box::new(Stmt::Expr(Expr::PostIncrement(LValueType::Name(
                    "a".to_owned(),
                )))),
            ),
        );
        assert_stmt(
            r#"do
                a++
            while (a < 10)"#,
            Stmt::DoWhile(
                Expr::Comparison(
                    CmpOperator::LessThan,
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(10f64)),
                ),
                Box::new(Stmt::Expr(Expr::PostIncrement(LValueType::Name(
                    "a".to_owned(),
                )))),
            ),
        );
        assert_stmt(
            "do if (a == 5) break; else a++; while (a < 10)",
            Stmt::DoWhile(
                Expr::Comparison(
                    CmpOperator::LessThan,
                    Box::new(Expr::LValue(LValueType::Name("a".to_owned()))),
                    Box::new(Expr::Number(10f64)),
                ),
                Box::new(Stmt::IfElse(
                    Expr::Comparison(
                        CmpOperator::Equal,
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
            "do do a++; while (1); while (a < 10)",
            Stmt::DoWhile(
                Expr::Comparison(
                    CmpOperator::LessThan,
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
        assert_stmt(
            r#"{ c = c i; if (c == "21123") break; c = i c }"#,
            Stmt::Block(StmtList(vec![
                Stmt::Expr(Expr::Assign(
                    AssignType::Normal,
                    LValueType::Name("c".to_owned()),
                    Box::new(Expr::Concat(
                        Box::new(Expr::LValue(LValueType::Name("c".to_owned()))),
                        Box::new(Expr::LValue(LValueType::Name("i".to_owned()))),
                    )),
                )),
                Stmt::IfElse(
                    Expr::Comparison(
                        CmpOperator::Equal,
                        Box::new(Expr::LValue(LValueType::Name("c".to_owned()))),
                        Box::new(Expr::String("21123".to_owned())),
                    ),
                    Box::new(Stmt::Break),
                    None,
                ),
                Stmt::Expr(Expr::Assign(
                    AssignType::Normal,
                    LValueType::Name("c".to_owned()),
                    Box::new(Expr::Concat(
                        Box::new(Expr::LValue(LValueType::Name("i".to_owned()))),
                        Box::new(Expr::LValue(LValueType::Name("c".to_owned()))),
                    )),
                )),
            ])),
        );
    }

    #[test]
    fn print_stmt() {
        assert_stmt(
            "print 42",
            Stmt::Print(ExprList(vec![Expr::Number(42f64)]), None),
        );
        assert_stmt("print", Stmt::Print(ExprList(vec![]), None));
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
                ExprList(vec![Expr::Comparison(
                    CmpOperator::GreaterThan,
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
            "> aaa",
        );
    }
}
