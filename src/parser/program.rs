use crate::{
    interpreter::{functions::builtins::is_builtin, variables::is_special_variable},
    parser::{
        ast::*,
        expr::*,
        stmt::*,
        util::{parse_func_name, parse_var_name, skip_wrapping_spaces},
    },
};
use combine::{
    error::{ParseError, StreamError},
    parser::{
        char::{char, string},
        choice::{choice, optional},
        item::one_of,
        repeat::{many, sep_by, skip_many},
        sequence::between,
        Parser,
    },
    stream::{RangeStream, StreamErrorFor},
};
use std::collections::HashSet;

// import macros
use combine::{combine_parse_partial, combine_parser_impl, parse_mode, parser};

#[cfg(test)]
pub fn get_program(input: &str) -> Program {
    use combine::stream::state::State;

    let prog = parse_program().easy_parse(State::new(input));
    assert!(prog.is_ok(), "input: {}\n{}", input, prog.unwrap_err());
    let prog = prog.unwrap();
    // there is no input leftover
    assert!(prog.1.input.is_empty(), "{:?}", prog);
    prog.0
}

parser! {
    pub fn parse_program['a, I]()(I) -> Program
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        let seps1 = skip_many(one_of(" \t\r\n;".chars()));
        let seps2 = skip_many(one_of(" \t\r\n;".chars()));
        many(seps1.with(parse_item().skip(seps2))).map(Program::new)
    }
}

parser! {
    fn parse_item['a, I]()(I) -> Item
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice((
            parse_pattern()
                .and(parse_stmt_list())
                .map(|(pattern, action)| Item::PatternAction(Some(pattern), action)),
            parse_stmt_list().map(|stmts| Item::PatternAction(None, stmts)),
            parse_function_def(),
        ))
    }
}

parser! {
    fn parse_pattern['a, I]()(I) -> Pattern
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice((
            skip_wrapping_spaces(string("BEGIN")).map(|_| Pattern::Begin),
            skip_wrapping_spaces(string("END")).map(|_| Pattern::End),
            parse_expr()
                .and(optional(skip_wrapping_spaces(char(',')).and(skip_many(char('\n'))).with(parse_expr())))
                .map(|(e1, e2)| {
                    if let Some(e2) = e2 {
                        Pattern::Range(e1, e2)
                    } else {
                        Pattern::Expr(e1)
                    }
                }),
        ))
    }
}

parser! {
    fn parse_function_def['a, I]()(I) -> Item
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        (
            skip_wrapping_spaces(string("function")),
            parse_func_name(),
            between(
                skip_wrapping_spaces(char('(')),
                skip_wrapping_spaces(char(')')),
                sep_by(parse_var_name(), skip_wrapping_spaces(char(','))),
            ),
            parse_stmt_list(),
        )
            .and_then(|(_, fname, args, body): (&'static str, String, Vec<String>, StmtList)| {
                if is_builtin(&fname) {
                    let msg = format!("{}", crate::errors::ParseError::Builtin(fname));
                    let err = StreamErrorFor::<I>::message_message(msg);
                    return Err(err);
                }
                let set: HashSet<&String> = args.iter().collect();
                if set.len() != args.len() {
                    let msg = format!("{}", crate::errors::ParseError::DuplicateParams(fname));
                    let err = StreamErrorFor::<I>::message_message(msg);
                    Err(err)
                } else if args.iter().any(|arg| is_special_variable(arg)) {
                    let msg = format!("{}", crate::errors::ParseError::SpecialVariableAsParameter);
                    let err = StreamErrorFor::<I>::message_message(msg);
                    Err(err)
                } else {
                    Ok(Item::FunctionDef(fname, args, body))
                }
            })
    }
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
            Item::PatternAction(
                None,
                StmtList(vec![Stmt::Print(ExprList(vec![Expr::Number(42f64)]), None)]),
            ),
        );
        assert_item(
            "BEGIN { print 42 }",
            Item::PatternAction(
                Some(Pattern::Begin),
                StmtList(vec![Stmt::Print(ExprList(vec![Expr::Number(42f64)]), None)]),
            ),
        );
        assert_item(
            "/42/ { print 42 }",
            Item::PatternAction(
                Some(Pattern::Expr(Expr::Regexp(RegexEq::new("42")))),
                StmtList(vec![Stmt::Print(ExprList(vec![Expr::Number(42f64)]), None)]),
            ),
        );
        assert_item(
            "/aa/,/dd/ { print 42 }",
            Item::PatternAction(
                Some(Pattern::Range(
                    Expr::Regexp(RegexEq::new("aa")),
                    Expr::Regexp(RegexEq::new("dd")),
                )),
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

        let input = "function my_func3(a, a) {}";
        let prog = parse_program().easy_parse(State::new(input));
        assert!(prog.is_err(), "input: {}\n{:?}", input, prog.unwrap());
        let msg = format!("{}", prog.unwrap_err());
        assert!(msg.contains("Function my_func3 has duplicate parameters"));

        let input = "function my_func4(FS) {}";
        let prog = parse_program().easy_parse(State::new(input));
        assert!(prog.is_err(), "input: {}\n{:?}", input, prog.unwrap());
        let msg = format!("{}", prog.unwrap_err());
        assert!(msg.contains("Cannot use a special variable as a function parameter"));

        let input = "function index() {}";
        let prog = parse_program().easy_parse(State::new(input));
        assert!(prog.is_err(), "input: {}\n{:?}", input, prog.unwrap());
        let msg = format!("{}", prog.unwrap_err());
        assert!(msg.contains("Function index is a built-in"));
    }

    #[test]
    fn program() {
        assert_program(
            r#"BEGIN { print "start" } END { print "end" }"#,
            Program::new(vec![
                Item::PatternAction(
                    Some(Pattern::Begin),
                    StmtList(vec![Stmt::Print(
                        ExprList(vec![Expr::String("start".to_owned())]),
                        None,
                    )]),
                ),
                Item::PatternAction(
                    Some(Pattern::End),
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
                    Some(Pattern::Begin),
                    StmtList(vec![Stmt::Print(
                        ExprList(vec![Expr::String("start".to_owned())]),
                        None,
                    )]),
                ),
                Item::PatternAction(
                    Some(Pattern::End),
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
                    Some(Pattern::Begin),
                    StmtList(vec![Stmt::Print(
                        ExprList(vec![Expr::String("start".to_owned())]),
                        None,
                    )]),
                ),
                Item::PatternAction(
                    Some(Pattern::End),
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
