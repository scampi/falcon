use crate::parser::ast::*;
use crate::parser::util::{
    parse_func_name, parse_name, parse_regexp, parse_string, skip_wrapping_spaces,
};
use combine::parser::combinator::Try;
use combine::{
    error::{ParseError, StreamError},
    parser::{
        char::{char, digit, space, spaces, string},
        choice::{choice, optional},
        combinator::attempt,
        item::{one_of, value},
        repeat::{many1, sep_by, sep_by1},
        sequence::between,
        Parser,
    },
    stream::{RangeStream, Stream, StreamErrorFor},
};
use combine::parser::choice::ChoiceParser;
use regex::Regex;

// import macros
use combine::{combine_parse_partial, combine_parser_impl, parse_mode, parser};

// TODO:
// - getline
// - builtin without args
// - array with the body as a group with comma-separated exprs

pub fn parse_expr<'a, I: 'a>() -> impl Parser<Input = I, Output = Expr> + 'a
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    do_parse_expr(false)
}

pub fn parse_print_expr<'a, I: 'a>() -> impl Parser<Input = I, Output = Expr> + 'a
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    do_parse_expr(true)
}

parser! {
    fn do_parse_expr['a, I](print_expr: bool)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        parse_conditional_expr(
            *print_expr,
            parse_or_expr(
                *print_expr,
                parse_and_expr(
                    *print_expr,
                    parse_array_expr(parse_match_expr(
                        *print_expr,
                        parse_comparison_expr(
                            *print_expr,
                            parse_concat_expr(parse_add_sub_expr(parse_mul_div_mod_expr(
                                parse_pow_expr(leaf()),
                            ))),
                        ),
                    )),
                ),
            ),
        )
    }
}

parser! {
    fn parse_expr_list['a, I]()(I) -> ExprList
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        sep_by(parse_expr(), attempt(skip_wrapping_spaces(char(',')))).map(|exprs| ExprList(exprs))
    }
}

parser! {
    pub fn parse_print_expr_list1['a, I]()(I) -> ExprList
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        sep_by1(parse_print_expr(), attempt(skip_wrapping_spaces(char(',')))).map(|exprs| ExprList(exprs))
    }
}

parser! {
    pub fn parse_expr_list1['a, I]()(I) -> ExprList
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        sep_by1(parse_expr(), attempt(skip_wrapping_spaces(char(',')))).map(|exprs| ExprList(exprs))
    }
}

parser! {
    fn parse_pow_expr['a, I, P](p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        fn right_assoc(le: Expr, re: Expr) -> Expr {
            match le {
                // pow has precedence over unary operator like -, +, and !
                Expr::UnaryMinus(minus) => Expr::UnaryMinus(Box::new(Expr::Pow(minus, Box::new(re)))),
                Expr::UnaryPlus(plus) => Expr::UnaryPlus(Box::new(Expr::Pow(plus, Box::new(re)))),
                Expr::LogicalNot(not) => Expr::LogicalNot(Box::new(Expr::Pow(not, Box::new(re)))),
                le @ _ => Expr::Pow(Box::new(le), Box::new(re)),
            }
        }

        p.and(optional(many1::<Vec<Expr>, _>(
            attempt(skip_wrapping_spaces(char('^'))).with(leaf()),
        )))
        .map(|(left_expr, pow_expr)| {
            if let Some(pow_expr) = pow_expr {
                let mut it = pow_expr.into_iter().rev();
                let e = it.next().unwrap();
                right_assoc(left_expr, it.fold(e, |re, le| right_assoc(le, re)))
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn parse_mul_div_mod_expr['a, I, P](p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        p.and(optional(many1::<Vec<(char, Expr)>, _>(
            attempt(skip_wrapping_spaces(one_of("*/%".chars()))).and(parse_pow_expr(leaf())),
        )))
        .map(|(left_expr, rest)| {
            if let Some(rest) = rest {
                rest.into_iter().fold(left_expr, |le, (op, re)| match op {
                    '*' => Expr::Mul(Box::new(le), Box::new(re)),
                    '/' => Expr::Div(Box::new(le), Box::new(re)),
                    '%' => Expr::Mod(Box::new(le), Box::new(re)),
                    _ => unreachable!(),
                })
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn parse_add_sub_expr['a, I, P](p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        p.and(optional(many1::<Vec<(char, Expr)>, _>(
            attempt(skip_wrapping_spaces(one_of("+-".chars())))
                .and(parse_mul_div_mod_expr(parse_pow_expr(leaf()))),
        )))
        .map(|(left_expr, rest)| {
            if let Some(rest) = rest {
                rest.into_iter().fold(left_expr, |le, (op, re)| match op {
                    '+' => Expr::Add(Box::new(le), Box::new(re)),
                    '-' => Expr::Minus(Box::new(le), Box::new(re)),
                    _ => unreachable!(),
                })
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn parse_concat_expr['a, I, P](p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        p.and(optional(many1::<Vec<Expr>, _>(attempt(
            space().with(parse_add_sub_expr(parse_mul_div_mod_expr(parse_pow_expr(leaf()))))),
        )))
        .map(|(left_expr, rest)| {
            if let Some(rest) = rest {
                rest.into_iter()
                    .fold(left_expr, |le, re| Expr::Concat(Box::new(le), Box::new(re)))
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn parse_comparison_expr['a, I, P](print_expr: bool, p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        let ops = if *print_expr {
            choice((
               attempt(skip_wrapping_spaces(string("<="))),
               attempt(skip_wrapping_spaces(string("<"))),
               attempt(skip_wrapping_spaces(string("!="))),
               attempt(skip_wrapping_spaces(string("=="))),
               attempt(skip_wrapping_spaces(string(">="))),
               // hack to have both tuples with the the same size
               attempt(skip_wrapping_spaces(string(">="))),
            ))
        } else {
            choice((
               attempt(skip_wrapping_spaces(string("<="))),
               attempt(skip_wrapping_spaces(string("<"))),
               attempt(skip_wrapping_spaces(string("!="))),
               attempt(skip_wrapping_spaces(string("=="))),
               attempt(skip_wrapping_spaces(string(">="))),
               attempt(skip_wrapping_spaces(string(">"))),
            ))
        };

        p.and(optional(
            ops
            .and(parse_concat_expr(parse_add_sub_expr(
                parse_mul_div_mod_expr(parse_pow_expr(leaf())),
            ))),
        ))
        .map(|(left_expr, rest)| {
            if let Some((op, right_expr)) = rest {
                Expr::Comparison(
                    CmpOperator::from(op),
                    Box::new(left_expr),
                    Box::new(right_expr),
                )
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn parse_match_expr['a, I, P](print_expr: bool, p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        p.and(optional(many1::<Vec<(&str, Expr)>, _>(
            choice((
                attempt(skip_wrapping_spaces(string("!~"))),
                attempt(skip_wrapping_spaces(string("~"))),
            ))
            .and(parse_comparison_expr(*print_expr, parse_concat_expr(
                parse_add_sub_expr(parse_mul_div_mod_expr(parse_pow_expr(leaf()))),
            ))),
        )))
        .map(|(left_expr, rest)| {
            if let Some(rest) = rest {
                rest.into_iter().fold(left_expr, |le, (op, re)| match op {
                    "~" => Expr::Match(Box::new(le), Box::new(re)),
                    "!~" => Expr::NonMatch(Box::new(le), Box::new(re)),
                    _ => unreachable!(),
                })
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn parse_array_expr['a, I, P](p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        p.and(optional(
            attempt(skip_wrapping_spaces(string("in"))).with(parse_name())
        ))
        .map(|(left_expr, array_name)| {
            if let Some(array_name) = array_name {
                Expr::Array(ExprList(vec![left_expr]), array_name)
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn parse_and_expr['a, I, P](print_expr: bool, p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        p.and(optional(many1::<Vec<Expr>, _>(
            attempt(skip_wrapping_spaces(string("&&"))).with(parse_array_expr(parse_match_expr(
                *print_expr,
                parse_comparison_expr(
                    *print_expr,
                    parse_concat_expr(parse_add_sub_expr(parse_mul_div_mod_expr(parse_pow_expr(
                        leaf(),
                    )))),
                ),
            ))),
        )))
        .map(|(left_expr, rest)| {
            if let Some(rest) = rest {
                rest.into_iter().fold(left_expr, |le, re|
                    Expr::LogicalAnd(
                            Box::new(le),
                            Box::new(re)
                    )
                )
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn parse_or_expr['a, I, P](print_expr: bool, p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        p.and(optional(many1::<Vec<Expr>, _>(
            attempt(skip_wrapping_spaces(string("||"))).with(parse_and_expr(
                *print_expr,
                parse_array_expr(parse_match_expr(
                    *print_expr,
                    parse_comparison_expr(
                        *print_expr,
                        parse_concat_expr(parse_add_sub_expr(parse_mul_div_mod_expr(
                            parse_pow_expr(leaf()),
                        ))),
                    ),
                )),
            )),
        )))
        .map(|(left_expr, rest)| {
            if let Some(rest) = rest {
                rest.into_iter().fold(left_expr, |le, re|
                    Expr::LogicalOr(
                            Box::new(le),
                            Box::new(re)
                    )
                )
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn parse_conditional_expr['a, I, P](print_expr: bool, p: P)(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input = I, Output = Expr> + 'a,
    ]
    {
        p.and(optional(
            attempt(skip_wrapping_spaces(char('?')))
                .with(parse_or_expr(
                    *print_expr,
                    parse_and_expr(
                        *print_expr,
                        parse_array_expr(parse_match_expr(
                            *print_expr,
                            parse_comparison_expr(
                                *print_expr,
                                parse_concat_expr(parse_add_sub_expr(parse_mul_div_mod_expr(
                                    parse_pow_expr(leaf()),
                                ))),
                            ),
                        )),
                    ),
                ))
                .and(skip_wrapping_spaces(char(':')).with(parse_conditional_expr(
                    *print_expr,
                    parse_or_expr(
                        *print_expr,
                        parse_and_expr(
                            *print_expr,
                            parse_array_expr(parse_match_expr(
                                *print_expr,
                                parse_comparison_expr(
                                    *print_expr,
                                    parse_concat_expr(parse_add_sub_expr(parse_mul_div_mod_expr(
                                        parse_pow_expr(leaf()),
                                    ))),
                                ),
                            )),
                        ),
                    ),
                ))),
        ))
        .map(|(left_expr, rest)| {
            if let Some((ok, ko)) = rest {
                Expr::Conditional(Box::new(left_expr), Box::new(ok), Box::new(ko))
            } else {
                left_expr
            }
        })
    }
}

parser! {
    fn leaf['a, I]()(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice((
            attempt(parse_string()).map(|s: String| Expr::String(s)),
            attempt(parse_regexp()).and_then(|ere: String| {
                match Regex::new(&ere) {
                    Ok(ere) => Ok(Expr::Regexp(RegexEq(ere))),
                    Err(e) => {
                        let msg = format!("{}", crate::errors::ParseError::InvalidRegex(e));
                        let err = StreamErrorFor::<I>::message_message(msg);
                        Err(err)
                    }
                }
            }),
            attempt(parse_number()),
            between(
                attempt(skip_wrapping_spaces(char('('))),
                skip_wrapping_spaces(char(')')),
                parse_expr(),
            )
            .map(|expr| Expr::Grouping(Box::new(expr))),
            attempt(parse_assignment()),
            attempt(skip_wrapping_spaces(string("++")))
                    .with(parse_lvalue())
                    .map(|lvalue| Expr::PreIncrement(lvalue)),
            attempt(skip_wrapping_spaces(string("--")))
                    .with(parse_lvalue())
                    .map(|lvalue| Expr::PreDecrement(lvalue)),
            attempt(skip_wrapping_spaces(char('+')))
                    .with(leaf())
                    .map(|expr| Expr::UnaryPlus(Box::new(expr))),
            attempt(skip_wrapping_spaces(char('-')))
                    .with(leaf())
                    .map(|expr| Expr::UnaryMinus(Box::new(expr))),
            attempt(skip_wrapping_spaces(char('!')))
                    .with(leaf())
                    .map(|expr| Expr::LogicalNot(Box::new(expr))),
            attempt(
                parse_lvalue()
                    .skip(skip_wrapping_spaces(string("++")))
                    .map(|lvalue| Expr::PostIncrement(lvalue)),
            ),
            attempt(
                parse_lvalue()
                    .skip(skip_wrapping_spaces(string("--")))
                    .map(|lvalue| Expr::PostDecrement(lvalue)),
            ),
            attempt(
                parse_func_name()
                .and(
                    between(
                        char('(').skip(spaces()),
                        skip_wrapping_spaces(char(')')),
                        parse_expr_list(),
                    )
                )
                .map(|(func_name, args)| Expr::FunctionCall(func_name, args)),
            ),
            attempt(parse_lvalue()).map(|lvalue| Expr::LValue(lvalue)),
        ))
    }
}

parser! {
    fn parse_number[I]()(I) -> Expr
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        spaces()
            .with(many1(digit()))
            .and(optional(char('.').with(many1(digit()))))
            .map(|(i, f): (String, Option<String>)| {
                let mut inum = i.parse::<f64>().unwrap();
                if let Some(f) = f {
                    let fnum = f.parse::<f64>().unwrap();
                    inum += fnum / (10 * f.len()) as f64;
                }
                if inum.signum() == 1.0 {
                    Expr::Number(inum)
                } else {
                    Expr::UnaryMinus(Box::new(Expr::Number(-inum)))
                }
            })
    }
}

parser! {
    fn parse_lvalue['a, I]()(I) -> LValueType
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice((
            attempt(
                parse_name()
                    .and(between(
                        skip_wrapping_spaces(char('[')),
                        skip_wrapping_spaces(char(']')),
                        parse_expr_list1(),
                    ))
                    .map(|(name, exprs)| LValueType::Brackets(name, exprs))
            ),
            attempt(spaces().skip(char('$')))
            .with(leaf())
            .map(|expr| LValueType::Dollar(Box::new(expr))),
            attempt(parse_name().map(|name| LValueType::Name(name))),
        ))
    }
}

parser! {
    fn parse_assignment['a, I]()(I) -> Expr
    where [
        I: RangeStream<Item = char, Range = &'a str> + 'a,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        parse_lvalue()
            .and(choice((
                attempt(skip_wrapping_spaces(string("="))),
                attempt(skip_wrapping_spaces(string("^="))),
                attempt(skip_wrapping_spaces(string("%="))),
                attempt(skip_wrapping_spaces(string("*="))),
                attempt(skip_wrapping_spaces(string("/="))),
                attempt(skip_wrapping_spaces(string("+="))),
                attempt(skip_wrapping_spaces(string("-="))),
            )))
            .and(parse_expr())
            .map(|((lvalue, op), rvalue)| {
                Expr::Assign(AssignType::new(&op), lvalue, Box::new(rvalue))
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::stream::state::State;

    fn assert_expr(input: &str, expected: Expr) {
        let expr = parse_expr().easy_parse(State::new(input));
        assert!(expr.is_ok(), "input: {}\n{}", input, expr.unwrap_err());
        let expr = expr.unwrap();
        assert_eq!(
            expr.0, expected,
            "\nexpected:\n{}\n\nactual:\n{}\n{:?}",
            expected, expr.0, expr
        );
    }

    fn assert_expr_with_leftovers(input: &str, expected: Expr, leftover: &str) {
        let expr = parse_expr().easy_parse(input);
        assert!(expr.is_ok(), "{:?}", expr);
        let expr = expr.unwrap();
        assert_eq!(expr.1, leftover);
        assert_eq!(expr.0, expected, "{}", expr.0);
    }

    #[test]
    fn leaves() {
        assert_expr("42", Expr::Number(42.0));
        assert_expr("42.5", Expr::Number(42.5));
        assert_expr("   42.5 \t\n  ", Expr::Number(42.5));
        assert_expr(r#""42""#, Expr::String("42".to_owned()));
        assert_expr("/42/", Expr::Regexp(RegexEq::new("42")));
        assert_expr("(42)", Expr::Grouping(Box::new(Expr::Number(42.0))));
        assert_expr("+ 42", Expr::UnaryPlus(Box::new(Expr::Number(42.0))));
        assert_expr("- 42", Expr::UnaryMinus(Box::new(Expr::Number(42.0))));
        assert_expr("! 42", Expr::LogicalNot(Box::new(Expr::Number(42.0))));
        assert_expr(
            "connor(1, 3, 5)",
            Expr::FunctionCall(
                "connor".to_owned(),
                ExprList(vec![
                    Expr::Number(1.0),
                    Expr::Number(3.0),
                    Expr::Number(5.0),
                ]),
            ),
        );
        assert_expr(
            "$0++",
            Expr::PostIncrement(LValueType::Dollar(Box::new(Expr::Number(0.0)))),
        );
        assert_expr(
            "var ++",
            Expr::PostIncrement(LValueType::Name("var".to_owned())),
        );
        assert_expr(
            "--var",
            Expr::PreDecrement(LValueType::Name("var".to_owned())),
        );
        assert_expr(
            "++ var",
            Expr::PreIncrement(LValueType::Name("var".to_owned())),
        );
        assert_expr(
            "var = 42",
            Expr::Assign(
                AssignType::Normal,
                LValueType::Name("var".to_owned()),
                Box::new(Expr::Number(42.0)),
            ),
        );
        assert_expr(
            "var -= 42",
            Expr::Assign(
                AssignType::Sub,
                LValueType::Name("var".to_owned()),
                Box::new(Expr::Number(42.0)),
            ),
        );
        assert_expr(
            "var *= 42",
            Expr::Assign(
                AssignType::Mul,
                LValueType::Name("var".to_owned()),
                Box::new(Expr::Number(42.0)),
            ),
        );
    }

    #[test]
    fn unary_plus() {
        assert_expr("+42", Expr::UnaryPlus(Box::new(Expr::Number(42.0))));
        assert_expr(
            "+ $0 ^ 2",
            Expr::UnaryPlus(Box::new(Expr::Pow(
                Box::new(Expr::LValue(LValueType::Dollar(Box::new(Expr::Number(
                    0.0,
                ))))),
                Box::new(Expr::Number(2.0)),
            ))),
        );
    }

    #[test]
    fn unary_minus() {
        assert_expr("-42", Expr::UnaryMinus(Box::new(Expr::Number(42.0))));
        assert_expr("- 42", Expr::UnaryMinus(Box::new(Expr::Number(42.0))));
        assert_expr(
            "- $0 ^ 2",
            Expr::UnaryMinus(Box::new(Expr::Pow(
                Box::new(Expr::LValue(LValueType::Dollar(Box::new(Expr::Number(
                    0.0,
                ))))),
                Box::new(Expr::Number(2.0)),
            ))),
        );
        assert_expr(
            "- --a ^ ++b",
            Expr::UnaryMinus(Box::new(Expr::Pow(
                Box::new(Expr::PreDecrement(LValueType::Name("a".to_string()))),
                Box::new(Expr::PreIncrement(LValueType::Name("b".to_string()))),
            ))),
        );
    }

    #[test]
    fn pow() {
        assert_expr(
            "2 ^ 3 ^ 4",
            Expr::Pow(
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Pow(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "2 ^ - 3 ^ 4",
            Expr::Pow(
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::UnaryMinus(Box::new(Expr::Pow(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(4.0)),
                )))),
            ),
        );
    }

    #[test]
    fn mul() {
        assert_expr(
            "2 * 3 * 4",
            Expr::Mul(
                Box::new(Expr::Mul(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 * - 3 * 4",
            Expr::Mul(
                Box::new(Expr::Mul(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::UnaryMinus(Box::new(Expr::Number(3.0)))),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
    }

    #[test]
    fn div() {
        assert_expr(
            "2 / 3 / 4",
            Expr::Div(
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 / - 3 / 4",
            Expr::Div(
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::UnaryMinus(Box::new(Expr::Number(3.0)))),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
    }

    #[test]
    fn modulo() {
        assert_expr(
            "2 % 3 % 4",
            Expr::Mod(
                Box::new(Expr::Mod(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 % - 3 % 4",
            Expr::Mod(
                Box::new(Expr::Mod(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::UnaryMinus(Box::new(Expr::Number(3.0)))),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
    }

    #[test]
    fn addition() {
        assert_expr(
            "2 + 3 + 4",
            Expr::Add(
                Box::new(Expr::Add(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 + - 3 + 4",
            Expr::Add(
                Box::new(Expr::Add(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::UnaryMinus(Box::new(Expr::Number(3.0)))),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 + 3 / 4",
            Expr::Add(
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "2 / 3 + 4",
            Expr::Add(
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 ^ 5 + 3 * 4",
            Expr::Add(
                Box::new(Expr::Pow(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(5.0)),
                )),
                Box::new(Expr::Mul(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "2 ^ 5 + 3 ^ 2 * 4 ^ 3",
            Expr::Add(
                Box::new(Expr::Pow(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(5.0)),
                )),
                Box::new(Expr::Mul(
                    Box::new(Expr::Pow(
                        Box::new(Expr::Number(3.0)),
                        Box::new(Expr::Number(2.0)),
                    )),
                    Box::new(Expr::Pow(
                        Box::new(Expr::Number(4.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                )),
            ),
        );
    }

    #[test]
    fn substraction() {
        assert_expr(
            "2 - 3 - 4",
            Expr::Minus(
                Box::new(Expr::Minus(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 - - 3 - 4",
            Expr::Minus(
                Box::new(Expr::Minus(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::UnaryMinus(Box::new(Expr::Number(3.0)))),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 - 3 / 4",
            Expr::Minus(
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "2 / 3 - 4",
            Expr::Minus(
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 ^ 5 - 3 * 4",
            Expr::Minus(
                Box::new(Expr::Pow(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(5.0)),
                )),
                Box::new(Expr::Mul(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
    }

    #[test]
    fn mix_add_sub() {
        assert_expr(
            "2 - 3 / 1 + 4 * 2 ^ 2",
            Expr::Add(
                Box::new(Expr::Minus(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Div(
                        Box::new(Expr::Number(3.0)),
                        Box::new(Expr::Number(1.0)),
                    )),
                )),
                Box::new(Expr::Mul(
                    Box::new(Expr::Number(4.0)),
                    Box::new(Expr::Pow(
                        Box::new(Expr::Number(2.0)),
                        Box::new(Expr::Number(2.0)),
                    )),
                )),
            ),
        );
        assert_expr(
            "2 - 3 + 4",
            Expr::Add(
                Box::new(Expr::Minus(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
    }

    #[test]
    fn mix_mul_pow() {
        assert_expr(
            "2 * 3 ^ 4",
            Expr::Mul(
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Pow(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "2 * 3 ^ 4 * 5",
            Expr::Mul(
                Box::new(Expr::Mul(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Pow(
                        Box::new(Expr::Number(3.0)),
                        Box::new(Expr::Number(4.0)),
                    )),
                )),
                Box::new(Expr::Number(5.0)),
            ),
        );
        assert_expr(
            "2 ^ 3 * 4",
            Expr::Mul(
                Box::new(Expr::Pow(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
    }

    #[test]
    fn mix_div_pow() {
        assert_expr(
            "2 / 3 ^ 4",
            Expr::Div(
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Pow(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "2 ^ 3 / 4",
            Expr::Div(
                Box::new(Expr::Pow(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
    }

    #[test]
    fn mix_div_mul() {
        assert_expr(
            "2 / 3 * 4",
            Expr::Mul(
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
        assert_expr(
            "2 * 3 / 4",
            Expr::Div(
                Box::new(Expr::Mul(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Number(4.0)),
            ),
        );
    }

    #[test]
    fn concat() {
        assert_expr(
            "2 / 3 4 + 3",
            Expr::Concat(
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Add(
                    Box::new(Expr::Number(4.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
        );
        assert_expr(
            "2 ^ 3 1 / 4",
            Expr::Concat(
                Box::new(Expr::Pow(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "1 2 3",
            Expr::Concat(
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Number(3.0)),
            ),
        );
    }

    #[test]
    fn comparison() {
        assert_expr(
            "1 + 2 == 2 / 3",
            Expr::Comparison(
                CmpOperator::Equal,
                Box::new(Expr::Add(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
        );
        assert_expr(
            "1 2 < 2 / 3 4 + 3",
            Expr::Comparison(
                CmpOperator::LessThan,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Concat(
                    Box::new(Expr::Div(
                        Box::new(Expr::Number(2.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                    Box::new(Expr::Add(
                        Box::new(Expr::Number(4.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                )),
            ),
        );
        assert_expr(
            "1 2 <= 2 / 3 4 + 3",
            Expr::Comparison(
                CmpOperator::LessThanOrEqual,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Concat(
                    Box::new(Expr::Div(
                        Box::new(Expr::Number(2.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                    Box::new(Expr::Add(
                        Box::new(Expr::Number(4.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                )),
            ),
        );
        assert_expr(
            "2 / 3 4 + 3 <= 1 2",
            Expr::Comparison(
                CmpOperator::LessThanOrEqual,
                Box::new(Expr::Concat(
                    Box::new(Expr::Div(
                        Box::new(Expr::Number(2.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                    Box::new(Expr::Add(
                        Box::new(Expr::Number(4.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                )),
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
            ),
        );
        assert_expr(
            "1 2 != 2 3",
            Expr::Comparison(
                CmpOperator::NotEqual,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
        );
        assert_expr(
            "1 2 == 2 3",
            Expr::Comparison(
                CmpOperator::Equal,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
        );
        assert_expr(
            "1 2 > 2 3",
            Expr::Comparison(
                CmpOperator::GreaterThan,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
        );
        assert_expr(
            "1 2 >= 2 3",
            Expr::Comparison(
                CmpOperator::GreaterThanOrEqual,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
        );
    }

    #[test]
    fn syntax_error() {
        assert_expr_with_leftovers(
            "1 2 < 2 / 3 < 4",
            Expr::Comparison(
                CmpOperator::LessThan,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
            " < 4",
        );
        assert_expr_with_leftovers(
            "1 2 == 2 / 3 > 4 + 3",
            Expr::Comparison(
                CmpOperator::Equal,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
            " > 4 + 3",
        );
        assert_expr_with_leftovers(
            "1 2 < 2 / 3 < 4 > 3",
            Expr::Comparison(
                CmpOperator::LessThan,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
            " < 4 > 3",
        );
        assert_expr_with_leftovers(
            "1 2 == 2 / 3 > 4 + 3 != 3",
            Expr::Comparison(
                CmpOperator::Equal,
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
            " > 4 + 3 != 3",
        );
    }

    #[test]
    fn r#match() {
        assert_expr(
            "1 ~ 2",
            Expr::Match(Box::new(Expr::Number(1.0)), Box::new(Expr::Number(2.0))),
        );
        assert_expr(
            "1 3 ~ 2 4",
            Expr::Match(
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "1 < 3 ~ 2 != 4",
            Expr::Match(
                Box::new(Expr::Comparison(
                    CmpOperator::LessThan,
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Comparison(
                    CmpOperator::NotEqual,
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "1 ~ 2 ~ 3",
            Expr::Match(
                Box::new(Expr::Match(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Number(3.0)),
            ),
        );
    }

    #[test]
    fn non_match() {
        assert_expr(
            "1 !~ 2",
            Expr::NonMatch(Box::new(Expr::Number(1.0)), Box::new(Expr::Number(2.0))),
        );
        assert_expr(
            "1 3 !~ 2 4",
            Expr::NonMatch(
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Concat(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "1 < 3 !~ 2 != 4",
            Expr::NonMatch(
                Box::new(Expr::Comparison(
                    CmpOperator::LessThan,
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(3.0)),
                )),
                Box::new(Expr::Comparison(
                    CmpOperator::NotEqual,
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            "1 ~ 2 !~ 3",
            Expr::NonMatch(
                Box::new(Expr::Match(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Number(3.0)),
            ),
        );
    }

    #[test]
    fn in_array() {
        assert_expr(
            "1 in array",
            Expr::Array(ExprList(vec![Expr::Number(1.0)]), String::from("array")),
        );
        assert_expr(
            "1 2 in array",
            Expr::Array(
                ExprList(vec![Expr::Concat(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )]),
                String::from("array"),
            ),
        );
        assert_expr(
            "1 + 2 in array",
            Expr::Array(
                ExprList(vec![Expr::Add(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )]),
                String::from("array"),
            ),
        );
    }

    #[test]
    fn logical_operators() {
        assert_expr(
            "1 && 2",
            Expr::LogicalAnd(Box::new(Expr::Number(1.0)), Box::new(Expr::Number(2.0))),
        );
        assert_expr(
            "1 || 2",
            Expr::LogicalOr(Box::new(Expr::Number(1.0)), Box::new(Expr::Number(2.0))),
        );
        assert_expr(
            "1 || 2 || 3",
            Expr::LogicalOr(
                Box::new(Expr::LogicalOr(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Number(3.0)),
            ),
        );
        assert_expr(
            "1 + 2 && 2 * 3",
            Expr::LogicalAnd(
                Box::new(Expr::Add(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Mul(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )),
            ),
        );
        assert_expr(
            "1 + 2 && 2 * 3 && 3 / 2",
            Expr::LogicalAnd(
                Box::new(Expr::LogicalAnd(
                    Box::new(Expr::Add(
                        Box::new(Expr::Number(1.0)),
                        Box::new(Expr::Number(2.0)),
                    )),
                    Box::new(Expr::Mul(
                        Box::new(Expr::Number(2.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                )),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(2.0)),
                )),
            ),
        );
        assert_expr(
            "1 + 2 && 2 * 3 || 3 / 2",
            Expr::LogicalOr(
                Box::new(Expr::LogicalAnd(
                    Box::new(Expr::Add(
                        Box::new(Expr::Number(1.0)),
                        Box::new(Expr::Number(2.0)),
                    )),
                    Box::new(Expr::Mul(
                        Box::new(Expr::Number(2.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                )),
                Box::new(Expr::Div(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(2.0)),
                )),
            ),
        );
        assert_expr(
            "1 + 2 || 2 * 3 && 3 / 2",
            Expr::LogicalOr(
                Box::new(Expr::Add(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::LogicalAnd(
                    Box::new(Expr::Mul(
                        Box::new(Expr::Number(2.0)),
                        Box::new(Expr::Number(3.0)),
                    )),
                    Box::new(Expr::Div(
                        Box::new(Expr::Number(3.0)),
                        Box::new(Expr::Number(2.0)),
                    )),
                )),
            ),
        );
        assert_expr(
            "1 2 in array1 || 3 4 in array2",
            Expr::LogicalOr(
                Box::new(Expr::Array(
                    ExprList(vec![Expr::Concat(
                        Box::new(Expr::Number(1.0)),
                        Box::new(Expr::Number(2.0)),
                    )]),
                    String::from("array1"),
                )),
                Box::new(Expr::Array(
                    ExprList(vec![Expr::Concat(
                        Box::new(Expr::Number(3.0)),
                        Box::new(Expr::Number(4.0)),
                    )]),
                    String::from("array2"),
                )),
            ),
        );
        assert_expr(
            "1 2 in array1 || 3 4 in array2 && 5 6 in array3",
            Expr::LogicalOr(
                Box::new(Expr::Array(
                    ExprList(vec![Expr::Concat(
                        Box::new(Expr::Number(1.0)),
                        Box::new(Expr::Number(2.0)),
                    )]),
                    String::from("array1"),
                )),
                Box::new(Expr::LogicalAnd(
                    Box::new(Expr::Array(
                        ExprList(vec![Expr::Concat(
                            Box::new(Expr::Number(3.0)),
                            Box::new(Expr::Number(4.0)),
                        )]),
                        String::from("array2"),
                    )),
                    Box::new(Expr::Array(
                        ExprList(vec![Expr::Concat(
                            Box::new(Expr::Number(5.0)),
                            Box::new(Expr::Number(6.0)),
                        )]),
                        String::from("array3"),
                    )),
                )),
            ),
        );
        assert_expr(
            "! $0 ^ 2",
            Expr::LogicalNot(Box::new(Expr::Pow(
                Box::new(Expr::LValue(LValueType::Dollar(Box::new(Expr::Number(
                    0.0,
                ))))),
                Box::new(Expr::Number(2.0)),
            ))),
        );
    }

    #[test]
    fn conditional() {
        assert_expr(
            "1 ? 2 : 3",
            Expr::Conditional(
                Box::new(Expr::Number(1.0)),
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Number(3.0)),
            ),
        );
        assert_expr(
            "1 || 2 ? 3 + 2 : 2 < 4",
            Expr::Conditional(
                Box::new(Expr::LogicalOr(
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Add(
                    Box::new(Expr::Number(3.0)),
                    Box::new(Expr::Number(2.0)),
                )),
                Box::new(Expr::Comparison(
                    CmpOperator::LessThan,
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(4.0)),
                )),
            ),
        );
        assert_expr(
            r#"num == 1 ? "number 1" : num == 2 ? "number 2" : "something else""#,
            Expr::Conditional(
                Box::new(Expr::Comparison(
                    CmpOperator::Equal,
                    Box::new(Expr::LValue(LValueType::Name("num".to_string()))),
                    Box::new(Expr::Number(1.0)),
                )),
                Box::new(Expr::String("number 1".to_string())),
                Box::new(Expr::Conditional(
                    Box::new(Expr::Comparison(
                        CmpOperator::Equal,
                        Box::new(Expr::LValue(LValueType::Name("num".to_string()))),
                        Box::new(Expr::Number(2.0)),
                    )),
                    Box::new(Expr::String("number 2".to_string())),
                    Box::new(Expr::String("something else".to_string())),
                )),
            ),
        );
        assert_expr(
            "1 ? 2 : 3 0 ? 1 : 2",
            Expr::Conditional(
                Box::new(Expr::Number(1.0)),
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Conditional(
                    Box::new(Expr::Concat(
                        Box::new(Expr::Number(3.0)),
                        Box::new(Expr::Number(0.0)),
                    )),
                    Box::new(Expr::Number(1.0)),
                    Box::new(Expr::Number(2.0)),
                )),
            ),
        );
        assert_expr(
            "1 ? ! a : ! _b",
            Expr::Conditional(
                Box::new(Expr::Number(1.0)),
                Box::new(Expr::LogicalNot(Box::new(Expr::LValue(LValueType::Name(
                    "a".to_string(),
                ))))),
                Box::new(Expr::LogicalNot(Box::new(Expr::LValue(LValueType::Name(
                    "_b".to_string(),
                ))))),
            ),
        );
    }

    #[test]
    fn string() {
        assert_expr(
            r#"1 ? "ok" : "ko""#,
            Expr::Conditional(
                Box::new(Expr::Number(1.0)),
                Box::new(Expr::String("ok".to_string())),
                Box::new(Expr::String("ko".to_string())),
            ),
        );
    }

    #[test]
    fn lvalue() {
        assert_expr("_a", Expr::LValue(LValueType::Name("_a".to_owned())));
        assert_expr(
            "$_b",
            Expr::LValue(LValueType::Dollar(Box::new(Expr::LValue(
                LValueType::Name("_b".to_owned()),
            )))),
        );
        assert_expr(
            "   $_b",
            Expr::LValue(LValueType::Dollar(Box::new(Expr::LValue(
                LValueType::Name("_b".to_owned()),
            )))),
        );
        assert_expr(
            "_c[1,2,3]",
            Expr::LValue(LValueType::Brackets(
                "_c".to_owned(),
                ExprList(vec![
                    Expr::Number(1.0),
                    Expr::Number(2.0),
                    Expr::Number(3.0),
                ]),
            )),
        );
        assert_expr(
            "a[2] * $0 + b",
            Expr::Add(
                Box::new(Expr::Mul(
                    Box::new(Expr::LValue(LValueType::Brackets(
                        "a".to_owned(),
                        ExprList(vec![Expr::Number(2.0)]),
                    ))),
                    Box::new(Expr::LValue(LValueType::Dollar(Box::new(Expr::Number(
                        0.0,
                    ))))),
                )),
                Box::new(Expr::LValue(LValueType::Name("b".to_owned()))),
            ),
        );
    }

    #[test]
    fn regexp() {
        assert_expr(
            "1 ? /ok/ : /ko/",
            Expr::Conditional(
                Box::new(Expr::Number(1.0)),
                Box::new(Expr::Regexp(RegexEq::new("ok"))),
                Box::new(Expr::Regexp(RegexEq::new("ko"))),
            ),
        );

        let input = r#"1 ? "ok" : /bad {/"#;
        let expr = parse_expr().easy_parse(State::new(input));
        assert!(expr.is_err(), "input: {}\n{:?}", input, expr.unwrap());
        let msg = format!("{}", expr.unwrap_err());
        assert!(msg.contains("Invalid regex"));

        let input = r#"/bad {/"#;
        let expr = parse_expr().easy_parse(State::new(input));
        assert!(expr.is_err(), "input: {}\n{:?}", input, expr.unwrap());
        let msg = format!("{}", expr.unwrap_err());
        assert!(msg.contains("Invalid regex"));
    }

    #[test]
    fn pre_increment_decrement() {
        assert_expr(
            "1 ? ++_a : --_b",
            Expr::Conditional(
                Box::new(Expr::Number(1.0)),
                Box::new(Expr::PreIncrement(LValueType::Name("_a".to_string()))),
                Box::new(Expr::PreDecrement(LValueType::Name("_b".to_string()))),
            ),
        );
        assert_expr(
            "--a ^ ++b",
            Expr::Pow(
                Box::new(Expr::PreDecrement(LValueType::Name("a".to_string()))),
                Box::new(Expr::PreIncrement(LValueType::Name("b".to_string()))),
            ),
        );
    }

    #[test]
    fn post_increment_decrement() {
        assert_expr(
            "1 ? _a++ : _b--",
            Expr::Conditional(
                Box::new(Expr::Number(1.0)),
                Box::new(Expr::PostIncrement(LValueType::Name("_a".to_string()))),
                Box::new(Expr::PostDecrement(LValueType::Name("_b".to_string()))),
            ),
        );
        assert_expr(
            "a-- ^ b++",
            Expr::Pow(
                Box::new(Expr::PostDecrement(LValueType::Name("a".to_string()))),
                Box::new(Expr::PostIncrement(LValueType::Name("b".to_string()))),
            ),
        );
    }

    #[test]
    fn grouping() {
        assert_expr(
            " ( 2 + 3 )",
            Expr::Grouping(Box::new(Expr::Add(
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Number(3.0)),
            ))),
        );
        assert_expr(
            "2 * (2 + 3)",
            Expr::Mul(
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Grouping(Box::new(Expr::Add(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Number(3.0)),
                )))),
            ),
        );
        assert_expr(
            "(-2) ^ 3",
            Expr::Pow(
                Box::new(Expr::Grouping(Box::new(Expr::UnaryMinus(Box::new(
                    Expr::Number(2.0),
                ))))),
                Box::new(Expr::Number(3.0)),
            ),
        );
        assert_expr(
            "5 / (2 * (2 + 3))",
            Expr::Div(
                Box::new(Expr::Number(5.0)),
                Box::new(Expr::Grouping(Box::new(Expr::Mul(
                    Box::new(Expr::Number(2.0)),
                    Box::new(Expr::Grouping(Box::new(Expr::Add(
                        Box::new(Expr::Number(2.0)),
                        Box::new(Expr::Number(3.0)),
                    )))),
                )))),
            ),
        );
    }

    #[test]
    fn function_call() {
        assert_expr(
            "my_func1()",
            Expr::FunctionCall("my_func1".to_string(), ExprList(vec![])),
        );
        assert_expr(
            "my_func2(2, 3)",
            Expr::FunctionCall(
                "my_func2".to_string(),
                ExprList(vec![Expr::Number(2.0), Expr::Number(3.0)]),
            ),
        );
        assert_expr(
            "my_func3(2 + 3, a)",
            Expr::FunctionCall(
                "my_func3".to_string(),
                ExprList(vec![
                    Expr::Add(Box::new(Expr::Number(2.0)), Box::new(Expr::Number(3.0))),
                    Expr::LValue(LValueType::Name("a".to_string())),
                ]),
            ),
        );
        assert_expr(
            "cos(rand())",
            Expr::FunctionCall(
                "cos".to_string(),
                ExprList(vec![Expr::FunctionCall(
                    "rand".to_string(),
                    ExprList(vec![]),
                )]),
            ),
        );
    }

    #[test]
    fn assignment() {
        assert_expr(
            "a = 42",
            Expr::Assign(
                AssignType::Normal,
                LValueType::Name("a".to_string()),
                Box::new(Expr::Number(42.0)),
            ),
        );
        assert_expr(
            "a ^= 42",
            Expr::Assign(
                AssignType::Pow,
                LValueType::Name("a".to_string()),
                Box::new(Expr::Number(42.0)),
            ),
        );
        assert_expr(
            "a %= 42",
            Expr::Assign(
                AssignType::Mod,
                LValueType::Name("a".to_string()),
                Box::new(Expr::Number(42.0)),
            ),
        );
        assert_expr(
            "a *= 42",
            Expr::Assign(
                AssignType::Mul,
                LValueType::Name("a".to_string()),
                Box::new(Expr::Number(42.0)),
            ),
        );
        assert_expr(
            "a /= 42",
            Expr::Assign(
                AssignType::Div,
                LValueType::Name("a".to_string()),
                Box::new(Expr::Number(42.0)),
            ),
        );
        assert_expr(
            "a += 42",
            Expr::Assign(
                AssignType::Add,
                LValueType::Name("a".to_string()),
                Box::new(Expr::Number(42.0)),
            ),
        );
        assert_expr(
            "a -= 42",
            Expr::Assign(
                AssignType::Sub,
                LValueType::Name("a".to_string()),
                Box::new(Expr::Number(42.0)),
            ),
        );
        assert_expr(
            "a = b = 42",
            Expr::Assign(
                AssignType::Normal,
                LValueType::Name("a".to_string()),
                Box::new(Expr::Assign(
                    AssignType::Normal,
                    LValueType::Name("b".to_string()),
                    Box::new(Expr::Number(42.0)),
                )),
            ),
        );
    }
}
