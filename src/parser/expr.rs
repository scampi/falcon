use nom::{types::CompleteStr, *};
use std::fmt;

use crate::parser::util::{parse_func_name, parse_name, parse_regexp, parse_string};

// TODO:
// - getline
// - builtin without args

#[derive(Debug, PartialEq)]
pub struct ExprList(pub Vec<Expr>);

impl fmt::Display for ExprList {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        for expr in &self.0 {
            write!(formatter, "{}, ", expr)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Grouping(Box<Expr>),
    UnaryPlus(Box<Expr>),
    UnaryMinus(Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Concat(Box<Expr>, Box<Expr>),
    Comparison(CmpOperator, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Box<Expr>),
    NonMatch(Box<Expr>, Box<Expr>),
    Array(ExprList, String),
    LogicalAnd(Box<Expr>, Box<Expr>),
    LogicalOr(Box<Expr>, Box<Expr>),
    LogicalNot(Box<Expr>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    Number(f64),
    String(String),
    LValue(LValueType),
    Regexp(String),
    PreIncrement(LValueType),
    PreDecrement(LValueType),
    PostIncrement(LValueType),
    PostDecrement(LValueType),
    FunctionCall(String, ExprList),
    Assign(AssignType, LValueType, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum CmpOperator {
    LessThan,
    LessThanOrEqual,
    NotEqual,
    Equal,
    GreaterThan,
    GreaterThanOrEqual,
}

impl CmpOperator {
    pub fn compare<T: PartialOrd>(&self, avalue: &T, bvalue: &T) -> bool {
        match self {
            CmpOperator::LessThan => avalue < bvalue,
            CmpOperator::LessThanOrEqual => avalue <= bvalue,
            CmpOperator::NotEqual => avalue != bvalue,
            CmpOperator::Equal => avalue == bvalue,
            CmpOperator::GreaterThan => avalue > bvalue,
            CmpOperator::GreaterThanOrEqual => avalue >= bvalue,
        }
    }
}

impl From<&str> for CmpOperator {
    fn from(s: &str) -> Self {
        match s {
            "<" => CmpOperator::LessThan,
            "<=" => CmpOperator::LessThanOrEqual,
            "!=" => CmpOperator::NotEqual,
            "==" => CmpOperator::Equal,
            ">" => CmpOperator::GreaterThan,
            ">=" => CmpOperator::GreaterThanOrEqual,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for CmpOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CmpOperator::LessThan => write!(f, "<"),
            CmpOperator::LessThanOrEqual => write!(f, "<="),
            CmpOperator::NotEqual => write!(f, "!="),
            CmpOperator::Equal => write!(f, "=="),
            CmpOperator::GreaterThan => write!(f, ">"),
            CmpOperator::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LValueType {
    Name(String),
    Dollar(Box<Expr>),
    Brackets(String, ExprList),
}

impl fmt::Display for LValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LValueType::Name(name) => write!(f, "{}", name),
            LValueType::Dollar(e) => write!(f, "{}", e),
            LValueType::Brackets(name, exprs) => write!(f, "{}[{}]", name, exprs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AssignType {
    Normal,
    Pow,
    Mod,
    Mul,
    Div,
    Add,
    Sub,
}

impl AssignType {
    fn new(s: &str) -> Self {
        match s {
            "=" => AssignType::Normal,
            "^=" => AssignType::Pow,
            "%=" => AssignType::Mod,
            "*=" => AssignType::Mul,
            "/=" => AssignType::Div,
            "+=" => AssignType::Add,
            "-=" => AssignType::Sub,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for AssignType {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignType::Normal => write!(formatter, r#"""#),
            AssignType::Pow => write!(formatter, r#"^""#),
            AssignType::Mod => write!(formatter, r#"%""#),
            AssignType::Mul => write!(formatter, r#"*""#),
            AssignType::Div => write!(formatter, r#"/""#),
            AssignType::Add => write!(formatter, r#"+""#),
            AssignType::Sub => write!(formatter, r#"-""#),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Grouping(e) => write!(formatter, "❨ {} ❩", e),
            Expr::UnaryPlus(e) => write!(formatter, "+( {} )", e),
            Expr::UnaryMinus(e) => write!(formatter, "-( {} )", e),
            Expr::Pow(l, r) => write!(formatter, "( {} ) ^ ( {} )", l, r),
            Expr::Mul(l, r) => write!(formatter, "( {} ) * ( {} )", l, r),
            Expr::Div(l, r) => write!(formatter, "( {} ) / ( {} )", l, r),
            Expr::Mod(l, r) => write!(formatter, "( {} ) % ( {} )", l, r),
            Expr::Add(l, r) => write!(formatter, "( {} ) + ( {} )", l, r),
            Expr::Minus(l, r) => write!(formatter, "( {} ) - ( {} )", l, r),
            Expr::Concat(l, r) => write!(formatter, "( {} )   ( {} )", l, r),
            Expr::Comparison(op, l, r) => write!(formatter, "( {} ) {} ( {} )", l, op, r),
            Expr::Match(l, r) => write!(formatter, "( {} ) ~ ( {} )", l, r),
            Expr::NonMatch(l, r) => write!(formatter, "( {} ) !~ ( {} )", l, r),
            Expr::Array(exprs, name) => write!(formatter, "({}) in {}", exprs, name),
            Expr::LogicalAnd(l, r) => write!(formatter, "( {} ) && ( {} )", l, r),
            Expr::LogicalOr(l, r) => write!(formatter, "( {} ) || ( {} )", l, r),
            Expr::LogicalNot(e) => write!(formatter, "!( {} )", e),
            Expr::Conditional(c, t, f) => write!(formatter, "( {} ) ? ( {} ) : ( {} )", c, t, f),

            Expr::Number(n) => write!(formatter, "{}", n),
            Expr::String(s) => write!(formatter, r#" "{}" "#, s),
            Expr::LValue(lvalue) => write!(formatter, "{}", lvalue),
            Expr::Regexp(ere) => write!(formatter, " /{}/ ", ere),
            Expr::PreIncrement(lvalue) => write!(formatter, " ++{} ", lvalue),
            Expr::PreDecrement(lvalue) => write!(formatter, " --{} ", lvalue),
            Expr::PostIncrement(lvalue) => write!(formatter, " {}++ ", lvalue),
            Expr::PostDecrement(lvalue) => write!(formatter, " {}-- ", lvalue),
            Expr::FunctionCall(fname, args) => write!(formatter, " {}({}) ", fname, args),
            Expr::Assign(op, l, v) => write!(formatter, " {} {} {} ", l, op, v),
        }
    }
}

#[cfg(test)]
pub fn parse_expr_str(input: &str) -> Expr {
    let complete_input = CompleteStr::from(input);
    do_parse_expr(complete_input, false).unwrap().1
}

pub fn parse_expr(input: CompleteStr) -> IResult<CompleteStr, Expr> {
    do_parse_expr(input, false)
}

pub fn parse_print_expr(input: CompleteStr) -> IResult<CompleteStr, Expr> {
    do_parse_expr(input, true)
}

#[rustfmt::skip]
fn do_parse_expr(input: CompleteStr, print_expr: bool) -> IResult<CompleteStr, Expr> {
    do_parse!(
        input,
        e0: leaf >>
        e1: call!(parse_pow_expr, e0) >>
        e2: call!(parse_mul_div_mod_expr, e1) >>
        e3: call!(parse_add_sub_expr, e2) >>
        e4: call!(parse_concat_expr, e3) >>
        e5: call!(parse_comparison_expr, print_expr, e4) >>
        e6: call!(parse_match_expr, print_expr, e5) >>
        e7: call!(parse_array_expr, e6) >>
        e8: call!(parse_and_expr, print_expr, e7) >>
        e9: call!(parse_or_expr, print_expr, e8) >>
        e10: call!(parse_conditional_expr, print_expr, e9) >>
        (e10)
    )
}

#[rustfmt::skip]
fn parse_pow_expr(input: CompleteStr, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(input, char!('^')) {
        Ok((input, _)) => do_parse!(
            input,
            right_expr: leaf >>
            next_expr: call!(parse_pow_expr, right_expr) >>
            (match left_expr {
                // pow has precedence over unary operator like -, +, and !
                Expr::UnaryMinus(minus) => {
                    Expr::UnaryMinus(Box::new(Expr::Pow(minus, Box::new(next_expr))))
                }
                Expr::UnaryPlus(plus) => {
                    Expr::UnaryPlus(Box::new(Expr::Pow(plus, Box::new(next_expr))))
                }
                Expr::LogicalNot(plus) => {
                    Expr::LogicalNot(Box::new(Expr::Pow(plus, Box::new(next_expr))))
                }
                _ => Expr::Pow(Box::new(left_expr), Box::new(next_expr)),
            })
        ),
        _ => Ok((input, left_expr)),
    }
}

#[rustfmt::skip]
fn parse_mul_div_mod_expr(input: CompleteStr, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(input, one_of!("*/%")) {
        Ok((input, op)) => do_parse!(
            input,
            right_expr: leaf >>
            lower_preceding_expr: call!(parse_pow_expr, right_expr) >>
            expr: value!(match op {
                '*' => Expr::Mul(Box::new(left_expr), Box::new(lower_preceding_expr)),
                '/' => Expr::Div(Box::new(left_expr), Box::new(lower_preceding_expr)),
                '%' => Expr::Mod(Box::new(left_expr), Box::new(lower_preceding_expr)),
                _ => unreachable!(),
            }) >>
            next_expr: call!(parse_mul_div_mod_expr, expr) >>
            (next_expr)
        ),
        _ => Ok((input, left_expr)),
    }
}

#[rustfmt::skip]
fn parse_add_sub_expr(input: CompleteStr, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(input, one_of!("+-")) {
        Ok((input, op)) => do_parse!(
            input,
            lower_preceding_expr:
                do_parse!(
                    e0: leaf >>
                    e1: call!(parse_pow_expr, e0) >>
                    e2: call!(parse_mul_div_mod_expr, e1) >>
                    (e2)
                ) >>
                expr: value!(match op {
                    '+' => Expr::Add(Box::new(left_expr), Box::new(lower_preceding_expr)),
                    '-' => Expr::Minus(Box::new(left_expr), Box::new(lower_preceding_expr)),
                    _ => unreachable!(),
                }) >>
                next_expr: call!(parse_add_sub_expr, expr) >>
                (next_expr)
        ),
        _ => Ok((input, left_expr)),
    }
}

#[rustfmt::skip]
fn parse_concat_expr(input: CompleteStr, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(input, leaf) {
        Ok((input, right_expr)) => do_parse!(
            input,
            lower_preceding_expr:
            do_parse!(
                e1: call!(parse_pow_expr, right_expr) >>
                e2: call!(parse_mul_div_mod_expr, e1) >>
                e3: call!(parse_add_sub_expr, e2) >>
                (e3)
            ) >>
            concat:
            value!(Expr::Concat(
                    Box::new(left_expr),
                    Box::new(lower_preceding_expr)
            )) >>
            next_expr: call!(parse_concat_expr, concat)
            >>
            (next_expr)
        ),
        _ => Ok((input, left_expr)),
    }
}

#[rustfmt::skip]
fn parse_comparison_expr(input: CompleteStr, print_expr: bool, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(
        input,
        alt!(
            tag!("<=")
            | tag!("<")
            | tag!("!=")
            | tag!("==")
            | tag!(">=")
            | cond_reduce!(!print_expr, tag!(">"))
        )
    ) {
        Ok((input, op)) => do_parse!(
            input,
            lower_preceding_expr:
            do_parse!(
                e0: leaf >>
                e1: call!(parse_pow_expr, e0) >>
                e2: call!(parse_mul_div_mod_expr, e1) >>
                e3: call!(parse_add_sub_expr, e2) >>
                e4: call!(parse_concat_expr, e3) >>
                (e4)
            ) >>
            (Expr::Comparison(CmpOperator::from(*op), Box::new(left_expr), Box::new(lower_preceding_expr)))
        ),
        _ => Ok((input, left_expr)),
    }
}

#[rustfmt::skip]
fn parse_match_expr(input: CompleteStr, print_expr: bool, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(input, alt!(tag!("!~") | tag!("~"))) {
        Ok((input, op)) => do_parse!(
            input,
            lower_preceding_expr:
            do_parse!(
                e0: leaf >>
                e1: call!(parse_pow_expr, e0) >>
                e2: call!(parse_mul_div_mod_expr, e1) >>
                e3: call!(parse_add_sub_expr, e2) >>
                e4: call!(parse_concat_expr, e3) >>
                e5: call!(parse_comparison_expr, print_expr, e4) >>
                (e5)
            ) >>
            expr: value!(match op.to_string().as_str() {
                "~" => Expr::Match(Box::new(left_expr), Box::new(lower_preceding_expr)),
                "!~" => Expr::NonMatch(Box::new(left_expr), Box::new(lower_preceding_expr)),
                _ => unreachable!(),
            }) >>
            next_expr: call!(parse_match_expr, print_expr, expr) >>
            (next_expr)
        ),
        _ => Ok((input, left_expr)),
    }
}

#[rustfmt::skip]
fn parse_array_expr(input: CompleteStr, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(input, tag!("in")) {
        Ok((input, _)) => do_parse!(
            input,
            name: parse_name >>
            (Expr::Array(ExprList(vec![left_expr]), name))
        ),
        _ => Ok((input, left_expr)),
    }
}

#[rustfmt::skip]
fn parse_and_expr(input: CompleteStr, print_expr: bool, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(input, tag!("&&")) {
        Ok((input, _)) => do_parse!(
            input,
            lower_preceding_expr:
            do_parse!(
                e0: leaf >>
                e1: call!(parse_pow_expr, e0) >>
                e2: call!(parse_mul_div_mod_expr, e1) >>
                e3: call!(parse_add_sub_expr, e2) >>
                e4: call!(parse_concat_expr, e3) >>
                e5: call!(parse_comparison_expr, print_expr, e4) >>
                e6: call!(parse_match_expr, print_expr, e5) >>
                e7: call!(parse_array_expr, e6) >>
                (e7)
            ) >>
            expr: value!(Expr::LogicalAnd(
                    Box::new(left_expr),
                    Box::new(lower_preceding_expr)
            )) >>
            next_expr: call!(parse_and_expr, print_expr, expr) >>
            (next_expr)
        ),
        _ => Ok((input, left_expr)),
    }
}

#[rustfmt::skip]
fn parse_or_expr(input: CompleteStr, print_expr: bool, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(input, tag!("||")) {
        Ok((input, _)) => do_parse!(
            input,
            lower_preceding_expr:
            do_parse!(
                e0: leaf >>
                e1: call!(parse_pow_expr, e0) >>
                e2: call!(parse_mul_div_mod_expr, e1) >>
                e3: call!(parse_add_sub_expr, e2) >>
                e4: call!(parse_concat_expr, e3) >>
                e5: call!(parse_comparison_expr, print_expr, e4) >>
                e6: call!(parse_match_expr, print_expr, e5) >>
                e7: call!(parse_array_expr, e6) >>
                e8: call!(parse_and_expr, print_expr, e7) >>
                (e8)
            ) >>
            expr: value!(Expr::LogicalOr(
                    Box::new(left_expr),
                    Box::new(lower_preceding_expr)
            )) >>
            next_expr: call!(parse_or_expr, print_expr, expr) >>
            (next_expr)
            ),
        _ => Ok((input, left_expr)),
    }
}

#[rustfmt::skip]
fn parse_conditional_expr(input: CompleteStr, print_expr: bool, left_expr: Expr) -> IResult<CompleteStr, Expr> {
    match ws!(input, char!('?')) {
        Ok((input, _)) => do_parse!(
            input,
            ok: do_parse!(
                e0: leaf >>
                e1: call!(parse_pow_expr, e0) >>
                e2: call!(parse_mul_div_mod_expr, e1) >>
                e3: call!(parse_add_sub_expr, e2) >>
                e4: call!(parse_concat_expr, e3) >>
                e5: call!(parse_comparison_expr, print_expr, e4) >>
                e6: call!(parse_match_expr, print_expr, e5) >>
                e7: call!(parse_array_expr, e6) >>
                e8: call!(parse_and_expr, print_expr, e7) >>
                e9: call!(parse_or_expr, print_expr, e8) >>
                (e9)
            ) >>
            ws!(char!(':')) >>
            ko: do_parse!(
                e0: leaf >>
                e1: call!(parse_pow_expr, e0) >>
                e2: call!(parse_mul_div_mod_expr, e1) >>
                e3: call!(parse_add_sub_expr, e2) >>
                e4: call!(parse_concat_expr, e3) >>
                e5: call!(parse_comparison_expr, print_expr, e4) >>
                e6: call!(parse_match_expr, print_expr, e5) >>
                e7: call!(parse_array_expr, e6) >>
                e8: call!(parse_and_expr, print_expr, e7) >>
                e9: call!(parse_or_expr, print_expr, e8) >>
                e10: call!(parse_conditional_expr, print_expr, e9) >>
                (e10)
            ) >>
            (Expr::Conditional(Box::new(left_expr), Box::new(ok), Box::new(ko)))
            ),
        _ => Ok((input, left_expr)),
    }
}

pub fn parse_expr_list(input: CompleteStr) -> IResult<CompleteStr, ExprList> {
    map!(
        input,
        separated_list!(ws!(char!(',')), parse_expr),
        |exprs| ExprList(exprs)
    )
}

pub fn parse_expr_list1(input: CompleteStr) -> IResult<CompleteStr, ExprList> {
    map!(
        input,
        separated_nonempty_list!(ws!(char!(',')), parse_expr),
        |exprs| ExprList(exprs)
    )
}

pub fn parse_print_expr_list1(input: CompleteStr) -> IResult<CompleteStr, ExprList> {
    map!(
        input,
        separated_nonempty_list!(ws!(char!(',')), parse_print_expr),
        |exprs| ExprList(exprs)
    )
}

#[rustfmt::skip]
fn leaf(input: CompleteStr) -> IResult<CompleteStr, Expr> {
    alt!(
        input,
        delimited!(
            ws!(char!('(')),
            parse_expr,
            ws!(char!(')'))
        ) => { |expr| Expr::Grouping(Box::new(expr)) }
        | preceded!(tag!("++"), parse_lvalue) => { |lvalue| Expr::PreIncrement(lvalue) }
        | preceded!(tag!("--"), parse_lvalue) => { |lvalue| Expr::PreDecrement(lvalue) }
        | double => {
            |num: f64| if num.signum() == 1.0 {
                Expr::Number(num)
            } else {
                Expr::UnaryMinus(Box::new(Expr::Number(-num)))
            }
        }
        | preceded!(ws!(char!('+')), leaf) => { |expr| Expr::UnaryPlus(Box::new(expr)) }
        | preceded!(ws!(char!('-')), leaf) => { |expr| Expr::UnaryMinus(Box::new(expr)) }
        | preceded!(ws!(char!('!')), leaf) => { |expr| Expr::LogicalNot(Box::new(expr)) }
        | map!(parse_string, |s| Expr::String(s.to_string()))
        | parse_regexp => { |ere: String| Expr::Regexp(ere.to_owned()) }
        | do_parse!(
            func_name: parse_func_name >>
            args: delimited!(
                char!('('),
                parse_expr_list,
                char!(')')
            ) >>
            (Expr::FunctionCall(func_name, args))
        )
        | parse_assignment
        | terminated!(parse_lvalue, tag!("++")) => { |lvalue| Expr::PostIncrement(lvalue) }
        | terminated!(parse_lvalue, tag!("--")) => { |lvalue| Expr::PostDecrement(lvalue) }
        | parse_lvalue => { |lvalue| Expr::LValue(lvalue) }
    )
}

#[rustfmt::skip]
fn parse_assignment(input: CompleteStr) -> IResult<CompleteStr, Expr> {
    do_parse!(
        input,
        lvalue: parse_lvalue >>
        op: ws!(alt!(tag!("^=") | tag!("%=") | tag!("*=") | tag!("/=") | tag!("+=") | tag!("-=") | tag!("="))) >>
        rvalue: parse_expr >>
        (Expr::Assign(AssignType::new(&op), lvalue, Box::new(rvalue)))
    )
}

#[rustfmt::skip]
fn parse_lvalue(input: CompleteStr) -> IResult<CompleteStr, LValueType> {
    alt!(
        input,
        do_parse!(
            name: parse_name >>
            exprs: delimited!(
                ws!(char!('[')),
                parse_expr_list1,
                ws!(char!(']'))
            ) >>
            (LValueType::Brackets(name, exprs))
        )
        | preceded!(preceded!(multispace0, char!('$')), leaf) => {
            |expr| LValueType::Dollar(Box::new(expr))
        }
        | parse_name => { |name: String| LValueType::Name(name) }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_expr(input: &str, expected: Expr) {
        let expr = parse_expr(CompleteStr::from(input));
        assert!(expr.is_ok(), "input: {}\n{:?}", input, expr);
        let expr = expr.unwrap();
        assert!(expr.0.is_empty(), "input: {}\n{:?}", input, expr);
        assert_eq!(
            expr.1, expected,
            "\nexpected:\n{}\n\nactual:\n{}",
            expected, expr.1
        );
    }

    fn assert_expr_with_leftovers(input: &str, expected: Expr, leftover: &str) {
        let expr = parse_expr(CompleteStr::from(input));
        assert!(expr.is_ok(), "{:?}", expr);
        let expr = expr.unwrap();
        assert!(!expr.0.is_empty());
        assert_eq!(expr.0.as_ref(), leftover);
        assert_eq!(expr.1, expected, "{}", expr.1);
    }

    #[test]
    fn number() {
        assert_expr("42", Expr::Number(42.0));
    }

    #[test]
    fn unary_plus() {
        assert_expr("+42", Expr::Number(42.0));
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
                Box::new(Expr::Regexp("ok".to_string())),
                Box::new(Expr::Regexp("ko".to_string())),
            ),
        );
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
