use crate::interpreter::functions::builtins::is_builtin;
use combine::{
    error::{ParseError, StreamError},
    parser::{
        char::{alpha_num, char, letter},
        item::one_of,
        range::{recognize, take_while1},
        repeat::{escaped, many, skip_many, skip_until},
        sequence::between,
        Parser,
    },
    stream::{RangeStream, Stream, StreamErrorFor, StreamOnce},
};

/// Returns a valid variable identifier.
pub fn parse_var_name<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_whitespaces()
        .with(letter().or(char('_')))
        .and(many::<String, _>(alpha_num().or(char('_'))))
        .map(|(start, rest)| format!("{}{}", start, rest))
        .and_then(|name: String| {
            if !is_keyword(&name) && !is_builtin(&name) {
                Ok(name)
            } else {
                Err(StreamErrorFor::<I>::message_message(format!(
                    "{}",
                    crate::errors::ParseError::InvalidName(name)
                )))
            }
        })
}

/// Returns a valid function identifier.
pub fn parse_func_name<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_whitespaces()
        .with(letter().or(char('_')))
        .and(many::<String, _>(alpha_num().or(char('_'))))
        .map(|(start, rest)| format!("{}{}", start, rest))
        .and_then(|name: String| {
            if !is_keyword(&name) {
                Ok(name)
            } else {
                Err(StreamErrorFor::<I>::message_message(format!(
                    "{}",
                    crate::errors::ParseError::InvalidName(name)
                )))
            }
        })
}

/// Returns true if the given string is a reserved keyword.
fn is_keyword(name: &str) -> bool {
    match name {
        "BEGIN" | "break" | "continue" | "delete" | "do" | "else" | "END" | "exit" | "for"
        | "function" | "getline" | "if" | "in" | "next" | "print" | "printf" | "return"
        | "while" => true,
        _ => false,
    }
}

/// Returns a quoted string, without the wrapping quotes.
pub fn parse_string<'a, I: 'a>() -> impl Parser<Input = I, Output = String> + 'a
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        skip_whitespaces().with(char('"')),
        char('"').skip(skip_whitespaces()),
        recognize(escaped(
            take_while1(|c| c != '"' && c != '\\'),
            '\\',
            one_of(r#"\"abfnrtv/c012345678"#.chars()),
        )),
    )
    .map(|s: &str| s.to_owned())
}

/// Returns a regular expression as a string.
pub fn parse_regexp<'a, I: 'a>() -> impl Parser<Input = I, Output = String> + 'a
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        skip_whitespaces().with(char('/')),
        char('/').skip(skip_whitespaces()),
        recognize(escaped(
            take_while1(|c| c != '/' && c != '\\'),
            '\\',
            one_of(r#"\"abfnrtv/c012345678"#.chars()),
        )),
    )
    .map(|s: &str| s.to_owned())
}

/// Skips tabulation, whitespace, and newline characters appearing before and
/// after the given parser.
pub fn skip_all_wrapping_spaces<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    skip_many(one_of("\r\n\t ".chars())).with(p.skip(skip_many(one_of("\r\n\t ".chars()))))
}

/// Skips tabulation and whitespace characters appearing before and after the
/// given parser.
pub fn skip_wrapping_spaces<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    skip_whitespaces().with(p.skip(skip_whitespaces()))
}

/// Skips tabulation and whitespace characters.
pub fn skip_whitespaces<I>() -> impl Parser<Input = I>
where
    I: Stream<Item = char>,
    <I as StreamOnce>::Error:
        ParseError<<I as StreamOnce>::Item, <I as StreamOnce>::Range, <I as StreamOnce>::Position>,
{
    skip_many(one_of("\t ".chars()))
}

/// Skip newline characters that appear before and after the given parser.
pub fn skip_wrapping_newlines<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    skip_newlines().with(p.skip(skip_newlines()))
}

/// Skip newline characters.
pub fn skip_newlines<I>() -> impl Parser<Input = I>
where
    I: Stream<Item = char>,
    <I as StreamOnce>::Error:
        ParseError<<I as StreamOnce>::Item, <I as StreamOnce>::Range, <I as StreamOnce>::Position>,
{
    skip_many(one_of("\r\n".chars()))
}

/// Skip comments appearing in the script.
/// A comment starts with a pound sign and all input till the end of the newline
/// is skipped.
pub fn skip_comments<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    skip_many(char('#').and(skip_until(char('\n'))).and(char('\n'))).with(p)
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::{
        parser::{char::digit, repeat::many},
        stream::state::State,
    };

    fn is_valid_name(name: &str) {
        let res = parse_var_name().easy_parse(name);
        assert!(res.is_ok(), "{:?}", res);
        let res = res.unwrap();
        assert_eq!(res.0, String::from(name), "{:?}", res);
    }

    fn is_not_valid_name(name: &str) {
        let res = parse_var_name().easy_parse(name);
        assert!(res.is_err(), "{:?}", res);
    }

    #[test]
    fn valid_name() {
        is_valid_name("gargoyles");
        is_valid_name("var_1");
        is_valid_name("gargoyles1");
        is_valid_name("_gargoyles");

        // parse_var_name should take all valid characters, and let the rest be handled
        // by another parser
        let res = parse_var_name().easy_parse("john connor");
        assert!(res.is_ok(), "{:?}", res);
        let res = res.unwrap();
        assert_eq!(res.0, String::from("john"), "{:?}", res);
        assert_eq!(res.1, String::from(" connor"), "{:?}", res);

        let res = parse_var_name().easy_parse("john++");
        assert!(res.is_ok(), "{:?}", res);
        let res = res.unwrap();
        assert_eq!(res.0, String::from("john"), "{:?}", res);
        assert_eq!(res.1, String::from("++"), "{:?}", res);
    }

    #[test]
    fn space() {
        let res = skip_wrapping_spaces(digit()).easy_parse("4");
        assert_eq!(res.unwrap().0, '4');
        let res = skip_wrapping_spaces(digit()).easy_parse("   5");
        assert_eq!(res.unwrap().0, '5');
        let res = skip_wrapping_spaces(digit()).easy_parse("6   ");
        assert_eq!(res.unwrap().0, '6');
        let res = skip_wrapping_spaces(digit()).easy_parse("    7   ");
        assert_eq!(res.unwrap().0, '7');
    }

    #[test]
    fn not_valid_name() {
        is_not_valid_name("0gargoyles");
        is_not_valid_name("continue");
        is_not_valid_name("rand");
        is_not_valid_name("#gargoyles1");
    }

    fn is_valid_string(s: &str) {
        let quoted = format!(r#""{}""#, s);
        let res = parse_string().easy_parse(State::new(quoted.as_str()));
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let res = res.unwrap();
        assert_eq!(res.0, String::from(s), "{:?}", res);
    }

    #[test]
    fn string() {
        is_valid_string("gargoyles");
        is_valid_string("john connor");
        is_valid_string(r#"aaa\/bbb"#);
        is_valid_string(r#"g\\ \a\b\f\n\r\t\v"#);
        is_valid_string(r#"aaa\"bbb"#);
        is_valid_string(r#"octal: \123 \1 \18"#);
        is_valid_string(r#"custom: \cy"#);
    }

    fn is_valid_regexp(s: &str) {
        let wrapped = format!("/{}/", s);
        let res = parse_regexp().easy_parse(State::new(wrapped.as_str()));
        assert!(res.is_ok(), "{}", res.unwrap_err());
        let res = res.unwrap();
        assert_eq!(res.0, String::from(s), "{:?}", res);
    }

    #[test]
    fn regexp() {
        is_valid_regexp("gargoyles");
        is_valid_regexp("john connor");
        is_valid_regexp(r#"aaa\/bbb"#);
    }

    #[test]
    fn comment() {
        let data = [("#abc\n#def\ntoto", "toto"), ("#def\nabc", "abc")];
        for (i, (input, expected)) in data.iter().enumerate() {
            let res = skip_comments(skip_wrapping_spaces(many::<String, _>(letter())))
                .easy_parse(State::new(*input));
            assert!(res.is_ok(), "failed on data[{}]: {}", i, res.unwrap_err());
            let res = res.unwrap();
            assert_eq!(
                res.0,
                String::from(*expected),
                "failed on data[{}]: {:?}",
                i,
                res
            );
        }
    }
}
