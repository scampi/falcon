use combine::{
    error::{ParseError, StreamError},
    parser::{
        char::{alpha_num, char, letter, spaces},
        item::one_of,
        range::{recognize, take_while1},
        repeat::{escaped, many},
        sequence::between,
        Parser,
    },
    stream::{RangeStream, Stream, StreamErrorFor, StreamOnce},
};

pub fn parse_name<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces()
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

pub fn parse_func_name<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces()
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

fn is_builtin(name: &str) -> bool {
    match name {
        "atan2" | "close" | "cos" | "exp" | "gsub" | "index" | "int" | "length" | "log"
        | "match" | "rand" | "sin" | "split" | "sprintf" | "sqrt" | "srand" | "sub" | "substr"
        | "system" | "tolower" | "toupper" => true,
        _ => false,
    }
}

fn is_keyword(name: &str) -> bool {
    match name {
        "BEGIN" | "break" | "continue" | "delete" | "do" | "else" | "END" | "exit" | "for"
        | "function" | "getline" | "if" | "in" | "next" | "print" | "printf" | "return"
        | "while" => true,
        _ => false,
    }
}

pub fn parse_string<'a, I: 'a>() -> impl Parser<Input = I, Output = String> + 'a
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        spaces().with(char('"')),
        char('"').skip(spaces()),
        recognize(escaped(
            take_while1(|c| c != '"' && c != '\\'),
            '\\',
            one_of(r#"\"abfnrtv/c012345678"#.chars()),
        )),
    )
    .map(|s: &str| s.to_owned())
}

pub fn parse_regexp<'a, I: 'a>() -> impl Parser<Input = I, Output = String> + 'a
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        spaces().with(char('/')),
        char('/').skip(spaces()),
        recognize(escaped(
            take_while1(|c| c != '/' && c != '\\'),
            '\\',
            one_of(r#"\"abfnrtv/c012345678"#.chars()),
        )),
    )
    .map(|s: &str| s.to_owned())
}

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
    spaces().with(p.skip(spaces()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::{parser::char::digit, stream::state::State};

    fn is_valid_name(name: &str) {
        let res = parse_name().easy_parse(name);
        assert!(res.is_ok(), "{:?}", res);
        let res = res.unwrap();
        assert_eq!(res.0, String::from(name), "{:?}", res);
    }

    fn is_not_valid_name(name: &str) {
        let res = parse_name().easy_parse(name);
        assert!(res.is_err(), "{:?}", res);
    }

    #[test]
    fn valid_name() {
        is_valid_name("gargoyles");
        is_valid_name("var_1");
        is_valid_name("gargoyles1");
        is_valid_name("_gargoyles");

        // parse_name should take all valid characters, and let the rest be handled by
        // another parser
        let res = parse_name().easy_parse("john connor");
        assert!(res.is_ok(), "{:?}", res);
        let res = res.unwrap();
        assert_eq!(res.0, String::from("john"), "{:?}", res);
        assert_eq!(res.1, String::from(" connor"), "{:?}", res);

        let res = parse_name().easy_parse("john++");
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
}
