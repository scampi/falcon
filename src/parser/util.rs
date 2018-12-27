use nom::{types::CompleteStr, *};

pub fn parse_name(input: CompleteStr) -> IResult<CompleteStr, String> {
    map!(
        input,
        verify!(
            take_while1!(|c: char| (c == '_' || c.is_alphanumeric()) && !c.is_whitespace()),
            |name: CompleteStr| is_valid_start(&name) && !is_builtin(&name) && !is_keyword(&name)
        ),
        |name: CompleteStr| name.to_string()
    )
}

// In opposite to the grammar, the func_name is either an user function or a builtin one
pub fn parse_func_name(input: CompleteStr) -> IResult<CompleteStr, String> {
    map!(
        input,
        verify!(
            take_while1!(|c: char| (c == '_' || c.is_alphanumeric()) && !c.is_whitespace()),
            |name: CompleteStr| is_valid_start(&name) && !is_keyword(&name)
        ),
        |name: CompleteStr| name.to_string()
    )
}

fn is_valid_start(name: &str) -> bool {
    if let Some(c) = name.chars().next() {
        c == '_' || c.is_alphabetic()
    } else {
        false
    }
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

pub fn parse_string(input: CompleteStr) -> IResult<CompleteStr, String> {
    map!(
        input,
        delimited!(
            char!('"'),
            escaped!(
                take_until_either1!("\\\""),
                '\\',
                alt!(
                    tag!("\\")
                        | tag!("\"")
                        | tag!("a")
                        | tag!("b")
                        | tag!("f")
                        | tag!("n")
                        | tag!("r")
                        | tag!("t")
                        | tag!("v")
                        | tag!("/")
                        | take_while_m_n!(1, 3, is_oct_digit)
                        | recognize!(preceded!(char!('c'), anychar))
                )
            ),
            char!('"')
        ),
        |lit| lit.to_string()
    )
}

pub fn parse_regexp(input: CompleteStr) -> IResult<CompleteStr, String> {
    map!(
        input,
        delimited!(
            char!('/'),
            escaped!(
                take_until_either1!("\\/"),
                '\\',
                alt!(
                    tag!("\\")
                        | tag!("\"")
                        | tag!("a")
                        | tag!("b")
                        | tag!("f")
                        | tag!("n")
                        | tag!("r")
                        | tag!("t")
                        | tag!("v")
                        | tag!("/")
                        | take_while_m_n!(1, 3, is_oct_digit)
                        | recognize!(preceded!(char!('c'), anychar))
                )
            ),
            char!('/')
        ),
        |ere| ere.to_string()
    )
}

fn is_oct_digit(c: char) -> bool {
    match c {
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn is_valid_name(name: &str) {
        let res = parse_name(CompleteStr::from(name));
        assert!(res.is_ok(), "{:?}", res);
        let res = res.unwrap();
        assert_eq!(res.1, String::from(name), "{:?}", res);
    }

    fn is_not_valid_name(name: &str) {
        let res = parse_name(CompleteStr::from(name));
        assert!(res.is_err(), "{:?}", res);
    }

    #[test]
    fn valid_name() {
        is_valid_name("gargoyles");
        is_valid_name("gargoyles1");
        is_valid_name("_gargoyles");
    }

    // parse_name should take all valid characters, and let the rest be handled by
    // another parser
    #[test]
    fn space() {
        let res = parse_name(CompleteStr::from("john connor"));
        assert!(res.is_ok(), "{:?}", res);
        let res = res.unwrap();
        assert_eq!(res.1, String::from("john"), "{:?}", res);
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
        let res = parse_string(CompleteStr::from(quoted.as_str()));
        assert!(res.is_ok(), "{:?}", res);
        let res = res.unwrap();
        assert_eq!(res.1, String::from(s), "{:?}", res);
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
        let res = parse_regexp(CompleteStr::from(wrapped.as_str()));
        assert!(res.is_ok(), "{:?}", res);
        let res = res.unwrap();
        assert_eq!(res.1, String::from(s), "{:?}", res);
    }

    #[test]
    fn regexp() {
        is_valid_regexp("gargoyles");
        is_valid_regexp("john connor");
        is_valid_regexp(r#"aaa\/bbb"#);
    }
}
