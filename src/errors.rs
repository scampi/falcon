//! Describe failures that can happen while parsing or executing a script.
use failure_derive::Fail;

/// An error that happened during the evaluation of a script.
#[derive(Debug, Fail)]
pub enum EvaluationError {
    /// A function which argument should be an array.
    #[fail(display = "Function '{}' expects an array", _0)]
    NotAnArray(String),
    /// A function that may be a builtin was called with an unexpected number of
    /// arguments.
    #[fail(display = "Function '{}' expects {} arguments but got {}", _0, _1, _2)]
    InvalidNumberOfArguments(String, usize, usize),
    /// Similar to `[#InvalidNumberOfArguments]` but the number of expected
    /// arguments falls within a range.
    #[fail(
        display = "Function '{}' expects {} or {} arguments but got {}",
        _0, _1, _2, _3
    )]
    InvalidNumberOfArgumentsRange(String, usize, usize, usize),
    /// Similar to `[#InvalidNumberOfArguments]` but there is a minimum number
    /// of expected arguments.
    #[fail(
        display = "Function '{}' expects at least {} arguments but got {}",
        _0, _1, _2
    )]
    InvalidNumberOfArgumentsLowerBound(String, usize, usize),
    /// A format string specifies a number of conversions but there is not
    /// enough arguments to pair them with.
    #[fail(display = "{}: not enough arguments to satisfy format string", _0)]
    MissingFormatStringArgs(String),
    /// A division by zero was attempted.
    #[fail(display = "division by zero attempted")]
    DivisionByZero,
    /// An attempt to access a field at a negative index was made.
    #[fail(display = "attempt to access field at a negative index {}", _0)]
    NegativeFieldIndex(isize),
    /// A regular expression failed to parse.
    #[fail(display = "Invalid regex: {}", _0)]
    InvalidRegex(regex::Error),
    /// A function is defined more than once.
    #[fail(display = "Duplicate function: {}", _0)]
    DuplicateFunction(String),
    /// A unknown function got called.
    #[fail(display = "Unknown function: {}", _0)]
    UnknownFunction(String),
    /// An array variable was used in a scalar context.
    ///
    /// # Example
    ///
    /// { a[0] = 123; b = a * 2 }
    #[fail(display = "Attempt to use an array in a scalar context")]
    UseArrayInScalarContext,
    /// A scalar variable was used as an array.
    ///
    /// # Example
    ///
    /// { a = 123; a[0] = 456 }
    #[fail(display = "Attempt to use a scalar as an array")]
    UseScalarAsArray,
    /// An I/O error occurred.
    #[fail(display = "{}: {}", _0, _1)]
    IoError(String, std::io::Error),
    /// An UTF-8 conversion error occurred.
    #[fail(display = "{}", _0)]
    Utf8Error(std::str::Utf8Error),
}

/// An error occurred while parsing the script.
#[derive(Debug, Fail)]
pub enum ParseError {
    /// A user-defined function cannot have the name of a builtin.
    #[fail(
        display = "Function {} is a built-in function, it cannot be redefined",
        _0
    )]
    Builtin(String),
    /// A user-defined function specified two or more parameters with the same
    /// name.
    #[fail(display = "Function {} has duplicate parameters", _0)]
    DuplicateParams(String),
    /// A function's or a variable's name is invalid.
    #[fail(display = "Invalid name: {}", _0)]
    InvalidName(String),
    /// A function's parameter cannot be the name of a special parameter, e.g.,
    /// NF.
    #[fail(display = "Cannot use a special variable as a function parameter")]
    SpecialVariableAsParameter,
    /// The script failed to be parsed till the end.
    #[fail(display = "Failed to parse program")]
    LeftOver,
    /// An error originating from the Combine library.
    #[fail(display = "{}", _0)]
    CombineError(String),
}

impl From<std::io::Error> for EvaluationError {
    fn from(e: std::io::Error) -> EvaluationError {
        EvaluationError::IoError(String::from("IO error"), e)
    }
}

impl From<std::str::Utf8Error> for EvaluationError {
    fn from(e: std::str::Utf8Error) -> EvaluationError {
        EvaluationError::Utf8Error(e)
    }
}

impl From<combine::stream::easy::Errors<char, &str, combine::stream::state::SourcePosition>>
    for ParseError
{
    fn from(
        e: combine::stream::easy::Errors<char, &str, combine::stream::state::SourcePosition>,
    ) -> ParseError {
        ParseError::CombineError(format!("{}", e))
    }
}
