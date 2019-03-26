use failure_derive::Fail;

#[derive(Debug, Fail)]
pub enum EvaluationError {
    #[fail(display = "division by zero attempted")]
    DivisionByZero,
    #[fail(display = "attempt to access field {}", _0)]
    NegativeFieldIndex(isize),
    #[fail(display = "Invalid regex: {}", _0)]
    InvalidRegex(regex::Error),
    #[fail(display = "Duplicate function: {}", _0)]
    DuplicateFunction(String),
    #[fail(display = "Unknown function: {}", _0)]
    UnknownFunction(String),
    #[fail(
        display = "Function {} called with {} arguments but only {} parameters were declared",
        _0, _1, _2
    )]
    TooManyArguments(String, usize, usize),
    #[fail(display = "Attempt to use an array in a scalar context")]
    UseArrayInScalarContext,
    #[fail(display = "Attempt to use a scalar as an array")]
    UseScalarAsArray,
    #[fail(display = "{}: {}", _0, _1)]
    IoError(String, std::io::Error),
}

#[derive(Debug, Fail)]
pub enum ParseError {
    #[fail(display = "Function {} has duplicate parameters", _0)]
    DuplicateParams(String),
    #[fail(display = "Invalid name: {}", _0)]
    InvalidName(String),
    #[fail(display = "Cannot use a special variable as a function parameter")]
    SpecialVariableAsParameter,
    #[fail(display = "Failed to parse program")]
    LeftOver,
    #[fail(display = "{}", _0)]
    CombineError(String),
}

impl From<std::io::Error> for EvaluationError {
    fn from(e: std::io::Error) -> EvaluationError {
        EvaluationError::IoError(String::from("IO error"), e)
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
