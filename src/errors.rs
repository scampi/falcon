use failure_derive::Fail;

#[derive(PartialEq, Debug, Fail)]
pub enum EvaluationError {
    #[fail(display = "division by zero attempted")]
    DivisionByZero,
    #[fail(display = "attempt to access field {}", _0)]
    NegativeFieldIndex(isize),
}

#[derive(PartialEq, Debug, Fail)]
pub enum ParseError {
    #[fail(display = "Invalid name: {}", _0)]
    InvalidName(String),
}
