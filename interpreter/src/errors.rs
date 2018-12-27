use failure_derive::Fail;

#[derive(PartialEq, Debug, Fail)]
pub enum EvaluationError {
    #[fail(display = "division by zero attempted")]
    DivisionByZero,
}
