use crate::errors::EvaluationError;

mod arrays;
mod expr;
mod record;
mod stmt;
mod value;
mod variables;

pub struct Context {
    vars: variables::Variables,
    record: record::Record,
    arrays: arrays::Arrays,
}

impl Context {
    fn new() -> Context {
        Context {
            vars: variables::Variables::new(),
            record: record::Record::new(),
            arrays: arrays::Arrays::new(),
        }
    }

    fn set_next_record(&mut self, record: String) {
        self.record.update_record(&mut self.vars, record);
        // update record numbers
        self.vars.fnr += 1;
        self.vars.nr += 1;
    }
}

trait Eval {
    type EvalResult;
    fn eval(&self, cxt: &mut Context) -> Result<Self::EvalResult, EvaluationError>;
}
