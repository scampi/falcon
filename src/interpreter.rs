use crate::{errors::EvaluationError, parser::ast::Program};

mod expr;
mod functions;
mod program;
mod record;
mod stmt;
mod value;
mod variables;

pub struct Runtime<'a> {
    vars: variables::Variables,
    record: record::Record,
    funcs: functions::Functions<'a>,
}

impl<'a> Runtime<'a> {
    fn new() -> Runtime<'a> {
        Runtime {
            vars: variables::Variables::new(),
            record: record::Record::new(),
            funcs: functions::Functions::new(),
        }
    }

    fn load_program(&mut self, prog: &'a Program) -> Result<(), EvaluationError> {
        self.funcs.load_functions(prog)
    }

    fn set_next_record(&mut self, record: String) {
        self.record.update_record(&mut self.vars, record);
        // update record numbers
        self.vars.fnr += 1;
        self.vars.nr += 1;
    }

    #[cfg(test)]
    fn clean(&mut self) {
        self.vars.clean();
        self.record.clean();
        self.funcs.clean();
    }
}

trait Eval {
    type EvalResult;
    fn eval<'a>(
        &self,
        vars: &'a mut variables::Variables,
        record: &mut record::Record,
        funcs: &functions::Functions,
    ) -> Result<Self::EvalResult, EvaluationError>;
}
