use crate::{
    errors::EvaluationError,
    interpreter::{
        record::Record,
        stmt::StmtResult,
        value::{ExprValue, VariableValue},
        variables::Variables,
        Eval,
    },
    parser::ast::{ExprList, Item, Program},
};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug)]
pub struct Functions<'a> {
    funcs: HashMap<String, &'a Item>,
}

impl<'a> Functions<'a> {
    pub fn new() -> Functions<'a> {
        Functions {
            funcs: HashMap::new(),
        }
    }

    #[cfg(test)]
    pub fn clean(&mut self) {
        self.funcs.clear();
    }

    pub fn load_functions(&mut self, prog: &'a Program) -> Result<(), EvaluationError> {
        for item in &prog.items {
            if let Item::FunctionDef(name, ..) = item {
                match self.funcs.entry(name.to_owned()) {
                    Entry::Occupied(_) => {
                        return Err(EvaluationError::DuplicateFunction(name.to_owned()));
                    },
                    Entry::Vacant(entry) => {
                        entry.insert(item);
                    },
                }
            }
        }
        Ok(())
    }

    pub fn call(
        &mut self,
        name: &str,
        args: &ExprList,
        vars: &mut Variables,
        record: &mut Record,
    ) -> Result<ExprValue, EvaluationError> {
        match self.funcs.get(name) {
            Some(Item::FunctionDef(_, params, stmts)) => {
                if args.len() > params.len() {
                    return Err(EvaluationError::TooManyArguments(
                        name.to_owned(),
                        args.len(),
                        params.len(),
                    ));
                }
                // setup local variables

                // evaluate the arguments with the current stack, then push in
                // a new one for this call.
                let args_iter = args.eval(vars, record, self)?.into_iter();
                vars.push_local_stack();

                let mut params_iter = params.iter();
                // The iterator for args needs to be called first since it may be shorter than
                // params. Zip will short-circuit and not call next on params_iter if args is
                // smaller. This allows to fill the locals map with any remaining params.
                for (arg, param) in args_iter.zip(params_iter.by_ref()) {
                    vars.init_local_var(param.to_owned(), VariableValue::Scalar(arg));
                }
                for param in params_iter {
                    vars.init_local_var(param.to_owned(), VariableValue::Uninitialised);
                }
                // execute the function
                let mut ret = ExprValue::Uninitialised;
                for stmt in &stmts.0 {
                    if let Some(res) = stmt.eval(vars, record, self)? {
                        match res {
                            StmtResult::Return(v) => {
                                ret = v;
                                break;
                            },
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                }
                // cleanup local variables
                vars.pop_local_stack();
                Ok(ret)
            },
            _ => Err(EvaluationError::UnknownFunction(name.to_owned())),
        }
    }
}
