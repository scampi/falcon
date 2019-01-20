use crate::{
    errors::EvaluationError,
    interpreter::{record::Record, stmt::StmtResult, value::Value, variables::Variables, Eval},
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
        &self,
        name: &str,
        args: &ExprList,
        vars: &mut Variables,
        record: &mut Record,
    ) -> Result<Value, EvaluationError> {
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
                vars.push_local_stack(params.as_slice(), args, record, self)?;
                // execute the function
                let mut ret = Value::Uninitialised;
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
                // pop the call stack
                vars.pop_local_stack();
                Ok(ret)
            },
            _ => Err(EvaluationError::UnknownFunction(name.to_owned())),
        }
    }
}
