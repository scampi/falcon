use crate::{
    errors::EvaluationError,
    interpreter::{record::Record, stmt::StmtResult, value::Value, variables::Variables, Eval},
    parser::ast::{ExprList, Item},
};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug)]
pub struct Functions {
    funcs: HashMap<String, Item>,
}

impl Functions {
    pub fn new() -> Functions {
        Functions {
            funcs: HashMap::new(),
        }
    }

    pub fn load_function(&mut self, function: Item) -> Result<(), EvaluationError> {
        if let Item::FunctionDef(name, ..) = &function {
            match self.funcs.entry(name.to_owned()) {
                Entry::Occupied(_) => {
                    return Err(EvaluationError::DuplicateFunction(name.to_owned()));
                },
                Entry::Vacant(entry) => {
                    entry.insert(function);
                },
            }
        } else {
            unreachable!()
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
