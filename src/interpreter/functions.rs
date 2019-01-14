use crate::{
    errors::EvaluationError,
    interpreter::{
        record::Record,
        stmt::StmtResult,
        value::{ExprValue, VariableValue},
        variables::Variables,
        Eval,
    },
    parser::ast::{AssignType, ExprList, Item, Program},
};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug)]
pub struct Functions<'a> {
    funcs: HashMap<String, &'a Item>,
    locals: Vec<HashMap<String, VariableValue>>,
}

impl<'a> Functions<'a> {
    pub fn new() -> Functions<'a> {
        Functions {
            funcs: HashMap::new(),
            locals: Vec::new(),
        }
    }

    #[cfg(test)]
    pub fn clean(&mut self) {
        self.funcs.clear();
        self.locals.clear();
    }

    pub fn is_local_var(&self, name: &str) -> bool {
        if let Some(locals) = self.locals.last() {
            locals.contains_key(name)
        } else {
            false
        }
    }

    pub fn get_local_var(
        &self,
        name: &str,
        subscript: Option<&str>,
    ) -> Result<ExprValue, EvaluationError> {
        if let Some(locals) = self.locals.last() {
            Variables::get_var(locals, name, subscript)
        } else {
            // should have been checked with #is_local_var
            unreachable!()
        }
    }

    pub fn set_local_var(
        &mut self,
        ty: &AssignType,
        name: &str,
        subscript: Option<&str>,
        new_value: ExprValue,
    ) -> Result<ExprValue, EvaluationError> {
        if let Some(locals) = self.locals.last_mut() {
            Variables::set_var(locals, ty, name, subscript, new_value)
        } else {
            // should have been checked with #is_local_var
            unreachable!()
        }
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
                let mut params_iter = params.iter();
                let mut locals = HashMap::new();
                // The iterator for args needs to be called first since it may be shorter than
                // params. Zip will short-circuit and not call next on params_iter if args is
                // smaller. This allows to fill the locals map with any remaining params.
                for (arg, param) in args
                    .eval(vars, record, self)?
                    .into_iter()
                    .zip(params_iter.by_ref())
                {
                    locals.insert(param.to_owned(), VariableValue::Scalar(arg));
                }
                for param in params_iter {
                    locals.insert(param.to_owned(), VariableValue::Uninitialised);
                }
                self.locals.push(locals);
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
                if self.locals.pop().is_none() {
                    unreachable!()
                }
                Ok(ret)
            },
            _ => Err(EvaluationError::UnknownFunction(name.to_owned())),
        }
    }
}
