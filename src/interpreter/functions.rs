//! The functions modules handles AWK's builtins and user-defined functions.
use crate::{
    errors::EvaluationError,
    interpreter::{
        functions::builtins::{call_builtin, is_builtin},
        stmt::StmtResult,
        value::Value,
        Eval, RuntimeMut,
    },
    parser::ast::{Expr, ExprList, Item, LValueType},
};
use std::{
    collections::{hash_map::Entry, HashMap},
    io::Write,
};

pub mod builtins;

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

    /// Loads the definition of a function.
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

    /// Evaluates a function of the given name with the given arguments.
    pub fn call<Output>(
        &self,
        name: &str,
        args: &ExprList,
        rt: &mut RuntimeMut<'_, Output>,
    ) -> Result<Value, EvaluationError>
    where
        Output: Write,
    {
        if is_builtin(name) {
            return call_builtin(name, args, rt);
        }
        match self.funcs.get(name) {
            Some(Item::FunctionDef(_, params, stmts)) => {
                if args.len() > params.len() {
                    return Err(EvaluationError::InvalidNumberOfArguments(
                        name.to_owned(),
                        args.len(),
                        params.len(),
                    ));
                }
                // setup local variables
                let mut locals = HashMap::new();
                let mut references = HashMap::new();
                let mut params_iter = params.iter();

                // The iterator for args needs to be called first since it may be shorter than
                // params. Zip will short-circuit and not call next on params_iter if args is
                // smaller. This allows to fill the locals map with any remaining params.
                for (arg, param) in args.0.iter().zip(params_iter.by_ref()) {
                    // Arrays are passed by reference and so the param will refer to the
                    // globally defined array instead of creating a new one within the
                    // function's scope.
                    if let Expr::LValue(LValueType::Name(name)) = arg {
                        if rt.vars.is_array(name) {
                            references.insert(param.to_owned(), name.to_owned());
                            continue;
                        }
                    }
                    locals.insert(param.to_owned(), arg.eval(rt)?);
                }
                for param in params_iter {
                    locals.insert(param.to_owned(), Value::Uninitialised);
                }
                rt.vars.push_local_stack(locals, references);
                // execute the function
                let mut ret = Value::Uninitialised;
                for stmt in &stmts.0 {
                    if let Some(res) = stmt.eval(rt)? {
                        match res {
                            StmtResult::Return(v) => {
                                ret = v;
                                break;
                            },
                            _ => unimplemented!("{:?}", res),
                        }
                    }
                }
                // drop the call stack
                rt.vars.drop_local_stack();
                Ok(ret)
            },
            _ => Err(EvaluationError::UnknownFunction(name.to_owned())),
        }
    }
}
