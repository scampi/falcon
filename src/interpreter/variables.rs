use crate::{errors::EvaluationError, interpreter::value::Value, parser::ast::AssignType};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug)]
struct FunctionCall {
    locals: HashMap<String, Value>,
    references: HashMap<String, String>,
}

impl FunctionCall {
    fn get<'a>(&'a self, globals: &'a HashMap<String, Value>, name: &str) -> Option<&'a Value> {
        if let Some(alias) = self.references.get(name) {
            if let Some(value) = globals.get(alias) {
                return Some(value);
            } else {
                unreachable!();
            }
        }
        self.locals.get(name)
    }
}

#[derive(Debug)]
pub struct Variables {
    pub globals: HashMap<String, Value>,
    locals: Vec<FunctionCall>,
    ///// The number of arguments
    //argc: usize,
    ///// The command line arguments
    //argv: Vec<String>,
    //convfmt: String,
    //env: HashMap<String, String>,
    ///// The pathname to the current input file
    //filename: String,
    /// The ordinal number of the current record in the current file
    pub fnr: usize,
    /// Input field separator regular expression
    pub fs: String,
    /// The number of fields in the current record
    pub nf: usize,
    /// The ordinal number of the current record from the start of input
    pub nr: usize,
    //ofmt: String,
    pub ofs: String,
    pub ors: String,
    //rlength: usize,
    //rs: String,
    //rstart: usize,
    pub subsep: String,
}

impl Variables {
    pub fn new() -> Variables {
        Variables {
            globals: HashMap::new(),
            locals: Vec::new(),
            fnr: 0,
            fs: String::from(" "),
            nf: 0,
            nr: 0,
            ofs: String::from(" "),
            ors: String::from("\n"),
            subsep: String::new(),
        }
    }

    #[cfg(test)]
    pub fn has_user_vars(&self) -> bool {
        !self.globals.is_empty()
    }

    fn is_special_variable(&self, name: &str) -> bool {
        match name {
            "FNR" | "FS" | "NF" | "NR" | "OFS" | "ORS" | "SUBSEP" => true,
            _ => false,
        }
    }

    pub fn push_local_stack(
        &mut self,
        locals: HashMap<String, Value>,
        references: HashMap<String, String>,
    ) {
        self.locals.push(FunctionCall { locals, references });
    }

    pub fn pop_local_stack(&mut self) {
        if self.locals.pop().is_none() {
            unreachable!()
        }
    }

    pub fn array_keys(&self, name: &str) -> Result<Vec<String>, EvaluationError> {
        let _keys = |value: &Value| match value {
            Value::Uninitialised => Ok(Vec::new()),
            Value::Array(array) => Ok(array.keys().map(|key| key.to_owned()).collect()),
            _ => return Err(EvaluationError::UseScalarAsArray),
        };
        if let Some(locals) = self.locals.last() {
            if let Some(value) = locals.get(&self.globals, name) {
                return _keys(value);
            }
        }
        if let Some(value) = self.globals.get(name) {
            _keys(value)
        } else {
            Ok(Vec::new())
        }
    }

    pub fn delete(&mut self, name: &str, key: &str) -> Result<(), EvaluationError> {
        let _delete = |vars: &mut HashMap<String, Value>, name: &str| {
            if let Entry::Occupied(mut entry) = vars.entry(name.to_owned()) {
                match entry.get_mut() {
                    Value::Uninitialised => (),
                    Value::Array(array) => {
                        if let Entry::Occupied(entry) = array.entry(key.to_owned()) {
                            entry.remove_entry();
                        }
                    },
                    _ => return Err(EvaluationError::UseScalarAsArray),
                }
            }
            return Ok(());
        };
        if let Some(FunctionCall {
            ref mut locals,
            references,
        }) = self.locals.last_mut()
        {
            if locals.contains_key(name) {
                return _delete(locals, name);
            } else if let Some(reference) = references.get(name) {
                return _delete(&mut self.globals, reference);
            }
        }
        return _delete(&mut self.globals, name);
    }

    pub fn array_key(values: Vec<Value>) -> Result<String, EvaluationError> {
        Ok(values
            .into_iter()
            .map(|v| v.as_string())
            .collect::<Vec<String>>()
            .join(""))
    }

    pub fn get(&self, name: &str, subscript: Option<&str>) -> Result<Value, EvaluationError> {
        match name {
            _ if self.is_special_variable(name) && subscript.is_some() => {
                Err(EvaluationError::UseScalarAsArray)
            },
            "FNR" => Ok(Value::from(self.fnr)),
            "FS" => Ok(Value::from(self.fs.to_owned())),
            "NF" => Ok(Value::from(self.nf)),
            "NR" => Ok(Value::from(self.nr)),
            "OFS" => Ok(Value::from(self.ofs.to_owned())),
            "ORS" => Ok(Value::from(self.ors.to_owned())),
            "SUBSEP" => Ok(Value::from(self.subsep.to_owned())),
            _ => {
                if let Some(value) = self.get_local_var(name, subscript) {
                    value
                } else {
                    Variables::get_var(&self.globals, name, subscript)
                }
            },
        }
    }

    fn get_local_var(
        &self,
        name: &str,
        subscript: Option<&str>,
    ) -> Option<Result<Value, EvaluationError>> {
        if let Some(FunctionCall { locals, references }) = self.locals.last() {
            if locals.contains_key(name) {
                return Some(Variables::get_var(locals, name, subscript));
            } else if let Some(reference) = references.get(name) {
                return Some(Variables::get_var(&self.globals, reference, subscript));
            }
        }
        None
    }

    fn get_var(
        vars: &HashMap<String, Value>,
        name: &str,
        subscript: Option<&str>,
    ) -> Result<Value, EvaluationError> {
        match vars.get(name) {
            Some(value) => match (subscript, value) {
                (Some(..), _) if value.is_scalar() => Err(EvaluationError::UseScalarAsArray),
                (None, Value::Array(_)) => Err(EvaluationError::UseArrayInScalarContext),
                (Some(index), Value::Array(array)) => match array.get(index) {
                    Some(value) => Ok(value.clone()),
                    None => Ok(Value::Uninitialised),
                },
                (None, _) => Ok(value.clone()),
                (_, Value::Uninitialised) => Ok(Value::Uninitialised),
                _ => unimplemented!(),
            },
            None => Ok(Value::Uninitialised),
        }
    }

    pub fn set(
        &mut self,
        ty: &AssignType,
        name: &str,
        subscript: Option<&str>,
        new_value: Value,
    ) -> Result<Value, EvaluationError> {
        match name {
            _ if self.is_special_variable(name) && subscript.is_some() => {
                Err(EvaluationError::UseScalarAsArray)
            },
            "FNR" => {
                let result = Value::compute(ty, Value::from(self.fnr), new_value)?;
                self.fnr = result.as_number() as usize;
                Ok(Value::from(self.fnr))
            },
            "FS" => {
                let result = Value::compute(ty, Value::from(self.fs.to_owned()), new_value)?;
                self.fs = result.as_string();
                Ok(Value::from(self.fs.to_owned()))
            },
            "NF" => {
                let result = Value::compute(ty, Value::from(self.nf), new_value)?;
                self.nf = result.as_number() as usize;
                Ok(Value::from(self.nf))
            },
            "NR" => {
                let result = Value::compute(ty, Value::from(self.nr), new_value)?;
                self.nr = result.as_number() as usize;
                Ok(Value::from(self.nr))
            },
            "OFS" => {
                let result = Value::compute(ty, Value::from(self.ofs.to_owned()), new_value)?;
                self.ofs = result.as_string();
                Ok(Value::from(self.ofs.to_owned()))
            },
            "ORS" => {
                let result = Value::compute(ty, Value::from(self.ors.to_owned()), new_value)?;
                self.ors = result.as_string();
                Ok(Value::from(self.ors.to_owned()))
            },
            "SUBSEP" => {
                let result = Value::compute(ty, Value::from(self.subsep.to_owned()), new_value)?;
                self.subsep = result.as_string();
                Ok(Value::from(self.subsep.to_owned()))
            },
            _ => {
                if let Some(value) = self.set_local_var(ty, name, subscript, &new_value) {
                    value
                } else {
                    Variables::set_var(&mut self.globals, ty, name, subscript, new_value)
                }
            },
        }
    }

    fn set_local_var(
        &mut self,
        ty: &AssignType,
        name: &str,
        subscript: Option<&str>,
        new_value: &Value,
    ) -> Option<Result<Value, EvaluationError>> {
        if let Some(FunctionCall { locals, references }) = self.locals.last_mut() {
            if locals.contains_key(name) {
                return Some(Variables::set_var(
                    locals,
                    ty,
                    name,
                    subscript,
                    new_value.clone(),
                ));
            } else if let Some(reference) = references.get(name) {
                return Some(Variables::set_var(
                    &mut self.globals,
                    ty,
                    reference,
                    subscript,
                    new_value.clone(),
                ));
            }
        }
        None
    }

    fn set_var(
        vars: &mut HashMap<String, Value>,
        ty: &AssignType,
        name: &str,
        subscript: Option<&str>,
        new_value: Value,
    ) -> Result<Value, EvaluationError> {
        match vars.entry(name.to_owned()) {
            Entry::Occupied(mut entry) => match (subscript, entry.get_mut()) {
                (Some(..), ref v) if v.is_scalar() => Err(EvaluationError::UseScalarAsArray),
                (None, Value::Array(..)) => Err(EvaluationError::UseArrayInScalarContext),
                (Some(index), Value::Uninitialised) => {
                    let mut array = HashMap::new();
                    let result = Value::compute(ty, Value::Uninitialised, new_value)?;
                    array.insert(index.to_owned(), result.clone());
                    entry.insert(Value::Array(array));
                    Ok(result)
                },
                (None, Value::Uninitialised) => {
                    let result = Value::from(Value::compute(ty, Value::Uninitialised, new_value)?);
                    entry.insert(result.clone());
                    Ok(result)
                },
                (Some(index), Value::Array(array)) => match array.entry(index.to_owned()) {
                    Entry::Occupied(mut entry) => {
                        let result = Value::compute(ty, entry.get().clone(), new_value)?;
                        entry.insert(result.clone());
                        Ok(result)
                    },
                    Entry::Vacant(entry) => {
                        let result = Value::compute(ty, Value::Uninitialised, new_value)?;
                        entry.insert(result.clone());
                        Ok(result)
                    },
                },
                (None, value) => {
                    let result = Value::from(Value::compute(ty, value.clone(), new_value)?);
                    entry.insert(result.clone());
                    Ok(result)
                },
                _ => unimplemented!(),
            },
            Entry::Vacant(entry) => match subscript {
                Some(index) => {
                    let mut array = HashMap::new();
                    let result = Value::compute(ty, Value::Uninitialised, new_value)?;
                    array.insert(index.to_owned(), result.clone());
                    entry.insert(Value::Array(array));
                    Ok(result)
                },
                None => {
                    let result = Value::from(Value::compute(ty, Value::Uninitialised, new_value)?);
                    entry.insert(result.clone());
                    Ok(result)
                },
            },
        }
    }
}
