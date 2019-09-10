//! This module manages variables appearing in the script. A variable is by
//! default global, but can be local if declared within a function. A variable
//! is either a scalar or an array and this module provides logic for
//! getting/setting from/to a variable. AWK scripts provide special variables
//! which are managed here as well.
use crate::{errors::EvaluationError, interpreter::value::Value, parser::ast::AssignType};
use std::collections::{hash_map::Entry, HashMap};

/// A FunctionCall is created when an user-defined function is evaluated. It
/// captures array variables that are passed as references, and copies the
/// values of other scalar arguments.
#[derive(Debug)]
struct FunctionCall {
    /// Scalar arguments passed to a function.
    locals: HashMap<String, Value>,
    /// Array arguments are passed by reference.
    /// The key is the variable name as declared in the function's signature,
    /// with the key the name of the array it references.
    references: HashMap<String, String>,
}

impl FunctionCall {
    /// Returns the value associated with the variable of the given name by
    /// looking into the call stack.
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
    function_calls: Vec<FunctionCall>,
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
    pub ofmt: String,
    pub ofs: String,
    pub ors: String,
    //rlength: usize,
    //rs: String,
    //rstart: usize,
    pub subsep: String,
}

pub fn is_special_variable(name: &str) -> bool {
    match name {
        "FNR" | "FS" | "NF" | "NR" | "OFMT" | "OFS" | "ORS" | "SUBSEP" => true,
        _ => false,
    }
}

impl Variables {
    pub fn new() -> Variables {
        Variables {
            globals: HashMap::new(),
            function_calls: Vec::new(),
            fnr: 0,
            fs: String::from(" "),
            nf: 0,
            nr: 0,
            ofmt: String::from("%.6g"),
            ofs: String::from(" "),
            ors: String::from("\n"),
            subsep: String::new(),
        }
    }

    #[cfg(test)]
    pub fn has_user_vars(&self) -> bool {
        !self.globals.is_empty()
    }

    pub fn push_local_stack(
        &mut self,
        locals: HashMap<String, Value>,
        references: HashMap<String, String>,
    ) {
        self.function_calls
            .push(FunctionCall { locals, references });
    }

    pub fn pop_local_stack(&mut self) {
        if self.function_calls.pop().is_none() {
            unreachable!()
        }
    }

    pub fn array_keys(&self, name: &str) -> Result<Vec<String>, EvaluationError> {
        let _keys = |value: &Value| match value {
            Value::Uninitialised => Ok(Vec::new()),
            Value::Array(array) => Ok(array.keys().map(|key| key.to_owned()).collect()),
            _ => return Err(EvaluationError::UseScalarAsArray),
        };
        if let Some(function_call) = self.function_calls.last() {
            if let Some(value) = function_call.get(&self.globals, name) {
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
        // To find the set of variables to delete the entry at key for the named array,
        // start from the top of calls:
        // - check locals of the call for a variable with given name, if found then done
        // - check the references
        //     - if missing, then done
        //     - if found, then check the parent call and iterate to the next call with
        //       the referenced name
        let mut vars = &mut self.globals;
        let mut referred_var = name;
        for FunctionCall { locals, references } in self.function_calls.iter_mut().rev() {
            if locals.contains_key(referred_var) {
                vars = locals;
                break;
            } else {
                match references.get(referred_var) {
                    Some(reference) => referred_var = reference,
                    None => break,
                }
            }
        }
        if let Entry::Occupied(mut entry) = vars.entry(referred_var.to_owned()) {
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
        Ok(())
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
            _ if is_special_variable(name) && subscript.is_some() => {
                Err(EvaluationError::UseScalarAsArray)
            },
            "FNR" => Ok(Value::from(self.fnr)),
            "FS" => Ok(Value::from(self.fs.to_owned())),
            "NF" => Ok(Value::from(self.nf)),
            "NR" => Ok(Value::from(self.nr)),
            "OFMT" => Ok(Value::from(self.ofmt.to_owned())),
            "OFS" => Ok(Value::from(self.ofs.to_owned())),
            "ORS" => Ok(Value::from(self.ors.to_owned())),
            "SUBSEP" => Ok(Value::from(self.subsep.to_owned())),
            _ => self.get_var(name, subscript),
        }
    }

    pub fn is_array(&self, name: &str) -> bool {
        if let Some(FunctionCall { locals, references }) = self.function_calls.last() {
            let is_array_reference = references.contains_key(name);
            let is_local_array = locals.get(name).map_or(false, |value| {
                if let Value::Array(_) = value {
                    true
                } else {
                    false
                }
            });
            if is_array_reference || is_local_array {
                return true;
            }
        }
        if let Some(Value::Array(_)) = self.globals.get(name) {
            true
        } else {
            false
        }
    }

    fn get_var(&self, name: &str, subscript: Option<&str>) -> Result<Value, EvaluationError> {
        // To find the set of variables to get the value from, start from the top of
        // calls:
        // - check locals of the call for a variable with given name, if found then done
        // - check the references
        //     - if missing, then done
        //     - if found, then check the parent call and iterate to the next call with
        //       the referenced name
        let mut vars = &self.globals;
        let mut referred_var = name;
        for FunctionCall { locals, references } in self.function_calls.iter().rev() {
            if locals.contains_key(referred_var) {
                vars = locals;
                break;
            } else {
                match references.get(referred_var) {
                    Some(reference) => referred_var = reference,
                    None => break,
                }
            }
        }
        match vars.get(referred_var) {
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
        ty: AssignType,
        name: &str,
        subscript: Option<&str>,
        new_value: Value,
    ) -> Result<Value, EvaluationError> {
        match name {
            _ if is_special_variable(name) && subscript.is_some() => {
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
            "OFMT" => {
                let result = Value::compute(ty, Value::from(self.ofmt.to_owned()), new_value)?;
                self.ofmt = result.as_string();
                Ok(Value::from(self.ofmt.to_owned()))
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
            _ => self.set_var(ty, name, subscript, new_value),
        }
    }

    fn set_var(
        &mut self,
        ty: AssignType,
        name: &str,
        subscript: Option<&str>,
        new_value: Value,
    ) -> Result<Value, EvaluationError> {
        // To find the set of variables to set the value to, start from the top of
        // calls:
        // - check locals of the call for a variable with given name, if found then done
        // - check the references
        //     - if missing, then done
        //     - if found, then check the parent call and iterate to the next call with
        //       the referenced name
        let mut vars = &mut self.globals;
        let mut referred_var = name;
        for FunctionCall { locals, references } in self.function_calls.iter_mut().rev() {
            if locals.contains_key(referred_var) {
                vars = locals;
                break;
            } else {
                match references.get(referred_var) {
                    Some(reference) => referred_var = reference,
                    None => break,
                }
            }
        }
        match vars.entry(referred_var.to_owned()) {
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
