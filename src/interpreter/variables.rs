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
    /// with the value the name of the array it references.
    references: HashMap<String, String>,
}

#[derive(Debug)]
pub struct Variables {
    /// The set of variables with a global scope.
    pub globals: HashMap<String, Value>,
    /// The function calls stack. Each call to a user-defined function adds an
    /// element to the stack.
    function_calls: Vec<FunctionCall>,
    ///// The number of arguments
    //argc: usize,
    ///// The command line arguments
    //argv: Vec<String>,
    //convfmt: String,
    //env: HashMap<String, String>,
    ///// The pathname to the current input file
    //filename: String,
    /// The ordinal number of the current record in the current file.
    pub fnr: usize,
    /// Input field separator regular expression.
    pub fs: String,
    /// The number of fields in the current record.
    pub nf: usize,
    /// The ordinal number of the current record from the start of input.
    pub nr: usize,
    /// The printf format for converting numbers to strings in output
    /// statements.
    pub ofmt: String,
    /// The print statement output field separation.
    pub ofs: String,
    /// The print statement output record separator.
    pub ors: String,
    //rlength: usize,
    //rs: String,
    //rstart: usize,
    /// The subscript separator string for multi-dimensional arrays.
    pub subsep: String,
}

/// Returns true if the given variable is a special variable in AWK. A special
/// variable cannot be re-defined in a script.
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

    /// Add a new function call to the stack, where `locals` is the set of
    /// variables which scope is limited to the function, and `references`
    /// is a set of array variables that points to those in the current set.
    pub fn push_local_stack(
        &mut self,
        locals: HashMap<String, Value>,
        references: HashMap<String, String>,
    ) {
        self.function_calls
            .push(FunctionCall { locals, references });
    }

    /// Drops the last function call from the stack.
    pub fn drop_local_stack(&mut self) {
        self.function_calls.pop().unwrap();
    }

    /// Get the map of variales in which to look for the variable with the given
    /// name. A variable is looked up in a set of global variables by
    /// default. However, the scope of a variable in a function is limited
    /// to it. Therefore, in a function call, looking up a variable may from
    /// the set local to a function, not the global set.
    ///
    /// In order to get the correct variable set, the algorithm starts from the
    /// top of function calls:
    /// - check locals of the call for a variable with given name, if found then
    ///   done
    /// - check the references
    ///     - if missing, then done
    ///     - if found, then check the parent call and iterate to the next call
    ///       with the referenced name
    ///
    /// This method returns a tuple, with the first value being the map of
    /// variables resolved as per above algorithm, and the second value is
    /// the name of the variable in case the original variable was a
    /// reference.
    fn get_variables_set<'a>(&'a self, name: &'a str) -> (&'a HashMap<String, Value>, &'a str) {
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
        (vars, referred_var)
    }

    /// Like `[get_variables_set]` but allows to mutate the map of variables.
    fn get_variables_set_mut<'a>(
        &'a mut self,
        name: &'a str,
    ) -> (&'a mut HashMap<String, Value>, &'a str) {
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
        (vars, referred_var)
    }

    /// Returns the list of keys of the given named associative array.
    pub fn array_keys(&self, name: &str) -> Result<Vec<String>, EvaluationError> {
        let (vars, referred_var) = self.get_variables_set(name);
        match vars.get(referred_var) {
            Some(value) => match value {
                Value::Uninitialised => Ok(Vec::new()),
                Value::Array(array) => Ok(array.keys().map(|key| key.to_owned()).collect()),
                _ => Err(EvaluationError::UseScalarAsArray),
            },
            None => Ok(Vec::new()),
        }
    }

    /// Deletes the element at the given key in the named array.
    pub fn delete(&mut self, name: &str, key: &str) -> Result<(), EvaluationError> {
        let (vars, referred_var) = self.get_variables_set_mut(name);
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

    /// Returns the array key as string joined with the subscript separator.
    pub fn array_key(&self, key_values: Vec<Value>) -> Result<String, EvaluationError> {
        Ok(key_values
            .into_iter()
            .map(|v| v.as_string())
            .collect::<Vec<String>>()
            .join(&self.subsep))
    }

    /// Returns the value associated with the variable of the given name.
    /// If a subscript is given then the variable is assumed to be an array.
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

    /// Returns true if the variable of the given name is an array. The top of
    /// the call stack is also checked.
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

    /// Returns the value of the user-defined variable.
    fn get_var(&self, name: &str, subscript: Option<&str>) -> Result<Value, EvaluationError> {
        let (vars, referred_var) = self.get_variables_set(name);
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

    /// Sets the new_value to the variable of the given name. If a subscript is
    /// given then it is assumed the variable is an array and the subscript is
    /// the key.
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
        let (vars, referred_var) = self.get_variables_set_mut(name);
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
