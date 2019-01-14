use crate::{
    errors::EvaluationError,
    interpreter::{
        functions::Functions,
        value::{ExprValue, VariableValue},
    },
    parser::ast::AssignType,
};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug)]
pub struct Variables {
    vars: HashMap<String, VariableValue>,
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
    //ofs: String,
    //ors: String,
    //rlength: usize,
    //rs: String,
    //rstart: usize,
    pub subsep: String,
}

impl Variables {
    pub fn new() -> Variables {
        Variables {
            vars: HashMap::new(),
            fnr: 0,
            fs: String::from(" "),
            nf: 0,
            nr: 0,
            subsep: String::new(),
        }
    }

    #[cfg(test)]
    pub fn has_user_vars(&self) -> bool {
        !self.vars.is_empty()
    }

    #[cfg(test)]
    pub fn clean(&mut self) {
        self.vars.clear();
        self.fnr = 0;
        self.nr = 0;
        self.subsep.clear();
        self.fs.clear();
        self.fs.push_str(" ");
    }

    pub fn array_keys(&self, name: &str) -> Result<Vec<String>, EvaluationError> {
        if let Some(value) = self.vars.get(name) {
            match value {
                VariableValue::Uninitialised => Ok(Vec::new()),
                VariableValue::Array(array) => Ok(array.keys().map(|key| key.to_owned()).collect()),
                VariableValue::Scalar(..) => return Err(EvaluationError::UseScalarAsArray),
            }
        } else {
            Ok(Vec::new())
        }
    }

    pub fn delete(&mut self, name: &str, key: &str) -> Result<(), EvaluationError> {
        if let Entry::Occupied(mut entry) = self.vars.entry(name.to_owned()) {
            match entry.get_mut() {
                VariableValue::Uninitialised => (),
                VariableValue::Array(array) => {
                    if let Entry::Occupied(entry) = array.entry(key.to_owned()) {
                        entry.remove_entry();
                    }
                },
                VariableValue::Scalar(..) => return Err(EvaluationError::UseScalarAsArray),
            }
        }
        Ok(())
    }

    pub fn array_key(values: Vec<ExprValue>) -> Result<String, EvaluationError> {
        Ok(values
            .into_iter()
            .map(|v| v.as_string())
            .collect::<Vec<String>>()
            .join(""))
    }

    pub fn get(
        &self,
        funcs: &Functions,
        name: &str,
        subscript: Option<&str>,
    ) -> Result<ExprValue, EvaluationError> {
        match name {
            "FNR" | "FS" | "NF" | "NR" | "SUBSEP" if subscript.is_some() => {
                Err(EvaluationError::UseScalarAsArray)
            },
            "FNR" => Ok(ExprValue::from(self.fnr)),
            "FS" => Ok(ExprValue::from(self.fs.to_owned())),
            "NF" => Ok(ExprValue::from(self.nf)),
            "NR" => Ok(ExprValue::from(self.nr)),
            "SUBSEP" => Ok(ExprValue::from(self.subsep.to_owned())),
            _ if funcs.is_local_var(name) => funcs.get_local_var(name, subscript),
            _ => Variables::get_var(&self.vars, name, subscript),
        }
    }

    pub fn get_var(
        vars: &HashMap<String, VariableValue>,
        name: &str,
        subscript: Option<&str>,
    ) -> Result<ExprValue, EvaluationError> {
        match vars.get(name) {
            Some(value) => match (subscript, value) {
                (Some(..), VariableValue::Scalar(..)) => Err(EvaluationError::UseScalarAsArray),
                (None, VariableValue::Array(..)) => Err(EvaluationError::UseArrayInScalarContext),
                (Some(index), VariableValue::Array(array)) => match array.get(index) {
                    Some(value) => Ok(value.clone()),
                    None => Ok(ExprValue::Uninitialised),
                },
                (None, VariableValue::Scalar(value)) => Ok(value.clone()),
                (_, VariableValue::Uninitialised) => Ok(ExprValue::Uninitialised),
            },
            None => Ok(ExprValue::Uninitialised),
        }
    }

    pub fn set(
        &mut self,
        funcs: &mut Functions,
        ty: &AssignType,
        name: &str,
        subscript: Option<&str>,
        new_value: ExprValue,
    ) -> Result<ExprValue, EvaluationError> {
        match name {
            "FNR" | "FS" | "NF" | "NR" | "SUBSEP" if subscript.is_some() => {
                Err(EvaluationError::UseScalarAsArray)
            },
            "FNR" => {
                let result = ExprValue::compute(ty, ExprValue::from(self.fnr), new_value)?;
                self.fnr = result.as_number() as usize;
                Ok(ExprValue::from(self.fnr))
            },
            "FS" => {
                let result =
                    ExprValue::compute(ty, ExprValue::from(self.fs.to_owned()), new_value)?;
                self.fs = result.as_string();
                Ok(ExprValue::from(self.fs.to_owned()))
            },
            "NF" => {
                let result = ExprValue::compute(ty, ExprValue::from(self.nf), new_value)?;
                self.nf = result.as_number() as usize;
                Ok(ExprValue::from(self.nf))
            },
            "NR" => {
                let result = ExprValue::compute(ty, ExprValue::from(self.nr), new_value)?;
                self.nr = result.as_number() as usize;
                Ok(ExprValue::from(self.nr))
            },
            "SUBSEP" => {
                let result =
                    ExprValue::compute(ty, ExprValue::from(self.subsep.to_owned()), new_value)?;
                self.subsep = result.as_string();
                Ok(ExprValue::from(self.subsep.to_owned()))
            },
            _ if funcs.is_local_var(name) => funcs.set_local_var(ty, name, subscript, new_value),
            _ => Variables::set_var(&mut self.vars, ty, name, subscript, new_value),
        }
    }

    pub fn set_var(
        vars: &mut HashMap<String, VariableValue>,
        ty: &AssignType,
        name: &str,
        subscript: Option<&str>,
        new_value: ExprValue,
    ) -> Result<ExprValue, EvaluationError> {
        match vars.entry(name.to_owned()) {
            Entry::Occupied(mut entry) => match (subscript, entry.get_mut()) {
                (Some(..), VariableValue::Scalar(..)) => Err(EvaluationError::UseScalarAsArray),
                (None, VariableValue::Array(..)) => Err(EvaluationError::UseArrayInScalarContext),
                (Some(index), VariableValue::Uninitialised) => {
                    let mut array = HashMap::new();
                    let result = ExprValue::compute(ty, ExprValue::Uninitialised, new_value)?;
                    array.insert(index.to_owned(), result.clone());
                    entry.insert(VariableValue::Array(array));
                    Ok(result)
                },
                (None, VariableValue::Uninitialised) => {
                    let result = ExprValue::from(ExprValue::compute(
                        ty,
                        ExprValue::Uninitialised,
                        new_value,
                    )?);
                    entry.insert(VariableValue::Scalar(result.clone()));
                    Ok(result)
                },
                (Some(index), VariableValue::Array(array)) => match array.entry(index.to_owned()) {
                    Entry::Occupied(mut entry) => {
                        let result = ExprValue::compute(ty, entry.get().clone(), new_value)?;
                        entry.insert(result.clone());
                        Ok(result)
                    },
                    Entry::Vacant(entry) => {
                        let result = ExprValue::compute(ty, ExprValue::Uninitialised, new_value)?;
                        entry.insert(result.clone());
                        Ok(result)
                    },
                },
                (None, VariableValue::Scalar(value)) => {
                    let result = ExprValue::from(ExprValue::compute(ty, value.clone(), new_value)?);
                    entry.insert(VariableValue::Scalar(result.clone()));
                    Ok(result)
                },
            },
            Entry::Vacant(entry) => match subscript {
                Some(index) => {
                    let mut array = HashMap::new();
                    let result = ExprValue::compute(ty, ExprValue::Uninitialised, new_value)?;
                    array.insert(index.to_owned(), result.clone());
                    entry.insert(VariableValue::Array(array));
                    Ok(result)
                },
                None => {
                    let result = ExprValue::from(ExprValue::compute(
                        ty,
                        ExprValue::Uninitialised,
                        new_value,
                    )?);
                    entry.insert(VariableValue::Scalar(result.clone()));
                    Ok(result)
                },
            },
        }
    }
}
