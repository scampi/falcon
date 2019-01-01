use std::collections::{hash_map::Entry, HashMap};

use crate::{errors::EvaluationError, interpreter::value::Value, parser::expr::AssignType};

#[derive(Debug)]
pub struct Variables {
    vars: HashMap<String, Value>,
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

    pub fn get(&self, name: &str) -> Value {
        match name {
            "FNR" => Value::from(self.fnr),
            "FS" => Value::from(self.fs.to_owned()),
            "NF" => Value::from(self.nf),
            "NR" => Value::from(self.nr),
            "SUBSEP" => Value::from(self.subsep.to_owned()),
            _ => match self.vars.get(name) {
                Some(value) => value.clone(),
                None => Value::Uninitialised,
            },
        }
    }

    pub fn set(
        &mut self,
        ty: &AssignType,
        name: &str,
        value: Value,
    ) -> Result<Value, EvaluationError> {
        match name {
            "FNR" => {
                let result = Value::compute(ty, Value::from(self.fnr), value)?;
                self.fnr = result.as_number() as usize;
                Ok(Value::from(self.fnr))
            },
            "FS" => {
                let result = Value::compute(ty, Value::from(self.fs.to_owned()), value)?;
                self.fs = result.as_string();
                Ok(Value::from(self.fs.to_owned()))
            },
            "NF" => {
                let result = Value::compute(ty, Value::from(self.nf), value)?;
                self.nf = result.as_number() as usize;
                Ok(Value::from(self.nf))
            },
            "NR" => {
                let result = Value::compute(ty, Value::from(self.nr), value)?;
                self.nr = result.as_number() as usize;
                Ok(Value::from(self.nr))
            },
            "SUBSEP" => {
                let result = Value::compute(ty, Value::from(self.subsep.to_owned()), value)?;
                self.subsep = result.as_string();
                Ok(Value::from(self.subsep.to_owned()))
            },
            _ => match self.vars.entry(name.to_owned()) {
                Entry::Occupied(mut entry) => {
                    let result = Value::from(Value::compute(ty, entry.get().clone(), value)?);
                    entry.insert(result.clone());
                    Ok(result)
                },
                Entry::Vacant(entry) => {
                    let result = Value::from(Value::compute(ty, Value::Uninitialised, value)?);
                    entry.insert(result.clone());
                    Ok(result)
                },
            },
        }
    }
}
