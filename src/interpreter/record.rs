//! The record module handles an input row and splits it on the `FS` separator.
use crate::{
    errors::EvaluationError,
    interpreter::{value::Value, variables::Variables},
    parser::ast::AssignType,
};
use lazy_static::lazy_static;
use regex::Regex;

/// An input row.
#[derive(Debug)]
pub struct Record {
    /// The set of fields extracted from the current input row splited on `FS`.
    fields: Vec<String>,
    /// The current input row.
    record: String,
}

impl Record {
    pub fn new() -> Record {
        Record {
            fields: Vec::new(),
            record: String::new(),
        }
    }

    /// Update the current input row and updates related special variables.
    /// The passed record may not be the next input row: a script may mutate,
    /// e.g., a field of the current row, and therefore this is reflected to
    /// the Record state but also special variables such as `NF` in case the
    /// `FS` field separator changed.
    pub fn update_record(
        &mut self,
        vars: &mut Variables,
        record: String,
    ) -> Result<(), EvaluationError> {
        self.record = record;
        self.fields = if vars.fs == " " {
            lazy_static! {
                static ref FS_REGEX: Regex = Regex::new(r"[ \t]+").unwrap();
            }
            FS_REGEX
                .split(&self.record)
                .skip_while(|s| s.is_empty())
                .map(|s| s.to_owned())
                .collect()
        } else if vars.fs.len() == 1 {
            self.record.split(&vars.fs).map(|s| s.to_owned()).collect()
        } else {
            let fs_pattern = match Regex::new(&vars.fs) {
                Ok(p) => p,
                Err(e) => return Err(EvaluationError::InvalidRegex(e)),
            };
            fs_pattern
                .split(&self.record)
                .skip_while(|s| s.is_empty())
                .map(|s| s.to_owned())
                .collect()
        };
        vars.nf = self.fields.len();
        Ok(())
    }

    /// Returns the field at the given index. Field at index 0 is the full row.
    pub fn get(&self, index: isize) -> Result<Value, EvaluationError> {
        if index < 0 {
            return Err(EvaluationError::NegativeFieldIndex(index));
        }
        if index == 0 {
            return Ok(Value::from(self.record.to_owned()));
        }
        match self.fields.get(index as usize - 1) {
            Some(field) => Ok(Value::from(field.to_owned())),
            None => Ok(Value::Uninitialised),
        }
    }

    /// Sets the input row field with the given value.
    pub fn set(
        &mut self,
        vars: &mut Variables,
        ty: AssignType,
        index: isize,
        value: Value,
    ) -> Result<Value, EvaluationError> {
        if index < 0 {
            return Err(EvaluationError::NegativeFieldIndex(index));
        }
        let index = index as usize;
        if index == 0 {
            let value_str = value.as_string();
            let ret = Ok(Value::from(value_str.to_owned()));
            self.update_record(vars, value_str)?;
            ret
        } else {
            let index = index - 1;
            match self.fields.get_mut(index) {
                Some(f) => *f = Value::compute(ty, Value::from(f.to_string()), value)?.as_string(),
                None => {
                    for _ in 0..index - self.fields.len() {
                        self.fields.push(String::new());
                    }
                    self.fields
                        .push(Value::compute(ty, Value::Uninitialised, value)?.as_string());
                },
            };
            self.record = self.fields.join(&vars.fs);
            vars.nf = self.fields.len();
            Ok(Value::from(self.fields.get(index).unwrap().to_owned()))
        }
    }
}
