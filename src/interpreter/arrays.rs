use std::collections::{hash_map::Entry, HashMap};

use crate::{
    errors::EvaluationError,
    interpreter::{value::Value, Context, Eval},
    parser::ast::{AssignType, ExprList},
};

pub struct Arrays {
    arrays: HashMap<String, HashMap<String, Value>>,
}

impl Arrays {
    pub fn new() -> Arrays {
        Arrays {
            arrays: HashMap::new(),
        }
    }

    pub fn keys(&self, name: &str) -> Option<Vec<String>> {
        self.arrays
            .get(name)
            .map(|array| array.keys().map(|key| key.to_owned()).collect())
    }

    pub fn get(&self, name: &str, key: &str) -> Result<Value, EvaluationError> {
        match self.arrays.get(name) {
            Some(array) => match array.get(key) {
                Some(value) => Ok(value.clone()),
                None => Ok(Value::Uninitialised),
            },
            None => Ok(Value::Uninitialised),
        }
    }

    pub fn set(
        &mut self,
        ty: &AssignType,
        name: &str,
        key: String,
        value: Value,
    ) -> Result<Value, EvaluationError> {
        match self.arrays.entry(name.to_owned()) {
            Entry::Occupied(mut entry) => match entry.get_mut().entry(key) {
                Entry::Occupied(mut entry) => {
                    let result = Value::compute(ty, entry.get().clone(), value)?;
                    entry.insert(result.clone());
                    Ok(result)
                },
                Entry::Vacant(entry) => {
                    let result = Value::compute(ty, Value::Uninitialised, value)?;
                    entry.insert(result.clone());
                    Ok(result)
                },
            },
            Entry::Vacant(entry) => {
                let mut array = HashMap::new();
                let result = Value::compute(ty, Value::Uninitialised, value)?;
                array.insert(key, result.clone());
                entry.insert(array);
                Ok(result)
            },
        }
    }

    pub fn array_key(cxt: &mut Context, path: &ExprList) -> Result<String, EvaluationError> {
        let mut key_str = String::new();
        for expr in &path.0 {
            if !key_str.is_empty() {
                key_str.push_str(&cxt.vars.subsep);
            }
            key_str.push_str(&expr.eval(cxt)?.as_string());
        }
        Ok(key_str)
    }
}
