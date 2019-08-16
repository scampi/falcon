use crate::{
    errors::EvaluationError,
    interpreter::{Eval, RuntimeMut},
    parser::ast::OutputRedirection,
};
use std::{
    collections::{hash_map::Entry, HashMap},
    fs::{File, OpenOptions},
    io::Write,
};

pub fn file_name<Output: Write>(
    rt: &mut RuntimeMut<'_, Output>,
    redir: &OutputRedirection,
) -> Result<String, EvaluationError> {
    match redir {
        OutputRedirection::Truncate(expr) | OutputRedirection::Append(expr) => {
            Ok(expr.eval(rt)?.as_string())
        },
        _ => unimplemented!(),
    }
}

#[derive(Debug)]
pub struct Redirections {
    files: HashMap<String, File>,
}

impl Redirections {
    pub fn new() -> Redirections {
        Redirections {
            files: HashMap::new(),
        }
    }

    pub fn add_file(
        &mut self,
        path: &str,
        redir: &OutputRedirection,
    ) -> Result<(), EvaluationError> {
        match redir {
            OutputRedirection::Truncate(_) => {
                if let Entry::Vacant(entry) = self.files.entry(path.to_string()) {
                    let file = OpenOptions::new()
                        .create(true)
                        .write(true)
                        .truncate(true)
                        .open(path)?;
                    entry.insert(file);
                }
            },
            OutputRedirection::Append(_) => {
                if let Entry::Vacant(entry) = self.files.entry(path.to_string()) {
                    let file = OpenOptions::new().create(true).append(true).open(path)?;
                    entry.insert(file);
                }
            },
            OutputRedirection::Pipe(_) => unimplemented!(),
        }
        Ok(())
    }

    pub fn get_file(&mut self, path: &str) -> &mut File {
        self.files.get_mut(path).unwrap()
    }
}
