use assert_cmd::prelude::*;
use std::{
    fs::File,
    io::{prelude::*, Write},
    process::Command,
};
use tempfile::{NamedTempFile, TempPath};

pub fn run_test(input: Option<&str>, script: &str, output: &str) {
    let mut cmd = Command::cargo_bin("falcon").unwrap();

    match input {
        Some(input) => {
            let mut file = NamedTempFile::new().unwrap();
            file.write_all(input.as_bytes()).unwrap();
            let path = file.into_temp_path();
            cmd.arg(script).arg(&path);
            cmd.assert().stdout(output.to_owned());
        },
        None => {
            cmd.arg(script);
            cmd.assert().stdout(output.to_owned());
        },
    }
}

pub fn read_file(path: TempPath) -> String {
    let mut contents = String::new();
    File::open(path)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    contents
}
