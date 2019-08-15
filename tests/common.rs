use assert_cmd::prelude::*;
use std::{io::Write, process::Command};
use tempfile::NamedTempFile;

pub fn run_test(input: Option<&str>, script: &str, output: &str) {
    let mut cmd = Command::cargo_bin("falcon").unwrap();

    match input {
        Some(input) => {
            let mut file = NamedTempFile::new().unwrap();
            file.write_all(input.as_bytes()).unwrap();
            let path = file.into_temp_path();
            cmd.arg(script).arg(&path);
            let assert = cmd.assert();
            assert.stdout(output.to_owned());
        },
        None => {
            cmd.arg(script);
            let assert = cmd.assert();
            assert.stdout(output.to_owned());
        },
    }
}
