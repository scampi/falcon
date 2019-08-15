use assert_cmd::prelude::*;
use std::process::Command;

pub fn run_test(input: Option<&str>, script: &str, output: &str) {
    let mut cmd = Command::cargo_bin("falcon").unwrap();
    if let Some(input) = input {
        cmd.arg(script).with_stdin().buffer(input.as_bytes());
    } else {
        cmd.arg(script);
    }
    let assert = cmd.assert();
    assert.stdout(output.to_owned());
}
