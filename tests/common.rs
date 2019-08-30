use assert_cmd::prelude::*;
use std::{
    fs::File,
    io::{prelude::*, Write},
    process::Command,
};
use tempfile::{NamedTempFile, TempPath};

pub struct Test<'a> {
    input: Option<&'a str>,
    script: &'a str,
    stdout: Option<&'a str>,
    stderr: Option<&'a str>,
    should_fail: bool,
}

impl<'a> Test<'a> {
    pub fn new(script: &'a str) -> Self {
        Test {
            input: None,
            script,
            stdout: None,
            stderr: None,
            should_fail: false,
        }
    }

    pub fn input(mut self, input: &'a str) -> Self {
        self.input = Some(input);
        self
    }

    pub fn stdout(mut self, stdout: &'a str) -> Self {
        self.stdout = Some(stdout);
        self
    }

    pub fn stderr(mut self, stderr: &'a str) -> Self {
        self.stderr = Some(stderr);
        self
    }

    pub fn should_fail(mut self) -> Self {
        self.should_fail = true;
        self
    }
}

pub fn run_test(test: Test) {
    let mut cmd = Command::cargo_bin("falcon").unwrap();

    let cmd = match test.input {
        Some(input) => {
            let mut file = NamedTempFile::new().unwrap();
            file.write_all(input.as_bytes()).unwrap();
            let path = file.into_temp_path();
            cmd.arg(test.script).arg(&path);
            cmd.assert()
        },
        None => {
            cmd.arg(test.script);
            cmd.assert()
        },
    };
    let cmd = match (test.stdout, test.stderr) {
        (Some(stdout), None) => cmd.stdout(stdout.to_owned()).stderr(String::new()),
        (None, Some(stderr)) => cmd.stderr(stderr.to_owned()).stdout(String::new()),
        (Some(stdout), Some(stderr)) => cmd.stderr(stderr.to_owned()).stdout(stdout.to_owned()),
        (None, None) => cmd.stderr(String::new()).stdout(String::new()),
    };
    if test.should_fail {
        cmd.failure();
    } else {
        cmd.success();
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
