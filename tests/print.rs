mod common;

use common::run_test;
use std::{fs::File, io::prelude::*};
use tempfile::NamedTempFile;

#[test]
fn truncate_redirection() {
    let mut file = NamedTempFile::new().unwrap();
    file.write_all("john connor\n".as_bytes()).unwrap();
    let path = file.into_temp_path();
    let script = r#"{ printf "script: %s\n", $0 > "file" }"#;
    let script = script.replace("file", &path.to_string_lossy());

    run_test(Some("toto\ntata\n"), &script, "");

    let mut contents = String::new();
    File::open(path)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    assert_eq!(contents, "script: toto\nscript: tata\n");
}

#[test]
fn append_redirection() {
    let mut file = NamedTempFile::new().unwrap();
    file.write_all("john connor\n".as_bytes()).unwrap();
    let path = file.into_temp_path();
    let script = r#"{ printf "script: %s\n", $0 >> "file" }"#;
    let script = script.replace("file", &path.to_string_lossy());

    run_test(Some("toto\ntata\n"), &script, "");

    let mut contents = String::new();
    File::open(path)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    assert_eq!(contents, "john connor\nscript: toto\nscript: tata\n");
}
