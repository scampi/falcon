mod common;

use common::{read_file, run_test, Test};
use std::{fs::File, io::prelude::*};
use tempfile::NamedTempFile;

#[test]
fn truncate_redirection() {
    let mut file = NamedTempFile::new().unwrap();
    file.write_all("john connor\n".as_bytes()).unwrap();
    let path = file.into_temp_path();
    let script = r#"{ printf "script: %s\n", $0 > "file" }"#;
    let script = script.replace("file", &path.to_string_lossy());

    run_test(Test::new(&script).input("toto\ntata\n"));

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

    run_test(Test::new(&script).input("toto\ntata\n"));

    let contents = read_file(path);
    assert_eq!(contents, "john connor\nscript: toto\nscript: tata\n");
}

#[test]
fn escaped_characters() {
    let script = r#"{ print "hello\t%s", $0 }"#;
    run_test(Test::new(&script).input("john").stdout("hello\t%s john\n"));

    let script = "{ print }";
    run_test(
        Test::new(&script)
            .input("toto\ttata")
            .stdout("toto\ttata\n"),
    );
}
