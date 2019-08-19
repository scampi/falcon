/// taken from https://github.com/andychu/bwk/blob/41fdc2c8a3aa26c9c6ec3c797b85283d406b7f62/tests/T.builtin
mod common;

use common::run_test;
use std::{fs::File, io::prelude::*};
use tempfile::NamedTempFile;

#[test]
fn index_substr() {
    run_test(None, "BEGIN { print index(123, substr(123, 2)) }", "2\n")
}

#[test]
fn sin_cos() {
    run_test(
        None,
        r#"
        BEGIN {
          pi = 2 * atan2(1, 0)
          printf("%.5f %.3f %.3f %.5f %.3f\n",
                  pi, sin(pi), cos(pi/2), exp(log(pi)), log(exp(10)))
        }
        "#,
        "3.14159 0.000 0.000 3.14159 10.000\n",
    )
}

#[test]
fn toupper_tolower() {
    run_test(
        Some("hello, WORLD!\n"),
        r#"{ printf("%s|%s|%s\n", tolower($0), toupper($0), $0)}"#,
        "hello, world!|HELLO, WORLD!|hello, WORLD!\n",
    )
}

#[test]
fn rand() {
    let mut file1 = NamedTempFile::new().unwrap();
    let path1 = file1.into_temp_path();
    let mut file2 = NamedTempFile::new().unwrap();
    let path2 = file2.into_temp_path();

    let script = r#"
        BEGIN {
            s = srand(1)	# set a real random start
            for (i = 1; i <= 10; i++)
                    print rand() >"FILE1"
            srand(s)	# reset it
            for (i = 1; i <= 10; i++)
                    print rand() >"FILE2"
        }
        "#;
    let script = script.replace("FILE1", &path1.to_string_lossy());
    let script = script.replace("FILE2", &path2.to_string_lossy());

    run_test(None, &script, "");

    let mut contents1 = String::new();
    File::open(path1)
        .unwrap()
        .read_to_string(&mut contents1)
        .unwrap();
    let mut contents2 = String::new();
    File::open(path2)
        .unwrap()
        .read_to_string(&mut contents2)
        .unwrap();
    assert_eq!(contents1, contents2);
}
