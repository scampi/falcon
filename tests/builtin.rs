/// taken from https://github.com/andychu/bwk/blob/41fdc2c8a3aa26c9c6ec3c797b85283d406b7f62/tests/T.builtin
mod common;

use common::{read_file, run_test};
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
    let path1 = NamedTempFile::new().unwrap().into_temp_path();
    let path2 = NamedTempFile::new().unwrap().into_temp_path();

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

    let contents1 = read_file(path1);
    let contents2 = read_file(path2);
    assert_eq!(contents1, contents2);
}

#[test]
fn printf_arg_list_not_evaluated() {
    let script = r#"
        BEGIN {
            j = 1; sprintf("%d", 99, ++j)	# does j get incremented?
            if (j != 2)
                    print "BAD: T.builtin (printf arg list not evaluated)"
        }
        "#;
    run_test(None, &script, "");
}

#[test]
fn substr_arg_list_not_evaluated() {
    let script = r#"
        BEGIN {
            j = 1; substr("", 1, ++j)	# does j get incremented?
            if (j != 2)
                    print "BAD: T.builtin (substr arg list not evaluated)"
        }
        "#;
    run_test(None, &script, "");
}

#[test]
fn sub_arg_list_not_evaluated() {
    let script = r#"
        BEGIN {
            j = 1; sub(/1/, ++j, z)	# does j get incremented?
            if (j != 2)
                    print "BAD: T.builtin (sub() arg list not evaluated)"
        }
        "#;
    run_test(None, &script, "");
}

#[test]
fn sub() {
    let input = "aa\nnope\nba";
    let output = "ba 1\nnope 0\nbb 1\n";
    let script = r#"{ n = sub(/a/, "b"); print $0, n }"#;
    run_test(Some(input), &script, output);

    let input = "a1a";
    let output = "aba 1\n";
    let script = r#"{ n = sub(1, "b"); print $0, n }"#;
    run_test(Some(input), &script, output);

    let output = "ba 1\n";
    let script = r#"BEGIN { s = "aa"; n = sub("a", "b", s); print s, n }"#;
    run_test(None, &script, output);

    let input = "aa aa aa";
    let output = "aa ba aa ba 1\n";
    let script = r#"{ n = sub("a", "b", $2); print $0, $2, n }"#;
    run_test(Some(input), &script, output);

    let output = "ba 1\n";
    let script = r#"BEGIN { arr[0] = "aa"; n = sub("a", "b", arr[0]); print arr[0], n }"#;
    run_test(None, &script, output);

    let input = "nope\nhello john1!";
    let output = "nope 0\nhello connor, john1! 1\n";
    let script = r#"{ n = sub(/john[0-9]/, "connor, &"); print $0, n }"#;
    run_test(Some(input), &script, output);

    let output = "hello connor, &! 1\n";
    let script =
        r#"BEGIN { s = "hello john1!"; n = sub(/john[0-9]/, "connor, \\&", s); print s, n }"#;
    run_test(None, &script, output);

    let output = "hello john1 & john1! 1\n";
    let script = r#"BEGIN { s = "hello john1!"; n = sub(/john[0-9]/, "& \\& &", s); print s, n }"#;
    run_test(None, &script, output);
}
