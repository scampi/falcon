/// taken from https://github.com/andychu/bwk/blob/41fdc2c8a3aa26c9c6ec3c797b85283d406b7f62/tests/T.builtin
mod common;

use common::{read_file, run_test, Test};
use tempfile::NamedTempFile;

#[test]
fn index_substr() {
    run_test(Test::new("BEGIN { print index(123, substr(123, 2)) }").stdout("2\n"))
}

#[test]
fn sin_cos() {
    run_test(
        Test::new(
            r#"
        BEGIN {
          pi = 2 * atan2(1, 0)
          printf("%.5f %.3f %.3f %.5f %.3f\n",
                  pi, sin(pi), cos(pi/2), exp(log(pi)), log(exp(10)))
        }
        "#,
        )
        .stdout("3.14159 0.000 0.000 3.14159 10.000\n"),
    )
}

#[test]
fn toupper_tolower() {
    run_test(
        Test::new(r#"{ printf("%s|%s|%s\n", tolower($0), toupper($0), $0)}"#)
            .input("hello, WORLD!\n")
            .stdout("hello, world!|HELLO, WORLD!|hello, WORLD!\n"),
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

    run_test(Test::new(&script));

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
    run_test(Test::new(&script));
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
    run_test(Test::new(&script));
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
    run_test(Test::new(&script));
}

#[test]
fn sub() {
    let input = "aa\nnope\nba";
    let output = "ba 1\nnope 0\nbb 1\n";
    let script = r#"{ n = sub(/a/, "b"); print $0, n }"#;
    run_test(Test::new(&script).input(input).stdout(output));

    let input = "a1a";
    let output = "aba 1\n";
    let script = r#"{ n = sub(1, "b"); print $0, n }"#;
    run_test(Test::new(&script).input(input).stdout(output));

    let output = "ba 1\n";
    let script = r#"BEGIN { s = "aa"; n = sub("a", "b", s); print s, n }"#;
    run_test(Test::new(&script).stdout(output));

    let input = "aa aa aa";
    let output = "aa ba aa ba 1\n";
    let script = r#"{ n = sub("a", "b", $2); print $0, $2, n }"#;
    run_test(Test::new(&script).input(input).stdout(output));

    let output = "ba 1\n";
    let script = r#"BEGIN { arr[0] = "aa"; n = sub("a", "b", arr[0]); print arr[0], n }"#;
    run_test(Test::new(&script).stdout(output));

    let input = "nope\nhello john1!";
    let output = "nope 0\nhello connor, john1! 1\n";
    let script = r#"{ n = sub(/john[0-9]/, "connor, &"); print $0, n }"#;
    run_test(Test::new(&script).input(input).stdout(output));

    let output = "hello connor, &! 1\n";
    let script =
        r#"BEGIN { s = "hello john1!"; n = sub(/john[0-9]/, "connor, \\&", s); print s, n }"#;
    run_test(Test::new(&script).stdout(output));

    let output = "hello john1 & john1! 1\n";
    let script = r#"BEGIN { s = "hello john1!"; n = sub(/john[0-9]/, "& \\& &", s); print s, n }"#;
    run_test(Test::new(&script).stdout(output));

    let output = "helli%s\t! 1\n";
    let script = r#"BEGIN { s = "hello!"; n = sub("o", "i%s\t", s); print s, n }"#;
    run_test(Test::new(&script).stdout(output));
}

#[test]
fn length_too_many_args() {
    let script = r#"
        BEGIN {
            j = 1; length("zzzz", ++j, ++j)	# does j get incremented?
            if (j != 3)
                    print "BAD: T.builtin (excess length args not evaluated)"
        }
        "#;
    run_test(
        Test::new(&script)
            .stderr("Function 'length' expects 0 or 1 arguments but got 3\n")
            .should_fail(),
    );
}

#[test]
fn split() {
    let script = r#"
        BEGIN {
            n = split("aaa bbb ccc", a);
            if (n != 3 || a[1] != "aaa" || a[2] != "bbb" || a[3] != "ccc") {
                print "KO";
            }
        }
    "#;
    run_test(Test::new(&script));

    let script = r#"
        BEGIN {
            a[4] = "nope";
            n = split("aaa bbb ccc", a);
            if (n != 3 || a[1] != "aaa" || a[2] != "bbb" || a[3] != "ccc") {
                print "KO";
            }
        }
    "#;
    run_test(Test::new(&script));

    let script = r#"
        BEGIN {
            n = split("a1b2c3d", a, /[0-9]/);
            if (n != 4 || a[1] != "a" || a[2] != "b" || a[3] != "c" || a[4] != "d") {
                print "KO", n, a[1], a[2], a[3], a[4];
            }
        }
    "#;
    run_test(Test::new(&script));

    let script = r#"
        BEGIN {
            FS = "[0-9]";
            n = split("a1b2c3d45", a);
            if (n != 6 || a[1] != "a" || a[2] != "b" || a[3] != "c" || a[4] != "d" || a[5] != "" || a[6] != "") {
                print "KO", n, a[1], a[2], a[3], a[4], a[5], a[6];
            }
        }"#;
    run_test(Test::new(&script));
}
