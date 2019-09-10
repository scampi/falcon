//! Evaluate the AWK script against the input lines.
use crate::{
    errors::EvaluationError,
    parser::ast::{Item, Pattern, Program},
};
use std::io::Write;

mod expr;
pub mod functions;
mod record;
mod rnd;
mod stmt;
mod value;
pub mod variables;

/// Runtime contains all the components needed for the execution of a script.
#[derive(Debug)]
pub struct Runtime<'a, Output>
where
    Output: Write,
{
    /// The stream to print the script's output.
    output: &'a mut Output,
    /// The set of variables used in the script.
    vars: variables::Variables,
    /// The current line split on a separator.
    record: record::Record,
    /// User-defined functions and builtins.
    funcs: functions::Functions,
    /// List of files to redirect the output to.
    redirs: stmt::redirections::Redirections,
    /// Random functions of AWK.
    rnd: rnd::Rnd,
    /// Set of `BEGIN` patterns.
    begin_patterns: Vec<Item>,
    /// Set of `END` patterns.
    end_patterns: Vec<Item>,
    /// Set of patterns that are neither `BEGIN` nor `END`.
    patterns: Vec<EvalItem>,
}

/// A `Runtime`-version that allows to mutate one or more of its components.
pub struct RuntimeMut<'a, Output>
where
    Output: Write,
{
    output: &'a mut Output,
    vars: &'a mut variables::Variables,
    record: &'a mut record::Record,
    funcs: &'a functions::Functions,
    redirs: &'a mut stmt::redirections::Redirections,
    rnd: &'a mut rnd::Rnd,
}

impl<'a, Output> RuntimeMut<'a, Output>
where
    Output: Write,
{
    fn new(
        output: &'a mut Output,
        vars: &'a mut variables::Variables,
        record: &'a mut record::Record,
        funcs: &'a functions::Functions,
        redirs: &'a mut stmt::redirections::Redirections,
        rnd: &'a mut rnd::Rnd,
    ) -> RuntimeMut<'a, Output> {
        RuntimeMut {
            output,
            vars,
            record,
            funcs,
            redirs,
            rnd,
        }
    }
}

/// An `Item` to be evaluated against the current input line.
#[derive(Debug)]
struct EvalItem {
    item: Item,
    /// Whether or not the item is within a range pattern. If true, the `end`
    /// pattern is matched against.
    in_range: bool,
}

impl EvalItem {
    fn new(item: Item) -> EvalItem {
        EvalItem {
            item,
            in_range: false,
        }
    }
}

impl<'a, Output> Runtime<'a, Output>
where
    Output: Write,
{
    /// Creates a Runtime for the given AWK program.
    pub fn new(
        program: Program,
        output: &'a mut Output,
    ) -> Result<Runtime<'a, Output>, EvaluationError> {
        let mut rt = Runtime {
            output,
            patterns: Vec::with_capacity(program.items.len()),
            begin_patterns: Vec::with_capacity(program.items.len()),
            end_patterns: Vec::with_capacity(program.items.len()),
            vars: variables::Variables::new(),
            record: record::Record::new(),
            funcs: functions::Functions::new(),
            redirs: stmt::redirections::Redirections::new(),
            rnd: rnd::Rnd::new(),
        };
        for item in program.items.into_iter() {
            match item {
                Item::FunctionDef(..) => rt.funcs.load_function(item)?,
                Item::PatternAction(Some(Pattern::Begin), _) => rt.begin_patterns.push(item),
                Item::PatternAction(Some(Pattern::End), _) => rt.end_patterns.push(item),
                Item::PatternAction(..) => rt.patterns.push(EvalItem::new(item)),
            }
        }
        Ok(rt)
    }

    /// Sets the next input line.
    pub fn set_next_record(&mut self, record: String) {
        self.record.update_record(&mut self.vars, record);
        // update record numbers
        self.vars.fnr += 1;
        self.vars.nr += 1;
    }

    /// Evaluates the `BEGIN` patterns in order they appear in the script
    /// against the current input line.
    pub fn execute_begin_patterns(&mut self) -> Result<(), EvaluationError> {
        let mut rt_mut = RuntimeMut::new(
            &mut self.output,
            &mut self.vars,
            &mut self.record,
            &self.funcs,
            &mut self.redirs,
            &mut self.rnd,
        );
        for pattern in &self.begin_patterns {
            if let Item::PatternAction(Some(Pattern::Begin), stmts) = pattern {
                for stmt in &stmts.0 {
                    stmt.eval(&mut rt_mut)?;
                }
            } else {
                unreachable!()
            }
        }
        Ok(())
    }

    /// Evaluates the `END` patterns in order they appear in the script against
    /// the current input line.
    pub fn execute_end_patterns(&mut self) -> Result<(), EvaluationError> {
        let mut rt_mut = RuntimeMut::new(
            &mut self.output,
            &mut self.vars,
            &mut self.record,
            &self.funcs,
            &mut self.redirs,
            &mut self.rnd,
        );
        for pattern in &self.end_patterns {
            if let Item::PatternAction(Some(Pattern::End), stmts) = pattern {
                for stmt in &stmts.0 {
                    stmt.eval(&mut rt_mut)?;
                }
            } else {
                unreachable!()
            }
        }
        Ok(())
    }

    /// Evaluates all the patterns that are not `BEGIN` or `END` against the
    /// current input line.
    pub fn execute_main_patterns(&mut self) -> Result<(), EvaluationError> {
        let mut rt_mut = RuntimeMut::new(
            &mut self.output,
            &mut self.vars,
            &mut self.record,
            &self.funcs,
            &mut self.redirs,
            &mut self.rnd,
        );
        for eval_item in self.patterns.iter_mut() {
            if let Item::PatternAction(pattern, stmts) = &eval_item.item {
                match pattern {
                    None => {
                        for stmt in &stmts.0 {
                            stmt.eval(&mut rt_mut)?;
                        }
                    },
                    Some(Pattern::Expr(expr)) => {
                        if expr.eval(&mut rt_mut)?.as_bool() {
                            for stmt in &stmts.0 {
                                stmt.eval(&mut rt_mut)?;
                            }
                        }
                    },
                    Some(Pattern::Range(start, end)) => {
                        let execute = eval_item.in_range || start.eval(&mut rt_mut)?.as_bool();
                        if execute {
                            eval_item.in_range = !end.eval(&mut rt_mut)?.as_bool();
                            for stmt in &stmts.0 {
                                stmt.eval(&mut rt_mut)?;
                            }
                        }
                    },
                    _ => unreachable!(),
                }
            }
        }
        Ok(())
    }
}

/// Evaluates an element of the AWK script against the given Runtime.
trait Eval {
    type EvalResult;
    fn eval<Output>(
        &self,
        rt: &mut RuntimeMut<'_, Output>,
    ) -> Result<Self::EvalResult, EvaluationError>
    where
        Output: Write;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        interpreter::{value::Value, Runtime},
        parser::program::get_program,
    };
    use std::io::Cursor;

    #[test]
    fn invalid_function_call() {
        let prog = get_program(
            r#"function adder(var) { return 1 }
               function adder(var) { return 2 }
               { a = 42; adder(2); }"#,
        );

        let mut out = Cursor::new(Vec::new());
        match Runtime::new(prog, &mut out).unwrap_err() {
            EvaluationError::DuplicateFunction(ref name) if name == "adder" => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };

        let prog = get_program("function adder(var) { return 1 } { a = 42; adder(2, 3); }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        match rt.execute_main_patterns().unwrap_err() {
            EvaluationError::InvalidNumberOfArguments(ref name, 2, 1) if name == "adder" => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };

        let prog = get_program("function f(a) { a = 1 } { arr[0] = 42; f(arr) }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        match rt.execute_main_patterns().unwrap_err() {
            EvaluationError::UseArrayInScalarContext => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };

        let prog = get_program("function f(a) { a[0] = 1 } { f(42) }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        match rt.execute_main_patterns().unwrap_err() {
            EvaluationError::UseScalarAsArray => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };
    }

    #[test]
    fn custom_functions_scalar() {
        // update a global variable
        let prog = get_program("function adder(var) { a += var } { a = 42; adder(2); }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(44));

        // initialize a global variable and mutate a local variable with the same name
        // as a global variable
        let prog = get_program("function adder(a) { a += 10; b = a; } { a = 1; adder(5); }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(1));
        assert_eq!(rt.vars.get("b", None).unwrap(), Value::from(15));

        // assign return value of a function
        let prog = get_program(
            r#"function f1(a) { a += 10 }
               function f2(a) { a += 20; return a; }
               { a = f1(5); b = f2(5); }"#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::Uninitialised);
        assert_eq!(rt.vars.get("b", None).unwrap(), Value::from(25));

        // ensure function-locality of variables
        let prog = get_program(
            r#"function f1(a) { a += 10; return a; }
               function f2(a) { a += 20; return f1(1); }
               { b = f2(5); }"#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("b", None).unwrap(), Value::from(11));

        // declare the variable "b" scoped to the function
        let prog = get_program("function adder(a, b) { a += 10; b = a; } { a = 1; adder(5); }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(1));
        assert_eq!(rt.vars.get("b", None).unwrap(), Value::Uninitialised);

        // call another function from a function to verify locals are properly handled
        let prog = get_program(
            r#"function f1(a) { a += 10; return a }
               function f2(a) { a += f1(a); return a; }
               { a = f2(5); }"#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(20));

        // variable local to a function should be overridden if used in loop
        let prog = get_program(
            r#"function f(a) {
                   a += 42;
                   for (a = 0; a < 10; a++) continue;
                   return a;
               }
               { a = f(5) }"#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("a", None).unwrap(), Value::from(10));
    }

    #[test]
    fn custom_functions_arrays() {
        // update global array
        let prog = get_program("function adder(var) { a[0] += var } { a[0] = 42; adder(2); }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("a", Some("0")).unwrap(), Value::from(44));

        // update local array
        let prog = get_program(
            r#"
            function join(a, b, c, arr) {
                arr[0] = a;
                arr[1] = b;
                for (i in arr) {
                    c += arr[i];
                }
                return c;
            }
            { result = join(3, 5); }
            "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("result", None).unwrap(), Value::from(8));

        // pass array as argument
        let prog = get_program(
            r#"
            function join(arr, c) {
                for (i in arr) {
                    c += arr[i];
                }
                return c;
            }
            {
                my_array[0] = 5;
                my_array[1] = 6;
                result = join(my_array);
            }
            "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(
            rt.vars.get("result", None).unwrap(),
            Value::from(11),
            "{:?}",
            rt.vars
        );
        assert_eq!(
            rt.vars.get("c", None).unwrap(),
            Value::Uninitialised,
            "{:?}",
            rt.vars
        );

        // mutate an array by reference
        let prog = get_program(
            r#"
            function f(arr) {
                arr[0]++;
            }
            {
                my_array[0] = 5;
                f(my_array);
            }
            "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(rt.vars.get("my_array", Some("0")).unwrap(), Value::from(6));

        // delete an element of an array by reference
        let prog = get_program(
            r#"
            function f(arr) {
                delete arr[0];
            }
            {
                my_array[0] = 5;
                my_array[1] = 6;
                f(my_array);
            }
            "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_main_patterns().unwrap();
        assert_eq!(
            rt.vars.get("my_array", Some("0")).unwrap(),
            Value::Uninitialised
        );
        assert_eq!(rt.vars.get("my_array", Some("1")).unwrap(), Value::from(6));
    }

    #[test]
    fn assign_array() {
        let prog = get_program("{ a[0] = 42; b = a; }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        match rt.execute_main_patterns().unwrap_err() {
            EvaluationError::UseArrayInScalarContext => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };
    }

    #[test]
    fn use_scalar_with_for_in() {
        let prog = get_program("{ a = 42; for (i in a) continue; }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        match rt.execute_main_patterns().unwrap_err() {
            EvaluationError::UseScalarAsArray => (),
            err @ _ => panic!("Unexpected error: {}", err),
        };
    }

    #[test]
    fn special_patterns() {
        // ensure begin/end patterns are called when expected
        let prog = get_program(
            r#"
            BEGIN { firstname = "john" }
            { c = firstname ", " }
            END { fullname = c "connor" }
            "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        rt.execute_begin_patterns().unwrap();
        rt.execute_main_patterns().unwrap();
        rt.execute_end_patterns().unwrap();

        assert_eq!(
            rt.vars.get("firstname", None).unwrap(),
            Value::from("john".to_owned())
        );
        assert_eq!(
            rt.vars.get("c", None).unwrap(),
            Value::from("john, ".to_owned())
        );
        assert_eq!(
            rt.vars.get("fullname", None).unwrap(),
            Value::from("john, connor".to_owned())
        );

        // access FNR parameter
        let input = "1\n2";
        let prog = get_program(
            r#"
            BEGIN { begin_fnr = FNR }
            END { end_fnr = FNR }
            "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        rt.execute_begin_patterns().unwrap();
        for line in input.lines() {
            rt.set_next_record(line.to_owned());
            rt.execute_main_patterns().unwrap();
        }
        rt.execute_end_patterns().unwrap();

        assert_eq!(rt.vars.get("begin_fnr", None).unwrap(), Value::from(0));
        assert_eq!(rt.vars.get("end_fnr", None).unwrap(), Value::from(2));

        // access NF parameter
        let input = "a b c\na b";
        let prog = get_program(
            r#"
            BEGIN { begin_nf = NF }
            { inside_nf[NR] = NF }
            END { end_nf = NF }
            "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        rt.execute_begin_patterns().unwrap();
        for line in input.lines() {
            rt.set_next_record(line.to_owned());
            rt.execute_main_patterns().unwrap();
        }
        rt.execute_end_patterns().unwrap();

        assert_eq!(rt.vars.get("begin_nf", None).unwrap(), Value::from(0));
        assert_eq!(rt.vars.get("inside_nf", Some("1")).unwrap(), Value::from(3));
        assert_eq!(rt.vars.get("inside_nf", Some("2")).unwrap(), Value::from(2));
        assert_eq!(rt.vars.get("end_nf", None).unwrap(), Value::from(2));
    }

    #[test]
    fn expr_pattern() {
        let input = "pig\ndog\nsheep";
        let prog = get_program(
            r#"
            /sheep/ { name[NR] = "shaun" }
            /pig/ { name[NR] = "peppa" }
            "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        for line in input.lines() {
            rt.set_next_record(line.to_owned());
            rt.execute_main_patterns().unwrap();
        }

        let mut keys = rt.vars.array_keys("name").unwrap();
        keys.sort_unstable();
        assert_eq!(keys, vec!["1".to_owned(), "3".to_owned()]);
        assert_eq!(
            rt.vars.get("name", Some("1")).unwrap(),
            Value::from("peppa".to_owned())
        );
        assert_eq!(
            rt.vars.get("name", Some("3")).unwrap(),
            Value::from("shaun".to_owned())
        );
    }

    #[test]
    fn range_pattern() {
        // 2 consecutive ranges
        let input = "0\n1\n2\n3\n4\n1\n2\n3\n4";
        let prog = get_program(r#"/1/,/3/ { c = c " " NR ":" $0 }"#);
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        for line in input.lines() {
            rt.set_next_record(line.to_owned());
            rt.execute_main_patterns().unwrap();
        }
        assert_eq!(
            rt.vars.get("c", None).unwrap(),
            Value::from(" 2:1 3:2 4:3 6:1 7:2 8:3".to_owned())
        );

        // range starting and ending on the same line
        let input = r#"a b
aa bb
bb cc
cc dd
a b
aa bb
bb cc
cc dd
a b"#;
        let prog = get_program(
            r#"
                /^bb/,/cc$/
                { $2 = "john connor"; c[NR] = $0 }
            "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        for line in input.lines() {
            rt.set_next_record(line.to_owned());
            rt.execute_main_patterns().unwrap();
        }
        let mut keys = rt.vars.array_keys("c").unwrap();
        keys.sort_unstable();
        assert_eq!(keys, vec!["3".to_owned(), "7".to_owned()]);
        assert_eq!(
            rt.vars.get("c", Some("3")).unwrap(),
            Value::from("bb john connor".to_owned())
        );
        assert_eq!(
            rt.vars.get("c", Some("7")).unwrap(),
            Value::from("bb john connor".to_owned())
        );
    }

    #[test]
    fn multiple_whitespaces() {
        let input = "   \taaa bbb       ccc   \t  ";
        let prog = get_program("{ a = $1; b = $2; c = $3 }");
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        rt.set_next_record(input.to_owned());
        rt.execute_main_patterns().unwrap();
        assert_eq!(
            rt.vars.get("a", None).unwrap(),
            Value::from("aaa".to_owned())
        );
        assert_eq!(
            rt.vars.get("b", None).unwrap(),
            Value::from("bbb".to_owned())
        );
        assert_eq!(
            rt.vars.get("c", None).unwrap(),
            Value::from("ccc".to_owned())
        );
    }

    #[test]
    fn custom_fs_value() {
        let prog_str = |fs: &str| {
            format!(
                "BEGIN {{ FS=\"{}\" }} {{ for (i = 1; i <= NF; i++) arr[i] = $i }}",
                fs
            )
        };

        let input = "a.b";
        let prog = get_program(&prog_str("."));
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_begin_patterns().unwrap();
        rt.set_next_record(input.to_owned());
        rt.execute_main_patterns().unwrap();
        let keys = rt.vars.array_keys("arr").unwrap();
        assert_eq!(keys.len(), 2);
        assert_eq!(
            rt.vars.get("arr", Some("1")).unwrap(),
            Value::from("a".to_owned())
        );
        assert_eq!(
            rt.vars.get("arr", Some("2")).unwrap(),
            Value::from("b".to_owned())
        );

        let input = "aahereayouaaaawereaaaaaa";
        let prog = get_program(&prog_str("a+"));
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_begin_patterns().unwrap();
        rt.set_next_record(input.to_owned());
        rt.execute_main_patterns().unwrap();
        let keys = rt.vars.array_keys("arr").unwrap();
        assert_eq!(keys.len(), 3);
        assert_eq!(
            rt.vars.get("arr", Some("1")).unwrap(),
            Value::from("here".to_owned())
        );
        assert_eq!(
            rt.vars.get("arr", Some("2")).unwrap(),
            Value::from("you".to_owned())
        );
        assert_eq!(
            rt.vars.get("arr", Some("3")).unwrap(),
            Value::from("were".to_owned())
        );

        let input = "abcthisadcarcisaechere";
        let prog = get_program(&prog_str("a.c"));
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_begin_patterns().unwrap();
        rt.set_next_record(input.to_owned());
        rt.execute_main_patterns().unwrap();
        let keys = rt.vars.array_keys("arr").unwrap();
        assert_eq!(keys.len(), 4);
        assert_eq!(
            rt.vars.get("arr", Some("1")).unwrap(),
            Value::from("this".to_owned())
        );
        assert_eq!(
            rt.vars.get("arr", Some("2")).unwrap(),
            Value::from(String::new())
        );
        assert_eq!(
            rt.vars.get("arr", Some("3")).unwrap(),
            Value::from("is".to_owned())
        );
        assert_eq!(
            rt.vars.get("arr", Some("4")).unwrap(),
            Value::from("here".to_owned())
        );

        let input = "abcthisadcarcisaechere";
        let prog = get_program(&prog_str("(a.c)+"));
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();
        rt.execute_begin_patterns().unwrap();
        rt.set_next_record(input.to_owned());
        rt.execute_main_patterns().unwrap();
        let keys = rt.vars.array_keys("arr").unwrap();
        assert_eq!(keys.len(), 3);
        assert_eq!(
            rt.vars.get("arr", Some("1")).unwrap(),
            Value::from("this".to_owned())
        );
        assert_eq!(
            rt.vars.get("arr", Some("2")).unwrap(),
            Value::from("is".to_owned())
        );
        assert_eq!(
            rt.vars.get("arr", Some("3")).unwrap(),
            Value::from("here".to_owned())
        );
    }

    #[test]
    fn function_calls_array_references() {
        let prog = get_program(
            r#"
            function test1(c) {
                test2(c)
            }

            function test2(b, a) {
                a[0]--;
                b[0]++;
            }

            BEGIN { a[0] = 1; test1(a) }
        "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        rt.execute_begin_patterns().unwrap();
        assert_eq!(rt.vars.get("a", Some("0")).unwrap(), Value::from(2));
    }

    #[test]
    fn function_calls_array_references_delete() {
        let prog = get_program(
            r#"
            function test1(c) {
                test2(c)
            }

            function test2(b, a) {
                delete b[0];
                a[0]--;
            }

            BEGIN { a[0] = 1; test1(a) }
        "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        rt.execute_begin_patterns().unwrap();
        assert_eq!(rt.vars.get("a", Some("0")).unwrap(), Value::Uninitialised);
    }

    #[test]
    fn function_calls_local_array() {
        let prog = get_program(
            r#"
            function test1(c) {
                test2(c)
            }

            function test2(b, a) {
                a[0]--;
                test3(b, a)
            }

            function test3(b, a) {
                a[0]--;
                b[0]++;
            }

            BEGIN { a[0] = 1; test1(a) }
        "#,
        );
        let mut out = Cursor::new(Vec::new());
        let mut rt = Runtime::new(prog, &mut out).unwrap();

        rt.execute_begin_patterns().unwrap();
        assert_eq!(rt.vars.get("a", Some("0")).unwrap(), Value::from(2));
    }
}
