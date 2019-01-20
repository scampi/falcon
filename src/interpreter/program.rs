use crate::{
    errors::EvaluationError,
    interpreter::{functions::Functions, record::Record, variables::Variables, Eval},
    parser::ast::{Item, Program},
};

impl Eval for Program {
    type EvalResult = ();
    fn eval<'a>(
        &self,
        vars: &'a mut Variables,
        record: &mut Record,
        funcs: &Functions,
    ) -> Result<(), EvaluationError> {
        for item in &self.items {
            if let Item::PatternAction(pattern, stmts) = item {
                match pattern {
                    None => {
                        for stmt in &stmts.0 {
                            stmt.eval(vars, record, funcs)?;
                        }
                    },
                    Some(pattern) => unimplemented!(),
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        interpreter::{value::Value, Runtime},
        parser::program::get_program,
    };

    fn eval_program(prog: &Program, rt: &mut Runtime) -> Result<(), EvaluationError> {
        prog.eval(&mut rt.vars, &mut rt.record, &rt.funcs)
    }

    #[test]
    fn invalid_function_call() {
        let mut rt = Runtime::new();

        let prog = get_program(
            r#"function adder(var) { return 1 }
               function adder(var) { return 2 }
               { a = 42; adder(2); }"#,
        );
        let err = rt.load_program(&prog).unwrap_err();
        assert_eq!(err, EvaluationError::DuplicateFunction("adder".to_owned()));

        rt.clean();
        let prog = get_program("function adder(var) { return 1 } { a = 42; adder(2, 3); }");
        rt.load_program(&prog).unwrap();
        let err = eval_program(&prog, &mut rt).unwrap_err();
        assert_eq!(
            err,
            EvaluationError::TooManyArguments("adder".to_owned(), 2, 1)
        );

        rt.clean();
        let prog = get_program("function f(a) { a = 1 } { arr[0] = 42; f(arr) }");
        rt.load_program(&prog).unwrap();
        let err = eval_program(&prog, &mut rt).unwrap_err();
        assert_eq!(err, EvaluationError::UseArrayInScalarContext);

        rt.clean();
        let prog = get_program("function f(a) { a[0] = 1 } { f(42) }");
        rt.load_program(&prog).unwrap();
        let err = eval_program(&prog, &mut rt).unwrap_err();
        assert_eq!(err, EvaluationError::UseScalarAsArray);
    }

    #[test]
    fn custom_functions_scalar() {
        let mut rt = Runtime::new();

        // update a global variable
        let prog = get_program("function adder(var) { a += var } { a = 42; adder(2); }");
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("a", None), Ok(Value::from(44)));

        // initialize a global variable and mutate a local variable with the same name
        // as a global variable
        rt.clean();
        let prog = get_program("function adder(a) { a += 10; b = a; } { a = 1; adder(5); }");
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("a", None), Ok(Value::from(1)));
        assert_eq!(rt.vars.get("b", None), Ok(Value::from(15)));

        // assign return value of a function
        rt.clean();
        let prog = get_program(
            r#"function f1(a) { a += 10 }
               function f2(a) { a += 20; return a; }
               { a = f1(5); b = f2(5); }"#,
        );
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("a", None), Ok(Value::Uninitialised));
        assert_eq!(rt.vars.get("b", None), Ok(Value::from(25)));

        // ensure function-locality of variables
        rt.clean();
        let prog = get_program(
            r#"function f1(a) { a += 10; return a; }
               function f2(a) { a += 20; return f1(1); }
               { b = f2(5); }"#,
        );
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("b", None), Ok(Value::from(11)));

        // declare the variable "b" scoped to the function
        rt.clean();
        let prog = get_program("function adder(a, b) { a += 10; b = a; } { a = 1; adder(5); }");
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("a", None), Ok(Value::from(1)));
        assert_eq!(rt.vars.get("b", None), Ok(Value::Uninitialised));

        // call another function from a function to verify locals are properly handled
        rt.clean();
        let prog = get_program(
            r#"function f1(a) { a += 10; return a }
               function f2(a) { a += f1(a); return a; }
               { a = f2(5); }"#,
        );
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("a", None), Ok(Value::from(20)));

        // variable local to a function should be overridden if used in loop
        rt.clean();
        let prog = get_program(
            r#"function f(a) {
                   a += 42;
                   for (a = 0; a < 10; a++) continue;
                   return a;
               }
               { a = f(5) }"#,
        );
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("a", None), Ok(Value::from(10)));
    }

    #[test]
    fn custom_functions_arrays() {
        let mut rt = Runtime::new();

        // update global array
        let prog = get_program("function adder(var) { a[0] += var } { a[0] = 42; adder(2); }");
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("a", Some("0")), Ok(Value::from(44)));

        // update local array
        rt.clean();
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
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("result", None), Ok(Value::from(8)));

        // pass array as argument
        rt.clean();
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
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(
            rt.vars.get("result", None),
            Ok(Value::from(11)),
            "{:?}",
            rt.vars
        );
        assert_eq!(
            rt.vars.get("c", None),
            Ok(Value::Uninitialised),
            "{:?}",
            rt.vars
        );

        // mutate an array by reference
        rt.clean();
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
        rt.load_program(&prog).unwrap();
        eval_program(&prog, &mut rt).unwrap();
        assert_eq!(rt.vars.get("my_array", Some("0")), Ok(Value::from(6)));
    }

    #[test]
    fn assign_array() {
        let mut rt = Runtime::new();

        let prog = get_program("{ a[0] = 42; b = a; }");
        rt.load_program(&prog).unwrap();
        let err = eval_program(&prog, &mut rt).unwrap_err();
        assert_eq!(err, EvaluationError::UseArrayInScalarContext);
    }

    #[test]
    fn use_scalar_with_for_in() {
        let mut rt = Runtime::new();

        let prog = get_program("{ a = 42; for (i in a) continue; }");
        rt.load_program(&prog).unwrap();
        let err = eval_program(&prog, &mut rt).unwrap_err();
        assert_eq!(err, EvaluationError::UseScalarAsArray);
    }
}
