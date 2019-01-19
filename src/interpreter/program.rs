use crate::{
    errors::EvaluationError,
    interpreter::{functions::Functions, record::Record, variables::Variables, Eval},
    parser::ast::{Item, Program},
};

impl Eval for Program {
    type EvalResult = ();
    fn eval(
        &self,
        vars: &mut Variables,
        record: &mut Record,
        funcs: &mut Functions,
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
        interpreter::{value::ExprValue, Context},
        parser::program::get_program,
    };

    fn eval_program(prog: &Program, cxt: &mut Context) -> Result<(), EvaluationError> {
        prog.eval(&mut cxt.vars, &mut cxt.record, &mut cxt.funcs)
    }

    #[test]
    fn invalid_function_call() {
        let mut cxt = Context::new();

        let prog = get_program(
            r#"function adder(var) { return 1 }
               function adder(var) { return 2 }
               { a = 42; adder(2); }"#,
        );
        let err = cxt.load_program(&prog).unwrap_err();
        assert_eq!(err, EvaluationError::DuplicateFunction("adder".to_owned()));

        cxt.clean();
        let prog = get_program("function adder(var) { return 1 } { a = 42; adder(2, 3); }");
        cxt.load_program(&prog).unwrap();
        let err = eval_program(&prog, &mut cxt).unwrap_err();
        assert_eq!(
            err,
            EvaluationError::TooManyArguments("adder".to_owned(), 2, 1)
        );

        cxt.clean();
        let prog = get_program("function f(a) { a = 1 } { arr[0] = 42; f(arr) }");
        cxt.load_program(&prog).unwrap();
        let err = eval_program(&prog, &mut cxt).unwrap_err();
        assert_eq!(err, EvaluationError::UseArrayInScalarContext);

        cxt.clean();
        let prog = get_program("function f(a) { a[0] = 1 } { f(42) }");
        cxt.load_program(&prog).unwrap();
        let err = eval_program(&prog, &mut cxt).unwrap_err();
        assert_eq!(err, EvaluationError::UseScalarAsArray);
    }

    #[test]
    fn custom_functions_scalar() {
        let mut cxt = Context::new();

        // update a global variable
        let prog = get_program("function adder(var) { a += var } { a = 42; adder(2); }");
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a", None), Ok(ExprValue::from(44)));

        // initialize a global variable and mutate a local variable with the same name
        // as a global variable
        cxt.clean();
        let prog = get_program("function adder(a) { a += 10; b = a; } { a = 1; adder(5); }");
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a", None), Ok(ExprValue::from(1)));
        assert_eq!(cxt.vars.get("b", None), Ok(ExprValue::from(15)));

        // assign return value of a function
        cxt.clean();
        let prog = get_program(
            r#"function f1(a) { a += 10 }
               function f2(a) { a += 20; return a; }
               { a = f1(5); b = f2(5); }"#,
        );
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a", None), Ok(ExprValue::Uninitialised));
        assert_eq!(cxt.vars.get("b", None), Ok(ExprValue::from(25)));

        // ensure function-locality of variables
        cxt.clean();
        let prog = get_program(
            r#"function f1(a) { a += 10; return a; }
               function f2(a) { a += 20; return f1(1); }
               { b = f2(5); }"#,
        );
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("b", None), Ok(ExprValue::from(11)));

        // declare the variable "b" scoped to the function
        cxt.clean();
        let prog = get_program("function adder(a, b) { a += 10; b = a; } { a = 1; adder(5); }");
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a", None), Ok(ExprValue::from(1)));
        assert_eq!(cxt.vars.get("b", None), Ok(ExprValue::Uninitialised));

        // call another function from a function to verify locals are properly handled
        cxt.clean();
        let prog = get_program(
            r#"function f1(a) { a += 10; return a }
               function f2(a) { a += f1(a); return a; }
               { a = f2(5); }"#,
        );
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a", None), Ok(ExprValue::from(20)));

        // variable local to a function should be overridden if used in loop
        cxt.clean();
        let prog = get_program(
            r#"function f(a) {
                   a += 42;
                   for (a = 0; a < 10; a++) continue;
                   return a;
               }
               { a = f(5) }"#,
        );
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a", None), Ok(ExprValue::from(10)));
    }

    #[test]
    fn custom_functions_arrays() {
        let mut cxt = Context::new();

        // update global array
        let prog = get_program("function adder(var) { a[0] += var } { a[0] = 42; adder(2); }");
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("a", Some("0")), Ok(ExprValue::from(44)));

        // update local array
        cxt.clean();
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
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("result", None), Ok(ExprValue::from(8)));

        // pass array as argument
        cxt.clean();
        let prog = get_program(
            r#"
            function join(arr, c) {
                for (i in arr) {
                    c = arr[i] ", ";
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
        cxt.load_program(&prog).unwrap();
        eval_program(&prog, &mut cxt).unwrap();
        assert_eq!(cxt.vars.get("result", None), Ok(ExprValue::from(11)));
    }
}
