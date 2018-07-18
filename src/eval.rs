use parse::LispVal;
use std::collections::HashMap;

type EvalResult = Result<LispVal, String>;
type Scope = HashMap<String, LispVal>;

fn is_truthy(val: &LispVal) -> bool {
    match val {
        LispVal::Nil | LispVal::Bool(false) => false,
        _ => true,
    }
}

fn call_and(args: &[LispVal], _: &Scope) -> EvalResult {
    if args.is_empty() {
        return Ok(LispVal::Bool(true));
    }
    for arg in args {
        if !is_truthy(arg) {
            return Ok(arg.clone());
        }
    }
    Ok(args[args.len() - 1].clone())
}

fn call_or(args: &[LispVal], _: &Scope) -> EvalResult {
    for arg in args {
        if is_truthy(arg) {
            return Ok(arg.clone());
        }
    }
    Ok(args[args.len() - 1].clone())
}

fn call_plus(args: &[LispVal], _scope: &Scope) -> EvalResult {
    let mut plus_args: Vec<i32> = vec![];
    for arg in args {
        match arg {
            &LispVal::Number(i) => plus_args.push(i),
            v => return Err(format!("Unexpected arg to plus 3: {:?}", v)),
        }
    }
    Ok(LispVal::Number(plus_args.iter().sum()))
}

fn call_minus(args: &[LispVal], _scope: &Scope) -> EvalResult {
    let mut result: i32 = match &args[0] {
        LispVal::Number(i) => *i,
        x => return Err(format!("Unexpected arg to minus: {:?}", x)),
    };
    for arg in &args[1..] {
        match arg {
            LispVal::Number(i) => result -= i,
            x => return Err(format!("Unexpected arg to minus: {:?}", x)),
        }
    }
    Ok(LispVal::Number(result))
}

fn call_mult(args: &[LispVal], scope: &Scope) -> EvalResult {
    let mut result = match eval(args[0].clone(), &scope)? {
        LispVal::Number(i) => i,
        x => return Err(format!("Unexpected arg to mult: {:?}", x)),
    };
    for arg in &args[1..] {
        match eval(arg.clone(), &scope)? {
            LispVal::Number(i) => result *= i,
            x => return Err(format!("Unexpected arg to mult: {:?}", x)),
        }
    }
    Ok(LispVal::Number(result))
}

fn call_div(args: &[LispVal], scope: &Scope) -> EvalResult {
    let mut result = match eval(args[0].clone(), &scope)? {
        LispVal::Number(i) => i,
        x => return Err(format!("Unexpected arg to div: {:?}", x)),
    };
    for arg in &args[1..] {
        match eval(arg.clone(), &scope)? {
            LispVal::Number(i) => result /= i,
            x => return Err(format!("Unexpected arg to div: {:?}", x)),
        }
    }
    Ok(LispVal::Number(result))
}

fn call_let(args: &[LispVal], scope: &Scope) -> EvalResult {
    let mut new_scope = scope.clone();
    let bindings = match args[0] {
        LispVal::Vector(ref v) => v.clone(),
        _ => return Err("Expected vector as first argument to let".into()),
    };
    if bindings.len() % 2 != 0 {
        return Err("Odd number of binding args in let".into());
    }

    let expressions = args[1..].to_vec();
    if expressions.is_empty() {
        return Err("No expressions to eval in let".into());
    }

    let mut i = 0;
    while i < bindings.len() {
        let name = match bindings[i] {
            LispVal::Symbol(ref s) => s.clone(),
            _ => {
                return Err(format!(
                    "let cannot bind value to non-symbol {:?}",
                    &bindings[1]
                ))
            }
        };
        let value = bindings[i + 1].clone();
        let new_scope_copy = new_scope.clone();
        new_scope.insert(name, eval(value, &new_scope_copy)?);
        i += 2;
    }

    let mut result = Err("Nothing evaluated".into());
    for e in &expressions {
        result = eval(e.clone(), &new_scope);
    }
    result
}

fn call_function_by_symbol(symbol: &str, args: &[LispVal], scope: &Scope) -> EvalResult {
    match symbol {
        "and" => call_and(&args, &scope),
        "or" => call_or(&args, &scope),
        "+" => call_plus(&args, &scope),
        "-" => call_minus(&args, &scope),
        "*" => call_mult(&args, &scope),
        "/" => call_div(&args, &scope),
        "let" => call_let(&args, &scope),
        s => match scope.get(symbol) {
            Some(f) => call_lambda(f, args, scope),
            _ => Err(format!("undefined function {:?}", s)),
        },
    }
}

fn call_lambda(f: &LispVal, args: &[LispVal], scope: &Scope) -> EvalResult {
    let (params, body) = match f {
        LispVal::Lambda { params, body } => (params, body),
        _ => {
            return Err(format!(
                "unexpected LispVal while during call_lambda: {:?}",
                f
            ))
        }
    };
    let mut scope = scope.clone();
    for (p, arg) in params.iter().zip(args) {
        if let LispVal::Symbol(ref s) = p {
            scope.insert(s.clone(), arg.clone());
        } else {
            return Err(format!("invalid lambda param: {:?}", p));
        }
    }
    let b = body[0].clone();
    Ok(eval(b, &scope)?)
}

fn call_function(f: &LispVal, args: &[LispVal], scope: &Scope) -> EvalResult {
    match f {
        &LispVal::Symbol(ref fn_name) => call_function_by_symbol(fn_name, &args, &scope),
        lambda @ &LispVal::Lambda { .. } => call_lambda(lambda, args, scope),
        _ => Err(format!(
            "Don't know how to do that yet (call_function 2: {:?})",
            f
        )),
    }
}

pub fn eval(val: LispVal, scope: &Scope) -> Result<LispVal, String> {
    use LispVal::{Lambda, List, Number, Symbol, Vector};
    match val {
        List(vals) => {
            let f = &vals[0];
            let mut args = vec![];
            for arg in &vals[1..] {
                // Don't attempt to eval special form symbols
                let v = match *f {
                    Symbol(ref s) if s == "let" || s == "fn" => arg.clone(),
                    _ => eval(arg.clone(), &scope)?,
                };
                args.push(v);
            }
            call_function(&f, &args, &scope)
        }
        lambda @ Lambda { .. } => Ok(lambda),
        Vector(ref vals) => {
            let lst: Result<Vec<_>, String> =
                vals.iter().map(|v| eval(v.clone(), &scope)).collect();
            lst.map(Vector)
        }
        v @ Number(..) => Ok(v),
        Symbol(ref s) => match scope.get(s) {
            Some(v) => Ok(v.clone()),
            None => Err(format!("{} is not defined", s)),
        },
        _ => panic!("Don't know how to do that yet (eval: {:?})", val),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_scope() -> Scope {
        HashMap::new()
    }

    #[test]
    fn is_truthy_works() {
        assert_eq!(is_truthy(&LispVal::Bool(true)), true);
        assert_eq!(is_truthy(&LispVal::Bool(false)), false);
        assert_eq!(is_truthy(&LispVal::Nil), false);
        assert_eq!(is_truthy(&LispVal::Symbol("abcd".into())), true);
    }

    #[test]
    fn call_and_with_1_arg() {
        let args = vec![LispVal::Bool(true)];
        let result = call_and(&args, &new_scope());
        assert_eq!(result, Ok(LispVal::Bool(true)));
    }

    #[test]
    fn call_and_with_no_args() {
        let args = vec![];
        let result = call_and(&args, &new_scope());
        assert_eq!(result, Ok(LispVal::Bool(true)));
    }

    #[test]
    fn call_minus_works() {
        let args = vec![LispVal::Number(6), LispVal::Number(1), LispVal::Number(2)];
        let result = call_minus(&args, &new_scope());
        assert_eq!(result, Ok(LispVal::Number(3)));
    }

    #[test]
    fn call_let_works() {
        let args = vec![
            LispVal::Vector(vec![
                LispVal::Symbol("x".into()),
                LispVal::Number(1),
                LispVal::Symbol("y".into()),
                LispVal::Number(2),
            ]),
            LispVal::List(vec![
                LispVal::Symbol("+".into()),
                LispVal::Symbol("x".into()),
                LispVal::Symbol("y".into()),
            ]),
        ];
        let result = call_let(&args, &new_scope());
        assert_eq!(result, Ok(LispVal::Number(3)));
    }

    #[test]
    fn call_function_by_symbol_with_plus() {
        use LispVal::Number;
        let args = vec![Number(1), Number(2)];
        let expected = Number(3);
        let scope = HashMap::new();
        let result = call_function_by_symbol("+", &args, &scope);
        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn call_function_by_symbol_with_function_in_scope() {
        use LispVal::{Lambda, List, Number, Symbol};
        let args = vec![Number(1), Number(2)];
        let expected = Number(3);
        let mut scope = HashMap::new();
        let lambda = Lambda {
            params: vec![Symbol("x".to_string()), Symbol("y".to_string())],
            body: vec![List(vec![
                Symbol("+".to_string()),
                Symbol("x".to_string()),
                Symbol("y".to_string()),
            ])],
        };
        scope.insert("f".to_string(), lambda);
        let result = call_function_by_symbol("f", &args, &scope);
        assert_eq!(result, Ok(expected));
    }
}
