use std::collections::HashMap;
use parse::LispVal;

type EvalResult = Result<LispVal, String>;
type Scope = HashMap<String, LispVal>;

fn new_scope() -> Scope {
    HashMap::new()
}

fn is_truthy(val: &LispVal) -> bool {
    match val {
        &LispVal::Nil | &LispVal::Bool(false) => false,
        _ => true,
    }
}

#[test]
fn test_is_truthy() {
    assert_eq!(is_truthy(&LispVal::Bool(true)), true);
    assert_eq!(is_truthy(&LispVal::Bool(false)), false);
    assert_eq!(is_truthy(&LispVal::Nil), false);
    assert_eq!(is_truthy(&LispVal::Symbol("abcd".into())), true);
}

fn call_and(args: &Vec<LispVal>, _: &Scope) -> EvalResult {
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

#[test]
fn test_call_and_with_1_arg() {
    let args = vec![LispVal::Bool(true)];
    let result = call_and(&args, &new_scope());
    assert_eq!(result, Ok(LispVal::Bool(true)));
}

#[test]
fn test_call_and_with_no_args() {
    let args = vec![];
    let result = call_and(&args, &new_scope());
    assert_eq!(result, Ok(LispVal::Bool(true)));
}

fn call_or(args: &Vec<LispVal>, _: &Scope) -> EvalResult {
    for arg in args {
        if is_truthy(arg) {
            return Ok(arg.clone());
        }
    }
    Ok(args[args.len() - 1].clone())
}

fn call_plus(args: &Vec<LispVal>, scope: &Scope) -> EvalResult {
    let mut plus_args: Vec<i32> = vec![];
    for arg in args {
        match arg {
            &LispVal::Number(i) => plus_args.push(i),
            lst@&LispVal::List(..) => {
                let result = eval(lst.clone(), &scope)?;
                match result {
                    LispVal::Number(i) => plus_args.push(i),
                    v => return Err(format!("Unexpected arg to plus 1: {:?}", v)),
                }
            }
            s@&LispVal::Symbol(..) => {
                match eval(s.clone(), &scope)? {
                    LispVal::Number(i) => plus_args.push(i),
                    v => return Err(format!("Unexpected arg to plus 2: {:?}", v)),
                }

            }
            v => return Err(format!("Unexpected arg to plus 3: {:?}", v)),
        }
    }
    Ok(LispVal::Number(plus_args.iter().fold(0, |acc, i| acc + i)))
}

fn call_minus(args: &Vec<LispVal>, scope: &Scope) -> EvalResult {
    let mut result = match eval(args[0].clone(), &scope)? {
        LispVal::Number(i) => i,
        x => return Err(format!("Unexpected arg to minus: {:?}", x)),
    };
    for arg in args[1..].iter() {
        match eval(arg.clone(), &scope)? {
            LispVal::Number(ref i) => result -= i,
            x => return Err(format!("Unexpected arg to minus: {:?}", x)),
        }
    }
    Ok(LispVal::Number(result))
}

fn call_mult(args: &Vec<LispVal>, scope: &Scope) -> EvalResult {
    let mut result = match eval(args[0].clone(), &scope)? {
        LispVal::Number(i) => i,
        x => return Err(format!("Unexpected arg to mult: {:?}", x)),
    };
    for arg in args[1..].iter() {
        match eval(arg.clone(), &scope)? {
            LispVal::Number(ref i) => result *= i,
            x => return Err(format!("Unexpected arg to mult: {:?}", x)),
        }
    }
    Ok(LispVal::Number(result))
}

fn call_div(args: &Vec<LispVal>, scope: &Scope) -> EvalResult {
    let mut result = match eval(args[0].clone(), &scope)? {
        LispVal::Number(i) => i,
        x => return Err(format!("Unexpected arg to div: {:?}", x)),
    };
    for arg in args[1..].iter() {
        match eval(arg.clone(), &scope)? {
            LispVal::Number(ref i) => result /= i,
            x => return Err(format!("Unexpected arg to div: {:?}", x)),
        }
    }
    Ok(LispVal::Number(result))
}

fn call_let(args: &Vec<LispVal>, scope: &Scope) -> EvalResult {
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
        let name = match &bindings[i] {
            &LispVal::Symbol(ref s) => s.clone(),
            _ => return Err(format!("let cannot bind value to non-symbol {:?}", &bindings[1])),
        };
        let value = bindings[i+1].clone();
        let new_scope_copy = new_scope.clone();
        new_scope.insert(name, eval(value, &new_scope_copy)?);
        i += 2;
    }

    let mut result = Err("Nothing evaluated".into());
    for e in expressions.iter() {
        result = eval(e.clone(), &new_scope);
    }
    result
}

#[test]
fn test_call_let() {
    let args = vec![
        LispVal::Vector(vec![
            LispVal::Symbol("x".into()),
            LispVal::Number(1),
            LispVal::Symbol("y".into()),
            LispVal::Number(2),
        ]),
        LispVal::List(vec![
            LispVal::Function("+".into()),
            LispVal::Symbol("x".into()),
            LispVal::Symbol("y".into()),
        ]),
    ];
    let result = call_let(&args, &new_scope());
    assert_eq!(result, Ok(LispVal::Number(3)));
}

fn call_function(f: &LispVal, args: &Vec<LispVal>, scope: &Scope) -> EvalResult {
    match f {
        &LispVal::Function(ref fn_name) => match fn_name.as_ref() {
            "and" => call_and(&args, &scope),
            "or" => call_or(&args, &scope),
            "+" => call_plus(&args, &scope),
            "-" => call_minus(&args, &scope),
            "*" => call_mult(&args, &scope),
            "/" => call_div(&args, &scope),
            s => Err(format!("Don't know how to do that yet (call_function: {})", s)),
        },
        &LispVal::Let => call_let(&args, &scope),
        _ => return Err(format!("Don't know how to do that yet (call_function: {:?})", f)),
    }
}

pub fn eval(val: LispVal, scope: &Scope) -> Result<LispVal, String> {
    match val {
        LispVal::List(vals) => {
            let f = &vals[0];
            let args: Vec<LispVal> = Vec::from(&vals[1..]);
            return call_function(&f, &args, &scope);
        }
        LispVal::Vector(ref vals) => {
            let lst: Result<Vec<_>, String> = vals.iter().map(|v| eval(v.clone(), &scope)).collect();
            lst.map(|v| LispVal::Vector(v))
        }
        v@LispVal::Number(..) => Ok(v),
        LispVal::Symbol(ref s) => {
            match scope.get(s) {
                Some(v) => Ok(v.clone()),
                None => Err(format!("{} is not defined", s)),
            }
        }
        _ => panic!("Don't know how to do that yet (eval: {:?})", val),
    }
}