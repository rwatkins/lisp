use std::collections::HashMap;
use parse::LispVal;

type Scope = HashMap<String, LispVal>;

fn is_truthy(val: &LispVal) -> bool {
    match val {
        &LispVal::Nil | &LispVal::Bool(false) => false,
        _ => true,
    }
}

fn call_function(f: &LispVal, args: &Vec<LispVal>, scope: &Scope) -> Result<LispVal, String> {
    let fn_name = match f {
        &LispVal::Function(ref s) => s,
        _ => return Err(format!("Don't know how to do that yet (call_function: {:?})", f)),
    };

    match fn_name.as_ref() {
        "and" => {
            for arg in args {
                if !is_truthy(arg) {
                    return Ok(arg.clone());
                }
            }
            Ok(args[args.len() - 1].clone())
        }
        "or" => {
            for arg in args {
                if is_truthy(arg) {
                    return Ok(arg.clone());
                }
            }
            Ok(args[args.len() - 1].clone())
        }
        "+" => {
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
        "-" => {
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
        "*" => {
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
        "/" => {
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
        s => Err(format!("Don't know how to do that yet (call_function: {})", s)),
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
