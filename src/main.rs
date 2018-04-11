extern crate itertools;
extern crate regex;

mod lex;
mod parse;

use std::io::{self, Read};
use lex::lex;
use parse::{LispVal, parse};

fn is_truthy(val: &LispVal) -> bool {
    match val {
        &LispVal::Nil | &LispVal::Bool(false) => false,
        _ => true,
    }
}

fn call_function(f: &LispVal, args: &Vec<LispVal>) -> Result<LispVal, String> {
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
                        let result = eval(lst.clone())?;
                        match result {
                            LispVal::Number(i) => plus_args.push(i),
                            v => return Err(format!("Unexpected arg to plus: {:?}", v)),
                        }
                    }
                    v => return Err(format!("Unexpected arg to plus: {:?}", v)),
                }
            }
            Ok(LispVal::Number(plus_args.iter().fold(0, |acc, i| acc + i)))
        }
        "-" => {
            let mut result = match eval(args[0].clone())? {
                LispVal::Number(i) => i,
                x => return Err(format!("Unexpected arg to minus: {:?}", x)),
            };
            for arg in args[1..].iter() {
                match eval(arg.clone())? {
                    LispVal::Number(ref i) => result -= i,
                    x => return Err(format!("Unexpected arg to minus: {:?}", x)),
                }
            }
            Ok(LispVal::Number(result))
        }
        "*" => {
            let mut result = match eval(args[0].clone())? {
                LispVal::Number(i) => i,
                x => return Err(format!("Unexpected arg to mult: {:?}", x)),
            };
            for arg in args[1..].iter() {
                match eval(arg.clone())? {
                    LispVal::Number(ref i) => result *= i,
                    x => return Err(format!("Unexpected arg to mult: {:?}", x)),
                }
            }
            Ok(LispVal::Number(result))
        }
        "/" => {
            let mut result = match eval(args[0].clone())? {
                LispVal::Number(i) => i,
                x => return Err(format!("Unexpected arg to div: {:?}", x)),
            };
            for arg in args[1..].iter() {
                match eval(arg.clone())? {
                    LispVal::Number(ref i) => result /= i,
                    x => return Err(format!("Unexpected arg to div: {:?}", x)),
                }
            }
            Ok(LispVal::Number(result))
        }
        s => Err(format!("Don't know how to do that yet (call_function: {})", s)),
    }
}

fn eval(val: LispVal) -> Result<LispVal, String> {
    match val {
        LispVal::List(vals) => {
            let f = &vals[0];
            let args: Vec<LispVal> = Vec::from(&vals[1..]);
            return call_function(&f, &args);
        }
        LispVal::Vector(ref vals) => {
            let lst: Result<Vec<_>, String> = vals.iter().map(|v| eval(v.clone())).collect();
            lst.map(|v| LispVal::Vector(v))
        }
        v@LispVal::Number(..) => Ok(v),
        _ => panic!("Don't know how to do that yet (eval: {:?})", val),
    }
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();

    let mut tokens = lex(&s).expect("lexing failed");
    let ast = parse(&mut tokens).expect("parse failed");
    println!("{}", eval(ast[0].clone()).expect("eval failed"));
}
