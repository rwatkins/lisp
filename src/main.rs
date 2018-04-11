extern crate itertools;
extern crate regex;

mod lex;
use lex::*;

use std::collections::VecDeque;
use std::fmt;
use std::io::{self, Read};
use itertools::Itertools;

// data LispVal = Atom String
//              | List [LispVal]
//              | DottedList [LispVal] LispVal
//              | Number Integer
//              | String String
//              | Bool Bool
// deriving (Read, Show, Eq)

#[derive(Debug, PartialEq, Clone)]
enum LispVal {
    Symbol(String),
    List(Vec<LispVal>),
    Vector(Vec<LispVal>),
    Number(i32),
    Bool(bool),
    Function(String),
    Nil,
}

impl fmt::Display for LispVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LispVal::*;

        match *self {
            Symbol(ref s) => write!(f, "{}", s),
            List(ref vals) | Vector(ref vals) => write!(f, "[{}]", vals.iter().map(|s| format!("{}", s)).join(" ")),
            Number(i) => write!(f, "{}", i),
            Bool(b) => write!(f, "{}", b),
            Function(ref s) => write!(f, "<fn {}>", s),
            Nil => write!(f, "nil"),
        }
    }
}

fn parse_list(tokens: &mut VecDeque<Token>) -> Result<LispVal, String> {
    let mut list = vec![];
    let _ = match tokens.pop_front() {
        Some(Token::LParen) => (),
        t => return Err(format!("Unexpected token while parsing list: {:?}", t)),
    };

    loop {
        match tokens.pop_front() {
            None => return Err("Unexpected end of input".into()),
            Some(Token::RParen) => break,
            Some(Token::Whitespace(..)) => continue,
            Some(s @ Token::Symbol(..)) => {
                tokens.push_front(s);
                list.push(parse_symbol(tokens)?);
            }
            Some(Token::Number(i)) => list.push(LispVal::Number(i)),
            Some(Token::LParen) => {
                tokens.push_front(Token::LParen);
                let lst = parse_list(tokens)?;
                list.push(lst);
            }
            Some(Token::LBracket) => {
                tokens.push_front(Token::LBracket);
                let lst = parse_vector(tokens)?;
                list.push(lst);
            }
            Some(Token::RBracket) => return Err(format!("Unexpected token while parsing list: {:?}", Token::RBracket)),
        }
    }

    Ok(LispVal::List(list))
}

fn parse_vector(tokens: &mut VecDeque<Token>) -> Result<LispVal, String> {
    let mut list = vec![];
    let _ = match tokens.pop_front() {
        Some(Token::LBracket) => (),
        t => return Err(format!("Unexpected token while parsing vector: {:?}", t)),
    };

    loop {
        match tokens.pop_front() {
            None => return Err("Unexpected end of input".into()),
            Some(Token::RBracket) => break,
            Some(Token::Whitespace(..)) => continue,
            Some(s@Token::Symbol(..)) => {
                tokens.push_front(s);
                list.push(parse_symbol(tokens)?);
            }
            Some(Token::Number(i)) => list.push(LispVal::Number(i)),
            Some(Token::LParen) => {
                tokens.push_front(Token::LParen);
                let lst = parse_list(tokens)?;
                list.push(lst);
            }
            Some(Token::LBracket) => {
                tokens.push_front(Token::LBracket);
                let lst = parse_vector(tokens)?;
                list.push(lst);
            }
            Some(Token::RParen) => return Err(format!("Unexpected token while parsing vector: {:?}", Token::RParen)),
        }
    }

    Ok(LispVal::Vector(list))
}

fn peek(tokens: &VecDeque<Token>) -> Option<&Token> {
    tokens.front()
}

fn parse_number(tokens: &mut VecDeque<Token>) -> Result<LispVal, String> {
    match tokens.pop_front() {
        None => Err("Unexpected end of input".into()),
        Some(Token::Number(i)) => Ok(LispVal::Number(i)),
        Some(t) => Err(format!("Unexpected token while parsing number: {:?}", t)),
    }
}

fn parse_symbol(tokens: &mut VecDeque<Token>) -> Result<LispVal, String> {
    match tokens.pop_front() {
        None => Err("Unexpected end of input".into()),
        Some(Token::Symbol(s)) => {
            match s.as_ref() {
                "true" => Ok(LispVal::Bool(true)),
                "false" => Ok(LispVal::Bool(false)),
                "nil" => Ok(LispVal::Nil),
                "and" => Ok(LispVal::Function("and".into())),
                "or" => Ok(LispVal::Function("or".into())),
                "+" => Ok(LispVal::Function("+".into())),
                "-" => Ok(LispVal::Function("-".into())),
                "*" => Ok(LispVal::Function("*".into())),
                "/" => Ok(LispVal::Function("/".into())),
                _ => Ok(LispVal::Symbol(s)),
            }
        },
        Some(t) => Err(format!("Unexpected token while parsing symbol: {:?}", t)),
    }
}

#[test]
fn test_parse_symbol_can_parse_nil() {
    let mut tokens = VecDeque::new();
    tokens.push_front(Token::Symbol("nil".into()));
    assert_eq!(parse_symbol(&mut tokens), Ok(LispVal::Nil));
}

fn skip_whitespace(tokens: &mut VecDeque<Token>) {
    loop {
        match peek(tokens) {
            Some(&Token::Whitespace(..)) => {
                let _ = tokens.pop_front();
            }
            Some(_) | None => return,
        }
    }
}

fn parse(tokens: &[Token]) -> Result<Vec<LispVal>, String> {
    let tokens = Vec::from(tokens);
    let mut tokens = VecDeque::from(tokens);
    let mut vals = vec![];
    loop {
        match peek(&tokens) {
            Some(&Token::LParen) => vals.push(parse_list(&mut tokens)?),
            Some(&Token::Number(_)) => vals.push(parse_number(&mut tokens)?),
            Some(&Token::Symbol(_)) => vals.push(parse_symbol(&mut tokens)?),
            Some(&Token::Whitespace(_)) => skip_whitespace(&mut tokens),
            Some(&Token::RParen) => panic!("Unexpected right paren"),
            Some(&Token::LBracket) => vals.push(parse_vector(&mut tokens)?),
            Some(&Token::RBracket) => panic!("Unexpected right bracket"),
            None => break,
        };
    }

    Ok(vals)
}

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
