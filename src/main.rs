extern crate itertools;
extern crate regex;

use std::collections::VecDeque;
use std::fmt;
use std::io::{self, Read};
use std::str::FromStr;
use itertools::Itertools;
use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Number(i32),
    Whitespace(String),
}

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
    Number(i32),
    Nil,
    And,
    Or,
    Plus,
    Minus,
    Mult,
}

impl fmt::Display for LispVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LispVal::*;

        match *self {
            Symbol(ref s) => write!(f, "{}", s),
            List(ref vals) => write!(f, "[{}]", vals.iter().map(|s| format!("{}", s)).join(", ")),
            Number(i) => write!(f, "{}", i),
            Nil => write!(f, "nil"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Mult => write!(f, "*"),
        }
        // write!(f, "({}, {})", self.x, self.y)
    }
}

fn lex_whitespace(s: &str, i: usize) -> Option<(Token, usize)> {
    let re = Regex::new(r"(^\s+)").unwrap();
    let caps = re.captures(&s[i..])?;
    let whitespace = caps.get(1)?.as_str();
    Some((Token::Whitespace(whitespace.to_string()), i + whitespace.len()))
}

fn lex_lparen(s: &str, i: usize) -> Option<(Token, usize)> {
    if s[i..].starts_with('(') {
        Some((Token::LParen, i + 1))
    } else {
        None
    }
}

fn lex_rparen(s: &str, i: usize) -> Option<(Token, usize)> {
    if s[i..].starts_with(')') {
        Some((Token::RParen, i + 1))
    } else {
        None
    }
}

fn lex_symbol(s: &str, i: usize) -> Option<(Token, usize)> {
    let re = Regex::new(r"^([\-_+a-zA-Z\*]+[a-zA-Z0-9]*)").unwrap();
    let caps = re.captures(&s[i..])?;
    let symbol = caps.get(1)?.as_str();
    Some((Token::Symbol(symbol.to_string()), i + symbol.len()))
}

#[test]
fn test_word_is_a_valid_symbol() {
    assert_eq!(lex_symbol("asdf", 0), Some((Token::Symbol("asdf".into()), 4)));
}

#[test]
fn test_symbol_cannot_start_with_a_number() {
    assert_eq!(lex_symbol("9asdf", 0), None);
}

#[test]
fn test_lex_symbol_respects_index() {
    assert_eq!(lex_symbol("9 asdf", 2), Some((Token::Symbol("asdf".into()), 6)));
}

#[test]
fn test_lex_plus() {
    assert_eq!(lex_symbol("+", 0), Some((Token::Symbol("+".into()), 1)));
}

#[test]
fn test_lex_minus() {
    assert_eq!(lex_symbol("-", 0), Some((Token::Symbol("-".into()), 1)));
}

fn lex_number(s: &str, i: usize) -> Option<(Token, usize)> {
    let re = Regex::new(r"^(\d+)").unwrap();
    let caps = re.captures(&s[i..])?;
    let number = caps.get(1)?.as_str();
    Some((Token::Number(i32::from_str(number).unwrap()), i + number.len()))
}

fn lex_once(s: &str, index: usize) -> Option<(Token, usize)> {
    let token_fns = [
        lex_whitespace,
        lex_lparen,
        lex_rparen,
        lex_number,
        lex_symbol,
    ];

    for lex_fn in token_fns.iter() {
        if let Some((t, i)) = lex_fn(s, index) {
            return Some((t, i));
        }
    }

    None
}

fn lex(s: &str) -> Result<Vec<Token>, String> {
    let mut tokens = vec![];
    let mut index = 0;
    while index < s.len() {
        if let Some((t, i)) = lex_once(s, index) {
            tokens.push(t);
            index = i;
        } else {
            return Err(format!("Unexpected token: {}", s[index..index+1].to_string()));
        }
    }
    Ok(tokens)
}

fn parse_list(tokens: &mut VecDeque<Token>) -> Result<LispVal, String> {
    let mut list = vec![];
    let _ = match tokens.pop_front() {
        Some(Token::LParen) => (),
        t => return Err(format!("Unexpected token: {:?}", t)),
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
        }
    }

    Ok(LispVal::List(list))
}

fn peek(tokens: &VecDeque<Token>) -> Option<&Token> {
    tokens.front()
}

fn parse_number(tokens: &mut VecDeque<Token>) -> Result<LispVal, String> {
    match tokens.pop_front() {
        None => Err("Unexpected end of input".into()),
        Some(Token::Number(i)) => Ok(LispVal::Number(i)),
        Some(t) => Err(format!("Unexpected token: {:?}", t)),
    }
}

fn parse_symbol(tokens: &mut VecDeque<Token>) -> Result<LispVal, String> {
    match tokens.pop_front() {
        None => Err("Unexpected end of input".into()),
        Some(Token::Symbol(s)) => {
            match s.as_ref() {
                "and" => Ok(LispVal::And),
                "or" => Ok(LispVal::Or),
                "nil" => Ok(LispVal::Nil),
                "+" => Ok(LispVal::Plus),
                "-" => Ok(LispVal::Minus),
                "*" => Ok(LispVal::Mult),
                _ => Ok(LispVal::Symbol(s)),
            }
        },
        Some(t) => Err(format!("Unexpected token: {:?}", t)),
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
            Some(&Token::Symbol(_)) => assert!(false), //vals.push(parse_symbol(&mut tokens)?),
            Some(&Token::Whitespace(_)) => skip_whitespace(&mut tokens),
            Some(&Token::RParen) => panic!("Unexpected right paren"),
            None => break,
        };
    }

    Ok(vals)
}

fn is_truthy(val: &LispVal) -> bool {
    match val {
        &LispVal::Nil => false,
        _ => true,
    }
}

fn call_function(f: &LispVal, args: &Vec<LispVal>) -> Result<LispVal, String> {
    match f {
        &LispVal::Plus => {
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
        &LispVal::Or => {
            for arg in args {
                if is_truthy(arg) {
                    return Ok(arg.clone());
                }
            }
            Ok(args[args.len() - 1].clone())
        }
        &LispVal::Minus => {
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
        &LispVal::Mult => {
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
        _ => panic!("Don't know how to do that yet (call_function: {:?})", f),
    }
}

fn eval(val: LispVal) -> Result<LispVal, String> {
    match val {
        LispVal::List(vals) => {
            let f = &vals[0];
            let args: Vec<LispVal> = Vec::from(&vals[1..]);
            return call_function(&f, &args);
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
