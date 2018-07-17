extern crate itertools;
extern crate regex;

mod eval;
mod lex;
mod parse;

use std::collections::HashMap;
use std::io::{self, Read};
use eval::eval;
use lex::lex;
use parse::{LispVal, parse};

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let result = run_program(&s).unwrap();
    println!("{}", result);
}

fn run_program(program: &str) -> Result<LispVal, String> {
    let mut tokens = lex(&program).map_err(|e| format!("lex failed: {}", e))?;
    let ast = parse(&mut tokens).map_err(|e| format!("parse failed: {}", e))?;
    let scope = HashMap::new();
    eval(ast[0].clone(), &scope).map_err(|e| format!("eval failed: {}", e))
}

#[test]
fn test_run_program() {
    let program = "
    (let [f (fn [x y]
              (* (+ x y) 10))
          a 3
          b 4]
      (f a b))
    ";
    let expected = Ok(LispVal::Number(70));
    let result = run_program(program);
    assert_eq!(result, expected);
}

#[test]
fn test_run_program2() {
    let program = "
    (let [f (fn [x] (* x x))
          apply1 (fn [f x] (f (+ 1 x)))]
      (apply1 f 4))
    ";
    let expected = Ok(LispVal::Number(25));
    let result = run_program(program);
    assert_eq!(result, expected);
}

#[test]
fn test_run_program3() {
    let program = "
    (let [x 1
          f (fn [y] (+ x y))]
      (* (f 2) 4))
    ";
    let expected = Ok(LispVal::Number(12));
    let result = run_program(program);
    assert_eq!(result, expected);
}
