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

    let mut tokens = lex(&s).expect("lexing failed");
    let ast = parse(&mut tokens).expect("parse failed");

    let mut scope = HashMap::new();
    scope.insert("asdf".into(), LispVal::Number(10));

    println!("{}", eval(ast[0].clone(), &scope).expect("eval failed"));
}
