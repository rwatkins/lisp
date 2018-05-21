use std::collections::VecDeque;
use std::fmt;
use itertools::Itertools;
use lex::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum LispVal {
    Symbol(String),
    List(Vec<LispVal>),
    Vector(Vec<LispVal>),
    Number(i32),
    Bool(bool),
    Function(String),
    Let,
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
            Let => write!(f, "<let>"),
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
                "let" => Ok(LispVal::Let),
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

pub fn parse(tokens: &[Token]) -> Result<Vec<LispVal>, String> {
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
