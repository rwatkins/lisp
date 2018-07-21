use crate::lex::Token;
use itertools::Itertools;
use std::collections::VecDeque;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum LispVal {
    Symbol(String),
    List(Vec<LispVal>),
    Vector(Vec<LispVal>),
    Number(i32),
    Bool(bool),
    Lambda {
        params: Vec<LispVal>,
        body: Vec<LispVal>,
    },
    Nil,
}

type ParseResult = Result<LispVal, String>;

impl fmt::Display for LispVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LispVal::*;

        match *self {
            Symbol(ref s) => write!(f, "{}", s),
            List(ref vals) | Vector(ref vals) => {
                write!(f, "[{}]", vals.iter().map(|s| format!("{}", s)).join(" "))
            }
            Number(i) => write!(f, "{}", i),
            Bool(b) => write!(f, "{}", b),
            Lambda { .. } => write!(f, "<lambda>"),
            Nil => write!(f, "nil"),
        }
    }
}

fn parse_list(tokens: &mut VecDeque<Token>) -> ParseResult {
    let mut list = vec![];
    match tokens.pop_front() {
        Some(Token::LParen) => (),
        t => return Err(format!("Unexpected token while parsing list 1: {:?}", t)),
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
                let lst = parse_lambda_or_list(tokens)?;
                list.push(lst);
            }
            Some(Token::LBracket) => {
                tokens.push_front(Token::LBracket);
                let lst = parse_vector(tokens)?;
                list.push(lst);
            }
            Some(Token::RBracket) => {
                return Err(format!(
                    "Unexpected token while parsing list 2: {:?}",
                    Token::RBracket
                ))
            }
        }
    }

    Ok(LispVal::List(list))
}

fn parse_vector(tokens: &mut VecDeque<Token>) -> ParseResult {
    let mut list = vec![];
    match tokens.pop_front() {
        Some(Token::LBracket) => (),
        t => return Err(format!("Unexpected token while parsing vector 1: {:?}", t)),
    };

    loop {
        match tokens.pop_front() {
            None => return Err("Unexpected end of input".into()),
            Some(Token::RBracket) => break,
            Some(Token::Whitespace(..)) => continue,
            Some(s @ Token::Symbol(..)) => {
                tokens.push_front(s);
                list.push(parse_symbol(tokens)?);
            }
            Some(Token::Number(i)) => list.push(LispVal::Number(i)),
            Some(Token::LParen) => {
                tokens.push_front(Token::LParen);
                let lst = parse_lambda_or_list(tokens)?;
                list.push(lst);
            }
            Some(Token::LBracket) => {
                tokens.push_front(Token::LBracket);
                let lst = parse_vector(tokens)?;
                list.push(lst);
            }
            Some(Token::RParen) => {
                return Err(format!(
                    "Unexpected token while parsing vector 2: {:?}",
                    Token::RParen
                ))
            }
        }
    }

    Ok(LispVal::Vector(list))
}

fn peek(tokens: &VecDeque<Token>) -> Option<Token> {
    tokens.front().cloned()
}

fn parse_number(tokens: &mut VecDeque<Token>) -> ParseResult {
    match tokens.pop_front() {
        None => Err("Unexpected end of input".into()),
        Some(Token::Number(i)) => Ok(LispVal::Number(i)),
        Some(t) => Err(format!("Unexpected token while parsing number: {:?}", t)),
    }
}

fn parse_symbol(tokens: &mut VecDeque<Token>) -> ParseResult {
    match tokens.pop_front() {
        None => Err("Unexpected end of input".into()),
        Some(Token::Symbol(s)) => match s.as_ref() {
            "true" => Ok(LispVal::Bool(true)),
            "false" => Ok(LispVal::Bool(false)),
            "nil" => Ok(LispVal::Nil),
            _ => Ok(LispVal::Symbol(s)),
        },
        Some(t) => Err(format!("Unexpected token while parsing symbol: {:?}", t)),
    }
}

fn skip_whitespace(tokens: &mut VecDeque<Token>) {
    loop {
        match peek(tokens) {
            Some(Token::Whitespace(..)) => {
                let _ = tokens.pop_front();
            }
            Some(_) | None => return,
        }
    }
}

fn expect_lparen(tokens: &mut VecDeque<Token>) -> Result<Token, String> {
    match tokens.pop_front() {
        Some(Token::LParen) => Ok(Token::LParen),
        v => Err(format!("expected LParen, got {:?}", v)),
    }
}

fn expect_rparen(tokens: &mut VecDeque<Token>) -> Result<Token, String> {
    match tokens.pop_front() {
        Some(Token::RParen) => Ok(Token::RParen),
        v => Err(format!("expected RParen, got {:?}", v)),
    }
}

fn expect_symbol_fn(tokens: &mut VecDeque<Token>) -> Result<Token, String> {
    match tokens.pop_front() {
        Some(Token::Symbol(ref s)) if s == "fn" => Ok(Token::Symbol("fn".into())),
        v => Err(format!("expected Symbol \"fn\", got {:?}", v)),
    }
}

fn parse_lambda(tokens: &mut VecDeque<Token>) -> ParseResult {
    let _ = expect_lparen(tokens)?;
    skip_whitespace(tokens);
    let _ = expect_symbol_fn(tokens)?;
    skip_whitespace(tokens);
    let params = match parse_vector(tokens)? {
        LispVal::Vector(ref v) => v.clone(),
        v => return Err(format!("expected params vector, got {:?}", v)),
    };
    skip_whitespace(tokens);
    let body = parse_until_unexpected(tokens)?;
    let _ = expect_rparen(tokens)?;
    Ok(LispVal::Lambda { params, body })
}

fn parse_lambda_or_list(tokens: &mut VecDeque<Token>) -> ParseResult {
    let paren = tokens.pop_front().unwrap();
    skip_whitespace(tokens);
    let next = peek(&tokens);
    match next {
        Some(Token::Symbol(ref s)) if s == "fn" => {
            tokens.push_front(paren);
            parse_lambda(tokens)
        }
        _ => {
            tokens.push_front(paren);
            parse_list(tokens)
        }
    }
}

fn parse_until_unexpected(tokens: &mut VecDeque<Token>) -> Result<Vec<LispVal>, String> {
    let mut vals = vec![];
    loop {
        match peek(&tokens) {
            Some(Token::LParen) => vals.push(parse_lambda_or_list(tokens)?),
            Some(Token::Number(_)) => vals.push(parse_number(tokens)?),
            Some(Token::Symbol(_)) => vals.push(parse_symbol(tokens)?),
            Some(Token::Whitespace(_)) => skip_whitespace(tokens),
            Some(Token::RParen) => break,
            Some(Token::LBracket) => vals.push(parse_vector(tokens)?),
            Some(Token::RBracket) => break,
            None => break,
        };
    }

    Ok(vals)
}

pub fn parse(tokens: &[Token]) -> Result<Vec<LispVal>, String> {
    let mut tokens = VecDeque::from(Vec::from(tokens));
    let mut vals = vec![];
    loop {
        match peek(&tokens) {
            Some(Token::LParen) => vals.push(parse_lambda_or_list(&mut tokens)?),
            Some(Token::Number(_)) => vals.push(parse_number(&mut tokens)?),
            Some(Token::Symbol(_)) => vals.push(parse_symbol(&mut tokens)?),
            Some(Token::Whitespace(_)) => skip_whitespace(&mut tokens),
            Some(Token::RParen) => return Err(format!("Unexpected right paren. vals={:?}", vals)),
            Some(Token::LBracket) => vals.push(parse_vector(&mut tokens)?),
            Some(Token::RBracket) => panic!("Unexpected right bracket"),
            None => break,
        };
    }

    Ok(vals)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_symbol_can_parse_nil() {
        let mut tokens = VecDeque::new();
        tokens.push_front(Token::Symbol("nil".into()));
        assert_eq!(parse_symbol(&mut tokens), Ok(LispVal::Nil));
    }

    #[test]
    fn parse_lambda() {
        use crate::lex::Token::{LBracket, LParen, Number, RBracket, RParen, Symbol, Whitespace};

        let tokens = vec![
            LParen,
            Symbol("fn".into()),
            Whitespace(" ".into()),
            LBracket,
            Symbol("x".into()),
            RBracket,
            Whitespace(" ".into()),
            LParen,
            Symbol("+".into()),
            Whitespace(" ".into()),
            Symbol("x".into()),
            Whitespace(" ".into()),
            Number(1),
            RParen,
            RParen,
        ];
        let expected = vec![LispVal::Lambda {
            params: vec![LispVal::Symbol("x".into())],
            body: vec![LispVal::List(vec![
                LispVal::Symbol("+".into()),
                LispVal::Symbol("x".into()),
                LispVal::Number(1),
            ])],
        }];
        let result = parse(&tokens);
        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn parse_lambda_inside_let_binding() {
        use crate::lex::Token::{LBracket, LParen, Number, RBracket, RParen, Symbol, Whitespace};

        let tokens = vec![
            // Let start
            LParen,
            Symbol("let".into()),
            // Let bindings start
            LBracket,
            Symbol("f".into()),
            // Lambda start
            LParen,
            Symbol("fn".into()),
            Whitespace(" ".into()),
            LBracket,
            Symbol("x".into()),
            RBracket,
            Whitespace(" ".into()),
            LParen,
            Symbol("+".into()),
            Whitespace(" ".into()),
            Symbol("x".into()),
            Whitespace(" ".into()),
            Number(1),
            RParen,
            RParen,
            // Lambda end
            RBracket,
            // Let bindings end
            // Let body start
            Symbol("f".into()),
            // Let body end
            RParen,
            // Let end
        ];
        let expected = vec![LispVal::List(vec![
            LispVal::Symbol("let".into()),
            LispVal::Vector(vec![
                LispVal::Symbol("f".into()),
                LispVal::Lambda {
                    params: vec![LispVal::Symbol("x".into())],
                    body: vec![LispVal::List(vec![
                        LispVal::Symbol("+".into()),
                        LispVal::Symbol("x".into()),
                        LispVal::Number(1),
                    ])],
                },
            ]),
            LispVal::Symbol("f".into()),
        ])];
        let result = parse(&tokens);
        assert_eq!(result, Ok(expected));
    }
}
