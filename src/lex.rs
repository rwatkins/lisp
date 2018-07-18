use regex::Regex;
use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LParen,
    RParen,
    LBracket,
    RBracket,
    Symbol(String),
    Number(i32),
    Whitespace(String),
}

pub type LexResult = Option<(Token, usize)>;

fn lex_whitespace(s: &str, i: usize) -> Option<(Token, usize)> {
    let re = Regex::new(r"(^\s+)").unwrap();
    let caps = re.captures(&s[i..])?;
    let whitespace = caps.get(1)?.as_str();
    Some((
        Token::Whitespace(whitespace.to_string()),
        i + whitespace.len(),
    ))
}

fn lex_lparen(s: &str, i: usize) -> Option<(Token, usize)> {
    lex_char(&s, i, '(', Token::LParen)
}

fn lex_lbracket(s: &str, i: usize) -> Option<(Token, usize)> {
    lex_char(&s, i, '[', Token::LBracket)
}

fn lex_rbracket(s: &str, i: usize) -> LexResult {
    lex_char(&s, i, ']', Token::RBracket)
}

fn lex_rparen(s: &str, i: usize) -> Option<(Token, usize)> {
    lex_char(s, i, ')', Token::RParen)
}

fn lex_char(s: &str, i: usize, c: char, token: Token) -> Option<(Token, usize)> {
    if s[i..].starts_with(c) {
        Some((token, i + 1))
    } else {
        None
    }
}

fn lex_symbol(s: &str, i: usize) -> Option<(Token, usize)> {
    let re = Regex::new(r"^([\-_+a-zA-Z\*/]+[a-zA-Z0-9]*)").unwrap();
    let caps = re.captures(&s[i..])?;
    let symbol = caps.get(1)?.as_str();
    Some((Token::Symbol(symbol.to_string()), i + symbol.len()))
}

fn lex_number(s: &str, i: usize) -> Option<(Token, usize)> {
    let re = Regex::new(r"^(\d+)").unwrap();
    let caps = re.captures(&s[i..])?;
    let number = caps.get(1)?.as_str();
    Some((
        Token::Number(i32::from_str(number).unwrap()),
        i + number.len(),
    ))
}

fn lex_once(s: &str, index: usize) -> Option<(Token, usize)> {
    let token_fns = [
        lex_whitespace,
        lex_lparen,
        lex_rparen,
        lex_lbracket,
        lex_rbracket,
        lex_number,
        lex_symbol,
    ];

    for lex_fn in &token_fns {
        if let Some((t, i)) = lex_fn(s, index) {
            return Some((t, i));
        }
    }

    None
}

pub fn lex(s: &str) -> Result<Vec<Token>, String> {
    let mut tokens = vec![];
    let mut index = 0;
    while index < s.len() {
        if let Some((t, i)) = lex_once(s, index) {
            tokens.push(t);
            index = i;
        } else {
            return Err(format!(
                "Unexpected token: {}",
                s[index..index + 1].to_string()
            ));
        }
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn word_is_a_valid_symbol() {
        assert_eq!(
            lex_symbol("asdf", 0),
            Some((Token::Symbol("asdf".into()), 4))
        );
    }

    #[test]
    fn symbol_cannot_start_with_a_number() {
        assert_eq!(lex_symbol("9asdf", 0), None);
    }

    #[test]
    fn lex_symbol_respects_index() {
        assert_eq!(
            lex_symbol("9 asdf", 2),
            Some((Token::Symbol("asdf".into()), 6))
        );
    }

    #[test]
    fn lex_plus() {
        assert_eq!(lex_symbol("+", 0), Some((Token::Symbol("+".into()), 1)));
    }

    #[test]
    fn lex_minus() {
        assert_eq!(lex_symbol("-", 0), Some((Token::Symbol("-".into()), 1)));
    }
}
