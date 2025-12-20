use anyhow::{Result, bail};
use regex::Regex;
use std::collections::VecDeque;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Constant(i64),
    Int,
    Void,
    Return,
    OpenParanthesis,
    CloseParanthesis,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Decrement,
    Negation,
    Complement,
    Addition,
    Multiplication,
    Division,
    Modulo,
}

impl Token {
    pub fn patterns() -> Vec<(Regex, fn(&str) -> Token)> {
        vec![
            (Regex::new(r"\;").unwrap(), |_| Token::Semicolon),
            (Regex::new(r"\-\-").unwrap(), |_| Token::Decrement),
            (Regex::new(r"\-").unwrap(), |_| Token::Negation),
            (Regex::new(r"\+").unwrap(), |_| Token::Addition),
            (Regex::new(r"\*").unwrap(), |_| Token::Multiplication),
            (Regex::new(r"\/").unwrap(), |_| Token::Division),
            (Regex::new(r"\%").unwrap(), |_| Token::Modulo),
            (Regex::new(r"\~").unwrap(), |_| Token::Complement),
            (Regex::new(r"\(").unwrap(), |_| Token::OpenBrace),
            (Regex::new(r"\)").unwrap(), |_| Token::CloseBrace),
            (Regex::new(r"\{").unwrap(), |_| Token::OpenParanthesis),
            (Regex::new(r"\}").unwrap(), |_| Token::CloseParanthesis),
            (Regex::new(r"return\b").unwrap(), |_| Token::Return),
            (Regex::new(r"void\b").unwrap(), |_| Token::Void),
            (Regex::new(r"int\b").unwrap(), |_| Token::Int),
            (Regex::new(r"[0-9]+\b").unwrap(), |s| {
                Token::Constant(s.parse::<i64>().unwrap())
            }),
            (Regex::new(r"[a-zA-Z_]\w*\b").unwrap(), |s| {
                Token::Identifier(s.to_string())
            }),
        ]
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Identifier(x) => write!(f, "{}", x),
            Token::Constant(x) => write!(f, "{}", x),
            Token::Int => write!(f, "int"),
            Token::Void => write!(f, "void"),
            Token::Return => write!(f, "return"),
            Token::OpenParanthesis => write!(f, "{{"),
            Token::CloseParanthesis => write!(f, "}}"),
            Token::OpenBrace => write!(f, "("),
            Token::CloseBrace => write!(f, ")"),
            Token::Semicolon => write!(f, ";"),
            Token::Decrement => write!(f, "--"),
            Token::Negation => write!(f, "-"),
            Token::Complement => write!(f, "~"),
            Token::Addition => write!(f, "+"),
            Token::Multiplication => write!(f, "*"),
            Token::Division => write!(f, "/"),
            Token::Modulo => write!(f, "%"),
        }
    }
}

pub fn is_binary(token: &Token) -> bool {
    match token {
        Token::Negation
        | Token::Addition
        | Token::Multiplication
        | Token::Division
        | Token::Modulo => true,
        _ => false,
    }
}

pub fn precedence(token: &Token) -> usize {
    match token {
        Token::Negation | Token::Addition => 3,
        Token::Multiplication | Token::Division | Token::Modulo => 5,
        _ => 0,
    }
}

pub fn lex(content: String) -> Result<VecDeque<Token>> {
    let mut tokens: VecDeque<Token> = VecDeque::new();
    let mut remaining = content.trim_start();
    let patterns = Token::patterns();
    'outer: while !remaining.is_empty() {
        for (re, tok) in &patterns {
            if let Some(m) = re.find(remaining) {
                if !m.is_empty() && m.start() == 0 {
                    tokens.push_back(tok(m.as_str()));
                    remaining = remaining[m.end()..].trim_start();
                    continue 'outer;
                }
            }
        }

        bail!("Could not tokenize {}", remaining);
    }

    Ok(tokens)
}
