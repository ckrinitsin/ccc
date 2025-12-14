use anyhow::{Result, bail};
use regex::Regex;
use std::collections::VecDeque;

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
}

impl Token {
    pub fn patterns() -> Vec<(Regex, fn(&str) -> Token)> {
        vec![
            (Regex::new(r"\;").unwrap(), |_| Token::Semicolon),
            (Regex::new(r"\(").unwrap(), |_| Token::OpenBrace),
            (Regex::new(r"\)").unwrap(), |_| Token::CloseBrace),
            (Regex::new(r"\{").unwrap(), |_| Token::OpenParanthesis),
            (Regex::new(r"\}").unwrap(), |_| Token::CloseParanthesis),
            (Regex::new(r"return\b").unwrap(), |_| Token::Return),
            (Regex::new(r"void\b").unwrap(), |_| Token::Void),
            (Regex::new(r"int\b").unwrap(), |_| Token::Int),
            (Regex::new(r"[0-9]+\b").unwrap(), |s| Token::Constant(s.parse::<i64>().unwrap())),
            (Regex::new(r"[a-zA-Z_]\w*\b").unwrap(), |s| Token::Identifier(s.to_string())),
        ]
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
