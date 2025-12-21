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
    And,
    Or,
    Xor,
    LShift,
    RShift,
    Not,
    LAnd,
    LOr,
    Equal,
    NEqual,
    Less,
    Greater,
    LessEq,
    GreaterEq,
}

impl Token {
    pub fn patterns() -> Vec<(Regex, fn(&str) -> Token)> {
        vec![
            (Regex::new(r"\;").unwrap(), |_| Token::Semicolon),
            (Regex::new(r"<<").unwrap(), |_| Token::LShift),
            (Regex::new(r">>").unwrap(), |_| Token::RShift),
            (Regex::new(r"\&\&").unwrap(), |_| Token::LAnd),
            (Regex::new(r"\|\|").unwrap(), |_| Token::LOr),
            (Regex::new(r"\=\=").unwrap(), |_| Token::Equal),
            (Regex::new(r"\!\=").unwrap(), |_| Token::NEqual),
            (Regex::new(r"<\=").unwrap(), |_| Token::LessEq),
            (Regex::new(r">\=").unwrap(), |_| Token::GreaterEq),
            (Regex::new(r"<").unwrap(), |_| Token::Less),
            (Regex::new(r">").unwrap(), |_| Token::Greater),
            (Regex::new(r"\!").unwrap(), |_| Token::Not),
            (Regex::new(r"\-\-").unwrap(), |_| Token::Decrement),
            (Regex::new(r"\-").unwrap(), |_| Token::Negation),
            (Regex::new(r"\+").unwrap(), |_| Token::Addition),
            (Regex::new(r"\^").unwrap(), |_| Token::Xor),
            (Regex::new(r"\|").unwrap(), |_| Token::Or),
            (Regex::new(r"\&").unwrap(), |_| Token::And),
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
            Token::And => write!(f, "&"),
            Token::Or => write!(f, "|"),
            Token::Xor => write!(f, "^"),
            Token::LShift => write!(f, "<<"),
            Token::RShift => write!(f, ">>"),
            Token::Not => write!(f, "!"),
            Token::LAnd => write!(f, "&&"),
            Token::LOr => write!(f, "||"),
            Token::Equal => write!(f, "=="),
            Token::NEqual => write!(f, "!="),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::LessEq => write!(f, "<="),
            Token::GreaterEq => write!(f, ">="),
        }
    }
}

pub fn is_binary(token: &Token) -> bool {
    match token {
        Token::Negation
        | Token::Addition
        | Token::Multiplication
        | Token::Division
        | Token::Modulo
        | Token::And
        | Token::Or
        | Token::Xor
        | Token::LShift
        | Token::RShift
        | Token::LAnd
        | Token::LOr
        | Token::Equal
        | Token::NEqual
        | Token::Less
        | Token::Greater
        | Token::LessEq
        | Token::GreaterEq => true,
        _ => false,
    }
}

pub fn is_unary(token: &Token) -> bool {
    match token {
        Token::Negation | Token::Complement | Token::Not => true,
        _ => false,
    }
}

pub fn precedence(token: &Token) -> usize {
    match token {
        Token::Multiplication | Token::Division | Token::Modulo => 50,
        Token::Negation | Token::Addition => 45,
        Token::LShift | Token::RShift => 40,
        Token::Less | Token::LessEq | Token::Greater | Token::GreaterEq => 35,
        Token::Equal | Token::NEqual => 30,
        Token::And => 25,
        Token::Xor => 20,
        Token::Or => 15,
        Token::LAnd => 10,
        Token::LOr => 5,
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
