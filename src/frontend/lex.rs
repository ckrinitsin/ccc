use anyhow::{Result, bail};
use regex::Regex;
use std::collections::VecDeque;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Constant(i32),
    Int,
    Void,
    Return,
    OpenParanthesis,
    CloseParanthesis,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Decrement,
    Increment,
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
    Assignment,
    CAddition,
    CNegation,
    CMultiplication,
    CDivision,
    CModulo,
    CAnd,
    COr,
    CXor,
    CLShift,
    CRShift,
    If,
    Else,
    QuestionMark,
    Colon,
    Goto,
    Do,
    While,
    For,
    Break,
    Continue,
    Switch,
    Case,
    Default,
}

impl Token {
    pub fn patterns() -> Vec<(Regex, fn(&str) -> Token)> {
        vec![
            (Regex::new(r"<<\=").unwrap(), |_| Token::CLShift),
            (Regex::new(r">>\=").unwrap(), |_| Token::CRShift),
            (Regex::new(r"\-\=").unwrap(), |_| Token::CNegation),
            (Regex::new(r"\+\=").unwrap(), |_| Token::CAddition),
            (Regex::new(r"\^\=").unwrap(), |_| Token::CXor),
            (Regex::new(r"\|\=").unwrap(), |_| Token::COr),
            (Regex::new(r"\&\=").unwrap(), |_| Token::CAnd),
            (Regex::new(r"\*\=").unwrap(), |_| Token::CMultiplication),
            (Regex::new(r"\/\=").unwrap(), |_| Token::CDivision),
            (Regex::new(r"\%\=").unwrap(), |_| Token::CModulo),
            (Regex::new(r"\&\&").unwrap(), |_| Token::LAnd),
            (Regex::new(r"\|\|").unwrap(), |_| Token::LOr),
            (Regex::new(r"\=\=").unwrap(), |_| Token::Equal),
            (Regex::new(r"\!\=").unwrap(), |_| Token::NEqual),
            (Regex::new(r"<<").unwrap(), |_| Token::LShift),
            (Regex::new(r">>").unwrap(), |_| Token::RShift),
            (Regex::new(r"<\=").unwrap(), |_| Token::LessEq),
            (Regex::new(r">\=").unwrap(), |_| Token::GreaterEq),
            (Regex::new(r"\!").unwrap(), |_| Token::Not),
            (Regex::new(r"\+\+").unwrap(), |_| Token::Increment),
            (Regex::new(r"\-\-").unwrap(), |_| Token::Decrement),
            (Regex::new(r"\?").unwrap(), |_| Token::QuestionMark),
            (Regex::new(r"\:").unwrap(), |_| Token::Colon),
            (Regex::new(r"<").unwrap(), |_| Token::Less),
            (Regex::new(r">").unwrap(), |_| Token::Greater),
            (Regex::new(r"\-").unwrap(), |_| Token::Negation),
            (Regex::new(r"\=").unwrap(), |_| Token::Assignment),
            (Regex::new(r"\+").unwrap(), |_| Token::Addition),
            (Regex::new(r"\^").unwrap(), |_| Token::Xor),
            (Regex::new(r"\|").unwrap(), |_| Token::Or),
            (Regex::new(r"\&").unwrap(), |_| Token::And),
            (Regex::new(r"\*").unwrap(), |_| Token::Multiplication),
            (Regex::new(r"\/").unwrap(), |_| Token::Division),
            (Regex::new(r"\%").unwrap(), |_| Token::Modulo),
            (Regex::new(r"\~").unwrap(), |_| Token::Complement),
            (Regex::new(r"\{").unwrap(), |_| Token::OpenBrace),
            (Regex::new(r"\}").unwrap(), |_| Token::CloseBrace),
            (Regex::new(r"\(").unwrap(), |_| Token::OpenParanthesis),
            (Regex::new(r"\)").unwrap(), |_| Token::CloseParanthesis),
            (Regex::new(r"\;").unwrap(), |_| Token::Semicolon),
            (Regex::new(r"return\b").unwrap(), |_| Token::Return),
            (Regex::new(r"void\b").unwrap(), |_| Token::Void),
            (Regex::new(r"int\b").unwrap(), |_| Token::Int),
            (Regex::new(r"if\b").unwrap(), |_| Token::If),
            (Regex::new(r"do\b").unwrap(), |_| Token::Do),
            (Regex::new(r"while\b").unwrap(), |_| Token::While),
            (Regex::new(r"for\b").unwrap(), |_| Token::For),
            (Regex::new(r"break\b").unwrap(), |_| Token::Break),
            (Regex::new(r"continue\b").unwrap(), |_| Token::Continue),
            (Regex::new(r"else\b").unwrap(), |_| Token::Else),
            (Regex::new(r"goto\b").unwrap(), |_| Token::Goto),
            (Regex::new(r"switch\b").unwrap(), |_| Token::Switch),
            (Regex::new(r"case\b").unwrap(), |_| Token::Case),
            (Regex::new(r"default\b").unwrap(), |_| Token::Default),
            (Regex::new(r"[0-9]+\b").unwrap(), |s| {
                Token::Constant(s.parse::<i32>().unwrap())
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
            Token::OpenParanthesis => write!(f, "("),
            Token::CloseParanthesis => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Semicolon => write!(f, ";"),
            Token::Decrement => write!(f, "--"),
            Token::Increment => write!(f, "++"),
            Token::Assignment => write!(f, "="),
            Token::Complement => write!(f, "~"),
            Token::Negation => write!(f, "-"),
            Token::Addition => write!(f, "+"),
            Token::Multiplication => write!(f, "*"),
            Token::Division => write!(f, "/"),
            Token::Modulo => write!(f, "%"),
            Token::And => write!(f, "&"),
            Token::Or => write!(f, "|"),
            Token::Xor => write!(f, "^"),
            Token::LShift => write!(f, "<<"),
            Token::RShift => write!(f, ">>"),

            Token::CNegation => write!(f, "-="),
            Token::CAddition => write!(f, "+="),
            Token::CMultiplication => write!(f, "*="),
            Token::CDivision => write!(f, "/="),
            Token::CModulo => write!(f, "%="),
            Token::CAnd => write!(f, "&="),
            Token::COr => write!(f, "|="),
            Token::CXor => write!(f, "^="),
            Token::CLShift => write!(f, "<<="),
            Token::CRShift => write!(f, ">>="),

            Token::Not => write!(f, "!"),
            Token::LAnd => write!(f, "&&"),
            Token::LOr => write!(f, "||"),
            Token::Equal => write!(f, "=="),
            Token::NEqual => write!(f, "!="),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::LessEq => write!(f, "<="),
            Token::GreaterEq => write!(f, ">="),

            Token::QuestionMark => write!(f, "?"),
            Token::Colon => write!(f, ":"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Do => write!(f, "do"),
            Token::While => write!(f, "while"),
            Token::For => write!(f, "for"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Goto => write!(f, "goto"),
            Token::Switch => write!(f, "switch"),
            Token::Case => write!(f, "case"),
            Token::Default => write!(f, "default"),
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
        | Token::Assignment
        | Token::CAddition
        | Token::CNegation
        | Token::CMultiplication
        | Token::CDivision
        | Token::CModulo
        | Token::CAnd
        | Token::COr
        | Token::CXor
        | Token::CLShift
        | Token::CRShift
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
        | Token::GreaterEq
        | Token::QuestionMark => true,
        _ => false,
    }
}

pub fn is_unary(token: &Token) -> bool {
    match token {
        Token::Negation | Token::Complement | Token::Not | Token::Increment | Token::Decrement => {
            true
        }
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
        Token::LOr => 6,
        Token::QuestionMark => 4,
        Token::CAddition
        | Token::CNegation
        | Token::CMultiplication
        | Token::CDivision
        | Token::CModulo
        | Token::CAnd
        | Token::COr
        | Token::CXor
        | Token::CLShift
        | Token::CRShift
        | Token::Assignment => 2,
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
