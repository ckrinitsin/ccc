use anyhow::{Result, bail};
use regex::Regex;
use std::collections::VecDeque;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    IntConstant(i64),
    LongConstant(i64),
    Int,
    Long,
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
    Comma,
    Goto,
    Do,
    While,
    For,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Static,
    Extern,
}

impl Token {
    pub fn patterns() -> Vec<(Regex, fn(&str) -> Result<Token>)> {
        vec![
            (Regex::new(r"<<\=").unwrap(), |_| Ok(Token::CLShift)),
            (Regex::new(r">>\=").unwrap(), |_| Ok(Token::CRShift)),
            (Regex::new(r"\-\=").unwrap(), |_| Ok(Token::CNegation)),
            (Regex::new(r"\+\=").unwrap(), |_| Ok(Token::CAddition)),
            (Regex::new(r"\^\=").unwrap(), |_| Ok(Token::CXor)),
            (Regex::new(r"\|\=").unwrap(), |_| Ok(Token::COr)),
            (Regex::new(r"\&\=").unwrap(), |_| Ok(Token::CAnd)),
            (Regex::new(r"\*\=").unwrap(), |_| Ok(Token::CMultiplication)),
            (Regex::new(r"\/\=").unwrap(), |_| Ok(Token::CDivision)),
            (Regex::new(r"\%\=").unwrap(), |_| Ok(Token::CModulo)),
            (Regex::new(r"\&\&").unwrap(), |_| Ok(Token::LAnd)),
            (Regex::new(r"\|\|").unwrap(), |_| Ok(Token::LOr)),
            (Regex::new(r"\=\=").unwrap(), |_| Ok(Token::Equal)),
            (Regex::new(r"\!\=").unwrap(), |_| Ok(Token::NEqual)),
            (Regex::new(r"<<").unwrap(), |_| Ok(Token::LShift)),
            (Regex::new(r">>").unwrap(), |_| Ok(Token::RShift)),
            (Regex::new(r"<\=").unwrap(), |_| Ok(Token::LessEq)),
            (Regex::new(r">\=").unwrap(), |_| Ok(Token::GreaterEq)),
            (Regex::new(r"\!").unwrap(), |_| Ok(Token::Not)),
            (Regex::new(r"\+\+").unwrap(), |_| Ok(Token::Increment)),
            (Regex::new(r"\-\-").unwrap(), |_| Ok(Token::Decrement)),
            (Regex::new(r"\?").unwrap(), |_| Ok(Token::QuestionMark)),
            (Regex::new(r"\:").unwrap(), |_| Ok(Token::Colon)),
            (Regex::new(r"\,").unwrap(), |_| Ok(Token::Comma)),
            (Regex::new(r"<").unwrap(), |_| Ok(Token::Less)),
            (Regex::new(r">").unwrap(), |_| Ok(Token::Greater)),
            (Regex::new(r"\-").unwrap(), |_| Ok(Token::Negation)),
            (Regex::new(r"\=").unwrap(), |_| Ok(Token::Assignment)),
            (Regex::new(r"\+").unwrap(), |_| Ok(Token::Addition)),
            (Regex::new(r"\^").unwrap(), |_| Ok(Token::Xor)),
            (Regex::new(r"\|").unwrap(), |_| Ok(Token::Or)),
            (Regex::new(r"\&").unwrap(), |_| Ok(Token::And)),
            (Regex::new(r"\*").unwrap(), |_| Ok(Token::Multiplication)),
            (Regex::new(r"\/").unwrap(), |_| Ok(Token::Division)),
            (Regex::new(r"\%").unwrap(), |_| Ok(Token::Modulo)),
            (Regex::new(r"\~").unwrap(), |_| Ok(Token::Complement)),
            (Regex::new(r"\{").unwrap(), |_| Ok(Token::OpenBrace)),
            (Regex::new(r"\}").unwrap(), |_| Ok(Token::CloseBrace)),
            (Regex::new(r"\(").unwrap(), |_| Ok(Token::OpenParanthesis)),
            (Regex::new(r"\)").unwrap(), |_| Ok(Token::CloseParanthesis)),
            (Regex::new(r"\;").unwrap(), |_| Ok(Token::Semicolon)),
            (Regex::new(r"return\b").unwrap(), |_| Ok(Token::Return)),
            (Regex::new(r"void\b").unwrap(), |_| Ok(Token::Void)),
            (Regex::new(r"static\b").unwrap(), |_| Ok(Token::Static)),
            (Regex::new(r"extern\b").unwrap(), |_| Ok(Token::Extern)),
            (Regex::new(r"int\b").unwrap(), |_| Ok(Token::Int)),
            (Regex::new(r"long\b").unwrap(), |_| Ok(Token::Long)),
            (Regex::new(r"if\b").unwrap(), |_| Ok(Token::If)),
            (Regex::new(r"do\b").unwrap(), |_| Ok(Token::Do)),
            (Regex::new(r"while\b").unwrap(), |_| Ok(Token::While)),
            (Regex::new(r"for\b").unwrap(), |_| Ok(Token::For)),
            (Regex::new(r"break\b").unwrap(), |_| Ok(Token::Break)),
            (Regex::new(r"continue\b").unwrap(), |_| Ok(Token::Continue)),
            (Regex::new(r"else\b").unwrap(), |_| Ok(Token::Else)),
            (Regex::new(r"goto\b").unwrap(), |_| Ok(Token::Goto)),
            (Regex::new(r"switch\b").unwrap(), |_| Ok(Token::Switch)),
            (Regex::new(r"case\b").unwrap(), |_| Ok(Token::Case)),
            (Regex::new(r"default\b").unwrap(), |_| Ok(Token::Default)),
            (Regex::new(r"[0-9]+[lL]\b").unwrap(), |s| {
                let cons = s[..s.len() - 1].parse::<i64>()?;
                Ok(Token::LongConstant(cons))
            }),
            (Regex::new(r"[0-9]+\b").unwrap(), |s| {
                let cons = s.parse::<i64>()?;
                Ok(Token::IntConstant(cons))
            }),
            (Regex::new(r"[a-zA-Z_]\w*\b").unwrap(), |s| {
                Ok(Token::Identifier(s.to_string()))
            }),
        ]
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Identifier(x) => write!(f, "{}", x),
            Token::IntConstant(x) => write!(f, "{}", x),
            Token::LongConstant(x) => write!(f, "{}", x),
            Token::Int => write!(f, "int"),
            Token::Long => write!(f, "long"),
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
            Token::Comma => write!(f, ","),
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

            Token::Static => write!(f, "static"),
            Token::Extern => write!(f, "extern"),
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

pub fn is_assignment(tok: &Token) -> bool {
    match tok {
        Token::Assignment
        | Token::CAddition
        | Token::CNegation
        | Token::CMultiplication
        | Token::CDivision
        | Token::CModulo
        | Token::CAnd
        | Token::COr
        | Token::CXor
        | Token::CLShift
        | Token::CRShift => true,
        _ => false,
    }
}

pub fn is_specifier(token: &Token) -> bool {
    match token {
        Token::Extern | Token::Static => true,
        n if is_type_specifier(n) => true,
        _ => false,
    }
}

pub fn is_type_specifier(tok: &Token) -> bool {
    match tok {
        Token::Int | Token::Long => true,
        _ => false,
    }
}

pub fn is_constant(tok: &Token) -> bool {
    match tok {
        Token::IntConstant(_) | Token::LongConstant(_) => true,
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
                    tokens.push_back(tok(m.as_str())?);
                    remaining = remaining[m.end()..].trim_start();
                    continue 'outer;
                }
            }
        }

        bail!("Could not tokenize {}", remaining);
    }

    Ok(tokens)
}
