use crate::frontend::lex::{self, Token};
use anyhow::{Result, bail};
use std::collections::VecDeque;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Ast {
    Program(Function),
}

#[derive(Debug, PartialEq)]
pub enum Function {
    Function(String, Statement),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Constant(i64),
    Unary(UnaryOp, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negation,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    And,
    Or,
    Xor,
    LShift,
    RShift,
    LAnd,
    LOr,
    Equal,
    NEqual,
    Less,
    Greater,
    LessEq,
    GreaterEq,
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ast::Program(x) => write!(f, "Program:\n{}", x),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Function(name, body) => write!(f, "Function '{}':\n{}", name, body),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Return(x) => write!(f, "Return: {}", x),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Constant(c) => write!(f, "{}", c),
            Expression::Unary(op, expr) => write!(f, "{}{}", op, *expr),
            Expression::Binary(op, left, right) => write!(f, "{}{}{}", *left, op, *right),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Complement => write!(f, "~"),
            UnaryOp::Negation => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Addition => write!(f, "+"),
            BinaryOp::Subtraction => write!(f, "-"),
            BinaryOp::Multiplication => write!(f, "*"),
            BinaryOp::Division => write!(f, "/"),
            BinaryOp::Modulo => write!(f, "%"),
            BinaryOp::And => write!(f, "&"),
            BinaryOp::Or => write!(f, "|"),
            BinaryOp::Xor => write!(f, "^"),
            BinaryOp::LShift => write!(f, "<<"),
            BinaryOp::RShift => write!(f, ">>"),
            BinaryOp::LAnd => write!(f, "&&"),
            BinaryOp::LOr => write!(f, "||"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NEqual => write!(f, "!="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::LessEq => write!(f, "<="),
            BinaryOp::GreaterEq => write!(f, ">="),
        }
    }
}

fn expect_token(expect: Token, actual: Option<Token>) -> Result<()> {
    match actual {
        Some(x) => {
            if expect != x {
                bail!("Expected {} but found {}", expect, x);
            }
            Ok(())
        }
        None => bail!("Expected {}", expect),
    }
}

fn parse_constant(tokens: &mut VecDeque<Token>) -> Result<i64> {
    match tokens.pop_front() {
        Some(Token::Constant(x)) => Ok(x),
        Some(x) => bail!("Expected a constant but found {}", x),
        None => bail!("Expected a constant but file ended"),
    }
}

fn parse_identifier(tokens: &mut VecDeque<Token>) -> Result<String> {
    match tokens.pop_front() {
        Some(Token::Identifier(x)) => Ok(x),
        Some(x) => bail!("Expected an identifier but found {}", x),
        None => bail!("Expected an identifier but file ended"),
    }
}

fn parse_unop(tokens: &mut VecDeque<Token>) -> Result<UnaryOp> {
    match tokens.pop_front() {
        Some(Token::Complement) => Ok(UnaryOp::Complement),
        Some(Token::Negation) => Ok(UnaryOp::Negation),
        Some(Token::Not) => Ok(UnaryOp::Not),
        Some(x) => bail!("Expected an identifier but found {}", x),
        None => bail!("Expected an identifier but file ended"),
    }
}

fn parse_binop(tokens: &mut VecDeque<Token>) -> Result<BinaryOp> {
    match tokens.pop_front() {
        Some(Token::Addition) => Ok(BinaryOp::Addition),
        Some(Token::Negation) => Ok(BinaryOp::Subtraction),
        Some(Token::Multiplication) => Ok(BinaryOp::Multiplication),
        Some(Token::Division) => Ok(BinaryOp::Division),
        Some(Token::Modulo) => Ok(BinaryOp::Modulo),
        Some(Token::And) => Ok(BinaryOp::And),
        Some(Token::Or) => Ok(BinaryOp::Or),
        Some(Token::Xor) => Ok(BinaryOp::Xor),
        Some(Token::LShift) => Ok(BinaryOp::LShift),
        Some(Token::RShift) => Ok(BinaryOp::RShift),
        Some(Token::LAnd) => Ok(BinaryOp::LAnd),
        Some(Token::LOr) => Ok(BinaryOp::LOr),
        Some(Token::Equal) => Ok(BinaryOp::Equal),
        Some(Token::NEqual) => Ok(BinaryOp::NEqual),
        Some(Token::Less) => Ok(BinaryOp::Less),
        Some(Token::LessEq) => Ok(BinaryOp::LessEq),
        Some(Token::Greater) => Ok(BinaryOp::Greater),
        Some(Token::GreaterEq) => Ok(BinaryOp::GreaterEq),
        Some(x) => bail!("Expected an identifier but found {}", x),
        None => bail!("Expected an identifier but file ended"),
    }
}

fn parse_factor(tokens: &mut VecDeque<Token>) -> Result<Expression> {
    match &tokens[0] {
        Token::Constant(_) => {
            let constant = parse_constant(tokens)?;
            Ok(Expression::Constant(constant))
        }
        Token::Complement | Token::Negation | Token::Not => {
            let unop = parse_unop(tokens)?;
            let expr = parse_factor(tokens)?;
            Ok(Expression::Unary(unop, Box::new(expr)))
        }
        Token::OpenParanthesis => {
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let expr = parse_expression(tokens, 0)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            Ok(expr)
        }
        x => bail!("Broken expression: got {}", x),
    }
}

fn parse_expression(tokens: &mut VecDeque<Token>, order: usize) -> Result<Expression> {
    let mut left = parse_factor(tokens)?;
    while lex::is_binary(&tokens[0]) && lex::precedence(&tokens[0]) >= order {
        let prec = lex::precedence(&tokens[0]);
        let op = parse_binop(tokens)?;
        let right = parse_expression(tokens, prec + 1)?;
        left = Expression::Binary(op, Box::new(left), Box::new(right));
    }
    Ok(left)
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement> {
    expect_token(Token::Return, tokens.pop_front())?;
    let expression = parse_expression(tokens, 0)?;
    expect_token(Token::Semicolon, tokens.pop_front())?;
    Ok(Statement::Return(expression))
}

fn parse_function(tokens: &mut VecDeque<Token>) -> Result<Function> {
    expect_token(Token::Int, tokens.pop_front())?;
    let id = parse_identifier(tokens)?;
    expect_token(Token::OpenBrace, tokens.pop_front())?;
    expect_token(Token::Void, tokens.pop_front())?;
    expect_token(Token::CloseBrace, tokens.pop_front())?;
    expect_token(Token::OpenParanthesis, tokens.pop_front())?;
    let statement = parse_statement(tokens)?;
    expect_token(Token::CloseParanthesis, tokens.pop_front())?;
    Ok(Function::Function(id, statement))
}

pub fn parse_tokens(mut tokens: VecDeque<Token>) -> Result<Ast> {
    let result = Ast::Program(parse_function(&mut tokens)?);

    match tokens.pop_front() {
        Some(x) => bail!("Expected end of code but found {}", x),
        None => Ok(result),
    }
}
