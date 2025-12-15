use crate::lex::Token;
use anyhow::{bail, Result};
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
            Expression::Constant(x) => write!(f, "{}", x),
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

fn parse_expression(tokens: &mut VecDeque<Token>) -> Result<Expression> {
    let constant = parse_constant(tokens)?;
    Ok(Expression::Constant(constant))
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement> {
    expect_token(Token::Return, tokens.pop_front())?;
    let expression = parse_expression(tokens)?;
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
