use anyhow::{Result, bail};
use crate::lex::Token;
use std::fmt;
use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub enum Ast {
    Program(Box<Ast>),
    Function(Box<Ast>, Box<Ast>),
    Statement(Box<Ast>),
    Expression(Box<Ast>),
    Identifier(String),
    Constant(i64),
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ast::Program(x) => write!(f, "Program:\n{}", *x),
            Ast::Function(name, body) => write!(f, "Function '{}':\n{}", *name, *body),
            Ast::Statement(x) => write!(f, "Return:\n{}", *x),
            Ast::Expression(x) => write!(f, "Constant({})", *x),
            Ast::Identifier(x) => write!(f, "{}", x),
            Ast::Constant(x) => write!(f, "{}", x),
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
        },
        None => bail!("Expected {}", expect),
    }
}

fn parse_constant(tokens: &mut VecDeque<Token>) -> Result<Box<Ast>> {
    match tokens.pop_front() {
        Some(Token::Constant(x)) => Ok(Box::new(Ast::Constant(x))),
        Some(x) => bail!("Expected a constant but found {}", x),
        None => bail!("Expected a constant but file ended"),
    }
}

fn parse_identifier(tokens: &mut VecDeque<Token>) -> Result<Box<Ast>> {
    match tokens.pop_front() {
        Some(Token::Identifier(x)) => Ok(Box::new(Ast::Identifier(x))),
        Some(x) => bail!("Expected an identifier but found {}", x),
        None => bail!("Expected an identifier but file ended"),
    }
}

fn parse_expression(tokens: &mut VecDeque<Token>) -> Result<Box<Ast>> {
    let constant = parse_constant(tokens)?;
    Ok(Box::new(Ast::Expression(constant)))
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Box<Ast>> {
    expect_token(Token::Return, tokens.pop_front())?;
    let expression = parse_expression(tokens)?;
    expect_token(Token::Semicolon, tokens.pop_front())?;
    Ok(Box::new(Ast::Statement(expression)))
}

fn parse_function(tokens: &mut VecDeque<Token>) -> Result<Box<Ast>> {
    expect_token(Token::Int, tokens.pop_front())?;
    let id = parse_identifier(tokens)?;
    expect_token(Token::OpenBrace, tokens.pop_front())?;
    expect_token(Token::Void, tokens.pop_front())?;
    expect_token(Token::CloseBrace, tokens.pop_front())?;
    expect_token(Token::OpenParanthesis, tokens.pop_front())?;
    let statement = parse_statement(tokens)?;
    expect_token(Token::CloseParanthesis, tokens.pop_front())?;
    Ok(Box::new(Ast::Function(id, statement)))
}

pub fn parse_tokens(mut tokens: VecDeque<Token>) -> Result<Ast> {
    let result = Ast::Program(parse_function(&mut tokens)?);

    match tokens.pop_front() {
        Some(x) => bail!("Expected end of code but found {}", x),
        None => Ok(result),
    }
}
