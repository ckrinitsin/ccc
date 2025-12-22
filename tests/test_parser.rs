use ccc::frontend::lex::Token;
use ccc::frontend::parser::*;
use std::collections::VecDeque;

#[test]
fn valid1() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenParanthesis,
        Token::Void,
        Token::CloseParanthesis,
        Token::OpenBrace,
        Token::Return,
        Token::Constant(2),
        Token::Semicolon,
        Token::CloseBrace,
    ]);

    let result = parse_tokens(input).unwrap();
    let expected = Ast::Program(Function::Function(
        "main".to_string(),
        Statement::Return(Expression::Constant(2)),
    ));

    assert_eq!(result, expected);
}

#[test]
fn invalid1() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenParanthesis,
        Token::Void,
        Token::CloseParanthesis,
        Token::OpenBrace,
        Token::Return,
        Token::Constant(2),
        Token::CloseBrace,
    ]);

    let result = parse_tokens(input);

    assert!(result.is_err());
}

#[test]
fn binary_simple() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenParanthesis,
        Token::Void,
        Token::CloseParanthesis,
        Token::OpenBrace,
        Token::Return,
        Token::Constant(2),
        Token::Addition,
        Token::Constant(3),
        Token::Negation,
        Token::Constant(4),
        Token::Semicolon,
        Token::CloseBrace,
    ]);

    let result = parse_tokens(input).unwrap();
    let expected = Ast::Program(Function::Function(
        "main".to_string(),
        Statement::Return(Expression::Binary(
            BinaryOp::Subtraction,
            Box::new(Expression::Binary(
                BinaryOp::Addition,
                Box::new(Expression::Constant(2)),
                Box::new(Expression::Constant(3)),
            )),
            Box::new(Expression::Constant(4)),
        )),
    ));

    assert_eq!(result, expected);
}

#[test]
fn binary_precedence() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenParanthesis,
        Token::Void,
        Token::CloseParanthesis,
        Token::OpenBrace,
        Token::Return,
        Token::Constant(2),
        Token::Multiplication,
        Token::Constant(3),
        Token::Negation,
        Token::Constant(4),
        Token::Semicolon,
        Token::CloseBrace,
    ]);

    let result = parse_tokens(input).unwrap();
    let expected = Ast::Program(Function::Function(
        "main".to_string(),
        Statement::Return(Expression::Binary(
            BinaryOp::Subtraction,
            Box::new(Expression::Binary(
                BinaryOp::Multiplication,
                Box::new(Expression::Constant(2)),
                Box::new(Expression::Constant(3)),
            )),
            Box::new(Expression::Constant(4)),
        )),
    ));

    assert_eq!(result, expected);
}

#[test]
fn binary_precedence_switch() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenParanthesis,
        Token::Void,
        Token::CloseParanthesis,
        Token::OpenBrace,
        Token::Return,
        Token::Constant(3),
        Token::Addition,
        Token::Constant(4),
        Token::Multiplication,
        Token::Constant(8),
        Token::Semicolon,
        Token::CloseBrace,
    ]);

    let result = parse_tokens(input).unwrap();
    let expected = Ast::Program(Function::Function(
        "main".to_string(),
        Statement::Return(Expression::Binary(
            BinaryOp::Addition,
            Box::new(Expression::Constant(3)),
            Box::new(Expression::Binary(
                BinaryOp::Multiplication,
                Box::new(Expression::Constant(4)),
                Box::new(Expression::Constant(8)),
            )),
        )),
    ));

    assert_eq!(result, expected);
}

#[test]
fn braces_over_precedence() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenParanthesis,
        Token::Void,
        Token::CloseParanthesis,
        Token::OpenBrace,
        Token::Return,
        Token::OpenParanthesis,
        Token::Constant(3),
        Token::Addition,
        Token::Constant(4),
        Token::CloseParanthesis,
        Token::Multiplication,
        Token::Constant(8),
        Token::Semicolon,
        Token::CloseBrace,
    ]);

    let result = parse_tokens(input).unwrap();
    let expected = Ast::Program(Function::Function(
        "main".to_string(),
        Statement::Return(Expression::Binary(
            BinaryOp::Multiplication,
            Box::new(Expression::Binary(
                BinaryOp::Addition,
                Box::new(Expression::Constant(3)),
                Box::new(Expression::Constant(4)),
            )),
            Box::new(Expression::Constant(8)),
        )),
    ));

    assert_eq!(result, expected);
}

#[test]
fn invalid_braces() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenParanthesis,
        Token::Void,
        Token::CloseParanthesis,
        Token::OpenBrace,
        Token::Return,
        Token::OpenParanthesis,
        Token::Constant(3),
        Token::Addition,
        Token::CloseParanthesis,
        Token::Constant(4),
        Token::Multiplication,
        Token::Constant(8),
        Token::Semicolon,
        Token::CloseBrace,
    ]);

    let result = parse_tokens(input);

    assert!(result.is_err());
}

#[test]
fn invalid2() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenParanthesis,
        Token::Void,
        Token::CloseParanthesis,
        Token::OpenBrace,
        Token::Return,
        Token::Constant(2),
        Token::CloseBrace,
        Token::Identifier("foo".to_string()),
    ]);

    let result = parse_tokens(input);

    assert!(result.is_err());
}
