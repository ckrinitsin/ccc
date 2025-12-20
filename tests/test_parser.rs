use ccc::frontend::lex::Token;
use ccc::frontend::parser::*;
use std::collections::VecDeque;

#[test]
fn valid1() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenBrace,
        Token::Void,
        Token::CloseBrace,
        Token::OpenParanthesis,
        Token::Return,
        Token::Constant(2),
        Token::Semicolon,
        Token::CloseParanthesis,
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
        Token::OpenBrace,
        Token::Void,
        Token::CloseBrace,
        Token::OpenParanthesis,
        Token::Return,
        Token::Constant(2),
        Token::CloseParanthesis,
    ]);

    let result = parse_tokens(input);

    assert!(result.is_err());
}

#[test]
fn invalid2() {
    let input = VecDeque::from([
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenBrace,
        Token::Void,
        Token::CloseBrace,
        Token::OpenParanthesis,
        Token::Return,
        Token::Constant(2),
        Token::CloseParanthesis,
        Token::Identifier("foo".to_string()),
    ]);

    let result = parse_tokens(input);

    assert!(result.is_err());
}
