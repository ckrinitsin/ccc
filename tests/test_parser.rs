use ccc::lex::Token;
use ccc::parser::{parse_tokens, Ast};
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
    let expected = Ast::Program(Box::new(Ast::Function(
        Box::new(Ast::Identifier("main".to_string())),
        Box::new(Ast::Statement(Box::new(Ast::Expression(Box::new(Ast::Constant(2)))))),
    )));

    println!("{}", result);

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
