use ccc::frontend::lex::{Token, lex};

#[test]
fn valid1() {
    let input = "
    int main(void) {
        return 2;
    }
    ";

    let result = lex(input.to_string()).unwrap();
    let expected = vec![
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
    ];

    assert_eq!(result, expected);
}

#[test]
fn valid2() {
    let input = "
    int main(void) {
        return
    }
    ";

    let result = lex(input.to_string()).unwrap();
    let expected = vec![
        Token::Int,
        Token::Identifier("main".to_string()),
        Token::OpenParanthesis,
        Token::Void,
        Token::CloseParanthesis,
        Token::OpenBrace,
        Token::Return,
        Token::CloseBrace,
    ];

    assert_eq!(result, expected);
}

#[test]
fn invalid() {
    let input = "
    int main(void) {
        123ab
    }
    ";

    let result = lex(input.to_string());

    assert!(result.is_err());
}
