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
        Token::OpenBrace,
        Token::Void,
        Token::CloseBrace,
        Token::OpenParanthesis,
        Token::Return,
        Token::Constant(2),
        Token::Semicolon,
        Token::CloseParanthesis,
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
        Token::OpenBrace,
        Token::Void,
        Token::CloseBrace,
        Token::OpenParanthesis,
        Token::Return,
        Token::CloseParanthesis,
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
