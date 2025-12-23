use crate::frontend::lex::{self, Token};
use anyhow::{Result, bail};
use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub enum Ast {
    Program(Function),
}

#[derive(Debug, PartialEq)]
pub enum Function {
    Function(String, Block),
}

#[derive(Debug, PartialEq)]
pub enum Block {
    B(Vec<BlockItem>),
}

#[derive(Debug, PartialEq)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Declaration(String, Option<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Block),
    Labeled(String, Box<Statement>),
    Goto(String),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Constant(i64),
    Variable(String),
    Unary(UnaryOp, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    CompoundAssignment(BinaryOp, Box<Expression>, Box<Expression>),
    PostIncr(Box<Expression>),
    PostDecr(Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negation,
    Not,
    Increment,
    Decrement,
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

    Assignment,
}

fn get_compound_operator(tok: Option<Token>) -> Result<Option<BinaryOp>> {
    match tok {
        Some(Token::Assignment) => Ok(None),
        Some(Token::CAddition) => Ok(Some(BinaryOp::Addition)),
        Some(Token::CNegation) => Ok(Some(BinaryOp::Subtraction)),
        Some(Token::CMultiplication) => Ok(Some(BinaryOp::Multiplication)),
        Some(Token::CDivision) => Ok(Some(BinaryOp::Division)),
        Some(Token::CModulo) => Ok(Some(BinaryOp::Modulo)),
        Some(Token::CAnd) => Ok(Some(BinaryOp::And)),
        Some(Token::COr) => Ok(Some(BinaryOp::Or)),
        Some(Token::CXor) => Ok(Some(BinaryOp::Xor)),
        Some(Token::CLShift) => Ok(Some(BinaryOp::LShift)),
        Some(Token::CRShift) => Ok(Some(BinaryOp::RShift)),
        Some(_) => bail!("Expected compound operator"),
        None => bail!("End of file"),
    }
}

fn is_assignment(tok: &Token) -> bool {
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

fn expect_token(expect: Token, actual: Option<Token>) -> Result<()> {
    match actual {
        Some(x) => {
            if expect != x {
                match x {
                    Token::Identifier(val) => bail!("Expected {} but found {}", expect, val),
                    Token::Constant(val) => bail!("Expected {} but found {}", expect, val),
                    val => bail!("Expected {} but found {}", expect, val),
                }
            }
            Ok(())
        }
        None => bail!("Expected {}", expect),
    }
}

/* constant ::= An i64 */
fn parse_constant(tokens: &mut VecDeque<Token>) -> Result<i64> {
    match tokens.pop_front() {
        Some(Token::Constant(x)) => Ok(x),
        Some(x) => bail!("Expected a constant but found {}", x),
        None => bail!("Expected a constant but file ended"),
    }
}

/* identifier ::= A String */
fn parse_identifier(tokens: &mut VecDeque<Token>) -> Result<String> {
    match tokens.pop_front() {
        Some(Token::Identifier(x)) => Ok(x),
        Some(x) => bail!("Expected an identifier but found {}", x),
        None => bail!("Expected an identifier but file ended"),
    }
}

/* unop ::= "~" | "-" | ... | "--" */
fn parse_unop(tokens: &mut VecDeque<Token>) -> Result<UnaryOp> {
    match tokens.pop_front() {
        Some(Token::Complement) => Ok(UnaryOp::Complement),
        Some(Token::Negation) => Ok(UnaryOp::Negation),
        Some(Token::Not) => Ok(UnaryOp::Not),
        Some(Token::Increment) => Ok(UnaryOp::Increment),
        Some(Token::Decrement) => Ok(UnaryOp::Decrement),
        Some(x) => bail!("Expected an identifier but found {}", x),
        None => bail!("Expected an identifier but file ended"),
    }
}

/* binop ::= "+" | "-" | ... | "=" */
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
        Some(Token::Assignment) => Ok(BinaryOp::Assignment),
        Some(x) => bail!("Expected an identifier but found {}", x),
        None => bail!("Expected an identifier but file ended"),
    }
}

/* first_expr ::= <constant> | <identifier> | "(" <exp> ")" */
fn parse_first_expr(tokens: &mut VecDeque<Token>) -> Result<Expression> {
    match &tokens[0] {
        Token::Constant(_) => {
            let constant = parse_constant(tokens)?;
            Ok(Expression::Constant(constant))
        }
        Token::OpenParanthesis => {
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let expr = parse_expression(tokens, 0)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            Ok(expr)
        }
        Token::Identifier(_) => {
            let id = parse_identifier(tokens)?;
            Ok(Expression::Variable(id))
        }
        x => bail!("Broken expression: got {}", x),
    }
}

/* postfixes ::= { "++" | "--" } */
fn parse_postfixes(tokens: &mut VecDeque<Token>, expr: Expression) -> Result<Expression> {
    match &tokens[0] {
        Token::Increment => {
            expect_token(Token::Increment, tokens.pop_front())?;
            parse_postfixes(tokens, Expression::PostIncr(Box::new(expr)))
        }
        Token::Decrement => {
            expect_token(Token::Decrement, tokens.pop_front())?;
            parse_postfixes(tokens, Expression::PostDecr(Box::new(expr)))
        }
        _ => Ok(expr),
    }
}

/* factor ::= <unop> <factor> | <first_expr> <postfixes> */
fn parse_factor(tokens: &mut VecDeque<Token>) -> Result<Expression> {
    match &tokens[0] {
        Token::Complement | Token::Negation | Token::Not | Token::Increment | Token::Decrement => {
            let unop = parse_unop(tokens)?;
            let expr = parse_factor(tokens)?;
            Ok(Expression::Unary(unop, Box::new(expr)))
        }
        _ => {
            let expr = parse_first_expr(tokens)?;
            parse_postfixes(tokens, expr)
        }
    }
}

/* expression ::= <factor> | <expression> <binop> <expression>
 *  | <expression> "?" <expression> ":" <expression>*/
fn parse_expression(tokens: &mut VecDeque<Token>, order: usize) -> Result<Expression> {
    let mut left = parse_factor(tokens)?;
    while lex::is_binary(&tokens[0]) && lex::precedence(&tokens[0]) >= order {
        let prec = lex::precedence(&tokens[0]);
        if is_assignment(&tokens[0]) {
            let compound_op = tokens.pop_front();
            let right = parse_expression(tokens, prec)?;
            left = match get_compound_operator(compound_op)? {
                None => Expression::Assignment(Box::new(left), Box::new(right)),
                Some(x) => Expression::CompoundAssignment(x, Box::new(left), Box::new(right)),
            };
        } else if matches!(tokens[0], Token::QuestionMark) {
            expect_token(Token::QuestionMark, tokens.pop_front())?;
            let middle = parse_expression(tokens, 0)?;
            expect_token(Token::Colon, tokens.pop_front())?;
            let right = parse_expression(tokens, prec)?;
            left = Expression::Conditional(Box::new(left), Box::new(middle), Box::new(right));
        } else {
            let op = parse_binop(tokens)?;
            let right = parse_expression(tokens, prec + 1)?;
            left = Expression::Binary(op, Box::new(left), Box::new(right));
        }
    }
    Ok(left)
}

/* statement ::= "return" <expression> ";" | ";" | <expression> ";"
 *  | "if" "(" <expression> ")" <statement> [ "else" <statement> ]
 *  | <identifier> ":" <statement>
 *  | "goto" <identifier> ";"
 *  | <block> */
fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement> {
    match (&tokens[0], &tokens[1]) {
        (Token::Return, _) => {
            expect_token(Token::Return, tokens.pop_front())?;
            let expression = parse_expression(tokens, 0)?;
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Statement::Return(expression))
        }
        (Token::Semicolon, _) => {
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Statement::Null)
        }
        (Token::If, _) => {
            expect_token(Token::If, tokens.pop_front())?;
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let condition = parse_expression(tokens, 0)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            let if_statement = Box::new(parse_statement(tokens)?);
            let mut else_statement = None;
            if matches!(&tokens[0], Token::Else) {
                expect_token(Token::Else, tokens.pop_front())?;
                else_statement = Some(Box::new(parse_statement(tokens)?));
            }
            Ok(Statement::If(condition, if_statement, else_statement))
        }
        (Token::Identifier(_), Token::Colon) => {
            let id = parse_identifier(tokens)?;
            expect_token(Token::Colon, tokens.pop_front())?;
            let statement = parse_statement(tokens)?;
            return Ok(Statement::Labeled(id, Box::new(statement)));
        }
        (Token::Goto, _) => {
            expect_token(Token::Goto, tokens.pop_front())?;
            let label = Statement::Goto(parse_identifier(tokens)?);
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(label)
        }
        (Token::OpenBrace, _) => {
            let block = parse_block(tokens)?;
            Ok(Statement::Compound(block))
        }
        _ => {
            let expression = parse_expression(tokens, 0)?;
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Statement::Expression(expression))
        }
    }
}

/* declaration ::= "int" <identifier> [ "=" <expression> ] ";" */
fn parse_declaration(tokens: &mut VecDeque<Token>) -> Result<Declaration> {
    expect_token(Token::Int, tokens.pop_front())?;
    let id = parse_identifier(tokens)?;
    match tokens.pop_front() {
        Some(Token::Assignment) => {
            let expression = parse_expression(tokens, 0)?;
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Declaration::Declaration(id, Some(expression)))
        }
        Some(Token::Semicolon) => Ok(Declaration::Declaration(id, None)),
        Some(x) => bail!("Broken declaration: got {}", x),
        None => bail!("Broken declaration: end of file"),
    }
}

/* block-item ::= <statement> | <declaration> */
fn parse_block_item(tokens: &mut VecDeque<Token>) -> Result<BlockItem> {
    match &tokens[0] {
        Token::Int => Ok(BlockItem::D(parse_declaration(tokens)?)),
        _ => Ok(BlockItem::S(parse_statement(tokens)?)),
    }
}

/* block ::= "{" { <block-item> }  "}" */
fn parse_block(tokens: &mut VecDeque<Token>) -> Result<Block> {
    expect_token(Token::OpenBrace, tokens.pop_front())?;
    let mut body = Vec::new();
    while tokens[0] != Token::CloseBrace {
        body.push(parse_block_item(tokens)?);
    }
    expect_token(Token::CloseBrace, tokens.pop_front())?;
    Ok(Block::B(body))
}

/* function ::= "int" <identifier> "(" "void" ")" <block> */
fn parse_function(tokens: &mut VecDeque<Token>) -> Result<Function> {
    expect_token(Token::Int, tokens.pop_front())?;
    let id = parse_identifier(tokens)?;
    expect_token(Token::OpenParanthesis, tokens.pop_front())?;
    expect_token(Token::Void, tokens.pop_front())?;
    expect_token(Token::CloseParanthesis, tokens.pop_front())?;
    let block = parse_block(tokens)?;
    Ok(Function::Function(id, block))
}

/* program ::= <function> */
pub fn parse_tokens(mut tokens: VecDeque<Token>) -> Result<Ast> {
    let result = Ast::Program(parse_function(&mut tokens)?);

    match tokens.pop_front() {
        Some(x) => bail!("Expected end of code but found {}", x),
        None => Ok(result),
    }
}
