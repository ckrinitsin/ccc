use crate::frontend::{
    lex::{self, Token},
    type_check::Type,
};
use anyhow::{Result, bail};
use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub enum Ast {
    Program(Vec<Declaration>),
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    V(VariableDeclaration),
    F(FunctionDeclaration),
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
pub enum VariableDeclaration {
    D(String, Option<Expression>, Option<StorageClass>),
}

#[derive(Debug, PartialEq)]
pub enum FunctionDeclaration {
    D(String, Vec<String>, Option<Block>, Option<StorageClass>),
}

#[derive(Debug, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>, Option<String>),
    DoWhile(Box<Statement>, Expression, Option<String>),
    For(
        ForInit,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
        Option<String>,
    ),
    Switch(
        Expression,
        Box<Statement>,
        Option<String>,
        Vec<(Option<i32>, String)>,
    ),
    Default(Box<Statement>, Option<String>),
    Case(Expression, Box<Statement>, Option<String>),
    Continue(Option<String>),
    Break(Option<String>),
    Compound(Block),
    Labeled(String, Box<Statement>),
    Goto(String),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum ForInit {
    D(VariableDeclaration),
    E(Option<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Constant(i32),
    Variable(String),
    Unary(UnaryOp, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    CompoundAssignment(BinaryOp, Box<Expression>, Box<Expression>),
    PostIncr(Box<Expression>),
    PostDecr(Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Box<Expression>>),
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

/* constant ::= An i32 */
fn parse_constant(tokens: &mut VecDeque<Token>) -> Result<i32> {
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

/* factor ::= <unop> <factor> | <first_expr> <postfixes>
 *  | <identifier> "(" [ <exp> { "," <exp> } ] ")" */
fn parse_factor(tokens: &mut VecDeque<Token>) -> Result<Expression> {
    match (&tokens[0], &tokens[1]) {
        (Token::Complement, _)
        | (Token::Negation, _)
        | (Token::Not, _)
        | (Token::Increment, _)
        | (Token::Decrement, _) => {
            let unop = parse_unop(tokens)?;
            let expr = parse_factor(tokens)?;
            Ok(Expression::Unary(unop, Box::new(expr)))
        }
        (Token::Identifier(_), Token::OpenParanthesis) => {
            let id = parse_identifier(tokens)?;
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let mut args = Vec::new();
            while tokens[0] != Token::CloseParanthesis {
                if tokens[0] == Token::Comma {
                    expect_token(Token::Comma, tokens.pop_front())?;
                }
                args.push(Box::new(parse_expression(tokens, 0)?));
            }
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;

            Ok(Expression::FunctionCall(id, args))
        }
        _ => {
            let expr = parse_first_expr(tokens)?;
            parse_postfixes(tokens, expr)
        }
    }
}

/* expression ::= <factor> | <expression> <binop> <expression>
 *  | <expression> "?" <expression> ":" <expression> */
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

/* for_init ::= <declaration> | [ <expression> ] ";" */
fn parse_for_init(tokens: &mut VecDeque<Token>) -> Result<ForInit> {
    match &tokens[0] {
        Token::Int | Token::Extern | Token::Static => {
            let declaration = parse_declaration(tokens)?;
            match declaration {
                Declaration::V(variable_declaration) => Ok(ForInit::D(variable_declaration)),
                Declaration::F(_) => {
                    bail!("Function declaration is not allowed in initializer of for loop")
                }
            }
        }
        Token::Semicolon => {
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(ForInit::E(None))
        }
        _ => {
            let expr = Some(parse_expression(tokens, 0)?);
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(ForInit::E(expr))
        }
    }
}

/* statement ::= "return" <expression> ";" | ";" | <expression> ";"
 *  | "if" "(" <expression> ")" <statement> [ "else" <statement> ]
 *  | "while" "(" <expression> ")" <statement>
 *  | "do" <statement> "while" "(" <expression> ")" ";"
 *  | "for" "(" <for-init> [ <expression> ] ";" [ <expression> ] ")" <statement>
 *  | "switch" "(" <expression> ")" <statement>
 *  | "case" <expression> ":" <statement>
 *  | "default" ":" <statement>
 *  | "continue" ";"
 *  | "break" ";"
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
        (Token::Continue, _) => {
            expect_token(Token::Continue, tokens.pop_front())?;
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Statement::Continue(None))
        }
        (Token::Break, _) => {
            expect_token(Token::Break, tokens.pop_front())?;
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Statement::Break(None))
        }
        (Token::While, _) => {
            expect_token(Token::While, tokens.pop_front())?;
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let condition = parse_expression(tokens, 0)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            let body = parse_statement(tokens)?;
            Ok(Statement::While(condition, Box::new(body), None))
        }
        (Token::Do, _) => {
            expect_token(Token::Do, tokens.pop_front())?;
            let body = parse_statement(tokens)?;
            expect_token(Token::While, tokens.pop_front())?;
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let condition = parse_expression(tokens, 0)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Statement::DoWhile(Box::new(body), condition, None))
        }
        (Token::For, _) => {
            expect_token(Token::For, tokens.pop_front())?;
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let for_init = parse_for_init(tokens)?;
            let mut condition = None;
            if !matches!(tokens[0], Token::Semicolon) {
                condition = Some(parse_expression(tokens, 0)?);
            }
            expect_token(Token::Semicolon, tokens.pop_front())?;
            let mut step = None;
            if !matches!(tokens[0], Token::CloseParanthesis) {
                step = Some(parse_expression(tokens, 0)?);
            }
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            let statement = parse_statement(tokens)?;
            Ok(Statement::For(
                for_init,
                condition,
                step,
                Box::new(statement),
                None,
            ))
        }
        (Token::Default, _) => {
            expect_token(Token::Default, tokens.pop_front())?;
            expect_token(Token::Colon, tokens.pop_front())?;
            let statement = parse_statement(tokens)?;
            Ok(Statement::Default(Box::new(statement), None))
        }
        (Token::Case, _) => {
            expect_token(Token::Case, tokens.pop_front())?;
            let expression = parse_expression(tokens, 0)?;
            expect_token(Token::Colon, tokens.pop_front())?;
            let statement = parse_statement(tokens)?;
            Ok(Statement::Case(expression, Box::new(statement), None))
        }
        (Token::Switch, _) => {
            expect_token(Token::Switch, tokens.pop_front())?;
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let expression = parse_expression(tokens, 0)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            let statement = parse_statement(tokens)?;
            Ok(Statement::Switch(
                expression,
                Box::new(statement),
                None,
                Vec::new(),
            ))
        }
        _ => {
            let expression = parse_expression(tokens, 0)?;
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Statement::Expression(expression))
        }
    }
}

/* block-item ::= <statement> | <declaration> */
fn parse_block_item(tokens: &mut VecDeque<Token>) -> Result<BlockItem> {
    match &tokens[0] {
        Token::Int | Token::Extern | Token::Static => Ok(BlockItem::D(parse_declaration(tokens)?)),
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

/* param-list ::= "void" | "int" <identifier> { "," "int" <identifier> } */
fn parse_param_list(tokens: &mut VecDeque<Token>) -> Result<Vec<String>> {
    match &tokens[0] {
        Token::Void => {
            expect_token(Token::Void, tokens.pop_front())?;
            Ok(Vec::new())
        }
        Token::Int => {
            expect_token(Token::Int, tokens.pop_front())?;
            let mut result = Vec::new();
            result.push(parse_identifier(tokens)?);
            while tokens[0] == Token::Comma {
                expect_token(Token::Comma, tokens.pop_front())?;
                expect_token(Token::Int, tokens.pop_front())?;
                result.push(parse_identifier(tokens)?);
            }
            Ok(result)
        }
        x => bail!("Wrong token in parameter list: {}", x),
    }
}

fn is_specifier(token: &Token) -> bool {
    match token {
        Token::Int | Token::Extern | Token::Static => true,
        _ => false,
    }
}

/* specifier ::= { "int" | "static" | "extern" }+ */
fn parse_specifier(tokens: &mut VecDeque<Token>) -> Result<(Type, Option<StorageClass>)> {
    let mut types: Vec<Type> = Vec::new();
    let mut storage_classes: Vec<StorageClass> = Vec::new();
    while is_specifier(&tokens[0]) {
        match tokens.pop_front() {
            Some(Token::Int) => types.push(Type::Int),
            Some(Token::Extern) => storage_classes.push(StorageClass::Extern),
            Some(Token::Static) => storage_classes.push(StorageClass::Static),
            x => bail!("Expected specifier, got {:?}", x),
        }
    }

    if types.len() != 1 {
        bail!("Invalid type specifier: {:?}", types);
    }

    if storage_classes.len() > 1 {
        bail!("Invalid storage class specifier: {:?}", storage_classes);
    }

    let Some(type_specifier) = types.pop() else {
        bail!("Invalid type specifier: {:?}", types);
    };

    Ok((type_specifier, storage_classes.pop()))
}

/* declaration ::= <specifier> ( <variable_declaration> | <function_declaration> ) */
/* function_declaration ::= "int" <identifier> "(" <param-list> ")" ( <block> | ";" ) */
/* variable_declaration ::= "int" <identifier> [ "=" <expression> ] ";" */
fn parse_declaration(tokens: &mut VecDeque<Token>) -> Result<Declaration> {
    let specifier = parse_specifier(tokens)?;
    let id = parse_identifier(tokens)?;
    match &tokens[0] {
        Token::OpenParanthesis => {
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let param_list = parse_param_list(tokens)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            let block = match &tokens[0] {
                Token::Semicolon => {
                    expect_token(Token::Semicolon, tokens.pop_front())?;
                    None
                }
                _ => Some(parse_block(tokens)?),
            };
            Ok(Declaration::F(FunctionDeclaration::D(
                id,
                param_list,
                block,
                specifier.1,
            )))
        }
        Token::Assignment => {
            expect_token(Token::Assignment, tokens.pop_front())?;
            let expression = parse_expression(tokens, 0)?;
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Declaration::V(VariableDeclaration::D(
                id,
                Some(expression),
                specifier.1,
            )))
        }
        Token::Semicolon => {
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Declaration::V(VariableDeclaration::D(
                id,
                None,
                specifier.1,
            )))
        }
        x => bail!("Expected variable or function declaration, got {}", x),
    }
}

/* program ::= { <function-declaration> } */
pub fn parse_tokens(mut tokens: VecDeque<Token>) -> Result<Ast> {
    let mut result = Vec::new();
    while tokens.len() > 0 && is_specifier(&tokens[0]) {
        result.push(parse_declaration(&mut tokens)?);
    }

    match tokens.pop_front() {
        Some(x) => bail!("Expected end of code but found {}", x),
        None => Ok(Ast::Program(result)),
    }
}
