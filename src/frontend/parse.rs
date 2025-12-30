use crate::frontend::{
    ast::*,
    lex::{self, Token},
};
use anyhow::{Result, bail};
use std::collections::VecDeque;

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

fn expect_token(expect: Token, actual: Option<Token>) -> Result<()> {
    match actual {
        Some(x) => {
            if expect != x {
                match x {
                    Token::Identifier(val) => bail!("Expected {} but found {}", expect, val),
                    Token::IntConstant(val) => bail!("Expected {} but found {}", expect, val),
                    Token::LongConstant(val) => bail!("Expected {} but found {}", expect, val),
                    val => bail!("Expected {} but found {}", expect, val),
                }
            }
            Ok(())
        }
        None => bail!("Expected {}", expect),
    }
}

/* constant ::= An int or a long */
fn parse_constant(tokens: &mut VecDeque<Token>) -> Result<Const> {
    match tokens.pop_front() {
        Some(Token::IntConstant(x)) => {
            if x <= 2_i64.pow(31) - 1 {
                Ok(Const::Int(x as i32))
            } else {
                Ok(Const::Long(x))
            }
        }
        Some(Token::LongConstant(x)) => Ok(Const::Long(x)),
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

/* type_specifier ::= "int" | "long" */
fn parse_type_specifier(tokens: &mut VecDeque<Token>) -> Result<Type> {
    match tokens.pop_front() {
        Some(Token::Int) => Ok(Type::Int),
        Some(Token::Long) => Ok(Type::Long),
        Some(x) => bail!("Expected type specifier but got {}", x),
        None => bail!("Expected a type specifier but file ended"),
    }
}

/* type_specifier_list ::= { type_specifier }+ */
fn parse_type_specifier_list(tokens: &mut VecDeque<Token>) -> Result<Type> {
    let mut types = Vec::new();
    while lex::is_type_specifier(&tokens[0]) {
        types.push(parse_type_specifier(tokens)?);
    }
    parse_type_helper(types)
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
        x if lex::is_constant(x) => {
            let constant = parse_constant(tokens)?;
            Ok(Expression::Constant(constant, None))
        }
        Token::OpenParanthesis => {
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let expr = parse_expression(tokens, 0)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            Ok(expr)
        }
        Token::Identifier(_) => {
            let id = parse_identifier(tokens)?;
            Ok(Expression::Variable(id, None))
        }
        x => bail!("Broken expression: got {}", x),
    }
}

/* postfixes ::= { "++" | "--" } */
fn parse_postfixes(tokens: &mut VecDeque<Token>, expr: Expression) -> Result<Expression> {
    match &tokens[0] {
        Token::Increment => {
            expect_token(Token::Increment, tokens.pop_front())?;
            parse_postfixes(tokens, Expression::PostIncr(Box::new(expr), None))
        }
        Token::Decrement => {
            expect_token(Token::Decrement, tokens.pop_front())?;
            parse_postfixes(tokens, Expression::PostDecr(Box::new(expr), None))
        }
        _ => Ok(expr),
    }
}

/* factor ::= <unop> <factor> | <first_expr> <postfixes>
 *  | <identifier> "(" [ <exp> { "," <exp> } ] ")"
 *  | "(" <type_specifier> ")" <factor> */
fn parse_factor(tokens: &mut VecDeque<Token>) -> Result<Expression> {
    match (&tokens[0], &tokens[1]) {
        (Token::Complement, _)
        | (Token::Negation, _)
        | (Token::Not, _)
        | (Token::Increment, _)
        | (Token::Decrement, _) => {
            let unop = parse_unop(tokens)?;
            let expr = parse_factor(tokens)?;
            Ok(Expression::Unary(unop, Box::new(expr), None))
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

            Ok(Expression::FunctionCall(id, args, None))
        }
        (Token::OpenParanthesis, n) if lex::is_type_specifier(n) => {
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let type_spec = parse_type_specifier(tokens)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            let factor = parse_factor(tokens)?;
            Ok(Expression::Cast(type_spec, Box::new(factor), None))
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
        if lex::is_assignment(&tokens[0]) {
            let compound_op = tokens.pop_front();
            let right = parse_expression(tokens, prec)?;
            left = match get_compound_operator(compound_op)? {
                None => Expression::Assignment(Box::new(left), Box::new(right), None),
                Some(x) => Expression::CompoundAssignment(x, Box::new(left), Box::new(right), None),
            };
        } else if matches!(tokens[0], Token::QuestionMark) {
            expect_token(Token::QuestionMark, tokens.pop_front())?;
            let middle = parse_expression(tokens, 0)?;
            expect_token(Token::Colon, tokens.pop_front())?;
            let right = parse_expression(tokens, prec)?;
            left = Expression::Conditional(Box::new(left), Box::new(middle), Box::new(right), None);
        } else {
            let op = parse_binop(tokens)?;
            let right = parse_expression(tokens, prec + 1)?;
            left = Expression::Binary(op, Box::new(left), Box::new(right), None);
        }
    }
    Ok(left)
}

/* for_init ::= <declaration> | [ <expression> ] ";" */
fn parse_for_init(tokens: &mut VecDeque<Token>) -> Result<ForInit> {
    if lex::is_specifier(&tokens[0]) {
        let declaration = parse_declaration(tokens)?;
        match declaration {
            Declaration::V(variable_declaration) => return Ok(ForInit::D(variable_declaration)),
            Declaration::F(_) => {
                bail!("Function declaration is not allowed in initializer of for loop");
            }
        }
    }
    match &tokens[0] {
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
    if lex::is_specifier(&tokens[0]) {
        Ok(BlockItem::D(parse_declaration(tokens)?))
    } else {
        Ok(BlockItem::S(parse_statement(tokens)?))
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

/* param-list ::= "void" | <type_specifier> <identifier> { "," <type_specifier> <identifier> } */
fn parse_param_list(tokens: &mut VecDeque<Token>) -> Result<(Vec<Type>, Vec<String>)> {
    match &tokens[0] {
        Token::Void => {
            expect_token(Token::Void, tokens.pop_front())?;
            Ok((Vec::new(), Vec::new()))
        }
        n if lex::is_type_specifier(n) => {
            let mut types = Vec::new();
            let mut ids = Vec::new();
            types.push(parse_type_specifier_list(tokens)?);
            ids.push(parse_identifier(tokens)?);
            while tokens[0] == Token::Comma {
                expect_token(Token::Comma, tokens.pop_front())?;
                types.push(parse_type_specifier_list(tokens)?);
                ids.push(parse_identifier(tokens)?);
            }
            Ok((types, ids))
        }
        x => bail!("Wrong token in parameter list: {}", x),
    }
}

/* Helper for extracting the correct type from different combinations */
fn parse_type_helper(types: Vec<Type>) -> Result<Type> {
    match types[..] {
        [Type::Int] => Ok(Type::Int),
        [Type::Long] | [Type::Int, Type::Long] | [Type::Long, Type::Int] => Ok(Type::Long),
        _ => bail!("Invalid type specifier {:?}", types),
    }
}

/* storage_class_specifier ::= "static" | "extern" */
fn parse_storage_class_specifier(tokens: &mut VecDeque<Token>) -> Result<StorageClass> {
    match tokens.pop_front() {
        Some(Token::Extern) => Ok(StorageClass::Extern),
        Some(Token::Static) => Ok(StorageClass::Static),
        Some(x) => bail!("Expected a storage class specifier but found {}", x),
        None => bail!("Expected a storage class specifier but file ended"),
    }
}

/* specifier_list ::= { <type_specifier> | <storage_class_specifier> }+ */
fn parse_specifier_list(tokens: &mut VecDeque<Token>) -> Result<(Type, Option<StorageClass>)> {
    let mut types: Vec<Type> = Vec::new();
    let mut storage_classes: Vec<StorageClass> = Vec::new();
    while lex::is_specifier(&tokens[0]) {
        if lex::is_type_specifier(&tokens[0]) {
            types.push(parse_type_specifier(tokens)?);
        } else {
            storage_classes.push(parse_storage_class_specifier(tokens)?);
        }
    }

    if storage_classes.len() > 1 {
        bail!("Invalid storage class specifier: {:?}", storage_classes);
    }

    let type_specifier = parse_type_helper(types)?;

    Ok((type_specifier, storage_classes.pop()))
}

/* declaration ::= <specifier> ( <variable_declaration> | <function_declaration> ) */
/* function_declaration ::= <identifier> "(" <param-list> ")" ( <block> | ";" ) */
/* variable_declaration ::= <identifier> [ "=" <expression> ] ";" */
fn parse_declaration(tokens: &mut VecDeque<Token>) -> Result<Declaration> {
    let specifier = parse_specifier_list(tokens)?;
    let id = parse_identifier(tokens)?;
    match &tokens[0] {
        Token::OpenParanthesis => {
            expect_token(Token::OpenParanthesis, tokens.pop_front())?;
            let (param_types, param_ids) = parse_param_list(tokens)?;
            expect_token(Token::CloseParanthesis, tokens.pop_front())?;
            let block = match &tokens[0] {
                Token::Semicolon => {
                    expect_token(Token::Semicolon, tokens.pop_front())?;
                    None
                }
                _ => Some(parse_block(tokens)?),
            };
            let func_type = Type::Function(
                param_types.into_iter().map(Box::new).collect(),
                Box::new(specifier.0),
            );
            Ok(Declaration::F(FunctionDeclaration::D(
                id,
                param_ids,
                block,
                func_type,
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
                specifier.0,
                specifier.1,
            )))
        }
        Token::Semicolon => {
            expect_token(Token::Semicolon, tokens.pop_front())?;
            Ok(Declaration::V(VariableDeclaration::D(
                id,
                None,
                specifier.0,
                specifier.1,
            )))
        }
        x => bail!("Expected variable or function declaration, got {}", x),
    }
}

/* program ::= { <function-declaration> } */
pub fn parse_tokens(mut tokens: VecDeque<Token>) -> Result<Ast> {
    let mut result = Vec::new();
    while tokens.len() > 0 && lex::is_specifier(&tokens[0]) {
        result.push(parse_declaration(&mut tokens)?);
    }

    match tokens.pop_front() {
        Some(x) => bail!("Expected end of code but found {}", x),
        None => Ok(Ast::Program(result)),
    }
}
