use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::frontend::parser::{
    Ast, BlockItem, Declaration, Expression, Function, Statement, UnaryOp,
};
use anyhow::{Result, bail};

fn gen_temp_local(id: String) -> String {
    let counter = COUNTER.fetch_add(1, Ordering::SeqCst);
    id + ":" + &counter.to_string()
}

fn resolve_expression(
    expr: Expression,
    hash_map: &mut HashMap<String, String>,
) -> Result<Expression> {
    match expr {
        Expression::Variable(x) => {
            if let Some(r) = hash_map.get(&x) {
                return Ok(Expression::Variable(r.to_string()));
            } else {
                bail!("Undeclared variable {}", x);
            }
        }
        Expression::Unary(unary_op, expression) => {
            if (matches!(unary_op, UnaryOp::Increment) || matches!(unary_op, UnaryOp::Decrement))
                && !matches!(*expression, Expression::Variable(_))
            {
                bail!("{:?} is not a valid lvalue", *expression);
            }
            Ok(Expression::Unary(
                unary_op,
                Box::new(resolve_expression(*expression, hash_map)?),
            ))
        }
        Expression::Binary(binary_op, left, right) => Ok(Expression::Binary(
            binary_op,
            Box::new(resolve_expression(*left, hash_map)?),
            Box::new(resolve_expression(*right, hash_map)?),
        )),
        Expression::Assignment(left, right) => {
            if !matches!(*left, Expression::Variable(_)) {
                bail!("{:?} is not a valid lvalue", *left);
            }
            Ok(Expression::Assignment(
                Box::new(resolve_expression(*left, hash_map)?),
                Box::new(resolve_expression(*right, hash_map)?),
            ))
        }
        Expression::CompoundAssignment(binary_op, left, right) => {
            if !matches!(*left, Expression::Variable(_)) {
                bail!("{:?} is not a valid lvalue", *left);
            }
            Ok(Expression::CompoundAssignment(
                binary_op,
                Box::new(resolve_expression(*left, hash_map)?),
                Box::new(resolve_expression(*right, hash_map)?),
            ))
        }
        Expression::PostIncr(expr) => {
            if !matches!(*expr, Expression::Variable(_)) {
                bail!("{:?} is not a valid lvalue", *expr);
            }
            Ok(Expression::PostIncr(Box::new(resolve_expression(
                *expr, hash_map,
            )?)))
        }
        Expression::PostDecr(expr) => {
            if !matches!(*expr, Expression::Variable(_)) {
                bail!("{:?} is not a valid lvalue", *expr);
            }
            Ok(Expression::PostDecr(Box::new(resolve_expression(
                *expr, hash_map,
            )?)))
        }
        c => Ok(c),
    }
}

fn resolve_statement(
    statement: Statement,
    hash_map: &mut HashMap<String, String>,
) -> Result<Statement> {
    match statement {
        Statement::Return(expression) => {
            Ok(Statement::Return(resolve_expression(expression, hash_map)?))
        }
        Statement::Expression(expression) => Ok(Statement::Expression(resolve_expression(
            expression, hash_map,
        )?)),
        Statement::Null => Ok(Statement::Null),
    }
}

fn resolve_declaration(
    decl: Declaration,
    hash_map: &mut HashMap<String, String>,
) -> Result<Declaration> {
    match decl {
        Declaration::Declaration(id, opt_expression) => {
            if hash_map.contains_key(&id) {
                bail!("Duplicate declaration of {}", id);
            }
            let unique = gen_temp_local(id.clone());
            hash_map.insert(id, unique.clone());
            if let Some(expr) = opt_expression {
                let expr = resolve_expression(expr, hash_map)?;
                return Ok(Declaration::Declaration(unique, Some(expr)));
            }
            Ok(Declaration::Declaration(unique, None))
        }
    }
}

fn resolve_block_item(
    block: BlockItem,
    hash_map: &mut HashMap<String, String>,
) -> Result<BlockItem> {
    match block {
        BlockItem::S(statement) => Ok(BlockItem::S(resolve_statement(statement, hash_map)?)),
        BlockItem::D(declaration) => Ok(BlockItem::D(resolve_declaration(declaration, hash_map)?)),
    }
}

fn resolve_function(func: Function) -> Result<Function> {
    let mut hash_map: HashMap<String, String> = HashMap::new();
    match func {
        Function::Function(x, block_items) => {
            let mut new_block_items = Vec::new();
            for item in block_items {
                new_block_items.push(resolve_block_item(item, &mut hash_map)?);
            }
            Ok(Function::Function(x, new_block_items))
        }
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn resolve_ast(ast: Ast) -> Result<Ast> {
    COUNTER.store(0, Ordering::SeqCst);
    match ast {
        Ast::Program(function) => Ok(Ast::Program(resolve_function(function)?)),
    }
}
