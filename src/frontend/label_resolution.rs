use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::frontend::parse::{Ast, Block, BlockItem, FunctionDeclaration, Statement};
use anyhow::{Result, bail};

fn gen_label(id: String) -> String {
    let counter = COUNTER.fetch_add(1, Ordering::SeqCst);
    id + ".." + &counter.to_string()
}

fn resolve_goto_statement(
    statement: Statement,
    hash_map: &mut HashMap<String, String>,
) -> Result<Statement> {
    match statement {
        Statement::Goto(id) => {
            if let Some(r) = hash_map.get(&id) {
                return Ok(Statement::Goto(r.to_string()));
            } else {
                bail!("Undeclared label {}", id);
            }
        }
        Statement::If(cond, statement1, statement2) => {
            if let Some(st2) = statement2 {
                return Ok(Statement::If(
                    cond,
                    Box::new(resolve_goto_statement(*statement1, hash_map)?),
                    Some(Box::new(resolve_goto_statement(*st2, hash_map)?)),
                ));
            }
            Ok(Statement::If(
                cond,
                Box::new(resolve_goto_statement(*statement1, hash_map)?),
                None,
            ))
        }
        Statement::Labeled(x, statement) => Ok(Statement::Labeled(
            x,
            Box::new(resolve_goto_statement(*statement, hash_map)?),
        )),
        Statement::Compound(block) => Ok(Statement::Compound(resolve_goto_block(block, hash_map)?)),
        Statement::While(expression, statement, label) => Ok(Statement::While(
            expression,
            Box::new(resolve_goto_statement(*statement, hash_map)?),
            label,
        )),
        Statement::DoWhile(statement, expression, label) => Ok(Statement::DoWhile(
            Box::new(resolve_goto_statement(*statement, hash_map)?),
            expression,
            label,
        )),
        Statement::For(for_init, condition, step, body, label) => Ok(Statement::For(
            for_init,
            condition,
            step,
            Box::new(resolve_goto_statement(*body, hash_map)?),
            label,
        )),
        Statement::Switch(condition, statement, label, cases) => Ok(Statement::Switch(
            condition,
            Box::new(resolve_goto_statement(*statement, hash_map)?),
            label,
            cases,
        )),
        Statement::Case(expression, statement, label) => Ok(Statement::Case(
            expression,
            Box::new(resolve_goto_statement(*statement, hash_map)?),
            label,
        )),
        Statement::Default(statement, label) => Ok(Statement::Default(
            Box::new(resolve_goto_statement(*statement, hash_map)?),
            label,
        )),
        c => Ok(c),
    }
}

fn resolve_label_statement(
    statement: Statement,
    hash_map: &mut HashMap<String, String>,
) -> Result<Statement> {
    match statement {
        Statement::Labeled(id, statement) => {
            if hash_map.contains_key(&id) {
                bail!("Duplicate label: {}", id);
            }

            let new_label = gen_label(id.clone());
            hash_map.insert(id, new_label.clone());
            Ok(Statement::Labeled(
                new_label,
                Box::new(resolve_label_statement(*statement, hash_map)?),
            ))
        }
        Statement::If(cond, statement1, statement2) => {
            if let Some(st2) = statement2 {
                return Ok(Statement::If(
                    cond,
                    Box::new(resolve_label_statement(*statement1, hash_map)?),
                    Some(Box::new(resolve_label_statement(*st2, hash_map)?)),
                ));
            }
            Ok(Statement::If(
                cond,
                Box::new(resolve_label_statement(*statement1, hash_map)?),
                None,
            ))
        }
        Statement::Compound(block) => {
            Ok(Statement::Compound(resolve_label_block(block, hash_map)?))
        }
        Statement::While(expression, statement, label) => Ok(Statement::While(
            expression,
            Box::new(resolve_label_statement(*statement, hash_map)?),
            label,
        )),
        Statement::DoWhile(statement, expression, label) => Ok(Statement::DoWhile(
            Box::new(resolve_label_statement(*statement, hash_map)?),
            expression,
            label,
        )),
        Statement::For(for_init, condition, step, body, label) => Ok(Statement::For(
            for_init,
            condition,
            step,
            Box::new(resolve_label_statement(*body, hash_map)?),
            label,
        )),
        Statement::Switch(condition, statement, label, cases) => Ok(Statement::Switch(
            condition,
            Box::new(resolve_label_statement(*statement, hash_map)?),
            label,
            cases,
        )),
        Statement::Case(expression, statement, label) => Ok(Statement::Case(
            expression,
            Box::new(resolve_label_statement(*statement, hash_map)?),
            label,
        )),
        Statement::Default(statement, label) => Ok(Statement::Default(
            Box::new(resolve_label_statement(*statement, hash_map)?),
            label,
        )),
        c => Ok(c),
    }
}

fn resolve_goto_block(block: Block, hash_map: &mut HashMap<String, String>) -> Result<Block> {
    match block {
        Block::B(block_items) => {
            let mut result = Vec::new();
            for item in block_items {
                let new_item = match item {
                    BlockItem::S(statement) => {
                        BlockItem::S(resolve_goto_statement(statement, hash_map)?)
                    }
                    c => c,
                };
                result.push(new_item);
            }
            Ok(Block::B(result))
        }
    }
}

fn resolve_label_block(block: Block, hash_map: &mut HashMap<String, String>) -> Result<Block> {
    match block {
        Block::B(block_items) => {
            let mut result = Vec::new();
            for item in block_items {
                let new_item = match item {
                    BlockItem::S(statement) => {
                        BlockItem::S(resolve_label_statement(statement, hash_map)?)
                    }
                    c => c,
                };
                result.push(new_item);
            }
            Ok(Block::B(result))
        }
    }
}

fn resolve_label_function_declaration(
    func: FunctionDeclaration,
    hash_map: &mut HashMap<String, String>,
) -> Result<FunctionDeclaration> {
    match func {
        FunctionDeclaration::D(name, args, block) => {
            let Some(bl) = block else {
                return Ok(FunctionDeclaration::D(name, args, block));
            };
            let block = resolve_label_block(bl, hash_map)?;
            let block = Some(resolve_goto_block(block, hash_map)?);
            Ok(FunctionDeclaration::D(name, args, block))
        }
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn label_resolution(ast: Ast) -> Result<Ast> {
    COUNTER.store(0, Ordering::SeqCst);

    match ast {
        Ast::Program(functions) => {
            let mut funcs = Vec::new();
            for func in functions {
                let mut hash_map: HashMap<String, String> = HashMap::new();
                funcs.push(resolve_label_function_declaration(func, &mut hash_map)?);
            }
            Ok(Ast::Program(funcs))
        }
    }
}
