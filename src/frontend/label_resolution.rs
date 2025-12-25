use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::frontend::parser::{Ast, Block, BlockItem, Function, Statement};
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
        c => Ok(c),
    }
}

fn resolve_label_statement(
    statement: Statement,
    hash_map: &mut HashMap<String, String>,
    current_loop: Option<String>,
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
                Box::new(resolve_label_statement(*statement, hash_map, current_loop)?),
            ))
        }
        Statement::If(cond, statement1, statement2) => {
            if let Some(st2) = statement2 {
                return Ok(Statement::If(
                    cond,
                    Box::new(resolve_label_statement(
                        *statement1,
                        hash_map,
                        current_loop.clone(),
                    )?),
                    Some(Box::new(resolve_label_statement(
                        *st2,
                        hash_map,
                        current_loop,
                    )?)),
                ));
            }
            Ok(Statement::If(
                cond,
                Box::new(resolve_label_statement(
                    *statement1,
                    hash_map,
                    current_loop,
                )?),
                None,
            ))
        }
        Statement::Compound(block) => Ok(Statement::Compound(resolve_label_block(
            block,
            hash_map,
            current_loop,
        )?)),
        Statement::While(expression, statement, _) => {
            let label_loop = Some(gen_label("while".to_string()));
            Ok(Statement::While(
                expression,
                Box::new(resolve_label_statement(
                    *statement,
                    hash_map,
                    label_loop.clone(),
                )?),
                label_loop,
            ))
        }
        Statement::DoWhile(statement, expression, _) => {
            let label_loop = Some(gen_label("do_while".to_string()));
            Ok(Statement::DoWhile(
                Box::new(resolve_label_statement(
                    *statement,
                    hash_map,
                    label_loop.clone(),
                )?),
                expression,
                label_loop,
            ))
        }
        Statement::For(for_init, condition, step, body, _) => {
            let label_loop = Some(gen_label("for".to_string()));
            let body = resolve_label_statement(*body, hash_map, label_loop.clone())?;
            Ok(Statement::For(
                for_init,
                condition,
                step,
                Box::new(body),
                label_loop,
            ))
        }
        Statement::Break(_) => {
            if None == current_loop {
                bail!("Break is not inside a loop!");
            }
            Ok(Statement::Break(current_loop))
        }
        Statement::Continue(_) => {
            if None == current_loop {
                bail!("Continue is not inside a loop!");
            }
            Ok(Statement::Continue(current_loop))
        }
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

fn resolve_label_block(
    block: Block,
    hash_map: &mut HashMap<String, String>,
    current_loop: Option<String>,
) -> Result<Block> {
    match block {
        Block::B(block_items) => {
            let mut result = Vec::new();
            for item in block_items {
                let new_item = match item {
                    BlockItem::S(statement) => BlockItem::S(resolve_label_statement(
                        statement,
                        hash_map,
                        current_loop.clone(),
                    )?),
                    c => c,
                };
                result.push(new_item);
            }
            Ok(Block::B(result))
        }
    }
}

fn resolve_label_function(func: Function) -> Result<Function> {
    let mut hash_map: HashMap<String, String> = HashMap::new();
    match func {
        Function::Function(x, block) => {
            let block = resolve_label_block(block, &mut hash_map, None)?;
            let block = resolve_goto_block(block, &mut hash_map)?;
            Ok(Function::Function(x, block))
        }
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn label_resolution(ast: Ast) -> Result<Ast> {
    COUNTER.store(0, Ordering::SeqCst);
    match ast {
        Ast::Program(function) => Ok(Ast::Program(resolve_label_function(function)?)),
    }
}
