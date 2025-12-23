use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::frontend::parser::{Ast, BlockItem, Function, Statement};
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
        c => Ok(c),
    }
}

fn resolve_label_function(func: Function) -> Result<Function> {
    let mut hash_map: HashMap<String, String> = HashMap::new();
    match func {
        Function::Function(x, block_items) => {
            let mut labeled_items = Vec::new();
            for item in block_items {
                let new_item = match item {
                    BlockItem::S(statement) => {
                        BlockItem::S(resolve_label_statement(statement, &mut hash_map)?)
                    }
                    c => c,
                };
                labeled_items.push(new_item);
            }

            let mut result = Vec::new();
            for item in labeled_items {
                let new_item = match item {
                    BlockItem::S(statement) => {
                        BlockItem::S(resolve_goto_statement(statement, &mut hash_map)?)
                    }
                    c => c,
                };
                result.push(new_item);
            }
            Ok(Function::Function(x, result))
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
