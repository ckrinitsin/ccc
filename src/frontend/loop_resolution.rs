use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::frontend::parse::{
    Ast, Block, BlockItem, Declaration, Expression, FunctionDeclaration, Statement,
};
use anyhow::{Result, bail};

fn gen_label(id: String) -> String {
    let counter = COUNTER.fetch_add(1, Ordering::SeqCst);
    id + "..." + &counter.to_string()
}

fn resolve_loop_statement(
    statement: Statement,
    hash_map: &mut HashMap<String, String>,
    current_loop: Option<String>,
    current_switch: Option<String>,
    collected_cases: &mut Option<Vec<(Option<i32>, String)>>,
) -> Result<Statement> {
    match statement {
        Statement::Labeled(id, statement) => Ok(Statement::Labeled(
            id,
            Box::new(resolve_loop_statement(
                *statement,
                hash_map,
                current_loop,
                current_switch,
                collected_cases,
            )?),
        )),
        Statement::If(cond, statement1, statement2) => {
            if let Some(st2) = statement2 {
                return Ok(Statement::If(
                    cond,
                    Box::new(resolve_loop_statement(
                        *statement1,
                        hash_map,
                        current_loop.clone(),
                        current_switch.clone(),
                        collected_cases,
                    )?),
                    Some(Box::new(resolve_loop_statement(
                        *st2,
                        hash_map,
                        current_loop,
                        current_switch,
                        collected_cases,
                    )?)),
                ));
            }
            Ok(Statement::If(
                cond,
                Box::new(resolve_loop_statement(
                    *statement1,
                    hash_map,
                    current_loop,
                    current_switch,
                    collected_cases,
                )?),
                None,
            ))
        }
        Statement::Compound(block) => Ok(Statement::Compound(resolve_loop_block(
            block,
            hash_map,
            current_loop,
            current_switch,
            collected_cases,
        )?)),
        Statement::While(expression, statement, _) => {
            let label_loop = Some(gen_label("while".to_string()));
            Ok(Statement::While(
                expression,
                Box::new(resolve_loop_statement(
                    *statement,
                    hash_map,
                    label_loop.clone(),
                    None,
                    collected_cases,
                )?),
                label_loop,
            ))
        }
        Statement::DoWhile(statement, expression, _) => {
            let label_loop = Some(gen_label("do_while".to_string()));
            Ok(Statement::DoWhile(
                Box::new(resolve_loop_statement(
                    *statement,
                    hash_map,
                    label_loop.clone(),
                    None,
                    collected_cases,
                )?),
                expression,
                label_loop,
            ))
        }
        Statement::For(for_init, condition, step, body, _) => {
            let label_loop = Some(gen_label("for".to_string()));
            let body =
                resolve_loop_statement(*body, hash_map, label_loop.clone(), None, collected_cases)?;
            Ok(Statement::For(
                for_init,
                condition,
                step,
                Box::new(body),
                label_loop,
            ))
        }
        Statement::Break(_) => {
            if None == current_switch {
                if None == current_loop {
                    bail!("Break is not inside a loop!");
                }
                return Ok(Statement::Break(current_loop));
            }
            Ok(Statement::Break(current_switch))
        }
        Statement::Continue(_) => {
            if None == current_loop {
                bail!("Continue is not inside a loop!");
            }
            Ok(Statement::Continue(current_loop))
        }
        Statement::Case(expression, statement, _) => {
            let n_label = gen_label("case".to_string());
            let Some(unwrapped_cases) = collected_cases.as_mut() else {
                bail!("Case not inside a switch statement");
            };
            match expression {
                Expression::Constant(c) => {
                    if unwrapped_cases.iter().any(|(constant, _)| {
                        if let Some(con) = constant {
                            return *con == c;
                        }
                        false
                    }) {
                        bail!("Case {} is duplicated", c);
                    }
                    unwrapped_cases.push((Some(c), n_label.clone()));
                    Ok(Statement::Case(
                        expression,
                        Box::new(resolve_loop_statement(
                            *statement,
                            hash_map,
                            current_loop,
                            current_switch,
                            collected_cases,
                        )?),
                        Some(n_label),
                    ))
                }
                _ => bail!("Case expression is not a constant"),
            }
        }
        Statement::Switch(expression, statement, _, _) => {
            let n_label = Some(gen_label("switch".to_string()));
            let mut new_cases: Option<Vec<(Option<i32>, String)>> = Some(Vec::new());
            Ok(Statement::Switch(
                expression,
                Box::new(resolve_loop_statement(
                    *statement,
                    hash_map,
                    current_loop,
                    n_label.clone(),
                    &mut new_cases,
                )?),
                n_label,
                new_cases.unwrap(),
            ))
        }
        Statement::Default(statement, _) => {
            let n_label = gen_label("default".to_string());
            let Some(unwrapped_cases) = collected_cases.as_mut() else {
                bail!("Default not inside a switch statement");
            };
            if unwrapped_cases
                .iter()
                .any(|(constant, _)| *constant == None)
            {
                bail!("Duplicated defaults are not allowed");
            }
            unwrapped_cases.push((None, n_label.clone()));
            Ok(Statement::Default(
                Box::new(resolve_loop_statement(
                    *statement,
                    hash_map,
                    current_loop,
                    current_switch,
                    collected_cases,
                )?),
                Some(n_label),
            ))
        }
        c => Ok(c),
    }
}

fn resolve_loop_block(
    block: Block,
    hash_map: &mut HashMap<String, String>,
    current_loop: Option<String>,
    current_switch: Option<String>,
    collected_cases: &mut Option<Vec<(Option<i32>, String)>>,
) -> Result<Block> {
    match block {
        Block::B(block_items) => {
            let mut result = Vec::new();
            for item in block_items {
                let new_item = match item {
                    BlockItem::S(statement) => BlockItem::S(resolve_loop_statement(
                        statement,
                        hash_map,
                        current_loop.clone(),
                        current_switch.clone(),
                        collected_cases,
                    )?),
                    c => c,
                };
                result.push(new_item);
            }
            Ok(Block::B(result))
        }
    }
}

fn resolve_loop_declaration(
    decl: Declaration,
    hash_map: &mut HashMap<String, String>,
) -> Result<Declaration> {
    match decl {
        Declaration::F(FunctionDeclaration::D(name, args, block, storage_class)) => {
            let Some(bl) = block else {
                return Ok(Declaration::F(FunctionDeclaration::D(
                    name,
                    args,
                    block,
                    storage_class,
                )));
            };
            let block = Some(resolve_loop_block(bl, hash_map, None, None, &mut None)?);
            Ok(Declaration::F(FunctionDeclaration::D(
                name,
                args,
                block,
                storage_class,
            )))
        }
        x => Ok(x),
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn loop_resolution(ast: Ast) -> Result<Ast> {
    COUNTER.store(0, Ordering::SeqCst);

    match ast {
        Ast::Program(functions) => {
            let mut funcs = Vec::new();
            for func in functions {
                let mut hash_map: HashMap<String, String> = HashMap::new();
                funcs.push(resolve_loop_declaration(func, &mut hash_map)?);
            }
            Ok(Ast::Program(funcs))
        }
    }
}
