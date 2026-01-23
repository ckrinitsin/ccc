use std::collections::HashMap;

use crate::frontend::ast::{
    Ast, Block, BlockItem, Declaration, Expression, ForInit, FunctionDeclaration, Statement,
    StorageClass, UnaryOp, VariableDeclaration,
};
use crate::frontend::utils::counter::gen_var_local;
use anyhow::{Result, bail};

fn copy_hashmap(
    hash_map: &HashMap<String, (String, bool, bool)>,
) -> HashMap<String, (String, bool, bool)> {
    let mut result: HashMap<String, (String, bool, bool)> = HashMap::new();
    for item in hash_map {
        result.insert(item.0.clone(), (item.1.0.clone(), false, item.1.2.clone()));
    }
    result
}

fn resolve_expression(
    expr: Expression,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<Expression> {
    match expr {
        Expression::Variable(x, _) => {
            if let Some(r) = hash_map.get(&x) {
                return Ok(Expression::Variable(r.0.to_string(), None));
            } else {
                bail!("Undeclared variable {}", x);
            }
        }
        Expression::Unary(unary_op, expression, _) => {
            if (matches!(unary_op, UnaryOp::Increment) || matches!(unary_op, UnaryOp::Decrement))
                && !matches!(*expression, Expression::Variable(_, _))
            {
                bail!("{:?} is not a valid lvalue", *expression);
            }
            Ok(Expression::Unary(
                unary_op,
                Box::new(resolve_expression(*expression, hash_map)?),
                None,
            ))
        }
        Expression::Binary(binary_op, left, right, _) => Ok(Expression::Binary(
            binary_op,
            Box::new(resolve_expression(*left, hash_map)?),
            Box::new(resolve_expression(*right, hash_map)?),
            None,
        )),
        Expression::Assignment(left, right, _) => {
            if !matches!(*left, Expression::Variable(_, _)) {
                bail!("{:?} is not a valid lvalue", *left);
            }
            Ok(Expression::Assignment(
                Box::new(resolve_expression(*left, hash_map)?),
                Box::new(resolve_expression(*right, hash_map)?),
                None,
            ))
        }
        Expression::CompoundAssignment(binary_op, left, right, _) => {
            if !matches!(*left, Expression::Variable(_, _)) {
                bail!("{:?} is not a valid lvalue", *left);
            }
            Ok(Expression::CompoundAssignment(
                binary_op,
                Box::new(resolve_expression(*left, hash_map)?),
                Box::new(resolve_expression(*right, hash_map)?),
                None,
            ))
        }
        Expression::PostIncr(expr, _) => {
            if !matches!(*expr, Expression::Variable(_, _)) {
                bail!("{:?} is not a valid lvalue", *expr);
            }
            Ok(Expression::PostIncr(
                Box::new(resolve_expression(*expr, hash_map)?),
                None,
            ))
        }
        Expression::PostDecr(expr, _) => {
            if !matches!(*expr, Expression::Variable(_, _)) {
                bail!("{:?} is not a valid lvalue", *expr);
            }
            Ok(Expression::PostDecr(
                Box::new(resolve_expression(*expr, hash_map)?),
                None,
            ))
        }
        Expression::Conditional(left, middle, right, _) => Ok(Expression::Conditional(
            Box::new(resolve_expression(*left, hash_map)?),
            Box::new(resolve_expression(*middle, hash_map)?),
            Box::new(resolve_expression(*right, hash_map)?),
            None,
        )),
        Expression::FunctionCall(name, args, _) => {
            if !hash_map.contains_key(&name) {
                bail!("Undeclared identifier {}", name);
            }

            let mut new_args: Vec<Box<Expression>> = Vec::new();
            for arg in args {
                new_args.push(Box::new(resolve_expression(*arg, hash_map)?));
            }

            Ok(Expression::FunctionCall(
                hash_map.get(&name).unwrap().0.to_string(),
                new_args,
                None,
            ))
        }
        Expression::Cast(var_type, expression, _) => Ok(Expression::Cast(
            var_type,
            Box::new(resolve_expression(*expression, hash_map)?),
            None,
        )),
        c => Ok(c),
    }
}

fn resolve_optional_expression(
    opt_expression: Option<Expression>,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<Option<Expression>> {
    match opt_expression {
        Some(expression) => Ok(Some(resolve_expression(expression, hash_map)?)),
        None => Ok(None),
    }
}

fn resolve_for_init(
    for_init: ForInit,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<ForInit> {
    match for_init {
        ForInit::D(declaration) => Ok(ForInit::D(resolve_local_variable_declaration(
            declaration,
            hash_map,
        )?)),
        ForInit::E(opt_expression) => Ok(ForInit::E(resolve_optional_expression(
            opt_expression,
            hash_map,
        )?)),
    }
}

fn resolve_statement(
    statement: Statement,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<Statement> {
    match statement {
        Statement::Return(expression) => {
            Ok(Statement::Return(resolve_expression(expression, hash_map)?))
        }
        Statement::Expression(expression) => Ok(Statement::Expression(resolve_expression(
            expression, hash_map,
        )?)),
        Statement::Null => Ok(Statement::Null),
        Statement::If(condition, if_statement, else_statement) => match else_statement {
            Some(x) => Ok(Statement::If(
                resolve_expression(condition, hash_map)?,
                Box::new(resolve_statement(*if_statement, hash_map)?),
                Some(Box::new(resolve_statement(*x, hash_map)?)),
            )),
            None => Ok(Statement::If(
                resolve_expression(condition, hash_map)?,
                Box::new(resolve_statement(*if_statement, hash_map)?),
                None,
            )),
        },
        Statement::Labeled(id, statement) => Ok(Statement::Labeled(
            id,
            Box::new(resolve_statement(*statement, hash_map)?),
        )),
        Statement::Compound(block) => {
            let mut inner_hash_map = copy_hashmap(&hash_map);
            Ok(Statement::Compound(resolve_block(
                block,
                &mut inner_hash_map,
            )?))
        }
        Statement::While(expression, statement, label) => Ok(Statement::While(
            resolve_expression(expression, hash_map)?,
            Box::new(resolve_statement(*statement, hash_map)?),
            label,
        )),
        Statement::DoWhile(statement, expression, label) => Ok(Statement::DoWhile(
            Box::new(resolve_statement(*statement, hash_map)?),
            resolve_expression(expression, hash_map)?,
            label,
        )),
        Statement::For(for_init, condition, step, body, label) => {
            let mut inner_hash_map = copy_hashmap(&hash_map);
            let for_init = resolve_for_init(for_init, &mut inner_hash_map)?;
            let condition = resolve_optional_expression(condition, &mut inner_hash_map)?;
            let step = resolve_optional_expression(step, &mut inner_hash_map)?;
            let body = resolve_statement(*body, &mut inner_hash_map)?;
            Ok(Statement::For(
                for_init,
                condition,
                step,
                Box::new(body),
                label,
            ))
        }
        Statement::Case(expression, statement, label) => Ok(Statement::Case(
            expression,
            Box::new(resolve_statement(*statement, hash_map)?),
            label,
        )),
        Statement::Switch(expression, statement, label, cases) => Ok(Statement::Switch(
            resolve_expression(expression, hash_map)?,
            Box::new(resolve_statement(*statement, hash_map)?),
            label,
            cases,
        )),
        Statement::Default(statement, label) => Ok(Statement::Default(
            Box::new(resolve_statement(*statement, hash_map)?),
            label,
        )),
        c => Ok(c),
    }
}

fn resolve_local_variable_declaration(
    decl: VariableDeclaration,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<VariableDeclaration> {
    match decl {
        VariableDeclaration::D(id, opt_expression, var_type, storage_class) => {
            if let Some(prev_entry) = hash_map.get(&id) {
                if prev_entry.1
                    && !(prev_entry.2 && matches!(storage_class, Some(StorageClass::Extern)))
                {
                    bail!("Conflicting local declarations");
                }
            }

            if matches!(storage_class, Some(StorageClass::Extern)) {
                hash_map.insert(id.clone(), (id.clone(), true, true));
                return Ok(VariableDeclaration::D(
                    id,
                    opt_expression,
                    var_type,
                    storage_class,
                ));
            }

            let unique = gen_var_local(id.clone());
            hash_map.insert(id, (unique.clone(), true, false));
            if let Some(expr) = opt_expression {
                let expr = resolve_expression(expr, hash_map)?;
                return Ok(VariableDeclaration::D(
                    unique,
                    Some(expr),
                    var_type,
                    storage_class,
                ));
            }
            Ok(VariableDeclaration::D(
                unique,
                None,
                var_type,
                storage_class,
            ))
        }
    }
}

fn resolve_identifier_declaration(
    name: String,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<String> {
    if hash_map.contains_key(&name) && hash_map.get(&name).unwrap().1 {
        bail!("Duplicate declaration of {}", name);
    }
    let unique = gen_var_local(name.clone());
    hash_map.insert(name, (unique.clone(), true, false));
    Ok(unique)
}

fn resolve_file_scope_variable_declaration(
    decl: VariableDeclaration,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<VariableDeclaration> {
    match decl {
        VariableDeclaration::D(id, opt_expression, var_type, storage_class) => {
            hash_map.insert(id.clone(), (id.clone(), true, true));
            Ok(VariableDeclaration::D(
                id,
                opt_expression,
                var_type,
                storage_class,
            ))
        }
    }
}

fn resolve_file_scope_function_declaration(
    decl: FunctionDeclaration,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<FunctionDeclaration> {
    match decl {
        FunctionDeclaration::D(name, params, block, var_type, storage_class) => {
            if hash_map.contains_key(&name)
                && hash_map.get(&name).unwrap().1
                && !hash_map.get(&name).unwrap().2
            {
                bail!("Duplicate declaration of {}", name);
            }

            hash_map.insert(name.clone(), (name.clone(), true, true));
            let mut inner_map = copy_hashmap(hash_map);
            let mut new_params = Vec::new();
            for param in params {
                new_params.push(resolve_identifier_declaration(param, &mut inner_map)?);
            }

            let mut new_block = None;
            if let Some(bl) = block {
                new_block = Some(resolve_block(bl, &mut inner_map)?);
            }

            Ok(FunctionDeclaration::D(
                name,
                new_params,
                new_block,
                var_type,
                storage_class,
            ))
        }
    }
}

fn resolve_local_function_declaration(
    decl: FunctionDeclaration,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<FunctionDeclaration> {
    match &decl {
        FunctionDeclaration::D(_, _, block, _, storage_class) => {
            if *block != None {
                bail!("Function definition inside another function is not allowed!");
            }
            if matches!(storage_class, Some(StorageClass::Static)) {
                bail!("Static in block scope function is not allowed");
            }
        }
    }
    resolve_file_scope_function_declaration(decl, hash_map)
}

fn resolve_block_item(
    item: BlockItem,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<BlockItem> {
    match item {
        BlockItem::S(statement) => Ok(BlockItem::S(resolve_statement(statement, hash_map)?)),
        BlockItem::D(declaration) => Ok(BlockItem::D(resolve_local_declaration(
            declaration,
            hash_map,
        )?)),
    }
}

fn resolve_block(
    block: Block,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<Block> {
    match block {
        Block::B(block_items) => {
            let mut new_block_items = Vec::new();
            for item in block_items {
                new_block_items.push(resolve_block_item(item, hash_map)?);
            }
            Ok(Block::B(new_block_items))
        }
    }
}

fn resolve_local_declaration(
    decl: Declaration,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<Declaration> {
    match decl {
        Declaration::V(variable_declaration) => Ok(Declaration::V(
            resolve_local_variable_declaration(variable_declaration, hash_map)?,
        )),
        Declaration::F(function_declaration) => Ok(Declaration::F(
            resolve_local_function_declaration(function_declaration, hash_map)?,
        )),
    }
}

fn resolve_file_scope_declaration(
    decl: Declaration,
    hash_map: &mut HashMap<String, (String, bool, bool)>,
) -> Result<Declaration> {
    match decl {
        Declaration::V(variable_declaration) => Ok(Declaration::V(
            resolve_file_scope_variable_declaration(variable_declaration, hash_map)?,
        )),
        Declaration::F(function_declaration) => Ok(Declaration::F(
            resolve_file_scope_function_declaration(function_declaration, hash_map)?,
        )),
    }
}

pub fn variable_resolution(ast: Ast) -> Result<Ast> {
    match ast {
        Ast::Program(functions) => {
            /* HashMap<name, (unique_name, current_scope, external_linkage)> */
            let mut hash_map: HashMap<String, (String, bool, bool)> = HashMap::new();
            let mut funcs = Vec::new();
            for func in functions {
                funcs.push(resolve_file_scope_declaration(func, &mut hash_map)?);
            }
            Ok(Ast::Program(funcs))
        }
    }
}
