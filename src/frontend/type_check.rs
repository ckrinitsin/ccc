use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::frontend::parse::{
    Ast, Block, BlockItem, Declaration, Expression, ForInit, FunctionDeclaration, Statement,
    VariableDeclaration,
};
use anyhow::{Result, bail};

#[derive(PartialEq)]
pub enum Type {
    Int,
    Function(i32 /* args count */),
}

fn typecheck_expression(
    expr: Expression,
    hash_map: &mut HashMap<String, (Type, bool)>,
) -> Result<Expression> {
    match expr {
        Expression::Variable(x) => {
            if Type::Int != hash_map.get(&x).unwrap().0 {
                bail!("Function name used as a variable {}", x);
            }
            Ok(Expression::Variable(x))
        }
        Expression::Unary(unary_op, expression) => Ok(Expression::Unary(
            unary_op,
            Box::new(typecheck_expression(*expression, hash_map)?),
        )),
        Expression::Binary(binary_op, left, right) => Ok(Expression::Binary(
            binary_op,
            Box::new(typecheck_expression(*left, hash_map)?),
            Box::new(typecheck_expression(*right, hash_map)?),
        )),
        Expression::Assignment(left, right) => Ok(Expression::Assignment(
            Box::new(typecheck_expression(*left, hash_map)?),
            Box::new(typecheck_expression(*right, hash_map)?),
        )),
        Expression::CompoundAssignment(binary_op, left, right) => {
            Ok(Expression::CompoundAssignment(
                binary_op,
                Box::new(typecheck_expression(*left, hash_map)?),
                Box::new(typecheck_expression(*right, hash_map)?),
            ))
        }
        Expression::PostIncr(expr) => Ok(Expression::PostIncr(Box::new(typecheck_expression(
            *expr, hash_map,
        )?))),
        Expression::PostDecr(expr) => Ok(Expression::PostDecr(Box::new(typecheck_expression(
            *expr, hash_map,
        )?))),
        Expression::Conditional(left, middle, right) => Ok(Expression::Conditional(
            Box::new(typecheck_expression(*left, hash_map)?),
            Box::new(typecheck_expression(*middle, hash_map)?),
            Box::new(typecheck_expression(*right, hash_map)?),
        )),
        Expression::FunctionCall(name, args) => match hash_map.get(&name).unwrap().0 {
            Type::Int => bail!("Variable used as a function name"),
            Type::Function(param_count) if param_count as usize == args.len() => {
                let mut new_args = Vec::new();
                for arg in args {
                    new_args.push(Box::new(typecheck_expression(*arg, hash_map)?));
                }
                Ok(Expression::FunctionCall(name, new_args))
            }
            Type::Function(param_count) => bail!(
                "Function {} called with {} arguments, but it has {}",
                name,
                args.len(),
                param_count,
            ),
        },
        c => Ok(c),
    }
}

fn typecheck_optional_expression(
    opt_expression: Option<Expression>,
    hash_map: &mut HashMap<String, (Type, bool)>,
) -> Result<Option<Expression>> {
    match opt_expression {
        Some(expression) => Ok(Some(typecheck_expression(expression, hash_map)?)),
        None => Ok(None),
    }
}

fn typecheck_for_init(
    for_init: ForInit,
    hash_map: &mut HashMap<String, (Type, bool)>,
) -> Result<ForInit> {
    match for_init {
        ForInit::D(declaration) => Ok(ForInit::D(typecheck_variable_declaration(
            declaration,
            hash_map,
        )?)),
        ForInit::E(opt_expression) => Ok(ForInit::E(typecheck_optional_expression(
            opt_expression,
            hash_map,
        )?)),
    }
}

fn typecheck_statement(
    statement: Statement,
    hash_map: &mut HashMap<String, (Type, bool)>,
) -> Result<Statement> {
    match statement {
        Statement::Return(expression) => Ok(Statement::Return(typecheck_expression(
            expression, hash_map,
        )?)),
        Statement::Expression(expression) => Ok(Statement::Expression(typecheck_expression(
            expression, hash_map,
        )?)),
        Statement::Null => Ok(Statement::Null),
        Statement::If(condition, if_statement, else_statement) => match else_statement {
            Some(x) => Ok(Statement::If(
                typecheck_expression(condition, hash_map)?,
                Box::new(typecheck_statement(*if_statement, hash_map)?),
                Some(Box::new(typecheck_statement(*x, hash_map)?)),
            )),
            None => Ok(Statement::If(
                typecheck_expression(condition, hash_map)?,
                Box::new(typecheck_statement(*if_statement, hash_map)?),
                None,
            )),
        },
        Statement::Labeled(id, statement) => Ok(Statement::Labeled(
            id,
            Box::new(typecheck_statement(*statement, hash_map)?),
        )),
        Statement::Compound(block) => Ok(Statement::Compound(typecheck_block(block, hash_map)?)),
        Statement::While(expression, statement, label) => Ok(Statement::While(
            typecheck_expression(expression, hash_map)?,
            Box::new(typecheck_statement(*statement, hash_map)?),
            label,
        )),
        Statement::DoWhile(statement, expression, label) => Ok(Statement::DoWhile(
            Box::new(typecheck_statement(*statement, hash_map)?),
            typecheck_expression(expression, hash_map)?,
            label,
        )),
        Statement::For(for_init, condition, step, body, label) => {
            let for_init = typecheck_for_init(for_init, hash_map)?;
            let condition = typecheck_optional_expression(condition, hash_map)?;
            let step = typecheck_optional_expression(step, hash_map)?;
            let body = typecheck_statement(*body, hash_map)?;
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
            Box::new(typecheck_statement(*statement, hash_map)?),
            label,
        )),
        Statement::Switch(expression, statement, label, cases) => Ok(Statement::Switch(
            typecheck_expression(expression, hash_map)?,
            Box::new(typecheck_statement(*statement, hash_map)?),
            label,
            cases,
        )),
        Statement::Default(statement, label) => Ok(Statement::Default(
            Box::new(typecheck_statement(*statement, hash_map)?),
            label,
        )),
        c => Ok(c),
    }
}

fn typecheck_variable_declaration(
    decl: VariableDeclaration,
    hash_map: &mut HashMap<String, (Type, bool)>,
) -> Result<VariableDeclaration> {
    match decl {
        VariableDeclaration::D(id, opt_expression) => {
            hash_map.insert(id.clone(), (Type::Int, false));
            if let Some(expr) = opt_expression {
                let expr = typecheck_expression(expr, hash_map)?;
                return Ok(VariableDeclaration::D(id, Some(expr)));
            }
            Ok(VariableDeclaration::D(id, None))
        }
    }
}

fn typecheck_function_declaration(
    decl: FunctionDeclaration,
    hash_map: &mut HashMap<String, (Type, bool)>,
) -> Result<FunctionDeclaration> {
    match decl {
        FunctionDeclaration::D(name, params, block) => {
            let function_type = Type::Function(params.len() as i32);
            let has_body = block != None;
            let mut defined = false;

            if let Some(old_decl) = hash_map.get(&name) {
                if function_type != old_decl.0 {
                    bail!("Incompatible function declarations");
                }
                defined = old_decl.1;
                if has_body && defined {
                    bail!("Function is defined more than once");
                }
            }

            hash_map.insert(name.clone(), (function_type, has_body | defined));

            let mut new_block = None;
            if let Some(body) = block {
                for param in &params {
                    hash_map.insert(param.clone(), (Type::Int, false));
                }
                new_block = Some(typecheck_block(body, hash_map)?);
            }

            Ok(FunctionDeclaration::D(name, params, new_block))
        }
    }
}

fn typecheck_declaration(
    decl: Declaration,
    hash_map: &mut HashMap<String, (Type, bool)>,
) -> Result<Declaration> {
    match decl {
        Declaration::V(variable_declaration) => Ok(Declaration::V(typecheck_variable_declaration(
            variable_declaration,
            hash_map,
        )?)),
        Declaration::F(function_declaration) => Ok(Declaration::F(typecheck_function_declaration(
            function_declaration,
            hash_map,
        )?)),
    }
}

fn typecheck_block_item(
    item: BlockItem,
    hash_map: &mut HashMap<String, (Type, bool)>,
) -> Result<BlockItem> {
    match item {
        BlockItem::S(statement) => Ok(BlockItem::S(typecheck_statement(statement, hash_map)?)),
        BlockItem::D(declaration) => {
            Ok(BlockItem::D(typecheck_declaration(declaration, hash_map)?))
        }
    }
}

fn typecheck_block(block: Block, hash_map: &mut HashMap<String, (Type, bool)>) -> Result<Block> {
    match block {
        Block::B(block_items) => {
            let mut new_block_items = Vec::new();
            for item in block_items {
                new_block_items.push(typecheck_block_item(item, hash_map)?);
            }
            Ok(Block::B(new_block_items))
        }
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn type_check(ast: Ast) -> Result<Ast> {
    COUNTER.store(0, Ordering::SeqCst);

    match ast {
        Ast::Program(functions) => {
            /* HashMap<name, (type, defined)> */
            let mut hash_map: HashMap<String, (Type, bool)> = HashMap::new();
            let mut funcs = Vec::new();
            for func in functions {
                funcs.push(typecheck_function_declaration(func, &mut hash_map)?);
            }
            Ok(Ast::Program(funcs))
        }
    }
}
