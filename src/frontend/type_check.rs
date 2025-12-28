use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::frontend::parse::{
    Ast, Block, BlockItem, Declaration, Expression, ForInit, FunctionDeclaration, Statement,
    StorageClass, VariableDeclaration,
};
use anyhow::{Result, bail};

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Function(i32 /* args count */),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IdentifierAttributes {
    FunctionAttributes(bool, bool /* defined, global */),
    StaticAttributes(InitialValue, bool /* init, global */),
    LocalAttr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InitialValue {
    Tentative,
    Initial(i32),
    NoInit,
}

fn typecheck_expression(
    expr: Expression,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
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
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Option<Expression>> {
    match opt_expression {
        Some(expression) => Ok(Some(typecheck_expression(expression, hash_map)?)),
        None => Ok(None),
    }
}

fn typecheck_for_init(
    for_init: ForInit,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<ForInit> {
    match for_init {
        ForInit::D(declaration) => {
            let decl = typecheck_local_variable_declaration(declaration, hash_map)?;
            match &decl {
                VariableDeclaration::D(_, _, storage_class) => {
                    if *storage_class != None {
                        bail!("For loop headers cannot have a storage class");
                    }
                }
            }
            Ok(ForInit::D(decl))
        }
        ForInit::E(opt_expression) => Ok(ForInit::E(typecheck_optional_expression(
            opt_expression,
            hash_map,
        )?)),
    }
}

fn typecheck_statement(
    statement: Statement,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
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

fn typecheck_file_scope_variable_declaration(
    decl: VariableDeclaration,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<VariableDeclaration> {
    match decl {
        VariableDeclaration::D(name, initializer, storage_class) => {
            let mut init_value = match initializer {
                Some(Expression::Constant(x)) => InitialValue::Initial(x),
                Some(_) => bail!("Non-constant initializer"),
                None => {
                    if matches!(storage_class, Some(StorageClass::Extern)) {
                        InitialValue::NoInit
                    } else {
                        InitialValue::Tentative
                    }
                }
            };
            let mut global = !matches!(storage_class, Some(StorageClass::Static));

            if let Some(old_decl) = hash_map.get(&name) {
                if !matches!(old_decl.0, Type::Int) {
                    bail!("Function redeclaration as variable is not allowed");
                }

                match &old_decl.1 {
                    IdentifierAttributes::StaticAttributes(decl_init_val, decl_global) => {
                        if matches!(storage_class, Some(StorageClass::Extern)) {
                            global = *decl_global;
                        } else if global != *decl_global {
                            bail!("Variable linkage doesn't match");
                        }

                        match decl_init_val {
                            InitialValue::Tentative => {
                                if !matches!(init_value, InitialValue::Initial(_)) {
                                    init_value = InitialValue::Tentative;
                                }
                            }
                            InitialValue::Initial(_) => {
                                if matches!(init_value, InitialValue::Initial(_)) {
                                    bail!("Conflicting global variable definitions");
                                } else {
                                    init_value = decl_init_val.clone();
                                }
                            }
                            InitialValue::NoInit => (),
                        };
                    }
                    _ => bail!("No static attributes found for global variable declaration"),
                };
            }

            let attrs = IdentifierAttributes::StaticAttributes(init_value, global);
            hash_map.insert(name.clone(), (Type::Int, attrs));
            Ok(VariableDeclaration::D(name, initializer, storage_class))
        }
    }
}

fn typecheck_local_variable_declaration(
    decl: VariableDeclaration,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<VariableDeclaration> {
    match decl {
        VariableDeclaration::D(name, opt_expression, storage_class) => {
            let mut new_init = opt_expression;
            match storage_class {
                Some(StorageClass::Extern) => {
                    if new_init != None {
                        bail!("Initializer on local extern variable not allowed");
                    }
                    if let Some(old_decl) = hash_map.get(&name) {
                        if !matches!(old_decl.0, Type::Int) {
                            bail!("Function redeclared as variable");
                        }
                    } else {
                        hash_map.insert(
                            name.clone(),
                            (
                                Type::Int,
                                IdentifierAttributes::StaticAttributes(InitialValue::NoInit, true),
                            ),
                        );
                    }
                }
                Some(StorageClass::Static) => {
                    let init_value = match new_init {
                        Some(Expression::Constant(x)) => InitialValue::Initial(x),
                        Some(_) => {
                            bail!("Non-constant initializer on local static variable not allowed")
                        }
                        None => InitialValue::Initial(0),
                    };
                    hash_map.insert(
                        name.clone(),
                        (
                            Type::Int,
                            IdentifierAttributes::StaticAttributes(init_value, false),
                        ),
                    );
                }
                None => {
                    hash_map.insert(name.clone(), (Type::Int, IdentifierAttributes::LocalAttr));
                    if let Some(init) = new_init {
                        new_init = Some(typecheck_expression(init, hash_map)?);
                    }
                }
            };

            Ok(VariableDeclaration::D(name, new_init, storage_class))
        }
    }
}

fn typecheck_function_declaration(
    decl: FunctionDeclaration,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<FunctionDeclaration> {
    match decl {
        FunctionDeclaration::D(name, params, block, storage_class) => {
            let function_type = Type::Function(params.len() as i32);
            let has_body = block != None;
            let mut defined = false;
            let mut global = !matches!(storage_class, Some(StorageClass::Static));

            if let Some(old_decl) = hash_map.get(&name) {
                if function_type != old_decl.0 {
                    bail!("Incompatible function declarations");
                }

                match old_decl.1 {
                    IdentifierAttributes::FunctionAttributes(decl_defined, decl_global) => {
                        defined = decl_defined;
                        if has_body && defined {
                            bail!("Function is defined more than once");
                        }
                        if decl_global && !global {
                            bail!("Non-static function declaration after static one");
                        }
                        global = decl_global;
                    }
                    _ => bail!("No function attributes found for function declaration"),
                };
            }

            let attr = IdentifierAttributes::FunctionAttributes(has_body | defined, global);
            hash_map.insert(name.clone(), (function_type, attr.clone()));

            let mut new_block = None;
            if let Some(body) = block {
                for param in &params {
                    hash_map.insert(param.clone(), (Type::Int, attr.clone()));
                }
                new_block = Some(typecheck_block(body, hash_map)?);
            }

            Ok(FunctionDeclaration::D(
                name,
                params,
                new_block,
                storage_class,
            ))
        }
    }
}

fn typecheck_file_scope_declaration(
    decl: Declaration,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Declaration> {
    match decl {
        Declaration::V(variable_declaration) => Ok(Declaration::V(
            typecheck_file_scope_variable_declaration(variable_declaration, hash_map)?,
        )),
        Declaration::F(function_declaration) => Ok(Declaration::F(typecheck_function_declaration(
            function_declaration,
            hash_map,
        )?)),
    }
}

fn typecheck_local_declaration(
    decl: Declaration,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Declaration> {
    match decl {
        Declaration::V(variable_declaration) => Ok(Declaration::V(
            typecheck_local_variable_declaration(variable_declaration, hash_map)?,
        )),
        Declaration::F(function_declaration) => Ok(Declaration::F(typecheck_function_declaration(
            function_declaration,
            hash_map,
        )?)),
    }
}

fn typecheck_block_item(
    item: BlockItem,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<BlockItem> {
    match item {
        BlockItem::S(statement) => Ok(BlockItem::S(typecheck_statement(statement, hash_map)?)),
        BlockItem::D(declaration) => Ok(BlockItem::D(typecheck_local_declaration(
            declaration,
            hash_map,
        )?)),
    }
}

fn typecheck_block(
    block: Block,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Block> {
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

pub fn type_check(ast: Ast) -> Result<(Ast, HashMap<String, (Type, IdentifierAttributes)>)> {
    COUNTER.store(0, Ordering::SeqCst);
    /* HashMap<name, (type, attributes)> */
    let mut hash_map: HashMap<String, (Type, IdentifierAttributes)> = HashMap::new();

    match ast {
        Ast::Program(functions) => {
            let mut funcs = Vec::new();
            for func in functions {
                funcs.push(typecheck_file_scope_declaration(func, &mut hash_map)?);
            }
            Ok((Ast::Program(funcs), hash_map))
        }
    }
}
