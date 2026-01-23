use std::{collections::HashMap, iter::zip};

use crate::frontend::ast::{
    Ast, BinaryOp, Block, BlockItem, Const, Declaration, Expression, ForInit, FunctionDeclaration,
    Statement, StorageClass, Type, UnaryOp, VariableDeclaration,
};
use anyhow::{Result, bail};

#[derive(Debug, PartialEq, Clone)]
pub enum IdentifierAttributes {
    FunctionAttributes(bool, bool /* defined, global */),
    StaticAttributes(InitialValue, bool /* init, global */),
    LocalAttr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInit),
    NoInit,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StaticInit {
    IntInit(i32),
    LongInit(i64),
}

pub fn get_expression_type(expr: &Expression) -> Result<&Type> {
    match expr {
        Expression::Constant(_, Some(var_type)) => Ok(var_type),
        Expression::Variable(_, Some(var_type)) => Ok(var_type),
        Expression::Cast(_, _, Some(var_type)) => Ok(var_type),
        Expression::Unary(_, _, Some(var_type)) => Ok(var_type),
        Expression::Binary(_, _, _, Some(var_type)) => Ok(var_type),
        Expression::Assignment(_, _, Some(var_type)) => Ok(var_type),
        Expression::CompoundAssignment(_, _, _, Some(var_type)) => Ok(var_type),
        Expression::PostIncr(_, Some(var_type)) => Ok(var_type),
        Expression::PostDecr(_, Some(var_type)) => Ok(var_type),
        Expression::Conditional(_, _, _, Some(var_type)) => Ok(var_type),
        Expression::FunctionCall(_, _, Some(var_type)) => Ok(var_type),
        _ => bail!("Expressions should have a type at this point"),
    }
}

fn set_expression_type(expr: Expression, new_type: Type) -> Result<Expression> {
    match expr {
        Expression::Constant(a, _) => Ok(Expression::Constant(a, Some(new_type))),
        Expression::Variable(a, _) => Ok(Expression::Variable(a, Some(new_type))),
        Expression::Cast(a, b, _) => Ok(Expression::Cast(a, b, Some(new_type))),
        Expression::Unary(a, b, _) => Ok(Expression::Unary(a, b, Some(new_type))),
        Expression::Binary(a, b, c, _) => Ok(Expression::Binary(a, b, c, Some(new_type))),
        Expression::Assignment(a, b, _) => Ok(Expression::Assignment(a, b, Some(new_type))),
        Expression::CompoundAssignment(a, b, c, _) => {
            Ok(Expression::CompoundAssignment(a, b, c, Some(new_type)))
        }
        Expression::PostIncr(a, _) => Ok(Expression::PostIncr(a, Some(new_type))),
        Expression::PostDecr(a, _) => Ok(Expression::PostDecr(a, Some(new_type))),
        Expression::Conditional(a, b, c, _) => Ok(Expression::Conditional(a, b, c, Some(new_type))),
        Expression::FunctionCall(a, b, _) => Ok(Expression::FunctionCall(a, b, Some(new_type))),
    }
}

fn get_common_type(type1: Type, type2: Type) -> Result<Type> {
    if type1 == type2 {
        return Ok(type1);
    } else {
        return Ok(Type::Long);
    }
}

fn convert_const_to_static_init(constant: &Const) -> Result<StaticInit> {
    match constant {
        Const::Int(x) => Ok(StaticInit::IntInit(*x)),
        Const::Long(x) => Ok(StaticInit::LongInit(*x)),
    }
}

fn convert_const_to_type(constant: &Const) -> Result<Type> {
    match constant {
        Const::Int(_) => Ok(Type::Int),
        Const::Long(_) => Ok(Type::Long),
    }
}

fn set_const_type(constant: Const, new_type: Type) -> Result<Const> {
    match constant {
        Const::Int(x) => match new_type {
            Type::Int => Ok(constant),
            Type::Long => Ok(Const::Long(x as i64)),
            Type::Function(_, _) => bail!("Const to function is not allowed"),
        },
        Const::Long(x) => match new_type {
            Type::Int => {
                let mut val = x;
                while val > 2_i64.pow(31) - 1 {
                    val -= 2_i64.pow(32);
                }
                Ok(Const::Int(val as i32))
            }
            Type::Long => Ok(constant),
            Type::Function(_, _) => bail!("Const to function is not allowed"),
        },
    }
}

fn set_init_value_type(init: StaticInit, new_type: Type) -> Result<StaticInit> {
    match init {
        StaticInit::IntInit(x) => match new_type {
            Type::Int => Ok(init),
            Type::Long => Ok(StaticInit::LongInit(x as i64)),
            Type::Function(_, _) => bail!("StaticInit to function is not allowed"),
        },
        StaticInit::LongInit(x) => match new_type {
            Type::Int => {
                let mut val = x;
                while val > 2_i64.pow(31) - 1 {
                    val -= 2_i64.pow(32);
                }
                Ok(StaticInit::IntInit(val as i32))
            }
            Type::Long => Ok(init),
            Type::Function(_, _) => bail!("StaticInit to function is not allowed"),
        },
    }
}

fn typecheck_expression(
    expr: Expression,
    hash_map: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Expression> {
    match expr {
        Expression::Variable(x, _) => {
            if let Some(var) = hash_map.get(&x) {
                if matches!(var.0, Type::Function(_, _)) {
                    bail!("Function name used as a variable {}", x);
                }
                return Ok(Expression::Variable(x, Some(var.0.clone())));
            }
            bail!("Variable {} not found in symbol table", x);
        }
        Expression::Constant(x, _) => match x {
            Const::Int(i) => Ok(Expression::Constant(Const::Int(i), Some(Type::Int))),
            Const::Long(l) => Ok(Expression::Constant(Const::Long(l), Some(Type::Long))),
        },
        Expression::Unary(unary_op, expression, _) => {
            let expr = typecheck_expression(*expression, hash_map)?;
            let expr_type = get_expression_type(&expr)?.clone();
            match unary_op {
                UnaryOp::Not => Ok(Expression::Unary(unary_op, Box::new(expr), Some(Type::Int))),
                _ => Ok(Expression::Unary(unary_op, Box::new(expr), Some(expr_type))),
            }
        }
        Expression::Binary(binary_op, left, right, _) => {
            let left = typecheck_expression(*left, hash_map)?;
            let right = typecheck_expression(*right, hash_map)?;
            if matches!(binary_op, BinaryOp::LAnd) || matches!(binary_op, BinaryOp::LOr) {
                return Ok(Expression::Binary(
                    binary_op,
                    Box::new(left),
                    Box::new(right),
                    Some(Type::Int),
                ));
            }
            if matches!(binary_op, BinaryOp::LShift) || matches!(binary_op, BinaryOp::RShift) {
                let left_type = get_expression_type(&left)?.clone();
                return Ok(Expression::Binary(
                    binary_op,
                    Box::new(left),
                    Box::new(right),
                    Some(left_type),
                ));
            }
            let common_type = get_common_type(
                get_expression_type(&left)?.clone(),
                get_expression_type(&right)?.clone(),
            )?;
            let left = Expression::Cast(
                common_type.clone(),
                Box::new(left),
                Some(common_type.clone()),
            );
            let right = Expression::Cast(
                common_type.clone(),
                Box::new(right),
                Some(common_type.clone()),
            );
            match binary_op {
                BinaryOp::Addition
                | BinaryOp::Subtraction
                | BinaryOp::Multiplication
                | BinaryOp::Division
                | BinaryOp::Modulo
                | BinaryOp::And
                | BinaryOp::Or
                | BinaryOp::Xor => Ok(Expression::Binary(
                    binary_op,
                    Box::new(left),
                    Box::new(right),
                    Some(common_type),
                )),
                _ => Ok(Expression::Binary(
                    binary_op,
                    Box::new(left),
                    Box::new(right),
                    Some(Type::Int),
                )),
            }
        }
        Expression::Assignment(left, right, _) => {
            let left = typecheck_expression(*left, hash_map)?;
            let right = typecheck_expression(*right, hash_map)?;
            let left_type = get_expression_type(&left)?.clone();
            let right =
                Expression::Cast(left_type.clone(), Box::new(right), Some(left_type.clone()));
            Ok(Expression::Assignment(
                Box::new(left),
                Box::new(right),
                Some(left_type),
            ))
        }
        Expression::CompoundAssignment(binary_op, left, right, _) => {
            let left = typecheck_expression(*left, hash_map)?;
            let right = typecheck_expression(*right, hash_map)?;
            let left_type = get_expression_type(&left)?.clone();

            if binary_op == BinaryOp::LShift || binary_op == BinaryOp::RShift {
                return Ok(Expression::CompoundAssignment(
                    binary_op,
                    Box::new(left),
                    Box::new(right),
                    Some(left_type),
                ));
            }

            let common_type = get_common_type(
                get_expression_type(&left)?.clone(),
                get_expression_type(&right)?.clone(),
            )?;
            let right = Expression::Cast(
                common_type.clone(),
                Box::new(right),
                Some(common_type.clone()),
            );
            Ok(Expression::CompoundAssignment(
                binary_op,
                Box::new(left),
                Box::new(right),
                Some(common_type),
            ))
        }
        Expression::PostIncr(expr, _) => {
            let expr = typecheck_expression(*expr, hash_map)?;
            let expr_type = get_expression_type(&expr)?.clone();
            Ok(Expression::PostIncr(Box::new(expr), Some(expr_type)))
        }
        Expression::PostDecr(expr, _) => {
            let expr = typecheck_expression(*expr, hash_map)?;
            let expr_type = get_expression_type(&expr)?.clone();
            Ok(Expression::PostDecr(Box::new(expr), Some(expr_type)))
        }

        Expression::Conditional(cond, then, else_expr, _) => {
            let cond = typecheck_expression(*cond, hash_map)?;
            let then = typecheck_expression(*then, hash_map)?;
            let else_expr = typecheck_expression(*else_expr, hash_map)?;
            let common_type = get_common_type(
                get_expression_type(&then)?.clone(),
                get_expression_type(&else_expr)?.clone(),
            )?;
            let then = set_expression_type(then, common_type.clone())?;
            let else_expr = set_expression_type(else_expr, common_type.clone())?;
            Ok(Expression::Conditional(
                Box::new(cond),
                Box::new(then),
                Box::new(else_expr),
                Some(common_type),
            ))
        }
        Expression::FunctionCall(name, args, _) => match hash_map.get(&name) {
            Some((Type::Function(param_types, ret_type), _)) => {
                if param_types.len() != args.len() {
                    bail!(
                        "Function {} called with {} arguments, but it has {}",
                        name,
                        args.len(),
                        param_types.len(),
                    );
                }
                let mut new_args = Vec::new();
                for (arg, param_type) in zip(args, param_types) {
                    let typed_arg = Expression::Cast(
                        *param_type.clone(),
                        Box::new(typecheck_expression(*arg, hash_map)?),
                        Some(*param_type.clone()),
                    );
                    new_args.push(Box::new(typed_arg));
                }
                Ok(Expression::FunctionCall(
                    name,
                    new_args,
                    Some(*ret_type.clone()),
                ))
            }
            _ => bail!("Variable used as a function name"),
        },
        Expression::Cast(new_type, expr, _) => Ok(Expression::Cast(
            new_type.clone(),
            Box::new(typecheck_expression(*expr, hash_map)?),
            Some(new_type),
        )),
    }
}

fn typecheck_optional_expression(
    opt_expression: Option<Expression>,
    hash_map: &HashMap<String, (Type, IdentifierAttributes)>,
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
                VariableDeclaration::D(_, _, _, storage_class) => {
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
    function_name: &String,
) -> Result<Statement> {
    match statement {
        Statement::Return(expression) => {
            let expr = typecheck_expression(expression, hash_map)?;
            if let Some((fun_type, _)) = hash_map.get(function_name) {
                match fun_type {
                    Type::Function(_, ret_type) => {
                        return Ok(Statement::Return(Expression::Cast(
                            *ret_type.clone(),
                            Box::new(expr),
                            Some(*ret_type.clone()),
                        )));
                    }
                    _ => bail!("Should be function"),
                }
            }
            Ok(Statement::Return(expr))
        }
        Statement::Expression(expression) => Ok(Statement::Expression(typecheck_expression(
            expression, hash_map,
        )?)),
        Statement::Null => Ok(Statement::Null),
        Statement::If(condition, if_statement, else_statement) => match else_statement {
            Some(x) => Ok(Statement::If(
                typecheck_expression(condition, hash_map)?,
                Box::new(typecheck_statement(*if_statement, hash_map, function_name)?),
                Some(Box::new(typecheck_statement(*x, hash_map, function_name)?)),
            )),
            None => Ok(Statement::If(
                typecheck_expression(condition, hash_map)?,
                Box::new(typecheck_statement(*if_statement, hash_map, function_name)?),
                None,
            )),
        },
        Statement::Labeled(id, statement) => Ok(Statement::Labeled(
            id,
            Box::new(typecheck_statement(*statement, hash_map, function_name)?),
        )),
        Statement::Compound(block) => Ok(Statement::Compound(typecheck_block(
            block,
            hash_map,
            function_name,
        )?)),
        Statement::While(expression, statement, label) => Ok(Statement::While(
            typecheck_expression(expression, hash_map)?,
            Box::new(typecheck_statement(*statement, hash_map, function_name)?),
            label,
        )),
        Statement::DoWhile(statement, expression, label) => Ok(Statement::DoWhile(
            Box::new(typecheck_statement(*statement, hash_map, function_name)?),
            typecheck_expression(expression, hash_map)?,
            label,
        )),
        Statement::For(for_init, condition, step, body, label) => {
            let for_init = typecheck_for_init(for_init, hash_map)?;
            let condition = typecheck_optional_expression(condition, hash_map)?;
            let step = typecheck_optional_expression(step, hash_map)?;
            let body = typecheck_statement(*body, hash_map, function_name)?;
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
            Box::new(typecheck_statement(*statement, hash_map, function_name)?),
            label,
        )),
        Statement::Switch(expression, statement, label, cases) => {
            let expr = typecheck_expression(expression, hash_map)?;
            let expr_type = get_expression_type(&expr)?;

            let mut new_cases = Vec::new();
            for (case, id) in &cases {
                if let Some(constant) = case {
                    let new_case = set_const_type(constant.clone(), expr_type.clone())?;
                    for (test_case, _) in &new_cases {
                        if let Some(cc) = test_case {
                            if *cc == new_case {
                                bail!("Duplication of cases in switch statement: {:?}", new_cases);
                            }
                        }
                    }
                    new_cases.push((Some(new_case), id.clone()));
                } else {
                    new_cases.push((None, id.clone()));
                }
            }
            Ok(Statement::Switch(
                expr,
                Box::new(typecheck_statement(*statement, hash_map, function_name)?),
                label,
                new_cases,
            ))
        }
        Statement::Default(statement, label) => Ok(Statement::Default(
            Box::new(typecheck_statement(*statement, hash_map, function_name)?),
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
        VariableDeclaration::D(name, initializer, var_type, storage_class) => {
            let mut init_value = match &initializer {
                Some(Expression::Constant(x, _)) => {
                    InitialValue::Initial(convert_const_to_static_init(x)?)
                }
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
                if old_decl.0 != var_type {
                    bail!("Type mismatching redeclaration of variables is not allowed");
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

            let init_value = match init_value {
                InitialValue::Tentative => init_value,
                InitialValue::Initial(static_init) => {
                    InitialValue::Initial(set_init_value_type(static_init, var_type.clone())?)
                }
                InitialValue::NoInit => init_value,
            };
            let initializer = match initializer {
                Some(e) => Some(set_expression_type(e, var_type.clone())?),
                None => None,
            };

            let attrs = IdentifierAttributes::StaticAttributes(init_value, global);
            hash_map.insert(name.clone(), (var_type.clone(), attrs));
            Ok(VariableDeclaration::D(
                name,
                initializer,
                var_type,
                storage_class,
            ))
        }
    }
}

fn typecheck_local_variable_declaration(
    decl: VariableDeclaration,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<VariableDeclaration> {
    match decl {
        VariableDeclaration::D(name, opt_expression, var_type, storage_class) => {
            let mut new_init = opt_expression;
            match storage_class {
                Some(StorageClass::Extern) => {
                    if new_init != None {
                        bail!("Initializer on local extern variable not allowed");
                    }
                    if let Some(old_decl) = hash_map.get(&name) {
                        if old_decl.0 != var_type {
                            bail!("Type mismatch in local variable declaration");
                        }
                    } else {
                        hash_map.insert(
                            name.clone(),
                            (
                                var_type.clone(),
                                IdentifierAttributes::StaticAttributes(InitialValue::NoInit, true),
                            ),
                        );
                    }
                }
                Some(StorageClass::Static) => {
                    let init = match &new_init {
                        Some(Expression::Constant(x, _)) => convert_const_to_static_init(x)?,
                        Some(_) => {
                            bail!("Non-constant initializer on local static variable not allowed")
                        }
                        None => StaticInit::IntInit(0),
                    };
                    hash_map.insert(
                        name.clone(),
                        (
                            var_type.clone(),
                            IdentifierAttributes::StaticAttributes(
                                InitialValue::Initial(set_init_value_type(init, var_type.clone())?),
                                false,
                            ),
                        ),
                    );
                }
                None => {
                    hash_map.insert(
                        name.clone(),
                        (var_type.clone(), IdentifierAttributes::LocalAttr),
                    );
                    if let Some(init) = new_init {
                        new_init = Some(typecheck_expression(init, hash_map)?);
                    }
                }
            };

            let new_init = match new_init {
                Some(e) => Some(set_expression_type(e, var_type.clone())?),
                None => None,
            };

            Ok(VariableDeclaration::D(
                name,
                new_init,
                var_type,
                storage_class,
            ))
        }
    }
}

fn typecheck_function_declaration(
    decl: FunctionDeclaration,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<FunctionDeclaration> {
    match decl {
        FunctionDeclaration::D(name, params, block, fun_type, storage_class) => {
            let has_body = block != None;
            let mut defined = false;
            let mut global = !matches!(storage_class, Some(StorageClass::Static));

            if let Some(old_decl) = hash_map.get(&name) {
                if fun_type != old_decl.0 {
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
            hash_map.insert(name.clone(), (fun_type.clone(), attr.clone()));

            let mut new_block = None;
            match &fun_type {
                Type::Function(items, _) => {
                    if let Some(body) = block {
                        for (param, item) in zip(&params, items) {
                            hash_map.insert(param.clone(), (*item.clone(), attr.clone()));
                        }
                        new_block = Some(typecheck_block(body, hash_map, &name)?);
                    }
                }
                _ => bail!("Fun type should be function"),
            };

            Ok(FunctionDeclaration::D(
                name,
                params,
                new_block,
                fun_type,
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
    function_name: &String,
) -> Result<BlockItem> {
    match item {
        BlockItem::S(statement) => Ok(BlockItem::S(typecheck_statement(
            statement,
            hash_map,
            function_name,
        )?)),
        BlockItem::D(declaration) => Ok(BlockItem::D(typecheck_local_declaration(
            declaration,
            hash_map,
        )?)),
    }
}

fn typecheck_block(
    block: Block,
    hash_map: &mut HashMap<String, (Type, IdentifierAttributes)>,
    function_name: &String,
) -> Result<Block> {
    match block {
        Block::B(block_items) => {
            let mut new_block_items = Vec::new();
            for item in block_items {
                new_block_items.push(typecheck_block_item(item, hash_map, function_name)?);
            }
            Ok(Block::B(new_block_items))
        }
    }
}

pub fn type_check(ast: Ast) -> Result<(Ast, HashMap<String, (Type, IdentifierAttributes)>)> {
    /* HashMap<name, (type, attributes)> */
    let mut hash_map: HashMap<String, (Type, IdentifierAttributes)> = HashMap::new();

    match ast {
        Ast::Program(declarations) => {
            let mut new_decls = Vec::new();
            for decl in declarations {
                new_decls.push(typecheck_file_scope_declaration(decl, &mut hash_map)?);
            }
            Ok((Ast::Program(new_decls), hash_map))
        }
    }
}
