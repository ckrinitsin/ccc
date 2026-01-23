use crate::frontend::ir::{BinOp, Instruction, Operand, TAC, TopLevel, UnOp};
use crate::frontend::{
    ast::{self, Type},
    semantic_analysis::type_check::{self, IdentifierAttributes, StaticInit, get_expression_type},
    utils::counter::{gen_label, gen_tmp, reset_counter},
};
use anyhow::{Result, bail};
use std::collections::HashMap;

fn gen_temp(
    var_type_opt: Option<Type>,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Operand> {
    let Some(var_type) = var_type_opt else {
        bail!("No expression type found in lifting process");
    };
    let name = gen_tmp();
    symbol_table.insert(name.clone(), (var_type, IdentifierAttributes::LocalAttr));
    Ok(Operand::Variable(name))
}

fn parse_unary_op(expr: ast::UnaryOp) -> Result<UnOp> {
    match expr {
        ast::UnaryOp::Complement => Ok(UnOp::Complement),
        ast::UnaryOp::Negation => Ok(UnOp::Negation),
        ast::UnaryOp::Not => Ok(UnOp::Not),
        ast::UnaryOp::Increment => Ok(UnOp::Increment),
        ast::UnaryOp::Decrement => Ok(UnOp::Decrement),
    }
}

fn parse_binary_op(expr: ast::BinaryOp) -> Result<BinOp> {
    match expr {
        ast::BinaryOp::Addition => Ok(BinOp::Addition),
        ast::BinaryOp::Subtraction => Ok(BinOp::Subtraction),
        ast::BinaryOp::Multiplication => Ok(BinOp::Multiplication),
        ast::BinaryOp::Division => Ok(BinOp::Division),
        ast::BinaryOp::Modulo => Ok(BinOp::Modulo),
        ast::BinaryOp::And => Ok(BinOp::And),
        ast::BinaryOp::Or => Ok(BinOp::Or),
        ast::BinaryOp::Xor => Ok(BinOp::Xor),
        ast::BinaryOp::LShift => Ok(BinOp::LShift),
        ast::BinaryOp::RShift => Ok(BinOp::RShift),
        ast::BinaryOp::Equal => Ok(BinOp::Equal),
        ast::BinaryOp::NEqual => Ok(BinOp::NEqual),
        ast::BinaryOp::Less => Ok(BinOp::Less),
        ast::BinaryOp::Greater => Ok(BinOp::Greater),
        ast::BinaryOp::LessEq => Ok(BinOp::LessEq),
        ast::BinaryOp::GreaterEq => Ok(BinOp::GreaterEq),
        x => bail!("{:?} should be handled seperately", x),
    }
}

fn parse_expression(
    expr: ast::Expression,
    instructions: &mut Vec<Instruction>,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Operand> {
    match expr {
        ast::Expression::Constant(c, _) => Ok(Operand::Constant(c)),
        ast::Expression::Unary(unary_op, expression, var_type) => {
            let src = parse_expression(*expression, instructions, symbol_table)?;
            let op = parse_unary_op(unary_op)?;

            let dst;
            if matches!(op, UnOp::Increment) || matches!(op, UnOp::Decrement) {
                dst = src.clone();
            } else {
                dst = gen_temp(var_type, symbol_table)?;
            }
            instructions.push(Instruction::Unary(op, src, dst.clone()));

            Ok(dst)
        }
        ast::Expression::Binary(
            binary_op @ (ast::BinaryOp::LAnd | ast::BinaryOp::LOr),
            expression1,
            expression2,
            var_type,
        ) => {
            let label = gen_label();
            let end_label = label.to_string() + "_end";
            let src1 = parse_expression(*expression1, instructions, symbol_table)?;
            let dst = gen_temp(var_type, symbol_table)?;
            if binary_op == ast::BinaryOp::LAnd {
                instructions.push(Instruction::JumpIfZero(src1, label.clone()));
            } else {
                instructions.push(Instruction::JumpIfNotZero(src1, label.clone()));
            }
            let src2 = parse_expression(*expression2, instructions, symbol_table)?;
            if binary_op == ast::BinaryOp::LAnd {
                instructions.push(Instruction::JumpIfZero(src2, label.clone()));
                instructions.push(Instruction::Copy(
                    Operand::Constant(ast::Const::Int(1)),
                    dst.clone(),
                ));
                instructions.push(Instruction::Jump(end_label.clone()));
                instructions.push(Instruction::Label(label)); // TODO: look if labels are correct
                instructions.push(Instruction::Copy(
                    Operand::Constant(ast::Const::Int(0)),
                    dst.clone(),
                ));
            } else {
                instructions.push(Instruction::JumpIfNotZero(src2, label.clone()));
                instructions.push(Instruction::Copy(
                    Operand::Constant(ast::Const::Int(0)),
                    dst.clone(),
                ));
                instructions.push(Instruction::Jump(end_label.clone()));
                instructions.push(Instruction::Label(label));
                instructions.push(Instruction::Copy(
                    Operand::Constant(ast::Const::Int(1)),
                    dst.clone(),
                ));
            }

            instructions.push(Instruction::Label(end_label));

            Ok(dst)
        }
        ast::Expression::Binary(binary_op, expression1, expression2, var_type) => {
            let src1 = parse_expression(*expression1, instructions, symbol_table)?;
            let src2 = parse_expression(*expression2, instructions, symbol_table)?;
            let dst = gen_temp(var_type, symbol_table)?;
            let op = parse_binary_op(binary_op)?;

            instructions.push(Instruction::Binary(op, src1, src2, dst.clone()));

            Ok(dst)
        }
        ast::Expression::Variable(v, _) => Ok(Operand::Variable(v)),
        ast::Expression::Assignment(var, right, _) => {
            if let ast::Expression::Variable(v, _) = *var {
                let src = parse_expression(*right, instructions, symbol_table)?;
                instructions.push(Instruction::Copy(src, Operand::Variable(v.clone())));
                Ok(Operand::Variable(v))
            } else {
                bail!("Lvalue of Assignment must be a variable!");
            }
        }
        ast::Expression::CompoundAssignment(binary_op, var, right, expr_type) => {
            if let ast::Expression::Variable(v, Some(var_type)) = *var {
                let right_type = get_expression_type(&right)?.clone();
                let right_operand = parse_expression(*right, instructions, symbol_table)?;
                if Some(var_type) == expr_type {
                    instructions.push(Instruction::Binary(
                        parse_binary_op(binary_op)?,
                        Operand::Variable(v.clone()),
                        right_operand,
                        Operand::Variable(v.clone()),
                    ));
                } else {
                    let tmp = gen_temp(expr_type, symbol_table)?;
                    match right_type {
                        Type::Int => {
                            instructions.push(Instruction::Truncate(
                                Operand::Variable(v.clone()),
                                tmp.clone(),
                            ));
                            instructions.push(Instruction::Binary(
                                parse_binary_op(binary_op)?,
                                tmp.clone(),
                                right_operand,
                                tmp.clone(),
                            ));
                            instructions
                                .push(Instruction::SignExtend(tmp, Operand::Variable(v.clone())));
                        }
                        Type::Long => {
                            instructions.push(Instruction::SignExtend(
                                Operand::Variable(v.clone()),
                                tmp.clone(),
                            ));
                            instructions.push(Instruction::Binary(
                                parse_binary_op(binary_op)?,
                                tmp.clone(),
                                right_operand,
                                tmp.clone(),
                            ));
                            instructions
                                .push(Instruction::Truncate(tmp, Operand::Variable(v.clone())));
                        }

                        Type::Function(_, _) => bail!("function?"),
                    }
                }
                Ok(Operand::Variable(v))
            } else {
                bail!("Lvalue of Assignment must be a variable!");
            }
        }
        ast::Expression::PostIncr(expr, var_type) => {
            let dst = gen_temp(var_type, symbol_table)?;
            let src = parse_expression(*expr, instructions, symbol_table)?;
            instructions.push(Instruction::Copy(src.clone(), dst.clone()));
            instructions.push(Instruction::Binary(
                BinOp::Addition,
                src.clone(),
                Operand::Constant(ast::Const::Int(1)),
                src,
            ));
            Ok(dst)
        }
        ast::Expression::PostDecr(expr, var_type) => {
            let dst = gen_temp(var_type, symbol_table)?;
            let src = parse_expression(*expr, instructions, symbol_table)?;
            instructions.push(Instruction::Copy(src.clone(), dst.clone()));
            instructions.push(Instruction::Binary(
                BinOp::Subtraction,
                src.clone(),
                Operand::Constant(ast::Const::Int(1)),
                src,
            ));
            Ok(dst)
        }
        ast::Expression::Conditional(left, middle, right, var_type) => {
            let dst = gen_temp(var_type, symbol_table)?;
            let label_false = gen_label();
            let end_label = label_false.to_string() + "_end";

            let cond = parse_expression(*left, instructions, symbol_table)?;
            instructions.push(Instruction::JumpIfZero(cond, label_false.clone()));
            let middle = parse_expression(*middle, instructions, symbol_table)?;
            instructions.push(Instruction::Copy(middle, dst.clone()));
            instructions.push(Instruction::Jump(end_label.clone()));
            instructions.push(Instruction::Label(label_false));
            let right = parse_expression(*right, instructions, symbol_table)?;
            instructions.push(Instruction::Copy(right, dst.clone()));
            instructions.push(Instruction::Label(end_label));

            Ok(dst)
        }
        ast::Expression::FunctionCall(name, expressions, var_type) => {
            let dst = gen_temp(var_type, symbol_table)?;
            let mut operands = Vec::new();
            for expr in expressions {
                operands.push(parse_expression(*expr, instructions, symbol_table)?);
            }

            instructions.push(Instruction::FunctionCall(name, operands, dst.clone()));

            Ok(dst)
        }
        ast::Expression::Cast(target_type, expression, _) => {
            let expr_type = get_expression_type(&expression)?.clone();
            let result = parse_expression(*expression, instructions, symbol_table)?;
            if target_type == expr_type {
                return Ok(result);
            }
            let dst = gen_temp(Some(target_type.clone()), symbol_table)?;
            if target_type == Type::Long {
                instructions.push(Instruction::SignExtend(result, dst.clone()));
            } else {
                instructions.push(Instruction::Truncate(result, dst.clone()));
            }
            return Ok(dst);
        }
    }
}

fn parse_for_init(
    for_init: ast::ForInit,
    instructions: &mut Vec<Instruction>,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<()> {
    match for_init {
        ast::ForInit::D(declaration) => {
            parse_variable_declaration(declaration, instructions, symbol_table)
        }
        ast::ForInit::E(opt_expression) => match opt_expression {
            Some(expression) => {
                parse_expression(expression, instructions, symbol_table)?;
                Ok(())
            }
            None => Ok(()),
        },
    }
}

fn parse_statement(
    statement: ast::Statement,
    instructions: &mut Vec<Instruction>,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<()> {
    match statement {
        ast::Statement::Return(expression) => {
            let dst = parse_expression(expression, instructions, symbol_table)?;
            instructions.push(Instruction::Ret(dst));
            Ok(())
        }
        ast::Statement::Expression(expression) => {
            parse_expression(expression, instructions, symbol_table)?;
            Ok(())
        }
        ast::Statement::Null => Ok(()),
        ast::Statement::If(condition, if_statement, else_statement) => {
            let label_else = gen_label();
            let end_label = label_else.to_string() + "_else";

            let cond = parse_expression(condition, instructions, symbol_table)?;
            instructions.push(Instruction::JumpIfZero(cond, label_else.clone()));
            parse_statement(*if_statement, instructions, symbol_table)?;
            instructions.push(Instruction::Jump(end_label.clone()));
            instructions.push(Instruction::Label(label_else));
            if let Some(x) = else_statement {
                parse_statement(*x, instructions, symbol_table)?;
            }
            instructions.push(Instruction::Label(end_label));

            Ok(())
        }
        ast::Statement::Labeled(label, statement) => {
            instructions.push(Instruction::Label(label));
            parse_statement(*statement, instructions, symbol_table)
        }
        ast::Statement::Goto(label) => {
            instructions.push(Instruction::Jump(label));
            Ok(())
        }
        ast::Statement::Compound(block) => parse_block(block, instructions, symbol_table),
        ast::Statement::While(expression, statement, label) => {
            instructions.push(Instruction::Label(label.clone().unwrap() + "_continue"));
            let cond = parse_expression(expression, instructions, symbol_table)?;
            instructions.push(Instruction::JumpIfZero(
                cond,
                label.clone().unwrap() + "_break",
            ));
            parse_statement(*statement, instructions, symbol_table)?;
            instructions.push(Instruction::Jump(label.clone().unwrap() + "_continue"));
            instructions.push(Instruction::Label(label.unwrap() + "_break"));
            Ok(())
        }
        ast::Statement::DoWhile(statement, expression, label) => {
            instructions.push(Instruction::Label(label.clone().unwrap() + "_start"));
            parse_statement(*statement, instructions, symbol_table)?;
            instructions.push(Instruction::Label(label.clone().unwrap() + "_continue"));
            let cond = parse_expression(expression, instructions, symbol_table)?;
            instructions.push(Instruction::JumpIfNotZero(
                cond,
                label.clone().unwrap() + "_start",
            ));
            instructions.push(Instruction::Label(label.unwrap() + "_break"));
            Ok(())
        }
        ast::Statement::For(for_init, opt_condition, opt_step, statement, label) => {
            parse_for_init(for_init, instructions, symbol_table)?;
            instructions.push(Instruction::Label(label.clone().unwrap() + "_start"));
            if let Some(condition) = opt_condition {
                let cond = parse_expression(condition, instructions, symbol_table)?;
                instructions.push(Instruction::JumpIfZero(
                    cond,
                    label.clone().unwrap() + "_break",
                ));
            }
            parse_statement(*statement, instructions, symbol_table)?;
            instructions.push(Instruction::Label(label.clone().unwrap() + "_continue"));
            if let Some(step) = opt_step {
                parse_expression(step, instructions, symbol_table)?;
            }

            instructions.push(Instruction::Jump(label.clone().unwrap() + "_start"));
            instructions.push(Instruction::Label(label.unwrap() + "_break"));
            Ok(())
        }
        ast::Statement::Continue(label) => {
            instructions.push(Instruction::Jump(label.unwrap() + "_continue"));
            Ok(())
        }
        ast::Statement::Break(label) => {
            instructions.push(Instruction::Jump(label.unwrap() + "_break"));
            Ok(())
        }
        ast::Statement::Switch(expression, statement, label, items) => {
            let expr_type = get_expression_type(&expression)?.clone();
            let switch_operand = parse_expression(expression, instructions, symbol_table)?;
            let mut default_case: Option<String> = None;

            for item in items {
                if item.0 == None {
                    default_case = Some(item.1);
                    continue;
                }
                let cmp_tmp = gen_temp(Some(expr_type.clone()), symbol_table)?;
                if let Some(constant) = item.0 {
                    instructions.push(Instruction::Binary(
                        BinOp::Equal,
                        switch_operand.clone(),
                        Operand::Constant(constant),
                        cmp_tmp.clone(),
                    ));
                    instructions.push(Instruction::JumpIfNotZero(cmp_tmp, item.1));
                }
            }

            if let Some(str) = default_case {
                instructions.push(Instruction::Jump(str));
            } else {
                instructions.push(Instruction::Jump(label.clone().unwrap() + "_break"));
            }

            parse_statement(*statement, instructions, symbol_table)?;
            instructions.push(Instruction::Label(label.unwrap() + "_break"));
            Ok(())
        }
        ast::Statement::Default(statement, label) => {
            instructions.push(Instruction::Label(label.unwrap()));
            parse_statement(*statement, instructions, symbol_table)?;
            Ok(())
        }
        ast::Statement::Case(_, statement, label) => {
            instructions.push(Instruction::Label(label.unwrap()));
            parse_statement(*statement, instructions, symbol_table)?;
            Ok(())
        }
    }
}

fn parse_variable_declaration(
    decl: ast::VariableDeclaration,
    instructions: &mut Vec<Instruction>,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<()> {
    match decl {
        ast::VariableDeclaration::D(id, opt_expression, _, storage_class) => {
            if storage_class == None
                && let Some(expression) = opt_expression
            {
                let src = parse_expression(expression, instructions, symbol_table)?;
                instructions.push(Instruction::Copy(src, Operand::Variable(id)));
            }
            Ok(())
        }
    }
}

fn parse_declaration(
    decl: ast::Declaration,
    instructions: &mut Vec<Instruction>,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<()> {
    match decl {
        ast::Declaration::V(variable_declaration) => {
            parse_variable_declaration(variable_declaration, instructions, symbol_table)
        }
        ast::Declaration::F(_) => Ok(()),
    }
}

fn parse_block_item(
    bl: ast::BlockItem,
    instructions: &mut Vec<Instruction>,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<()> {
    match bl {
        ast::BlockItem::S(statement) => parse_statement(statement, instructions, symbol_table),
        ast::BlockItem::D(declaration) => {
            parse_declaration(declaration, instructions, symbol_table)
        }
    }
}

fn parse_block(
    bl: ast::Block,
    instructions: &mut Vec<Instruction>,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<()> {
    match bl {
        ast::Block::B(block_items) => {
            for block in block_items {
                parse_block_item(block, instructions, symbol_table)?;
            }
            Ok(())
        }
    }
}

fn parse_function_declaration(
    fun: ast::FunctionDeclaration,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Option<TopLevel>> {
    let mut instructions = Vec::new();
    match fun {
        ast::FunctionDeclaration::D(name, params, body, _, _) => {
            if let Some(bl) = body {
                parse_block(bl, &mut instructions, symbol_table)?;
                instructions.push(Instruction::Ret(Operand::Constant(ast::Const::Int(0))));

                // TODO: write symbol_table util functions (is_global, is_static...)
                let global = match symbol_table.get(&name) {
                    Some((_, IdentifierAttributes::FunctionAttributes(_, glob))) => glob,
                    _ => bail!("Function declarations should be in symbol table"),
                };

                Ok(Some(TopLevel::Function(
                    name,
                    *global,
                    params,
                    instructions,
                )))
            } else {
                Ok(None)
            }
        }
    }
}

fn convert_symbols_to_ir(
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Vec<TopLevel>> {
    let mut result = Vec::new();
    for (name, (var_type, id_attr)) in symbol_table {
        match id_attr {
            IdentifierAttributes::StaticAttributes(initial_value, global) => match initial_value {
                type_check::InitialValue::Tentative => result.push(TopLevel::StaticVariable(
                    name.clone(),
                    *global,
                    var_type.clone(),
                    match var_type {
                        Type::Int => StaticInit::IntInit(0),
                        Type::Long => StaticInit::LongInit(0),
                        Type::Function(_, _) => bail!("No function allowed here"),
                    },
                )),
                type_check::InitialValue::Initial(i) => result.push(TopLevel::StaticVariable(
                    name.clone(),
                    *global,
                    var_type.clone(),
                    i.clone(),
                )),
                type_check::InitialValue::NoInit => (),
            },
            _ => (),
        }
    }

    Ok(result)
}

pub fn lift_to_ir(
    prog: ast::Ast,
    symbol_table: &mut HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<TAC> {
    reset_counter();
    match prog {
        ast::Ast::Program(functions) => {
            let mut top_level_ir = Vec::new();
            for func in functions {
                match func {
                    ast::Declaration::V(_) => (),
                    ast::Declaration::F(function_declaration) => {
                        if let Some(function) =
                            parse_function_declaration(function_declaration, symbol_table)?
                        {
                            top_level_ir.push(function);
                        }
                    }
                }
            }
            let mut result = convert_symbols_to_ir(symbol_table)?;
            result.append(&mut top_level_ir);
            Ok(TAC::Program(result))
        }
    }
}
