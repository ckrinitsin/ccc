use anyhow::{Result, bail};
use std::{
    collections::HashMap,
    fmt,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::frontend::{
    parse,
    type_check::{IdentifierAttributes, Type},
};

#[derive(Debug, PartialEq)]
pub enum TAC {
    Program(Vec<TopLevel>),
}

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Function(String, bool, Vec<String>, Vec<Instruction>),
    StaticVariable(String, bool, i32 /* name, global, init_value */),
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Unary(UnOp, Operand, Operand),
    Binary(BinOp, Operand, Operand, Operand),
    Copy(Operand, Operand),
    Jump(String),
    JumpIfZero(Operand, String),
    JumpIfNotZero(Operand, String),
    Label(String),
    Ret(Operand),
    FunctionCall(String, Vec<Operand>, Operand),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Constant(i32),
    Variable(String),
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Complement,
    Negation,
    Not,
    Increment,
    Decrement,
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    And,
    Or,
    Xor,
    LShift,
    RShift,
    Equal,
    NEqual,
    Less,
    Greater,
    LessEq,
    GreaterEq,
}

impl fmt::Display for TAC {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TAC::Program(functions) => {
                for func in functions {
                    write!(f, "{}\n", func)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for TopLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TopLevel::Function(name, params, body, _) => {
                write!(f, "{} {:?}:\n", name, params)?;
                for i in body {
                    write!(f, "  {}\n", i)?;
                }
                Ok(())
            }
            TopLevel::StaticVariable(name, _, val) => write!(f, "glob {} = {}", name, val),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Unary(op, src, dst) => write!(f, "{} = {}{}", dst, op, src),
            Instruction::Binary(op, src1, src2, dst) => {
                write!(f, "{} = {} {} {}", dst, src1, op, src2)
            }
            Instruction::Ret(val) => write!(f, "return {}", val),
            Instruction::Copy(src, dst) => write!(f, "{} = {}", dst, src),
            Instruction::Jump(label) => write!(f, "Jump({})", label),
            Instruction::JumpIfZero(operand, label) => {
                write!(f, "JumpIfZero({}, {})", operand, label)
            }
            Instruction::JumpIfNotZero(operand, label) => {
                write!(f, "JumpIfNotZero({}, {})", operand, label)
            }
            Instruction::Label(label) => write!(f, "\n{}:", label),
            Instruction::FunctionCall(name, params, dst) => {
                write!(f, "{} = Call({}, {:?})", dst, name, params)
            }
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Constant(x) => write!(f, "{}", x),
            Operand::Variable(id) => write!(f, "{}", id),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOp::Complement => write!(f, "~"),
            UnOp::Negation => write!(f, "-"),
            UnOp::Not => write!(f, "!"),
            UnOp::Increment => write!(f, "++"),
            UnOp::Decrement => write!(f, "--"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Addition => write!(f, "+"),
            BinOp::Subtraction => write!(f, "-"),
            BinOp::Multiplication => write!(f, "*"),
            BinOp::Division => write!(f, "/"),
            BinOp::Modulo => write!(f, "%"),
            BinOp::And => write!(f, "&"),
            BinOp::Or => write!(f, "|"),
            BinOp::Xor => write!(f, "^"),
            BinOp::LShift => write!(f, "<<"),
            BinOp::RShift => write!(f, ">>"),
            BinOp::Equal => write!(f, "=="),
            BinOp::NEqual => write!(f, "!="),
            BinOp::Less => write!(f, "<"),
            BinOp::Greater => write!(f, ">"),
            BinOp::LessEq => write!(f, "<="),
            BinOp::GreaterEq => write!(f, ">="),
        }
    }
}

static COUNTER_TMP: AtomicUsize = AtomicUsize::new(0);
static COUNTER_LABEL: AtomicUsize = AtomicUsize::new(0);

fn gen_temp() -> Operand {
    let counter = COUNTER_TMP.fetch_add(1, Ordering::SeqCst);
    Operand::Variable("tmp.".to_string() + &counter.to_string())
}

fn gen_label() -> String {
    let counter = COUNTER_LABEL.fetch_add(1, Ordering::SeqCst);
    "_".to_string() + &counter.to_string()
}

fn parse_unary_op(expr: parse::UnaryOp) -> Result<UnOp> {
    match expr {
        parse::UnaryOp::Complement => Ok(UnOp::Complement),
        parse::UnaryOp::Negation => Ok(UnOp::Negation),
        parse::UnaryOp::Not => Ok(UnOp::Not),
        parse::UnaryOp::Increment => Ok(UnOp::Increment),
        parse::UnaryOp::Decrement => Ok(UnOp::Decrement),
    }
}

fn parse_binary_op(expr: parse::BinaryOp) -> Result<BinOp> {
    match expr {
        parse::BinaryOp::Addition => Ok(BinOp::Addition),
        parse::BinaryOp::Subtraction => Ok(BinOp::Subtraction),
        parse::BinaryOp::Multiplication => Ok(BinOp::Multiplication),
        parse::BinaryOp::Division => Ok(BinOp::Division),
        parse::BinaryOp::Modulo => Ok(BinOp::Modulo),
        parse::BinaryOp::And => Ok(BinOp::And),
        parse::BinaryOp::Or => Ok(BinOp::Or),
        parse::BinaryOp::Xor => Ok(BinOp::Xor),
        parse::BinaryOp::LShift => Ok(BinOp::LShift),
        parse::BinaryOp::RShift => Ok(BinOp::RShift),
        parse::BinaryOp::Equal => Ok(BinOp::Equal),
        parse::BinaryOp::NEqual => Ok(BinOp::NEqual),
        parse::BinaryOp::Less => Ok(BinOp::Less),
        parse::BinaryOp::Greater => Ok(BinOp::Greater),
        parse::BinaryOp::LessEq => Ok(BinOp::LessEq),
        parse::BinaryOp::GreaterEq => Ok(BinOp::GreaterEq),
        x => bail!("{:?} should be handled seperately", x),
    }
}

fn parse_expression(
    expr: parse::Expression,
    instructions: &mut Vec<Instruction>,
) -> Result<Operand> {
    match expr {
        parse::Expression::Constant(c) => Ok(Operand::Constant(c)),
        parse::Expression::Unary(unary_op, expression) => {
            let src = parse_expression(*expression, instructions)?;
            let op = parse_unary_op(unary_op)?;

            let dst;
            if matches!(op, UnOp::Increment) || matches!(op, UnOp::Decrement) {
                dst = src.clone();
            } else {
                dst = gen_temp();
            }
            instructions.push(Instruction::Unary(op, src, dst.clone()));

            Ok(dst)
        }
        parse::Expression::Binary(
            binary_op @ (parse::BinaryOp::LAnd | parse::BinaryOp::LOr),
            expression1,
            expression2,
        ) => {
            let label = gen_label();
            let end_label = label.to_string() + "_end";
            let src1 = parse_expression(*expression1, instructions)?;
            let dst = gen_temp();
            if binary_op == parse::BinaryOp::LAnd {
                instructions.push(Instruction::JumpIfZero(src1, label.clone()));
            } else {
                instructions.push(Instruction::JumpIfNotZero(src1, label.clone()));
            }
            // TODO: try to remove the clones (references in enum?)
            let src2 = parse_expression(*expression2, instructions)?;
            if binary_op == parse::BinaryOp::LAnd {
                instructions.push(Instruction::JumpIfZero(src2, label.clone()));
                instructions.push(Instruction::Copy(Operand::Constant(1), dst.clone()));
                instructions.push(Instruction::Jump(end_label.clone()));
                instructions.push(Instruction::Label(label));
                instructions.push(Instruction::Copy(Operand::Constant(0), dst.clone()));
            } else {
                instructions.push(Instruction::JumpIfNotZero(src2, label.clone()));
                instructions.push(Instruction::Copy(Operand::Constant(0), dst.clone()));
                instructions.push(Instruction::Jump(end_label.clone()));
                instructions.push(Instruction::Label(label));
                instructions.push(Instruction::Copy(Operand::Constant(1), dst.clone()));
            }

            instructions.push(Instruction::Label(end_label));

            Ok(dst)
        }
        parse::Expression::Binary(binary_op, expression1, expression2) => {
            let src1 = parse_expression(*expression1, instructions)?;
            let src2 = parse_expression(*expression2, instructions)?;
            let dst = gen_temp();
            let op = parse_binary_op(binary_op)?;

            instructions.push(Instruction::Binary(op, src1, src2, dst.clone()));

            Ok(dst)
        }
        parse::Expression::Variable(v) => Ok(Operand::Variable(v)),
        parse::Expression::Assignment(var, right) => {
            if let parse::Expression::Variable(v) = *var {
                let src = parse_expression(*right, instructions)?;
                instructions.push(Instruction::Copy(src, Operand::Variable(v.clone())));
                Ok(Operand::Variable(v))
            } else {
                bail!("Lvalue of Assignment must be a variable!");
            }
        }
        parse::Expression::CompoundAssignment(binary_op, var, right) => {
            if let parse::Expression::Variable(v) = *var {
                let right = parse_expression(*right, instructions)?;
                instructions.push(Instruction::Binary(
                    parse_binary_op(binary_op)?,
                    Operand::Variable(v.clone()),
                    right,
                    Operand::Variable(v.clone()),
                ));
                Ok(Operand::Variable(v))
            } else {
                bail!("Lvalue of Assignment must be a variable!");
            }
        }
        parse::Expression::PostIncr(expr) => {
            let dst = gen_temp();
            let src = parse_expression(*expr, instructions)?;
            instructions.push(Instruction::Copy(src.clone(), dst.clone()));
            instructions.push(Instruction::Binary(
                BinOp::Addition,
                src.clone(),
                Operand::Constant(1),
                src,
            ));
            Ok(dst)
        }
        parse::Expression::PostDecr(expr) => {
            let dst = gen_temp();
            let src = parse_expression(*expr, instructions)?;
            instructions.push(Instruction::Copy(src.clone(), dst.clone()));
            instructions.push(Instruction::Binary(
                BinOp::Subtraction,
                src.clone(),
                Operand::Constant(1),
                src,
            ));
            Ok(dst)
        }
        parse::Expression::Conditional(left, middle, right) => {
            let dst = gen_temp();
            let label_false = gen_label();
            let end_label = label_false.to_string() + "_end";

            let cond = parse_expression(*left, instructions)?;
            instructions.push(Instruction::JumpIfZero(cond, label_false.clone()));
            let middle = parse_expression(*middle, instructions)?;
            instructions.push(Instruction::Copy(middle, dst.clone()));
            instructions.push(Instruction::Jump(end_label.clone()));
            instructions.push(Instruction::Label(label_false));
            let right = parse_expression(*right, instructions)?;
            instructions.push(Instruction::Copy(right, dst.clone()));
            instructions.push(Instruction::Label(end_label));

            Ok(dst)
        }
        parse::Expression::FunctionCall(name, expressions) => {
            let dst = gen_temp();
            let mut operands = Vec::new();
            for expr in expressions {
                operands.push(parse_expression(*expr, instructions)?);
            }

            instructions.push(Instruction::FunctionCall(name, operands, dst.clone()));

            Ok(dst)
        }
    }
}

fn parse_for_init(for_init: parse::ForInit, instructions: &mut Vec<Instruction>) -> Result<()> {
    match for_init {
        parse::ForInit::D(declaration) => parse_variable_declaration(declaration, instructions),
        parse::ForInit::E(opt_expression) => match opt_expression {
            Some(expression) => {
                parse_expression(expression, instructions)?;
                Ok(())
            }
            None => Ok(()),
        },
    }
}

fn parse_statement(statement: parse::Statement, instructions: &mut Vec<Instruction>) -> Result<()> {
    match statement {
        parse::Statement::Return(expression) => {
            let dst = parse_expression(expression, instructions)?;
            instructions.push(Instruction::Ret(dst));
            Ok(())
        }
        parse::Statement::Expression(expression) => {
            parse_expression(expression, instructions)?;
            Ok(())
        }
        parse::Statement::Null => Ok(()),
        parse::Statement::If(condition, if_statement, else_statement) => {
            let label_else = gen_label();
            let end_label = label_else.to_string() + "_else";

            let cond = parse_expression(condition, instructions)?;
            instructions.push(Instruction::JumpIfZero(cond, label_else.clone()));
            parse_statement(*if_statement, instructions)?;
            instructions.push(Instruction::Jump(end_label.clone()));
            instructions.push(Instruction::Label(label_else));
            if let Some(x) = else_statement {
                parse_statement(*x, instructions)?;
            }
            instructions.push(Instruction::Label(end_label));

            Ok(())
        }
        parse::Statement::Labeled(label, statement) => {
            instructions.push(Instruction::Label(label));
            parse_statement(*statement, instructions)
        }
        parse::Statement::Goto(label) => {
            instructions.push(Instruction::Jump(label));
            Ok(())
        }
        parse::Statement::Compound(block) => parse_block(block, instructions),
        parse::Statement::While(expression, statement, label) => {
            instructions.push(Instruction::Label(label.clone().unwrap() + "_continue"));
            let cond = parse_expression(expression, instructions)?;
            instructions.push(Instruction::JumpIfZero(
                cond,
                label.clone().unwrap() + "_break",
            ));
            parse_statement(*statement, instructions)?;
            instructions.push(Instruction::Jump(label.clone().unwrap() + "_continue"));
            instructions.push(Instruction::Label(label.unwrap() + "_break"));
            Ok(())
        }
        parse::Statement::DoWhile(statement, expression, label) => {
            instructions.push(Instruction::Label(label.clone().unwrap() + "_start"));
            parse_statement(*statement, instructions)?;
            instructions.push(Instruction::Label(label.clone().unwrap() + "_continue"));
            let cond = parse_expression(expression, instructions)?;
            instructions.push(Instruction::JumpIfNotZero(
                cond,
                label.clone().unwrap() + "_start",
            ));
            instructions.push(Instruction::Label(label.unwrap() + "_break"));
            Ok(())
        }
        parse::Statement::For(for_init, opt_condition, opt_step, statement, label) => {
            parse_for_init(for_init, instructions)?;
            instructions.push(Instruction::Label(label.clone().unwrap() + "_start"));
            if let Some(condition) = opt_condition {
                let cond = parse_expression(condition, instructions)?;
                instructions.push(Instruction::JumpIfZero(
                    cond,
                    label.clone().unwrap() + "_break",
                ));
            }
            parse_statement(*statement, instructions)?;
            instructions.push(Instruction::Label(label.clone().unwrap() + "_continue"));
            if let Some(step) = opt_step {
                parse_expression(step, instructions)?;
            }

            instructions.push(Instruction::Jump(label.clone().unwrap() + "_start"));
            instructions.push(Instruction::Label(label.unwrap() + "_break"));
            Ok(())
        }
        parse::Statement::Continue(label) => {
            instructions.push(Instruction::Jump(label.unwrap() + "_continue"));
            Ok(())
        }
        parse::Statement::Break(label) => {
            instructions.push(Instruction::Jump(label.unwrap() + "_break"));
            Ok(())
        }
        parse::Statement::Switch(expression, statement, label, items) => {
            let switch_operand = parse_expression(expression, instructions)?;
            let mut default_case: Option<String> = None;

            for item in items {
                if item.0 == None {
                    default_case = Some(item.1);
                    continue;
                }
                let cmp_tmp = gen_temp();
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

            parse_statement(*statement, instructions)?;
            instructions.push(Instruction::Label(label.unwrap() + "_break"));
            Ok(())
        }
        parse::Statement::Default(statement, label) => {
            instructions.push(Instruction::Label(label.unwrap()));
            parse_statement(*statement, instructions)?;
            Ok(())
        }
        parse::Statement::Case(_, statement, label) => {
            instructions.push(Instruction::Label(label.unwrap()));
            parse_statement(*statement, instructions)?;
            Ok(())
        }
    }
}

fn parse_variable_declaration(
    decl: parse::VariableDeclaration,
    instructions: &mut Vec<Instruction>,
) -> Result<()> {
    match decl {
        parse::VariableDeclaration::D(id, opt_expression, storage_class) => {
            if storage_class == None
                && let Some(expression) = opt_expression
            {
                let src = parse_expression(expression, instructions)?;
                instructions.push(Instruction::Copy(src, Operand::Variable(id)));
            }
            Ok(())
        }
    }
}

fn parse_declaration(decl: parse::Declaration, instructions: &mut Vec<Instruction>) -> Result<()> {
    match decl {
        parse::Declaration::V(variable_declaration) => {
            parse_variable_declaration(variable_declaration, instructions)
        }
        parse::Declaration::F(_) => Ok(()),
    }
}

fn parse_block_item(bl: parse::BlockItem, instructions: &mut Vec<Instruction>) -> Result<()> {
    match bl {
        parse::BlockItem::S(statement) => parse_statement(statement, instructions),
        parse::BlockItem::D(declaration) => parse_declaration(declaration, instructions),
    }
}

fn parse_block(bl: parse::Block, instructions: &mut Vec<Instruction>) -> Result<()> {
    match bl {
        parse::Block::B(block_items) => {
            for block in block_items {
                parse_block_item(block, instructions)?;
            }
            Ok(())
        }
    }
}

fn parse_function_declaration(
    fun: parse::FunctionDeclaration,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Option<TopLevel>> {
    let mut instructions = Vec::new();
    match fun {
        parse::FunctionDeclaration::D(name, params, body, _) => {
            if let Some(bl) = body {
                parse_block(bl, &mut instructions)?;
                instructions.push(Instruction::Ret(Operand::Constant(0)));

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
    for (name, (_, id_attr)) in symbol_table {
        match id_attr {
            IdentifierAttributes::StaticAttributes(initial_value, global) => match initial_value {
                super::type_check::InitialValue::Tentative => {
                    result.push(TopLevel::StaticVariable(name.clone(), *global, 0))
                }
                super::type_check::InitialValue::Initial(i) => {
                    result.push(TopLevel::StaticVariable(name.clone(), *global, *i))
                }
                super::type_check::InitialValue::NoInit => (),
            },
            _ => (),
        }
    }

    Ok(result)
}

pub fn lift_to_ir(
    prog: parse::Ast,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<TAC> {
    COUNTER_TMP.store(0, Ordering::SeqCst);
    COUNTER_LABEL.store(0, Ordering::SeqCst);
    match prog {
        parse::Ast::Program(functions) => {
            let mut top_level_ir = Vec::new();
            for func in functions {
                match func {
                    parse::Declaration::V(_) => (),
                    parse::Declaration::F(function_declaration) => {
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
