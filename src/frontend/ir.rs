use anyhow::{Result, bail};
use std::{
    fmt,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::frontend::parser;

#[derive(Debug, PartialEq)]
pub enum TAC {
    Program(Function),
}

#[derive(Debug, PartialEq)]
pub enum Function {
    Function(String, Vec<Instruction>),
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
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Constant(i64),
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
            TAC::Program(x) => write!(f, "{}", x),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Function(name, body) => {
                write!(f, "{}:\n", name)?;
                for i in body {
                    write!(f, "  {}\n", i)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Unary(op, src, dst) => write!(f, "{} = {}{}", dst, op, src),
            Instruction::Binary(op, src1, src2, dst) => {
                write!(f, "{} = {}{}{}", dst, src1, op, src2)
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

fn parse_unary_op(expr: parser::UnaryOp) -> Result<UnOp> {
    match expr {
        parser::UnaryOp::Complement => Ok(UnOp::Complement),
        parser::UnaryOp::Negation => Ok(UnOp::Negation),
        parser::UnaryOp::Not => Ok(UnOp::Not),
        parser::UnaryOp::Increment => Ok(UnOp::Increment),
        parser::UnaryOp::Decrement => Ok(UnOp::Decrement),
    }
}

fn parse_binary_op(expr: parser::BinaryOp) -> Result<BinOp> {
    match expr {
        parser::BinaryOp::Addition => Ok(BinOp::Addition),
        parser::BinaryOp::Subtraction => Ok(BinOp::Subtraction),
        parser::BinaryOp::Multiplication => Ok(BinOp::Multiplication),
        parser::BinaryOp::Division => Ok(BinOp::Division),
        parser::BinaryOp::Modulo => Ok(BinOp::Modulo),
        parser::BinaryOp::And => Ok(BinOp::And),
        parser::BinaryOp::Or => Ok(BinOp::Or),
        parser::BinaryOp::Xor => Ok(BinOp::Xor),
        parser::BinaryOp::LShift => Ok(BinOp::LShift),
        parser::BinaryOp::RShift => Ok(BinOp::RShift),
        parser::BinaryOp::Equal => Ok(BinOp::Equal),
        parser::BinaryOp::NEqual => Ok(BinOp::NEqual),
        parser::BinaryOp::Less => Ok(BinOp::Less),
        parser::BinaryOp::Greater => Ok(BinOp::Greater),
        parser::BinaryOp::LessEq => Ok(BinOp::LessEq),
        parser::BinaryOp::GreaterEq => Ok(BinOp::GreaterEq),
        x => bail!("{:?} should be handled seperately", x),
    }
}

fn parse_expression(
    expr: parser::Expression,
    instructions: &mut Vec<Instruction>,
) -> Result<Operand> {
    match expr {
        parser::Expression::Constant(c) => Ok(Operand::Constant(c)),
        parser::Expression::Unary(unary_op, expression) => {
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
        parser::Expression::Binary(
            binary_op @ (parser::BinaryOp::LAnd | parser::BinaryOp::LOr),
            expression1,
            expression2,
        ) => {
            let label = gen_label();
            let end_label = label.to_string() + "_end";
            let src1 = parse_expression(*expression1, instructions)?;
            let dst = gen_temp();
            if binary_op == parser::BinaryOp::LAnd {
                instructions.push(Instruction::JumpIfZero(src1, label.clone()));
            } else {
                instructions.push(Instruction::JumpIfNotZero(src1, label.clone()));
            }

            let src2 = parse_expression(*expression2, instructions)?;
            if binary_op == parser::BinaryOp::LAnd {
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
        parser::Expression::Binary(binary_op, expression1, expression2) => {
            let src1 = parse_expression(*expression1, instructions)?;
            let src2 = parse_expression(*expression2, instructions)?;
            let dst = gen_temp();
            let op = parse_binary_op(binary_op)?;

            instructions.push(Instruction::Binary(op, src1, src2, dst.clone()));

            Ok(dst)
        }
        parser::Expression::Variable(v) => Ok(Operand::Variable(v)),
        parser::Expression::Assignment(var, right) => {
            if let parser::Expression::Variable(v) = *var {
                let src = parse_expression(*right, instructions)?;
                instructions.push(Instruction::Copy(src, Operand::Variable(v.clone())));
                Ok(Operand::Variable(v))
            } else {
                bail!("Lvalue of Assignment must be a variable!");
            }
        }
        parser::Expression::CompoundAssignment(binary_op, var, right) => {
            if let parser::Expression::Variable(v) = *var {
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
        parser::Expression::PostIncr(expr) => {
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
        parser::Expression::PostDecr(expr) => {
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
    }
}

fn parse_statement(
    statement: parser::Statement,
    instructions: &mut Vec<Instruction>,
) -> Result<()> {
    match statement {
        parser::Statement::Return(expression) => {
            let dst = parse_expression(expression, instructions)?;
            instructions.push(Instruction::Ret(dst));
            Ok(())
        }
        parser::Statement::Expression(expression) => {
            parse_expression(expression, instructions)?;
            Ok(())
        }
        parser::Statement::Null => Ok(()),
    }
}

fn parse_declaration(decl: parser::Declaration, instructions: &mut Vec<Instruction>) -> Result<()> {
    match decl {
        parser::Declaration::Declaration(_, None) => Ok(()),
        parser::Declaration::Declaration(id, Some(expression)) => {
            let src = parse_expression(expression, instructions)?;
            instructions.push(Instruction::Copy(src, Operand::Variable(id)));
            Ok(())
        }
    }
}

fn parse_block_item(bl: parser::BlockItem, instructions: &mut Vec<Instruction>) -> Result<()> {
    match bl {
        parser::BlockItem::S(statement) => parse_statement(statement, instructions),
        parser::BlockItem::D(declaration) => parse_declaration(declaration, instructions),
    }
}

fn parse_function(fun: parser::Function) -> Result<Function> {
    let mut instructions = Vec::new();
    match fun {
        parser::Function::Function(name, body) => {
            for block in body {
                parse_block_item(block, &mut instructions)?;
            }
            instructions.push(Instruction::Ret(Operand::Constant(0)));
            Ok(Function::Function(name, instructions))
        }
    }
}

pub fn lift_to_ir(prog: parser::Ast) -> Result<TAC> {
    COUNTER_TMP.store(0, Ordering::SeqCst);
    COUNTER_LABEL.store(0, Ordering::SeqCst);
    match prog {
        parser::Ast::Program(fun) => Ok(TAC::Program(parse_function(fun)?)),
    }
}
