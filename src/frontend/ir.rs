use anyhow::Result;
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
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
}

impl fmt::Display for TAC {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TAC::Program(x) => write!(f, "{}\n", x),
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
        }
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gen_temp() -> Operand {
    let counter = COUNTER.fetch_add(1, Ordering::SeqCst);
    Operand::Variable("tmp.".to_owned() + &counter.to_string())
}

fn parse_unary_op(expr: parser::UnaryOp) -> Result<UnOp> {
    match expr {
        parser::UnaryOp::Complement => Ok(UnOp::Complement),
        parser::UnaryOp::Negation => Ok(UnOp::Negation),
    }
}

fn parse_binary_op(expr: parser::BinaryOp) -> Result<BinOp> {
    match expr {
        parser::BinaryOp::Addition => Ok(BinOp::Addition),
        parser::BinaryOp::Subtraction => Ok(BinOp::Subtraction),
        parser::BinaryOp::Multiplication => Ok(BinOp::Multiplication),
        parser::BinaryOp::Division => Ok(BinOp::Division),
        parser::BinaryOp::Modulo => Ok(BinOp::Modulo),
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
            let dst = gen_temp();
            let op = parse_unary_op(unary_op)?;

            instructions.push(Instruction::Unary(op, src, dst.clone()));

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
    }
}

fn parse_statement(statement: parser::Statement) -> Result<Vec<Instruction>> {
    let mut instructions = Vec::new();
    match statement {
        parser::Statement::Return(expression) => {
            let dst = parse_expression(expression, &mut instructions)?;
            instructions.push(Instruction::Ret(dst));
            Ok(instructions)
        }
    }
}

fn parse_function(fun: parser::Function) -> Result<Function> {
    match fun {
        parser::Function::Function(name, body) => {
            Ok(Function::Function(name, parse_statement(body)?))
        }
    }
}

pub fn lift_to_ir(prog: parser::Ast) -> Result<TAC> {
    COUNTER.store(0, Ordering::SeqCst);
    match prog {
        parser::Ast::Program(fun) => Ok(TAC::Program(parse_function(fun)?)),
    }
}
