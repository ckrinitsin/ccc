use crate::parser;
use anyhow::Result;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Asm {
    Program(Function),
}

#[derive(Debug, PartialEq)]
pub enum Function {
    Function(String, Vec<Instruction>),
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand),
    Ret,
}

#[derive(Debug, PartialEq)]
pub enum Operand {
    Immediate(i64),
    Register,
}

impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Asm::Program(x) => write!(f, "{}\n  .section .note.GNU-stack,\"\",@progbits", x),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Function(name, body) => {
                write!(f, "  .globl {}\n{}:\n", name, name)?;
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
            Instruction::Mov(src, dst) => write!(f, "movl {}, {}", src, dst),
            Instruction::Ret => write!(f, "ret"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Immediate(x) => write!(f, "${}", x),
            Operand::Register => write!(f, "%eax"),
        }
    }
}

fn parse_operand(expr: parser::Expression) -> Result<Operand> {
    match expr {
        parser::Expression::Constant(c) => Ok(Operand::Immediate(c)),
    }
}

fn parse_instructions(statement: parser::Statement) -> Result<Vec<Instruction>> {
    match statement {
        parser::Statement::Return(expr) => Ok(vec![
            Instruction::Mov(parse_operand(expr)?, Operand::Register),
            Instruction::Ret,
        ]),
    }
}

fn parse_function(fun: parser::Function) -> Result<Function> {
    match fun {
        parser::Function::Function(name, body) => {
            Ok(Function::Function(name, parse_instructions(body)?))
        }
    }
}

pub fn parse_ast(prog: parser::Ast) -> Result<Asm> {
    match prog {
        parser::Ast::Program(fun) => Ok(Asm::Program(parse_function(fun)?)),
    }
}
