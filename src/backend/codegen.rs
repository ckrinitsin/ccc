use crate::frontend::ir;
use anyhow::{Result, bail};
use std::collections::HashMap;

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
    Unary(UnOp, Operand),
    Binary(BinOp, Operand, Operand),
    Idiv(Operand),
    Cdq,
    AllocStack(i32),
    Ret,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Immediate(i64),
    Register(Reg),
    Pseudo(String),
    Stack(i32),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Reg {
    RAX,
    RCX,
    CL,
    RDX,
    R10,
    R11,
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
    LShift,
    RShift,
}

fn parse_operand(expr: ir::Operand) -> Result<Operand> {
    match expr {
        ir::Operand::Constant(c) => Ok(Operand::Immediate(c)),
        ir::Operand::Variable(var) => Ok(Operand::Pseudo(var)),
    }
}

fn parse_unary(unary: ir::UnOp) -> Result<UnOp> {
    match unary {
        ir::UnOp::Complement => Ok(UnOp::Not),
        ir::UnOp::Negation => Ok(UnOp::Neg),
    }
}

fn parse_binary(binary: ir::BinOp) -> Result<BinOp> {
    match binary {
        ir::BinOp::Addition => Ok(BinOp::Add),
        ir::BinOp::Subtraction => Ok(BinOp::Sub),
        ir::BinOp::Multiplication => Ok(BinOp::Mul),
        ir::BinOp::And => Ok(BinOp::And),
        ir::BinOp::Or => Ok(BinOp::Or),
        ir::BinOp::Xor => Ok(BinOp::Xor),
        ir::BinOp::LShift => Ok(BinOp::LShift),
        ir::BinOp::RShift => Ok(BinOp::RShift),
        x => bail!("{} should be handled seperately", x),
    }
}

fn parse_instructions(instructions: Vec<ir::Instruction>) -> Result<Vec<Instruction>> {
    let mut result = Vec::new();
    for instr in instructions {
        match instr {
            ir::Instruction::Unary(un_op, src, dst) => {
                let dst = parse_operand(dst)?;
                result.push(Instruction::Mov(parse_operand(src)?, dst.clone()));
                result.push(Instruction::Unary(parse_unary(un_op)?, dst));
            }
            ir::Instruction::Ret(value) => {
                result.push(Instruction::Mov(
                    parse_operand(value)?,
                    Operand::Register(Reg::RAX),
                ));
                result.push(Instruction::Ret);
            }
            ir::Instruction::Binary(ir::BinOp::Division, src1, src2, dst) => {
                result.push(Instruction::Mov(
                    parse_operand(src1)?,
                    Operand::Register(Reg::RAX),
                ));
                result.push(Instruction::Cdq);
                result.push(Instruction::Idiv(parse_operand(src2)?));
                result.push(Instruction::Mov(
                    Operand::Register(Reg::RAX),
                    parse_operand(dst)?,
                ));
            }
            ir::Instruction::Binary(ir::BinOp::Modulo, src1, src2, dst) => {
                result.push(Instruction::Mov(
                    parse_operand(src1)?,
                    Operand::Register(Reg::RAX),
                ));
                result.push(Instruction::Cdq);
                result.push(Instruction::Idiv(parse_operand(src2)?));
                result.push(Instruction::Mov(
                    Operand::Register(Reg::RDX),
                    parse_operand(dst)?,
                ));
            }
            ir::Instruction::Binary(
                bin_op @ (ir::BinOp::LShift | ir::BinOp::RShift),
                src1,
                src2,
                dst,
            ) => {
                let dst = parse_operand(dst)?;
                result.push(Instruction::Mov(parse_operand(src1)?, dst.clone()));
                result.push(Instruction::Mov(
                    parse_operand(src2)?,
                    Operand::Register(Reg::RCX),
                ));
                result.push(Instruction::Binary(
                    parse_binary(bin_op)?,
                    Operand::Register(Reg::CL),
                    dst,
                ));
            }
            ir::Instruction::Binary(bin_op, src1, src2, dst) => {
                let dst = parse_operand(dst)?;
                result.push(Instruction::Mov(parse_operand(src1)?, dst.clone()));
                result.push(Instruction::Binary(
                    parse_binary(bin_op)?,
                    parse_operand(src2)?,
                    dst,
                ));
            }
        }
    }
    Ok(result)
}

fn parse_function(fun: ir::Function) -> Result<Function> {
    match fun {
        ir::Function::Function(name, body) => {
            Ok(Function::Function(name, parse_instructions(body)?))
        }
    }
}

fn replace_pseudo_operand(
    operand: Operand,
    hash_map: &mut HashMap<String, i32>,
) -> Result<Operand> {
    match operand {
        Operand::Pseudo(key) => match hash_map.get(&key) {
            Some(x) => Ok(Operand::Stack(*x)),
            None => {
                let n_val = match hash_map.iter().max_by_key(|entry| entry.1) {
                    Some(x) => x.1 + 4,
                    None => 4,
                };
                hash_map.insert(key, n_val);
                Ok(Operand::Stack(n_val))
            }
        },
        _ => Ok(operand),
    }
}

fn replace_pseudo(asm: Asm) -> Result<Asm> {
    let mut hash_map: HashMap<String, i32> = HashMap::new();
    let mut instr_pseudoless: Vec<Instruction> = Vec::new();
    let function_name;
    match asm {
        Asm::Program(function) => match function {
            Function::Function(name, instructions) => {
                function_name = name;
                for instr in instructions {
                    match instr {
                        Instruction::Mov(op1, op2) => {
                            let n_op1 = replace_pseudo_operand(op1, &mut hash_map)?;
                            let n_op2 = replace_pseudo_operand(op2, &mut hash_map)?;
                            instr_pseudoless.push(Instruction::Mov(n_op1, n_op2));
                        }
                        Instruction::Unary(un_op, op) => {
                            let n_op = replace_pseudo_operand(op, &mut hash_map)?;
                            instr_pseudoless.push(Instruction::Unary(un_op, n_op));
                        }
                        Instruction::Binary(bin_op, op1, op2) => {
                            let n_op1 = replace_pseudo_operand(op1, &mut hash_map)?;
                            let n_op2 = replace_pseudo_operand(op2, &mut hash_map)?;
                            instr_pseudoless.push(Instruction::Binary(bin_op, n_op1, n_op2));
                        }
                        Instruction::Idiv(op) => {
                            let n_op = replace_pseudo_operand(op, &mut hash_map)?;
                            instr_pseudoless.push(Instruction::Idiv(n_op));
                        }
                        x => instr_pseudoless.push(x),
                    }
                }
            }
        },
    }
    match hash_map.iter().max_by_key(|entry| entry.1) {
        Some(x) => instr_pseudoless.insert(0, Instruction::AllocStack(*x.1)),
        None => (),
    };
    Ok(Asm::Program(Function::Function(
        function_name,
        instr_pseudoless,
    )))
}

fn is_mem_access(op: &Operand) -> bool {
    match op {
        Operand::Stack(_) => true,
        _ => false,
    }
}

fn fix_mem_accesses(asm: Asm) -> Result<Asm> {
    let mut new_instr: Vec<Instruction> = Vec::new();
    let function_name;
    match asm {
        Asm::Program(function) => match function {
            Function::Function(name, instructions) => {
                function_name = name;
                for instr in instructions {
                    match instr {
                        Instruction::Mov(op1, op2) => {
                            if is_mem_access(&op1) && is_mem_access(&op2) {
                                new_instr.push(Instruction::Mov(op1, Operand::Register(Reg::R10)));
                                new_instr.push(Instruction::Mov(Operand::Register(Reg::R10), op2));
                            } else {
                                new_instr.push(Instruction::Mov(op1, op2));
                            }
                        }
                        Instruction::Idiv(Operand::Immediate(c)) => {
                            new_instr.push(Instruction::Mov(
                                Operand::Immediate(c),
                                Operand::Register(Reg::R10),
                            ));
                            new_instr.push(Instruction::Idiv(Operand::Register(Reg::R10)));
                        }
                        Instruction::Binary(BinOp::Mul, op1, op2) => {
                            if is_mem_access(&op2) {
                                new_instr.push(Instruction::Mov(
                                    op2.clone(),
                                    Operand::Register(Reg::R11),
                                ));
                                new_instr.push(Instruction::Binary(
                                    BinOp::Mul,
                                    op1,
                                    Operand::Register(Reg::R11),
                                ));
                                new_instr.push(Instruction::Mov(Operand::Register(Reg::R11), op2));
                            }
                        }
                        Instruction::Binary(bin_op, op1, op2) => {
                            if is_mem_access(&op1) && is_mem_access(&op2) {
                                new_instr.push(Instruction::Mov(op1, Operand::Register(Reg::R10)));
                                new_instr.push(Instruction::Binary(
                                    bin_op,
                                    Operand::Register(Reg::R10),
                                    op2,
                                ));
                            } else {
                                new_instr.push(Instruction::Binary(bin_op, op1, op2));
                            }
                        }
                        x => new_instr.push(x),
                    }
                }
            }
        },
    }
    Ok(Asm::Program(Function::Function(function_name, new_instr)))
}

pub fn gen_asm(prog: ir::TAC) -> Result<Asm> {
    let mut asm = match prog {
        ir::TAC::Program(fun) => Asm::Program(parse_function(fun)?),
    };

    asm = replace_pseudo(asm)?;

    asm = fix_mem_accesses(asm)?;

    Ok(asm)
}
