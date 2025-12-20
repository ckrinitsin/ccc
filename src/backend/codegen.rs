use crate::frontend::ir;
use anyhow::Result;
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
    R10,
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Neg,
    Not,
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
    let mut instr_pseudoless: Vec<Instruction> = Vec::new();
    let function_name;
    match asm {
        Asm::Program(function) => match function {
            Function::Function(name, instructions) => {
                function_name = name;
                for instr in instructions {
                    match instr {
                        Instruction::Mov(op1, op2) => {
                            if is_mem_access(&op1) && is_mem_access(&op2) {
                                instr_pseudoless
                                    .push(Instruction::Mov(op1, Operand::Register(Reg::R10)));
                                instr_pseudoless
                                    .push(Instruction::Mov(Operand::Register(Reg::R10), op2));
                            } else {
                                instr_pseudoless.push(Instruction::Mov(op1, op2));
                            }
                        }
                        x => instr_pseudoless.push(x),
                    }
                }
            }
        },
    }
    Ok(Asm::Program(Function::Function(
        function_name,
        instr_pseudoless,
    )))
}

pub fn gen_asm(prog: ir::TAC) -> Result<Asm> {
    let mut asm = match prog {
        ir::TAC::Program(fun) => Asm::Program(parse_function(fun)?),
    };

    asm = replace_pseudo(asm)?;

    asm = fix_mem_accesses(asm)?;

    Ok(asm)
}
