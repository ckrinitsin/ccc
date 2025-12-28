use crate::frontend::ir;
use crate::frontend::type_check::{IdentifierAttributes, Type};
use anyhow::{Result, bail};
use std::cmp::min;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Asm {
    Program(Vec<TopLevel>),
}

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Function(String, bool, Vec<Instruction>),
    StaticVariable(String, bool, i32),
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand),
    Unary(UnOp, Operand),
    Binary(BinOp, Operand, Operand),
    Cmp(Operand, Operand),
    Idiv(Operand),
    Cdq,
    Jump(String),
    JmpCC(Condition, String),
    SetCC(Condition, Operand),
    Label(String),
    AllocStack(i32),
    DeallocStack(i32),
    Push(Operand),
    Call(String),
    Ret,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Immediate(i32),
    Register(Reg),
    Pseudo(String),
    Stack(i32),
    Data(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Reg {
    RAX,
    RCX,
    RDX,
    RDI,
    RSI,
    R8,
    R9,
    R10,
    R11,
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Neg,
    Not,
    Dec,
    Inc,
}

#[derive(Debug, PartialEq)]
pub enum Condition {
    E,
    NE,
    G,
    GE,
    L,
    LE,
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

fn parse_condition(bin_op: ir::BinOp) -> Result<Condition> {
    match bin_op {
        ir::BinOp::Equal => Ok(Condition::E),
        ir::BinOp::NEqual => Ok(Condition::NE),
        ir::BinOp::Less => Ok(Condition::L),
        ir::BinOp::Greater => Ok(Condition::G),
        ir::BinOp::LessEq => Ok(Condition::LE),
        ir::BinOp::GreaterEq => Ok(Condition::GE),
        x => bail!("{} doesn't produce a condition", x),
    }
}

fn parse_unary(unary: ir::UnOp) -> Result<UnOp> {
    match unary {
        ir::UnOp::Complement => Ok(UnOp::Not),
        ir::UnOp::Negation => Ok(UnOp::Neg),
        ir::UnOp::Increment => Ok(UnOp::Inc),
        ir::UnOp::Decrement => Ok(UnOp::Dec),
        ir::UnOp::Not => bail!("! should be handled seperately"),
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

fn parse_function_call(
    name: String,
    args: Vec<ir::Operand>,
    ret: ir::Operand,
    instructions: &mut Vec<Instruction>,
) -> Result<()> {
    let reg_args_mapping = vec![Reg::RDI, Reg::RSI, Reg::RDX, Reg::RCX, Reg::R8, Reg::R9];
    let reg_args = &args[..min(6, args.len())];
    let stack_args = &args[min(6, args.len())..];
    let stack_padding = if stack_args.len() % 2 == 1 {
        instructions.push(Instruction::AllocStack(8));
        8
    } else {
        0
    };

    let mut index = 0;
    for arg in reg_args {
        let reg = reg_args_mapping[index].clone();
        let asm_arg = parse_operand(arg.clone())?;
        instructions.push(Instruction::Mov(asm_arg, Operand::Register(reg)));
        index += 1;
    }

    for arg in stack_args.into_iter().rev() {
        let asm_arg = parse_operand(arg.clone())?;
        match asm_arg {
            Operand::Immediate(_) | Operand::Register(_) => {
                instructions.push(Instruction::Push(asm_arg))
            }
            _ => {
                instructions.push(Instruction::Mov(asm_arg, Operand::Register(Reg::RAX)));
                instructions.push(Instruction::Push(Operand::Register(Reg::RAX)))
            }
        }
    }

    instructions.push(Instruction::Call(name));
    instructions.push(Instruction::DeallocStack(
        (8 * stack_args.len() + stack_padding) as i32,
    ));

    let ret = parse_operand(ret)?;
    instructions.push(Instruction::Mov(Operand::Register(Reg::RAX), ret));
    Ok(())
}

fn parse_instructions(instructions: Vec<ir::Instruction>) -> Result<Vec<Instruction>> {
    let mut result = Vec::new();
    for instr in instructions {
        match instr {
            ir::Instruction::Unary(ir::UnOp::Not, src, dst) => {
                let dst = parse_operand(dst)?;
                result.push(Instruction::Cmp(Operand::Immediate(0), parse_operand(src)?));
                result.push(Instruction::Mov(Operand::Immediate(0), dst.clone()));
                result.push(Instruction::SetCC(Condition::E, dst));
            }
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
                    Operand::Register(Reg::RCX),
                    dst,
                ));
            }
            ir::Instruction::Binary(
                bin_op @ (ir::BinOp::Equal
                | ir::BinOp::NEqual
                | ir::BinOp::Less
                | ir::BinOp::LessEq
                | ir::BinOp::Greater
                | ir::BinOp::GreaterEq),
                src1,
                src2,
                dst,
            ) => {
                let dst = parse_operand(dst)?;
                result.push(Instruction::Cmp(parse_operand(src2)?, parse_operand(src1)?));
                result.push(Instruction::Mov(Operand::Immediate(0), dst.clone()));
                result.push(Instruction::SetCC(parse_condition(bin_op)?, dst));
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
            ir::Instruction::Copy(src, dst) => {
                result.push(Instruction::Mov(parse_operand(src)?, parse_operand(dst)?))
            }
            ir::Instruction::Jump(label) => result.push(Instruction::Jump(label)),
            ir::Instruction::JumpIfZero(operand, label) => {
                result.push(Instruction::Cmp(
                    Operand::Immediate(0),
                    parse_operand(operand)?,
                ));
                result.push(Instruction::JmpCC(Condition::E, label));
            }
            ir::Instruction::JumpIfNotZero(operand, label) => {
                result.push(Instruction::Cmp(
                    Operand::Immediate(0),
                    parse_operand(operand)?,
                ));
                result.push(Instruction::JmpCC(Condition::NE, label));
            }
            ir::Instruction::Label(label) => result.push(Instruction::Label(label)),
            ir::Instruction::FunctionCall(name, params, ret) => {
                parse_function_call(name, params, ret, &mut result)?
            }
        }
    }
    Ok(result)
}

fn parse_function(fun: ir::TopLevel) -> Result<TopLevel> {
    match fun {
        ir::TopLevel::Function(name, global, params, body) => {
            let reg_args_mapping = vec![Reg::RDI, Reg::RSI, Reg::RDX, Reg::RCX, Reg::R8, Reg::R9];
            let mut instructions: Vec<Instruction> = Vec::new();
            let mut index = 0;
            for param in params {
                if index == reg_args_mapping.len() {
                    index = 16;
                }

                if index < reg_args_mapping.len() {
                    instructions.push(Instruction::Mov(
                        Operand::Register(reg_args_mapping[index].clone()),
                        Operand::Pseudo(param),
                    ));
                    index += 1;
                } else if index > reg_args_mapping.len() {
                    instructions.push(Instruction::Mov(
                        Operand::Stack(index as i32),
                        Operand::Pseudo(param),
                    ));
                    index += 8;
                }
            }

            instructions.append(&mut parse_instructions(body)?);
            Ok(TopLevel::Function(name, global, instructions))
        }
        ir::TopLevel::StaticVariable(name, global, init) => {
            Ok(TopLevel::StaticVariable(name, global, init))
        }
    }
}

fn replace_pseudo_operand(
    operand: Operand,
    hash_map: &mut HashMap<String, i32>,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Operand> {
    match operand {
        Operand::Pseudo(key) => match hash_map.get(&key) {
            Some(x) => Ok(Operand::Stack(*x)),
            None => {
                if let Some((_, symb_attr)) = symbol_table.get(&key) {
                    match symb_attr {
                        IdentifierAttributes::StaticAttributes(_, _) => {
                            return Ok(Operand::Data(key));
                        }
                        _ => (),
                    };
                }

                let n_val = match hash_map.iter().min_by_key(|entry| entry.1) {
                    Some(x) => x.1 - 4,
                    None => -4,
                };
                hash_map.insert(key, n_val);
                Ok(Operand::Stack(n_val))
            }
        },
        _ => Ok(operand),
    }
}

fn replace_pseudo(
    asm: Asm,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Asm> {
    let mut new_instructions = Vec::new();
    match asm {
        Asm::Program(functions) => {
            for function in functions {
                let mut instr_pseudoless: Vec<Instruction> = Vec::new();
                let mut hash_map: HashMap<String, i32> = HashMap::new();
                match function {
                    TopLevel::Function(name, global, instructions) => {
                        for instr in instructions {
                            match instr {
                                Instruction::Mov(op1, op2) => {
                                    let n_op1 =
                                        replace_pseudo_operand(op1, &mut hash_map, symbol_table)?;
                                    let n_op2 =
                                        replace_pseudo_operand(op2, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::Mov(n_op1, n_op2));
                                }
                                Instruction::Cmp(op1, op2) => {
                                    let n_op1 =
                                        replace_pseudo_operand(op1, &mut hash_map, symbol_table)?;
                                    let n_op2 =
                                        replace_pseudo_operand(op2, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::Cmp(n_op1, n_op2));
                                }
                                Instruction::Unary(un_op, op) => {
                                    let n_op =
                                        replace_pseudo_operand(op, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::Unary(un_op, n_op));
                                }
                                Instruction::SetCC(cond, op) => {
                                    let n_op =
                                        replace_pseudo_operand(op, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::SetCC(cond, n_op));
                                }
                                Instruction::Binary(bin_op, op1, op2) => {
                                    let n_op1 =
                                        replace_pseudo_operand(op1, &mut hash_map, symbol_table)?;
                                    let n_op2 =
                                        replace_pseudo_operand(op2, &mut hash_map, symbol_table)?;
                                    instr_pseudoless
                                        .push(Instruction::Binary(bin_op, n_op1, n_op2));
                                }
                                Instruction::Idiv(op) => {
                                    let n_op =
                                        replace_pseudo_operand(op, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::Idiv(n_op));
                                }
                                Instruction::Push(op) => {
                                    let n_op =
                                        replace_pseudo_operand(op, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::Push(n_op));
                                }
                                x => instr_pseudoless.push(x),
                            }
                        }

                        match hash_map.iter().min_by_key(|entry| entry.1) {
                            Some(x) => {
                                let padded = (-*x.1 + 15) / 16 * 16;
                                instr_pseudoless.insert(0, Instruction::AllocStack(padded));
                            }
                            None => (),
                        };
                        new_instructions.push(TopLevel::Function(name, global, instr_pseudoless));
                    }
                    static_variable => new_instructions.push(static_variable),
                }
            }
        }
    }
    Ok(Asm::Program(new_instructions))
}

fn is_mem_access(op: &Operand) -> bool {
    match op {
        Operand::Stack(_) | Operand::Data(_) => true,
        _ => false,
    }
}

fn fix_mem_accesses(asm: Asm) -> Result<Asm> {
    let mut new_functions: Vec<TopLevel> = Vec::new();
    match asm {
        Asm::Program(functions) => {
            for function in functions {
                let mut new_instr: Vec<Instruction> = Vec::new();
                match function {
                    TopLevel::Function(name, global, instructions) => {
                        for instr in instructions {
                            match instr {
                                Instruction::Mov(op1, op2) => {
                                    if is_mem_access(&op1) && is_mem_access(&op2) {
                                        new_instr.push(Instruction::Mov(
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        new_instr.push(Instruction::Mov(
                                            Operand::Register(Reg::R10),
                                            op2,
                                        ));
                                    } else {
                                        new_instr.push(Instruction::Mov(op1, op2));
                                    }
                                }
                                Instruction::Cmp(op1, op2) => {
                                    if is_mem_access(&op1) && is_mem_access(&op2) {
                                        new_instr.push(Instruction::Mov(
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        new_instr.push(Instruction::Cmp(
                                            Operand::Register(Reg::R10),
                                            op2,
                                        ));
                                    } else {
                                        match op2 {
                                            Operand::Immediate(c) => {
                                                new_instr.push(Instruction::Mov(
                                                    Operand::Immediate(c),
                                                    Operand::Register(Reg::R11),
                                                ));
                                                new_instr.push(Instruction::Cmp(
                                                    op1,
                                                    Operand::Register(Reg::R11),
                                                ));
                                            }
                                            _ => new_instr.push(Instruction::Cmp(op1, op2)),
                                        }
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
                                        new_instr.push(Instruction::Mov(
                                            Operand::Register(Reg::R11),
                                            op2,
                                        ));
                                    }
                                }
                                Instruction::Binary(bin_op, op1, op2) => {
                                    if is_mem_access(&op1) && is_mem_access(&op2) {
                                        new_instr.push(Instruction::Mov(
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
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
                        new_functions.push(TopLevel::Function(name, global, new_instr));
                    }
                    static_variable => new_functions.push(static_variable),
                }
            }
        }
    }
    Ok(Asm::Program(new_functions))
}

pub fn gen_asm(
    prog: ir::TAC,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Asm> {
    let mut asm = match prog {
        ir::TAC::Program(fun) => {
            let mut functions = Vec::new();
            for f in fun {
                functions.push(parse_function(f)?);
            }
            Asm::Program(functions)
        }
    };

    asm = replace_pseudo(asm, symbol_table)?;

    asm = fix_mem_accesses(asm)?;

    Ok(asm)
}
