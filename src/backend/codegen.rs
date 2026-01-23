use crate::frontend::ast::Type;
use crate::frontend::ir;
use crate::frontend::semantic_analysis::type_check::{IdentifierAttributes, StaticInit};
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
    StaticVariable(String, bool, i32, StaticInit),
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Mov(AssemblyType, Operand, Operand),
    Movsx(Operand, Operand),
    Unary(UnOp, AssemblyType, Operand),
    Binary(BinOp, AssemblyType, Operand, Operand),
    Cmp(AssemblyType, Operand, Operand),
    Idiv(AssemblyType, Operand),
    Cdq(AssemblyType),
    Jump(String),
    JmpCC(Condition, String),
    SetCC(Condition, Operand),
    Label(String),
    Push(Operand),
    Call(String),
    Ret,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Immediate(i64),
    Register(Reg),
    Pseudo(String),
    Stack(i64),
    Data(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssemblyType {
    Longword,
    Quadword,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Reg {
    RAX,
    RCX,
    RDX,
    RDI,
    RSI,
    RSP,
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

fn operand_to_asm_type(
    operand: &ir::Operand,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<AssemblyType> {
    match operand {
        ir::Operand::Constant(c) => match c {
            crate::frontend::ast::Const::Int(_) => Ok(AssemblyType::Longword),
            crate::frontend::ast::Const::Long(_) => Ok(AssemblyType::Quadword),
        },
        ir::Operand::Variable(var) => match symbol_table.get(var) {
            Some(x) => match x.0 {
                Type::Int => Ok(AssemblyType::Longword),
                Type::Long => Ok(AssemblyType::Quadword),
                Type::Function(_, _) => bail!("{} is no variable", var),
            },
            None => bail!("Variable {} not found in symbol table", var),
        },
    }
}

// TODO: refactor symb rtable util function
fn get_asm_type_from_symbol_table(
    name: &String,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<AssemblyType> {
    match symbol_table.get(name) {
        Some((var_type, _)) => match var_type {
            Type::Int => Ok(AssemblyType::Longword),
            Type::Long => Ok(AssemblyType::Quadword),
            Type::Function(_, _) => bail!("{} is no variable", name),
        },
        None => bail!("Could not find {} in symbol table", name),
    }
}

fn parse_operand(expr: ir::Operand) -> Result<Operand> {
    match expr {
        ir::Operand::Constant(c) => match c {
            crate::frontend::ast::Const::Int(i) => Ok(Operand::Immediate(i as i64)),
            crate::frontend::ast::Const::Long(l) => Ok(Operand::Immediate(l)),
        },
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
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<()> {
    let reg_args_mapping = vec![Reg::RDI, Reg::RSI, Reg::RDX, Reg::RCX, Reg::R8, Reg::R9];
    let reg_args = &args[..min(6, args.len())];
    let stack_args = &args[min(6, args.len())..];
    let stack_padding = if stack_args.len() % 2 == 1 {
        instructions.push(Instruction::Binary(
            BinOp::Sub,
            AssemblyType::Quadword,
            Operand::Immediate(8),
            Operand::Register(Reg::RSP),
        ));
        8
    } else {
        0
    };

    let mut index = 0;
    for arg in reg_args {
        let reg = reg_args_mapping[index].clone();
        let asm_arg = parse_operand(arg.clone())?;
        instructions.push(Instruction::Mov(
            operand_to_asm_type(arg, symbol_table)?,
            asm_arg,
            Operand::Register(reg),
        ));
        index += 1;
    }

    for arg in stack_args.into_iter().rev() {
        let asm_arg = parse_operand(arg.clone())?;
        let asm_type = operand_to_asm_type(arg, symbol_table)?;
        match asm_arg {
            Operand::Immediate(_) | Operand::Register(_) => {
                instructions.push(Instruction::Push(asm_arg))
            }
            _ => {
                if asm_type == AssemblyType::Quadword {
                    instructions.push(Instruction::Push(asm_arg))
                } else {
                    instructions.push(Instruction::Mov(
                        AssemblyType::Longword,
                        asm_arg,
                        Operand::Register(Reg::RAX),
                    ));
                    instructions.push(Instruction::Push(Operand::Register(Reg::RAX)))
                }
            }
        }
    }

    instructions.push(Instruction::Call(name));
    let rsp_restore_bytes = (8 * stack_args.len() + stack_padding) as i64;
    if rsp_restore_bytes != 0 {
        instructions.push(Instruction::Binary(
            BinOp::Add,
            AssemblyType::Quadword,
            Operand::Immediate(rsp_restore_bytes),
            Operand::Register(Reg::RSP),
        ));
    }

    let ret_type = operand_to_asm_type(&ret, symbol_table)?;
    let ret = parse_operand(ret)?;
    instructions.push(Instruction::Mov(ret_type, Operand::Register(Reg::RAX), ret));
    Ok(())
}

fn parse_instructions(
    instructions: Vec<ir::Instruction>,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Vec<Instruction>> {
    let mut result: Vec<Instruction> = Vec::new();
    for instr in instructions {
        match instr {
            ir::Instruction::Unary(ir::UnOp::Not, src, dst) => {
                let asm_type = operand_to_asm_type(&src, symbol_table)?;
                let dst = parse_operand(dst)?;
                result.push(Instruction::Cmp(
                    asm_type.clone(),
                    Operand::Immediate(0),
                    parse_operand(src)?,
                ));
                result.push(Instruction::Mov(
                    asm_type,
                    Operand::Immediate(0),
                    dst.clone(),
                ));
                result.push(Instruction::SetCC(Condition::E, dst));
            }
            ir::Instruction::Unary(un_op, src, dst) => {
                let asm_type = operand_to_asm_type(&src, symbol_table)?;
                let dst = parse_operand(dst)?;
                result.push(Instruction::Mov(
                    asm_type.clone(),
                    parse_operand(src)?,
                    dst.clone(),
                ));
                result.push(Instruction::Unary(parse_unary(un_op)?, asm_type, dst));
            }
            ir::Instruction::Ret(op) => {
                let asm_type = operand_to_asm_type(&op, symbol_table)?;
                result.push(Instruction::Mov(
                    asm_type,
                    parse_operand(op)?,
                    Operand::Register(Reg::RAX),
                ));
                result.push(Instruction::Ret);
            }
            ir::Instruction::Binary(ir::BinOp::Division, src1, src2, dst) => {
                let asm_type = operand_to_asm_type(&src1, symbol_table)?;
                result.push(Instruction::Mov(
                    asm_type.clone(),
                    parse_operand(src1)?,
                    Operand::Register(Reg::RAX),
                ));
                result.push(Instruction::Cdq(asm_type.clone()));
                result.push(Instruction::Idiv(asm_type.clone(), parse_operand(src2)?));
                result.push(Instruction::Mov(
                    asm_type,
                    Operand::Register(Reg::RAX),
                    parse_operand(dst)?,
                ));
            }
            ir::Instruction::Binary(ir::BinOp::Modulo, src1, src2, dst) => {
                let asm_type = operand_to_asm_type(&src1, symbol_table)?;
                result.push(Instruction::Mov(
                    asm_type.clone(),
                    parse_operand(src1)?,
                    Operand::Register(Reg::RAX),
                ));
                result.push(Instruction::Cdq(asm_type.clone()));
                result.push(Instruction::Idiv(asm_type.clone(), parse_operand(src2)?));
                result.push(Instruction::Mov(
                    asm_type,
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
                let asm_type = operand_to_asm_type(&src1, symbol_table)?;
                let dst = parse_operand(dst)?;
                result.push(Instruction::Mov(
                    asm_type.clone(),
                    parse_operand(src1)?,
                    dst.clone(),
                ));
                result.push(Instruction::Mov(
                    asm_type.clone(),
                    parse_operand(src2)?,
                    Operand::Register(Reg::RCX),
                ));
                result.push(Instruction::Binary(
                    parse_binary(bin_op)?,
                    asm_type,
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
                let asm_type = operand_to_asm_type(&src1, symbol_table)?;
                let dst = parse_operand(dst)?;
                result.push(Instruction::Cmp(
                    asm_type.clone(),
                    parse_operand(src2)?,
                    parse_operand(src1)?,
                ));
                result.push(Instruction::Mov(
                    asm_type.clone(),
                    Operand::Immediate(0),
                    dst.clone(),
                ));
                result.push(Instruction::SetCC(parse_condition(bin_op)?, dst));
            }
            ir::Instruction::Binary(bin_op, src1, src2, dst) => {
                let asm_type = operand_to_asm_type(&src1, symbol_table)?;
                let dst = parse_operand(dst)?;
                result.push(Instruction::Mov(
                    asm_type.clone(),
                    parse_operand(src1)?,
                    dst.clone(),
                ));
                result.push(Instruction::Binary(
                    parse_binary(bin_op)?,
                    asm_type,
                    parse_operand(src2)?,
                    dst,
                ));
            }
            ir::Instruction::Copy(src, dst) => {
                let asm_type = operand_to_asm_type(&src, symbol_table)?;
                result.push(Instruction::Mov(
                    asm_type,
                    parse_operand(src)?,
                    parse_operand(dst)?,
                ))
            }
            ir::Instruction::Jump(label) => result.push(Instruction::Jump(label)),
            ir::Instruction::JumpIfZero(operand, label) => {
                let asm_type = operand_to_asm_type(&operand, symbol_table)?;
                result.push(Instruction::Cmp(
                    asm_type.clone(),
                    Operand::Immediate(0),
                    parse_operand(operand)?,
                ));
                result.push(Instruction::JmpCC(Condition::E, label));
            }
            ir::Instruction::JumpIfNotZero(operand, label) => {
                let asm_type = operand_to_asm_type(&operand, symbol_table)?;
                result.push(Instruction::Cmp(
                    asm_type.clone(),
                    Operand::Immediate(0),
                    parse_operand(operand)?,
                ));
                result.push(Instruction::JmpCC(Condition::NE, label));
            }
            ir::Instruction::Label(label) => result.push(Instruction::Label(label)),
            ir::Instruction::FunctionCall(name, params, ret) => {
                parse_function_call(name, params, ret, &mut result, symbol_table)?
            }
            ir::Instruction::SignExtend(src, dst) => {
                result.push(Instruction::Movsx(parse_operand(src)?, parse_operand(dst)?));
            }
            ir::Instruction::Truncate(src, dst) => {
                result.push(Instruction::Mov(
                    AssemblyType::Longword,
                    parse_operand(src)?,
                    parse_operand(dst)?,
                ));
            }
        }
    }
    Ok(result)
}

fn parse_function(
    fun: ir::TopLevel,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<TopLevel> {
    match fun {
        ir::TopLevel::Function(name, global, params, body) => {
            let reg_args_mapping = vec![Reg::RDI, Reg::RSI, Reg::RDX, Reg::RCX, Reg::R8, Reg::R9];
            let mut instructions: Vec<Instruction> = Vec::new();
            let mut index = 0;
            for param in params {
                let asm_type = get_asm_type_from_symbol_table(&param, symbol_table)?;
                if index == reg_args_mapping.len() {
                    index = 16;
                }

                if index < reg_args_mapping.len() {
                    instructions.push(Instruction::Mov(
                        asm_type,
                        Operand::Register(reg_args_mapping[index].clone()),
                        Operand::Pseudo(param),
                    ));
                    index += 1;
                } else if index > reg_args_mapping.len() {
                    instructions.push(Instruction::Mov(
                        asm_type,
                        Operand::Stack(index as i64),
                        Operand::Pseudo(param),
                    ));
                    index += 8;
                }
            }

            instructions.append(&mut parse_instructions(body, symbol_table)?);
            Ok(TopLevel::Function(name, global, instructions))
        }
        ir::TopLevel::StaticVariable(name, global, var_type, init) => match var_type {
            Type::Int => Ok(TopLevel::StaticVariable(name, global, 4, init)),
            Type::Long => Ok(TopLevel::StaticVariable(name, global, 8, init)),
            Type::Function(_, _) => bail!("Found toplevel function"),
        },
    }
}

fn replace_pseudo_operand(
    operand: Operand,
    hash_map: &mut HashMap<String, i64>,
    symbol_table: &HashMap<String, (Type, IdentifierAttributes)>,
) -> Result<Operand> {
    match operand {
        Operand::Pseudo(key) => match hash_map.get(&key) {
            Some(x) => Ok(Operand::Stack(*x as i64)),
            None => {
                let mut size_on_stack: i64 = 0;
                if let Some((var_type, symb_attr)) = symbol_table.get(&key) {
                    match symb_attr {
                        IdentifierAttributes::StaticAttributes(_, _) => {
                            return Ok(Operand::Data(key));
                        }
                        _ => (),
                    };
                    match var_type {
                        Type::Int => size_on_stack = 4,
                        Type::Long => size_on_stack = 8,
                        Type::Function(_, _) => bail!("Function not allowed here"),
                    };
                }

                let n_val = match hash_map.iter().min_by_key(|entry| entry.1) {
                    Some(x) => (x.1 - size_on_stack) - (8 + ((x.1 - size_on_stack) % 8)),
                    None => -8,
                };
                hash_map.insert(key, n_val);
                Ok(Operand::Stack(n_val as i64))
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
                let mut hash_map: HashMap<String, i64> = HashMap::new();
                match function {
                    TopLevel::Function(name, global, instructions) => {
                        for instr in instructions {
                            match instr {
                                Instruction::Mov(asm_type, op1, op2) => {
                                    let n_op1 =
                                        replace_pseudo_operand(op1, &mut hash_map, symbol_table)?;
                                    let n_op2 =
                                        replace_pseudo_operand(op2, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::Mov(asm_type, n_op1, n_op2));
                                }
                                Instruction::Movsx(op1, op2) => {
                                    let n_op1 =
                                        replace_pseudo_operand(op1, &mut hash_map, symbol_table)?;
                                    let n_op2 =
                                        replace_pseudo_operand(op2, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::Movsx(n_op1, n_op2));
                                }
                                Instruction::Cmp(asm_type, op1, op2) => {
                                    let n_op1 =
                                        replace_pseudo_operand(op1, &mut hash_map, symbol_table)?;
                                    let n_op2 =
                                        replace_pseudo_operand(op2, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::Cmp(asm_type, n_op1, n_op2));
                                }
                                Instruction::Unary(asm_type, un_op, op) => {
                                    let n_op =
                                        replace_pseudo_operand(op, &mut hash_map, symbol_table)?;
                                    instr_pseudoless
                                        .push(Instruction::Unary(asm_type, un_op, n_op));
                                }
                                Instruction::SetCC(cond, op) => {
                                    let n_op =
                                        replace_pseudo_operand(op, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::SetCC(cond, n_op));
                                }
                                Instruction::Binary(asm_type, bin_op, op1, op2) => {
                                    let n_op1 =
                                        replace_pseudo_operand(op1, &mut hash_map, symbol_table)?;
                                    let n_op2 =
                                        replace_pseudo_operand(op2, &mut hash_map, symbol_table)?;
                                    instr_pseudoless
                                        .push(Instruction::Binary(asm_type, bin_op, n_op1, n_op2));
                                }
                                Instruction::Idiv(asm_type, op) => {
                                    let n_op =
                                        replace_pseudo_operand(op, &mut hash_map, symbol_table)?;
                                    instr_pseudoless.push(Instruction::Idiv(asm_type, n_op));
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
                                instr_pseudoless.insert(
                                    0,
                                    Instruction::Binary(
                                        BinOp::Sub,
                                        AssemblyType::Quadword,
                                        Operand::Immediate(padded),
                                        Operand::Register(Reg::RSP),
                                    ),
                                );
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

fn is_immediate(op: &Operand) -> bool {
    match op {
        Operand::Immediate(_) => true,
        _ => false,
    }
}

fn is_not_int_range(op: &Operand) -> bool {
    match op {
        Operand::Immediate(val) => {
            if *val <= 2_i64.pow(31) - 1 && *val >= -2_i64.pow(31) {
                return false;
            }
            true
        }
        Operand::Register(_) | Operand::Pseudo(_) | Operand::Stack(_) | Operand::Data(_) => false,
    }
}

// TODO: group cases into functions (i.e. both_operands_are_mem_accesses)
// for better overview
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
                                Instruction::Mov(asm_type, op1, op2) => {
                                    let op1 = if asm_type == AssemblyType::Quadword
                                        && is_not_int_range(&op1)
                                        && is_mem_access(&op2)
                                    {
                                        new_instr.push(Instruction::Mov(
                                            AssemblyType::Quadword,
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        Operand::Register(Reg::R10)
                                    } else {
                                        op1
                                    };
                                    let op1 = if asm_type == AssemblyType::Longword
                                        && !is_not_int_range(&op1)
                                    {
                                        match op1 {
                                            Operand::Immediate(x) => {
                                                Operand::Immediate(x % 2_i64.pow(32))
                                            }
                                            _ => op1,
                                        }
                                    } else {
                                        op1
                                    };
                                    if is_mem_access(&op1) && is_mem_access(&op2) {
                                        new_instr.push(Instruction::Mov(
                                            asm_type.clone(),
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        new_instr.push(Instruction::Mov(
                                            asm_type,
                                            Operand::Register(Reg::R10),
                                            op2,
                                        ));
                                    } else {
                                        new_instr.push(Instruction::Mov(asm_type, op1, op2));
                                    }
                                }
                                Instruction::Movsx(op1, op2) => {
                                    let new_op1 = if is_immediate(&op1) {
                                        new_instr.push(Instruction::Mov(
                                            AssemblyType::Longword,
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        Operand::Register(Reg::R10)
                                    } else {
                                        op1
                                    };
                                    if is_mem_access(&op2) {
                                        new_instr.push(Instruction::Movsx(
                                            new_op1,
                                            Operand::Register(Reg::R11),
                                        ));
                                        new_instr.push(Instruction::Mov(
                                            AssemblyType::Quadword,
                                            Operand::Register(Reg::R11),
                                            op2,
                                        ));
                                    } else {
                                        new_instr.push(Instruction::Movsx(new_op1, op2));
                                    }
                                }
                                Instruction::Push(op) => {
                                    let op = if is_not_int_range(&op) {
                                        new_instr.push(Instruction::Mov(
                                            AssemblyType::Quadword,
                                            op,
                                            Operand::Register(Reg::R10),
                                        ));
                                        Operand::Register(Reg::R10)
                                    } else {
                                        op
                                    };
                                    new_instr.push(Instruction::Push(op));
                                }
                                Instruction::Cmp(asm_type, op1, op2) => {
                                    let op1 = if asm_type == AssemblyType::Quadword
                                        && is_not_int_range(&op1)
                                    {
                                        new_instr.push(Instruction::Mov(
                                            AssemblyType::Quadword,
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        Operand::Register(Reg::R10)
                                    } else {
                                        op1
                                    };
                                    if is_mem_access(&op1) && is_mem_access(&op2) {
                                        new_instr.push(Instruction::Mov(
                                            asm_type.clone(),
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        new_instr.push(Instruction::Cmp(
                                            asm_type,
                                            Operand::Register(Reg::R10),
                                            op2,
                                        ));
                                    } else {
                                        match op2 {
                                            Operand::Immediate(c) => {
                                                new_instr.push(Instruction::Mov(
                                                    asm_type.clone(),
                                                    Operand::Immediate(c),
                                                    Operand::Register(Reg::R11),
                                                ));
                                                new_instr.push(Instruction::Cmp(
                                                    asm_type,
                                                    op1,
                                                    Operand::Register(Reg::R11),
                                                ));
                                            }
                                            _ => {
                                                new_instr.push(Instruction::Cmp(asm_type, op1, op2))
                                            }
                                        }
                                    }
                                }
                                Instruction::Idiv(asm_type, Operand::Immediate(c)) => {
                                    new_instr.push(Instruction::Mov(
                                        asm_type.clone(),
                                        Operand::Immediate(c),
                                        Operand::Register(Reg::R10),
                                    ));
                                    new_instr.push(Instruction::Idiv(
                                        asm_type,
                                        Operand::Register(Reg::R10),
                                    ));
                                }
                                Instruction::Binary(BinOp::Mul, asm_type, op1, op2) => {
                                    let op1 = if asm_type == AssemblyType::Quadword
                                        && is_not_int_range(&op1)
                                    {
                                        new_instr.push(Instruction::Mov(
                                            AssemblyType::Quadword,
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        Operand::Register(Reg::R10)
                                    } else {
                                        op1
                                    };
                                    if is_mem_access(&op2) {
                                        new_instr.push(Instruction::Mov(
                                            asm_type.clone(),
                                            op2.clone(),
                                            Operand::Register(Reg::R11),
                                        ));
                                        new_instr.push(Instruction::Binary(
                                            BinOp::Mul,
                                            asm_type.clone(),
                                            op1,
                                            Operand::Register(Reg::R11),
                                        ));
                                        new_instr.push(Instruction::Mov(
                                            asm_type,
                                            Operand::Register(Reg::R11),
                                            op2,
                                        ));
                                    }
                                }
                                Instruction::Binary(bin_op, asm_type, op1, op2) => {
                                    let op1 = if (bin_op == BinOp::Sub
                                        || bin_op == BinOp::Add
                                        || bin_op == BinOp::And
                                        || bin_op == BinOp::Or
                                        || bin_op == BinOp::Xor)
                                        && asm_type == AssemblyType::Quadword
                                        && is_not_int_range(&op1)
                                    {
                                        new_instr.push(Instruction::Mov(
                                            AssemblyType::Quadword,
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        Operand::Register(Reg::R10)
                                    } else {
                                        op1
                                    };
                                    if is_mem_access(&op1) && is_mem_access(&op2) {
                                        new_instr.push(Instruction::Mov(
                                            asm_type.clone(),
                                            op1,
                                            Operand::Register(Reg::R10),
                                        ));
                                        new_instr.push(Instruction::Binary(
                                            bin_op,
                                            asm_type,
                                            Operand::Register(Reg::R10),
                                            op2,
                                        ));
                                    } else {
                                        new_instr
                                            .push(Instruction::Binary(bin_op, asm_type, op1, op2));
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
                functions.push(parse_function(f, symbol_table)?);
            }
            Asm::Program(functions)
        }
    };

    asm = replace_pseudo(asm, symbol_table)?;

    asm = fix_mem_accesses(asm)?;

    Ok(asm)
}
