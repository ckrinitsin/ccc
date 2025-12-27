use crate::backend::codegen::*;
use std::fmt;

impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Asm::Program(x) => {
                for func in x {
                    write!(f, "{}\n", func)?;
                }
                write!(f, ".section .note.GNU-stack,\"\",@progbits")
            }
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Function(name, body) => {
                write!(
                    f,
                    "  .globl {}\n{}:\n  pushq %rbp\n  movq %rsp, %rbp\n",
                    name, name
                )?;
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
            Instruction::Ret => write!(f, "movq %rbp, %rsp\n  popq %rbp\n  ret"),
            Instruction::Unary(un_op, operand) => write!(f, "{} {}", un_op, operand),
            Instruction::AllocStack(x) => write!(f, "subq ${}, %rsp", x),
            Instruction::DeallocStack(x) => write!(f, "addq ${}, %rsp", x),
            Instruction::Binary(bin_op @ (BinOp::LShift | BinOp::RShift), operand1, operand2) => {
                write!(f, "{} {}, {}", bin_op, get_byte_operand(operand1), operand2)
            }
            Instruction::Binary(bin_op, operand1, operand2) => {
                write!(f, "{} {}, {}", bin_op, operand1, operand2)
            }
            Instruction::Idiv(operand) => write!(f, "idivl {}", operand),
            Instruction::Cdq => write!(f, "cdq"),
            Instruction::Cmp(op1, op2) => write!(f, "cmpl {}, {}", op1, op2),
            Instruction::Jump(label) => write!(f, "jmp .L{}", label),
            Instruction::JmpCC(condition, label) => write!(f, "j{} .L{}", condition, label),
            Instruction::SetCC(condition, operand) => {
                write!(f, "set{} {}", condition, get_byte_operand(operand))
            }
            Instruction::Label(label) => write!(f, ".L{}:", label),
            Instruction::Push(operand) => write!(f, "pushq {}", get_qword_operand(operand)),
            Instruction::Call(func) => write!(f, "call {}@PLT", func),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Immediate(x) => write!(f, "${}", x),
            Operand::Register(reg) => write!(f, "%{}", reg),
            Operand::Pseudo(x) => write!(f, "PSEUDO {}", x),
            Operand::Stack(x) => write!(f, "{}(%rbp)", x),
        }
    }
}

fn get_byte_operand(operand: &Operand) -> String {
    match operand {
        Operand::Register(reg) => match reg {
            Reg::RAX => "%al".to_string(),
            Reg::RCX => "%cl".to_string(),
            Reg::RDX => "%dl".to_string(),
            Reg::RDI => "%dil".to_string(),
            Reg::RSI => "%sil".to_string(),
            Reg::R8 => "%r8b".to_string(),
            Reg::R9 => "%r9b".to_string(),
            Reg::R10 => "%r10b".to_string(),
            Reg::R11 => "%r11b".to_string(),
        },
        x => x.to_string(),
    }
}

fn get_qword_operand(operand: &Operand) -> String {
    match operand {
        Operand::Register(reg) => match reg {
            Reg::RAX => "%rax".to_string(),
            Reg::RCX => "%rcx".to_string(),
            Reg::RDX => "%rdx".to_string(),
            Reg::RDI => "%rdi".to_string(),
            Reg::RSI => "%rsi".to_string(),
            Reg::R8 => "%r8".to_string(),
            Reg::R9 => "%r9".to_string(),
            Reg::R10 => "%r10".to_string(),
            Reg::R11 => "%r11".to_string(),
        },
        x => x.to_string(),
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reg::RAX => write!(f, "eax"),
            Reg::RCX => write!(f, "ecx"),
            Reg::RDX => write!(f, "edx"),
            Reg::RDI => write!(f, "edi"),
            Reg::RSI => write!(f, "esi"),
            Reg::R8 => write!(f, "r8d"),
            Reg::R9 => write!(f, "r9d"),
            Reg::R10 => write!(f, "r10d"),
            Reg::R11 => write!(f, "r11d"),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "negl"),
            UnOp::Not => write!(f, "notl"),
            UnOp::Inc => write!(f, "incl"),
            UnOp::Dec => write!(f, "decl"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "addl"),
            BinOp::Sub => write!(f, "subl"),
            BinOp::Mul => write!(f, "imull"),
            BinOp::And => write!(f, "andl"),
            BinOp::Or => write!(f, "orl"),
            BinOp::Xor => write!(f, "xorl"),
            BinOp::LShift => write!(f, "sall"),
            BinOp::RShift => write!(f, "sarl"),
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Condition::E => write!(f, "e"),
            Condition::NE => write!(f, "ne"),
            Condition::G => write!(f, "g"),
            Condition::GE => write!(f, "ge"),
            Condition::L => write!(f, "l"),
            Condition::LE => write!(f, "le"),
        }
    }
}
