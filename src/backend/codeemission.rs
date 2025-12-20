use crate::backend::codegen::*;
use std::fmt;

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
            Instruction::Binary(bin_op, operand1, operand2) => {
                write!(f, "{} {}, {}", bin_op, operand1, operand2)
            }
            Instruction::Idiv(operand) => write!(f, "idivl {}", operand),
            Instruction::Cdq => write!(f, "cdq"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Immediate(x) => write!(f, "${}", x),
            Operand::Register(reg) => write!(f, "%{}", reg),
            Operand::Pseudo(x) => write!(f, "PSEUDO {}", x),
            Operand::Stack(x) => write!(f, "-{}(%rbp)", x),
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reg::RAX => write!(f, "eax"),
            Reg::R10 => write!(f, "r10d"),
            Reg::RDX => write!(f, "edx"),
            Reg::R11 => write!(f, "r11d"),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "negl"),
            UnOp::Not => write!(f, "notl"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "addl"),
            BinOp::Sub => write!(f, "subl"),
            BinOp::Mul => write!(f, "imull"),
        }
    }
}
