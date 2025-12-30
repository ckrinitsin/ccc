use crate::backend::codegen::*;
use std::fmt;

impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Asm::Program(x) => {
                for top_level in x {
                    write!(f, "{}\n", top_level)?;
                }
                write!(f, ".section .note.GNU-stack,\"\",@progbits")
            }
        }
    }
}

fn write_global(f: &mut fmt::Formatter, name: &String, global: bool) -> fmt::Result {
    if global {
        write!(f, "  .globl {}\n", name)?;
    }
    Ok(())
}

fn write_alignment(f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "  .align 4\n")
}

impl fmt::Display for TopLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TopLevel::Function(name, global, body) => {
                write_global(f, name, *global)?;
                write!(f, "  .text\n{}:\n  pushq %rbp\n  movq %rsp, %rbp\n", name)?;
                for i in body {
                    write!(f, "  {}\n", i)?;
                }
                Ok(())
            }
            TopLevel::StaticVariable(name, global, alignment, init) => {
                write_global(f, name, *global)?;
                match init {
                    crate::frontend::type_check::StaticInit::IntInit(0) => {
                        write!(f, "  .bss\n")?;
                        write!(f, "  .align {}\n", alignment)?;
                        write!(f, "{}:\n  .zero 4\n", name)
                    }
                    crate::frontend::type_check::StaticInit::LongInit(0) => {
                        write!(f, "  .bss\n")?;
                        write!(f, "  .align {}\n", alignment)?;
                        write!(f, "{}:\n  .zero 8\n", name)
                    }
                    crate::frontend::type_check::StaticInit::IntInit(x) => {
                        write!(f, "  .data\n")?;
                        write!(f, "  .align {}\n", alignment)?;
                        write!(f, "{}:\n  .long {}\n", name, x)
                    }
                    crate::frontend::type_check::StaticInit::LongInit(x) => {
                        write!(f, "  .data\n")?;
                        write!(f, "  .align {}\n", alignment)?;
                        write!(f, "{}:\n  .quad {}\n", name, x)
                    }
                }
            }
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Mov(t, src, dst) => write!(
                f,
                "mov{} {}, {}",
                t,
                get_typed_operand(t, src),
                get_typed_operand(t, dst)
            ),
            Instruction::Movsx(src, dst) => write!(
                f,
                "movslq {}, {}",
                get_lword_operand(src),
                get_qword_operand(dst)
            ),
            Instruction::Ret => write!(f, "movq %rbp, %rsp\n  popq %rbp\n  ret"),
            Instruction::Unary(un_op, t, operand) => {
                write!(f, "{}{} {}", un_op, t, get_typed_operand(t, operand))
            }
            Instruction::Binary(
                bin_op @ (BinOp::LShift | BinOp::RShift),
                t,
                operand1,
                operand2,
            ) => {
                write!(
                    f,
                    "{}{} {}, {}",
                    bin_op,
                    t,
                    get_byte_operand(operand1),
                    get_typed_operand(t, operand2)
                )
            }
            Instruction::Binary(bin_op, t, operand1, operand2) => {
                write!(
                    f,
                    "{}{} {}, {}",
                    bin_op,
                    t,
                    get_typed_operand(t, operand1),
                    get_typed_operand(t, operand2)
                )
            }
            Instruction::Idiv(t, operand) => {
                write!(f, "idiv{} {}", t, get_typed_operand(t, operand))
            }
            Instruction::Cdq(AssemblyType::Longword) => write!(f, "cdq"),
            Instruction::Cdq(AssemblyType::Quadword) => write!(f, "cqo"),
            Instruction::Cmp(t, op1, op2) => write!(
                f,
                "cmp{} {}, {}",
                t,
                get_typed_operand(t, op1),
                get_typed_operand(t, op2)
            ),
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

impl fmt::Display for AssemblyType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssemblyType::Longword => write!(f, "l"),
            AssemblyType::Quadword => write!(f, "q"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Immediate(x) => write!(f, "${}", x),
            Operand::Pseudo(x) => write!(f, "PSEUDO {}", x),
            Operand::Stack(x) => write!(f, "{}(%rbp)", x),
            Operand::Data(x) => write!(f, "{}(%rip)", x),
            Operand::Register(_) => Ok(()),
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
            Reg::RSP => "%spl".to_string(),
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
            Reg::RSP => "%rsp".to_string(),
            Reg::R8 => "%r8".to_string(),
            Reg::R9 => "%r9".to_string(),
            Reg::R10 => "%r10".to_string(),
            Reg::R11 => "%r11".to_string(),
        },
        x => x.to_string(),
    }
}

fn get_lword_operand(operand: &Operand) -> String {
    match operand {
        Operand::Register(reg) => match reg {
            Reg::RAX => "%eax".to_string(),
            Reg::RCX => "%ecx".to_string(),
            Reg::RDX => "%edx".to_string(),
            Reg::RDI => "%edi".to_string(),
            Reg::RSI => "%esi".to_string(),
            Reg::RSP => "%esp".to_string(),
            Reg::R8 => "%r8d".to_string(),
            Reg::R9 => "%r9d".to_string(),
            Reg::R10 => "%r10d".to_string(),
            Reg::R11 => "%r11d".to_string(),
        },
        x => x.to_string(),
    }
}

fn get_typed_operand(asm_type: &AssemblyType, operand: &Operand) -> String {
    match asm_type {
        AssemblyType::Longword => get_lword_operand(operand),
        AssemblyType::Quadword => get_qword_operand(operand),
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "neg"),
            UnOp::Not => write!(f, "not"),
            UnOp::Inc => write!(f, "inc"),
            UnOp::Dec => write!(f, "dec"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Sub => write!(f, "sub"),
            BinOp::Mul => write!(f, "imul"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
            BinOp::Xor => write!(f, "xor"),
            BinOp::LShift => write!(f, "sal"),
            BinOp::RShift => write!(f, "sar"),
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
