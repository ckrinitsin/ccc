use crate::frontend::ast;
use crate::frontend::ir::{BinOp, Instruction, Operand, StaticInit, TAC, TopLevel, UnOp};
use std::fmt;

// TODO: rewrite display
impl fmt::Display for TAC {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TAC::Program(functions) => {
                for func in functions {
                    write!(f, "{}\n", func)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for TopLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TopLevel::Function(name, _, params, body) => {
                write!(f, "{} {:?}:\n", name, params)?;
                for i in body {
                    write!(f, "  {}\n", i)?;
                }
                Ok(())
            }
            TopLevel::StaticVariable(name, _, _, val) => write!(f, "static {} = {}", name, val),
        }
    }
}

impl fmt::Display for StaticInit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StaticInit::IntInit(x) => write!(f, "{}", x),
            StaticInit::LongInit(x) => write!(f, "{}", x),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Unary(op, src, dst) => write!(f, "{} = {}{}", dst, op, src),
            Instruction::Binary(op, src1, src2, dst) => {
                write!(f, "{} = {} {} {}", dst, src1, op, src2)
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
            Instruction::FunctionCall(name, params, dst) => {
                write!(f, "{} = Call({}, {:?})", dst, name, params)
            }
            Instruction::SignExtend(src, dst) => write!(f, "{} =s.e. {}", dst, src),
            Instruction::Truncate(src, dst) => write!(f, "{} =t. {}", dst, src),
        }
    }
}

impl fmt::Display for ast::Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ast::Const::Int(x) => write!(f, "{}", x),
            ast::Const::Long(x) => write!(f, "{}", x),
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
