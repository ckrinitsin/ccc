pub mod ir_pretty_print;
pub mod lift;
use crate::frontend::{
    ast::{self, Type},
    semantic_analysis::type_check::StaticInit,
};

#[derive(Debug, PartialEq)]
pub enum TAC {
    Program(Vec<TopLevel>),
}

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Function(String, bool, Vec<String>, Vec<Instruction>),
    StaticVariable(
        String,
        bool,
        Type,
        StaticInit, /* name, global, init_value */
    ),
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Unary(UnOp, Operand, Operand),
    Binary(BinOp, Operand, Operand, Operand),
    Copy(Operand, Operand),
    Jump(String),
    JumpIfZero(Operand, String),
    JumpIfNotZero(Operand, String),
    Label(String),
    Ret(Operand),
    SignExtend(Operand, Operand),
    Truncate(Operand, Operand),
    FunctionCall(String, Vec<Operand>, Operand),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Constant(ast::Const),
    Variable(String),
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Complement,
    Negation,
    Not,
    Increment,
    Decrement,
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    And,
    Or,
    Xor,
    LShift,
    RShift,
    Equal,
    NEqual,
    Less,
    Greater,
    LessEq,
    GreaterEq,
}
