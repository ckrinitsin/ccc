use ccc::backend::codegen::*;
use ccc::frontend::ir;

#[test]
fn simple_ir_to_asm() {
    let input = ir::TAC::Program(ir::Function::Function(
        "main".to_string(),
        vec![ir::Instruction::Ret(ir::Operand::Constant(2))],
    ));

    let result = gen_asm(input).unwrap();
    let expected = Asm::Program(Function::Function(
        "main".to_string(),
        vec![
            Instruction::Mov(Operand::Immediate(2), Operand::Register(Reg::RAX)),
            Instruction::Ret,
        ],
    ));

    assert_eq!(result, expected);
}

#[test]
fn replaces_pseudoregisters() {
    let input = ir::TAC::Program(ir::Function::Function(
        "main".to_string(),
        vec![
            ir::Instruction::Unary(
                ir::UnOp::Negation,
                ir::Operand::Constant(2),
                ir::Operand::Variable("tmp.0".to_string()),
            ),
            ir::Instruction::Unary(
                ir::UnOp::Complement,
                ir::Operand::Variable("tmp.0".to_string()),
                ir::Operand::Variable("tmp.1".to_string()),
            ),
            ir::Instruction::Unary(
                ir::UnOp::Negation,
                ir::Operand::Variable("tmp.1".to_string()),
                ir::Operand::Variable("tmp.2".to_string()),
            ),
            ir::Instruction::Ret(ir::Operand::Variable("tmp.2".to_string())),
        ],
    ));

    let result = gen_asm(input).unwrap();
    let expected = Asm::Program(Function::Function(
        "main".to_string(),
        vec![
            Instruction::AllocStack(12),
            Instruction::Mov(Operand::Immediate(2), Operand::Stack(4)),
            Instruction::Unary(UnOp::Neg, Operand::Stack(4)),
            Instruction::Mov(Operand::Stack(4), Operand::Register(Reg::R10)),
            Instruction::Mov(Operand::Register(Reg::R10), Operand::Stack(8)),
            Instruction::Unary(UnOp::Not, Operand::Stack(8)),
            Instruction::Mov(Operand::Stack(8), Operand::Register(Reg::R10)),
            Instruction::Mov(Operand::Register(Reg::R10), Operand::Stack(12)),
            Instruction::Unary(UnOp::Neg, Operand::Stack(12)),
            Instruction::Mov(Operand::Stack(12), Operand::Register(Reg::RAX)),
            Instruction::Ret,
        ],
    ));

    assert_eq!(result, expected);
}
