use ccc::codegen::{parse_ast, Asm, Function, Instruction, Operand};
use ccc::parser;

#[test]
fn valid1() {
    let input = parser::Ast::Program(parser::Function::Function(
        "main".to_string(),
        parser::Statement::Return(parser::Expression::Constant(2)),
    ));

    let result = parse_ast(input).unwrap();
    let expected = Asm::Program(Function::Function(
        "main".to_string(),
        vec![
            Instruction::Mov(Operand::Immediate(2), Operand::Register),
            Instruction::Ret,
        ],
    ));

    assert_eq!(result, expected);
}
