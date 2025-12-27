use ccc::frontend::ir::*;
use ccc::frontend::parse;

#[test]
fn simple_ast_to_ir() {
    let input = parse::Ast::Program(parse::Function::Function(
        "main".to_string(),
        parse::Block::B(vec![parse::BlockItem::S(parse::Statement::Return(
            parse::Expression::Constant(2),
        ))]),
    ));

    let result = lift_to_ir(input).unwrap();
    let expected = TAC::Program(Function::Function(
        "main".to_string(),
        vec![
            Instruction::Ret(Operand::Constant(2)),
            Instruction::Ret(Operand::Constant(0)),
        ],
    ));

    assert_eq!(result, expected);
}

#[test]
fn unop_to_ir() {
    let input = parse::Ast::Program(parse::Function::Function(
        "main".to_string(),
        parse::Block::B(vec![parse::BlockItem::S(parse::Statement::Return(
            parse::Expression::Unary(
                parse::UnaryOp::Complement,
                Box::new(parse::Expression::Constant(2)),
            ),
        ))]),
    ));

    let result = lift_to_ir(input).unwrap();
    let expected = TAC::Program(Function::Function(
        "main".to_string(),
        vec![
            Instruction::Unary(
                UnOp::Complement,
                Operand::Constant(2),
                Operand::Variable("tmp.0".to_string()),
            ),
            Instruction::Ret(Operand::Variable("tmp.0".to_string())),
            Instruction::Ret(Operand::Constant(0)),
        ],
    ));

    assert_eq!(result, expected);
}

#[test]
fn recursive_unop_to_ir() {
    let input = parse::Ast::Program(parse::Function::Function(
        "main".to_string(),
        parse::Block::B(vec![parse::BlockItem::S(parse::Statement::Return(
            parse::Expression::Unary(
                parse::UnaryOp::Negation,
                Box::new(parse::Expression::Unary(
                    parse::UnaryOp::Complement,
                    Box::new(parse::Expression::Unary(
                        parse::UnaryOp::Negation,
                        Box::new(parse::Expression::Constant(2)),
                    )),
                )),
            ),
        ))]),
    ));

    let result = lift_to_ir(input).unwrap();
    let expected = TAC::Program(Function::Function(
        "main".to_string(),
        vec![
            Instruction::Unary(
                UnOp::Negation,
                Operand::Constant(2),
                Operand::Variable("tmp.0".to_string()),
            ),
            Instruction::Unary(
                UnOp::Complement,
                Operand::Variable("tmp.0".to_string()),
                Operand::Variable("tmp.1".to_string()),
            ),
            Instruction::Unary(
                UnOp::Negation,
                Operand::Variable("tmp.1".to_string()),
                Operand::Variable("tmp.2".to_string()),
            ),
            Instruction::Ret(Operand::Variable("tmp.2".to_string())),
            Instruction::Ret(Operand::Constant(0)),
        ],
    ));

    assert_eq!(result, expected);
}
