use ccc::ir::{Function, Instruction, TAC, UnOp, Value, lift_to_ir};
use ccc::parser;

#[test]
fn simple_ast_to_ir() {
    let input = parser::Ast::Program(parser::Function::Function(
        "main".to_string(),
        parser::Statement::Return(parser::Expression::Constant(2)),
    ));

    let result = lift_to_ir(input).unwrap();
    let expected = TAC::Program(Function::Function(
        "main".to_string(),
        vec![Instruction::Ret(Value::Constant(2))],
    ));

    assert_eq!(result, expected);
}

#[test]
fn unop_to_ir() {
    let input = parser::Ast::Program(parser::Function::Function(
        "main".to_string(),
        parser::Statement::Return(parser::Expression::Unary(
            parser::UnaryOp::Complement,
            Box::new(parser::Expression::Constant(2)),
        )),
    ));

    let result = lift_to_ir(input).unwrap();
    let expected = TAC::Program(Function::Function(
        "main".to_string(),
        vec![
            Instruction::Unary(
                UnOp::Complement,
                Value::Constant(2),
                Value::Variable("tmp.0".to_string()),
            ),
            Instruction::Ret(Value::Variable("tmp.0".to_string())),
        ],
    ));

    assert_eq!(result, expected);
}

#[test]
fn recursive_unop_to_ir() {
    let input = parser::Ast::Program(parser::Function::Function(
        "main".to_string(),
        parser::Statement::Return(parser::Expression::Unary(
            parser::UnaryOp::Negation,
            Box::new(parser::Expression::Unary(
                parser::UnaryOp::Complement,
                Box::new(parser::Expression::Unary(
                    parser::UnaryOp::Negation,
                    Box::new(parser::Expression::Constant(2)),
                )),
            )),
        )),
    ));

    let result = lift_to_ir(input).unwrap();
    let expected = TAC::Program(Function::Function(
        "main".to_string(),
        vec![
            Instruction::Unary(
                UnOp::Negation,
                Value::Constant(2),
                Value::Variable("tmp.0".to_string()),
            ),
            Instruction::Unary(
                UnOp::Complement,
                Value::Variable("tmp.0".to_string()),
                Value::Variable("tmp.1".to_string()),
            ),
            Instruction::Unary(
                UnOp::Negation,
                Value::Variable("tmp.1".to_string()),
                Value::Variable("tmp.2".to_string()),
            ),
            Instruction::Ret(Value::Variable("tmp.2".to_string())),
        ],
    ));

    assert_eq!(result, expected);
}
