#[derive(Debug, PartialEq)]
pub enum Ast {
    Program(Vec<Declaration>),
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    V(VariableDeclaration),
    F(FunctionDeclaration),
}

#[derive(Debug, PartialEq)]
pub enum Block {
    B(Vec<BlockItem>),
}

#[derive(Debug, PartialEq)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug, PartialEq)]
pub enum VariableDeclaration {
    D(String, Option<Expression>, Type, Option<StorageClass>),
}

#[derive(Debug, PartialEq)]
pub enum FunctionDeclaration {
    D(
        String,
        Vec<String>,
        Option<Block>,
        Type,
        Option<StorageClass>,
    ),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Long,
    Function(
        Vec<Box<Type>>, /* arg types */
        Box<Type>,      /* ret type */
    ),
}

#[derive(Debug, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>, Option<String>),
    DoWhile(Box<Statement>, Expression, Option<String>),
    For(
        ForInit,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
        Option<String>,
    ),
    Switch(
        Expression,
        Box<Statement>,
        Option<String>,
        Vec<(Option<Const>, String)>,
    ),
    Default(Box<Statement>, Option<String>),
    Case(Expression, Box<Statement>, Option<String>),
    Continue(Option<String>),
    Break(Option<String>),
    Compound(Block),
    Labeled(String, Box<Statement>),
    Goto(String),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum ForInit {
    D(VariableDeclaration),
    E(Option<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Constant(Const, Option<Type>),
    Variable(String, Option<Type>),
    Cast(Type, Box<Expression>, Option<Type>), /* target type; expression to cast */
    Unary(UnaryOp, Box<Expression>, Option<Type>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>, Option<Type>),
    Assignment(Box<Expression>, Box<Expression>, Option<Type>),
    CompoundAssignment(BinaryOp, Box<Expression>, Box<Expression>, Option<Type>),
    PostIncr(Box<Expression>, Option<Type>),
    PostDecr(Box<Expression>, Option<Type>),
    Conditional(
        Box<Expression>,
        Box<Expression>,
        Box<Expression>,
        Option<Type>,
    ),
    FunctionCall(String, Vec<Box<Expression>>, Option<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Const {
    Int(i32),
    Long(i64),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negation,
    Not,
    Increment,
    Decrement,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
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
    LAnd,
    LOr,
    Equal,
    NEqual,
    Less,
    Greater,
    LessEq,
    GreaterEq,

    Assignment,
}
