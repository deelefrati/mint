use crate::token::Token;

type OpWithToken<Op> = (Op, Token);

#[derive(Clone, PartialEq, Debug)]
pub enum UnaryOp {
    Minus,
    Plus,
    Bang,
}

#[derive(Clone, PartialEq, Debug)]
pub enum BinaryOp {
    Equal,
    TypedEqual,
    NotEqual,
    TypedNotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Null,
    Boolean(bool),
    Number(f64),
    Str(String),
}
#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Binary(Box<Expr>, OpWithToken<BinaryOp>, Box<Expr>),
    Unary(OpWithToken<UnaryOp>, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(OpWithToken<Value>),
    Logical(OpWithToken<LogicalOp>, Box<Expr>),
}
