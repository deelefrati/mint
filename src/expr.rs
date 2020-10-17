use crate::token::Token;
use std::hash::{Hash, Hasher};
use std::ptr::hash;

type OpWithToken<Op> = (Op, Token);

#[derive(Clone, PartialEq, Debug)]
pub enum UnaryOp {
    Minus,
    Plus,
    Bang,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ComparationOp {
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
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
    Arithmetic(Box<Expr>, OpWithToken<ArithmeticOp>, Box<Expr>),
    Comparation(Box<Expr>, OpWithToken<ComparationOp>, Box<Expr>),
    Logical(Box<Expr>, OpWithToken<LogicalOp>, Box<Expr>),
    Unary(OpWithToken<UnaryOp>, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(OpWithToken<Value>),
}

impl Hash for &Expr {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: Hasher,
    {
        hash(self, hasher)
    }
}

impl Eq for &Expr {}
