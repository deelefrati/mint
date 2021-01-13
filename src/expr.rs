use crate::environment::Environment;
use crate::error::RuntimeError;
use crate::interpreter::Interpreter;
use crate::stmt::Stmt;
use crate::token::Token;
use crate::token_type::VarType;
use std::hash::{Hash, Hasher};
use std::ptr::hash;
pub type OpWithToken<Op> = (Op, Token);

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Plus => write!(f, "+"),
            UnaryOp::Bang => write!(f, "!"),
        }
    }
}
#[derive(Clone, PartialEq, Debug, Copy, Hash)]
pub enum UnaryOp {
    Minus,
    Plus,
    Bang,
}

impl std::fmt::Display for ArithmeticOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArithmeticOp::Add => write!(f, "+"),
            ArithmeticOp::Sub => write!(f, "-"),
            ArithmeticOp::Mul => write!(f, "*"),
            ArithmeticOp::Div => write!(f, "/"),
            ArithmeticOp::Mod => write!(f, "%"),
        }
    }
}
#[derive(Clone, PartialEq, Debug, Copy, Hash)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl std::fmt::Display for ComparationOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparationOp::NotEqual => write!(f, "!="),
            ComparationOp::StrictNotEqual => write!(f, "!=="),
            ComparationOp::Equal => write!(f, "=="),
            ComparationOp::StrictEqual => write!(f, "==="),
            ComparationOp::LessThan => write!(f, "<"),
            ComparationOp::LessEqual => write!(f, "<="),
            ComparationOp::GreaterThan => write!(f, ">"),
            ComparationOp::GreaterEqual => write!(f, ">="),
        }
    }
}
#[derive(Clone, PartialEq, Debug, Copy, Hash)]
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

impl std::fmt::Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOp::And => write!(f, "&&"),
            LogicalOp::Or => write!(f, "||"),
        }
    }
}
#[derive(Clone, PartialEq, Debug, Copy, Hash)]
pub enum LogicalOp {
    And,
    Or,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Boolean(bool) => write!(f, "{}", bool),
            Value::Number(num) => write!(f, "{}", num),
            Value::Str(string) => write!(f, "{}", string),
            Value::Fun(_) => write!(f, "function"),
        }
    }
}
#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Null,
    Boolean(bool),
    Number(f64),
    Str(String),
    Fun(Callable),
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Null | Value::Boolean(false) => false,
            Value::Number(n) => *n <= f64::EPSILON,
            _ => true,
        }
    }

    pub fn new_function(
        env: Environment,
        name: Token,
        params: Vec<(Token, VarType)>,
        body: Vec<Stmt>,
        return_type: VarType,
    ) -> Value {
        Value::Fun(Callable {
            env,
            name,
            params,
            body,
            return_type,
        })
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Arithmetic(Box<Expr>, OpWithToken<ArithmeticOp>, Box<Expr>),
    Comparation(Box<Expr>, OpWithToken<ComparationOp>, Box<Expr>),
    Logical(Box<Expr>, OpWithToken<LogicalOp>, Box<Expr>),
    Unary(OpWithToken<UnaryOp>, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(OpWithToken<Value>),
    Variable(Token, String),
    Call(Box<Expr>, Vec<Expr>),
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

#[derive(PartialEq, Clone, Debug)]
pub struct Callable {
    env: Environment,
    name: Token,
    params: Vec<(Token, VarType)>,
    body: Vec<Stmt>,
    return_type: VarType,
}

impl Callable {
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        interpreter.eval_func(self, args)
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn env(&self) -> &Environment {
        &self.env
    }

    pub fn params(&self) -> &Vec<(Token, VarType)> {
        &self.params
    }

    pub fn body(&self) -> &Vec<Stmt> {
        &self.body
    }
}
