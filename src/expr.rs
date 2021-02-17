#[allow(clippy::large_enum_variant)]
use crate::environment::Environment;
use crate::error::runtime::RuntimeError;
use crate::interpreter::Interpreter;
use crate::mint_type::{MintInstance, MintType};
use crate::stmt::Stmt;
use crate::token::Token;
use crate::token_type::VarType;
use std::{
    hash::{Hash, Hasher},
    ptr::hash,
};
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
            ComparationOp::StrictNotEqual => write!(f, "!=="),
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
    StrictEqual,
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

fn print_attrs(user_instance: &MintInstance) -> String {
    let mut result = "".to_string();
    for (key, value) in &user_instance.fields {
        result.push_str(&format!("  \"{}\": {} \n", key, value));
    }
    result
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Boolean(bool) => write!(f, "{}", bool),
            Value::Number(num) => write!(f, "{}", num),
            Value::Str(string) => write!(f, "{}", string),
            Value::Fun(callee) => write!(f, "{}", callee),
            Value::Type(mint_type) => write!(f, "{}", mint_type.name.lexeme()),
            Value::TypeInstance(mint_instance) => {
                write!(f, "Object: {{\n{}}}", print_attrs(mint_instance))
            }
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
    Type(MintType),
    TypeInstance(MintInstance),
}

impl Default for Value {
    fn default() -> Self {
        Value::Null
    }
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Null | Value::Boolean(false) => false,
            Value::Number(n) => *n >= f64::EPSILON,
            _ => true,
        }
    }

    pub fn new_function(
        env: Environment<Value>,
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
    Instantiate(Token, Vec<(Token, Expr)>),
    Get(Box<Expr>, Token),
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
impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[function {}]", self.name.lexeme())
    }
}

impl Expr {
    pub fn get_token(&self) -> &Token {
        match self {
            Expr::Arithmetic(_, op_and_token, _) => &op_and_token.1,
            Expr::Comparation(_, op_and_token, _) => &op_and_token.1,
            Expr::Logical(_, op_and_token, _) => &op_and_token.1,
            Expr::Unary(op_and_token, _) => &op_and_token.1,
            Expr::Grouping(expr) => expr.get_token(),
            Expr::Literal(op_and_token) => &op_and_token.1,
            Expr::Variable(token, _) => &token,
            Expr::Call(callee, _) => callee.get_token(),
            Expr::Get(_, token) => &token,
            Expr::Instantiate(token, _) => &token,
        }
    }

    pub fn get_expr_placement(&self) -> (usize, usize) {
        match self {
            Expr::Arithmetic(l, _, r) => (l.get_expr_placement().0, r.get_expr_placement().1),
            Expr::Comparation(l, _, r) => (l.get_expr_placement().0, r.get_expr_placement().1),
            Expr::Logical(l, _, r) => (l.get_expr_placement().0, r.get_expr_placement().1),
            Expr::Unary(_, r) => {
                let (x, y) = r.get_expr_placement();
                (x - 1, y)
            }
            Expr::Grouping(expr) => {
                let (x, y) = expr.get_expr_placement();
                (x - 1, y + 1)
            }
            Expr::Literal(op_and_token) => (op_and_token.1.starts_at(), op_and_token.1.ends_at()),
            Expr::Variable(token, _) => (token.starts_at(), token.ends_at()),
            Expr::Call(callee, params) => {
                let (x, mut y) = callee.get_expr_placement();

                if !params.is_empty() {
                    let (_, new_y) = params.last().unwrap().get_expr_placement();

                    y = new_y;
                } else {
                    y += 1;
                }

                (x, y + 1)
            }
            Expr::Get(expr, _) => expr.get_expr_placement(),
            Expr::Instantiate(token, _) => (token.starts_at(), token.ends_at()),
        }
    }

    pub fn get_line(&self) -> usize {
        self.get_token().line()
    }

    pub fn placement(&self) -> (usize, usize, usize) {
        let (x, y) = self.get_expr_placement();
        (self.get_line(), x, y)
    }
}
#[derive(PartialEq, Clone, Debug)]
pub struct Callable {
    env: Environment<Value>,
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
    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn env(&self) -> &Environment<Value> {
        &self.env
    }

    pub fn params(&self) -> &Vec<(Token, VarType)> {
        &self.params
    }

    pub fn body(&self) -> &Vec<Stmt> {
        &self.body
    }

    pub fn return_type(&self) -> &VarType {
        &self.return_type
    }

    pub fn params_types(&self) -> Vec<VarType> {
        let mut ret = vec![];
        for (_, var_type) in &self.params {
            ret.push(var_type.clone());
        }
        ret
    }
}
